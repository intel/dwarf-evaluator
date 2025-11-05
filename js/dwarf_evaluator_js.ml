open Dwarf_evaluator_sexp
open Js_of_ocaml
open Dom_html
open Sexplib
open Printf

(* Shorthand for coerced getElementById with assertion *)
let get id coerce_to = Option.get (getElementById_coerce id coerce_to)

let amp_re = Str.regexp {|&|}
let lt_re = Str.regexp {|<|}
let gt_re = Str.regexp {|>|}
let quot_re = Str.regexp {|"|}
let squot_re = Str.regexp {|'|}
(* FIXME: Using a typed representation of HTML while building could avoid
   the need for this, but for now we explicitly escape strings that may contain
   valid HTML controlled by the user. *)
let escapeHTML s = s
  |> Str.global_replace amp_re "&amp;"
  |> Str.global_replace lt_re "&lt;"
  |> Str.global_replace gt_re "&gt;"
  |> Str.global_replace quot_re "&quot;"
  |> Str.global_replace squot_re "&#039;"

(* Get the non-empty, non-comment part of line, or None if it doesn't exist *)
let noncomment_part line =
  List.nth_opt (String.split_on_char ';' line) 0
  |> Option.map String.trim
  |> function | Some "" -> None | s -> s

let context_re = Str.regexp ") ("
let context_default = (Sexp.to_string_hum (sexp_of_context_t Dwarf_evaluator.context))
  |> Str.global_replace context_re ")\n ("
let input_default =
"DW_OP_reg4 ; edit me!
DW_OP_lit1
DW_OP_offset
DW_OP_deref
DW_OP_stack_value"

(*
  Heuristically "Sexp-ify" the input.

  Goal: reduce noise for the common case by allowing the user to elide parens
  around non-0-ary constructors, and around the top-level list of op
  constructors.

  Approach:

    First, remove comments.

    Then, if the remaining input contains no parenthesis, surround any lines containing spaces with parens.

    Finally, surround everything in a top-level set of parens.

  Note: To avoid ambiguity in the case of all 0-ary constructors this means we
  need to require each constructor and its arguments appear on one line, and
  that each constructors be on a different line.
 *)
let preprocess input =
  let uncommented_lines = input
    |> String.split_on_char '\n'
    |> List.filter_map noncomment_part in
  let has_parens = uncommented_lines
    |> List.exists (fun line -> let c = String.contains line in c '(' || c ')') in
  if has_parens then
    uncommented_lines
    |> String.concat "\n"
  else
    uncommented_lines
    |> List.map (fun s -> if String.contains s ' ' then "(" ^ s ^ ")" else s)
    |> String.concat "\n"
    |> (fun s -> "(" ^ s ^ ")")

let span klass inner_html =
  sprintf {|<span class="%s">%s</span>|} klass inner_html

let rec html_of_stack_element elem =
  let html = match elem with
  | Val v ->
      span "stack_element_kind" "Val" ^
      html_of_value v
  | Loc l ->
      span "stack_element_kind" "Loc" ^
      html_of_location l
  in
  span "stack_element" html
and html_of_value v =
  span "value" (string_of_int v)
and html_of_location (storage, offset) =
  span "location_offset" (string_of_int offset) ^
  span "location_storage" (html_of_storage storage)
and html_of_storage storage =
  match storage with
  | Mem aspace -> span "storage" (sprintf "Mem(aspace=%d)" aspace)
  | Reg number -> span "storage" (sprintf "Reg(%d)" number)
  | Undefined -> span "storage" "Undef"
  | ImpData data ->
      span "storage" (String.escaped data |> escapeHTML |> sprintf {|Implicit(value="%s")|})
  | ImpPointer loc -> span "storage" (sprintf "Implicit(pointer=%s)" (html_of_location loc))
  | Composite parts ->
      let sorted_parts = List.sort (fun (s1, _, _) (s2, _, _) -> s1 - s2) parts in
      span "storage" "Composite" ^
      span "composite_parts" (List.fold_left (fun acc p -> acc ^ html_of_part p) "" sorted_parts)
and html_of_part (s, e, loc) =
  span "composite_part" (
    span "composite_part_start" (string_of_int s) ^
    span "composite_part_end" (string_of_int e) ^
    html_of_location loc
  )

let rec build_output_html context ops =
  build_output_html_impl "" context ops []
and build_output_html_impl result context ops stack =
  let (ops', stack', context') = Dwarf_evaluator.eval_one ops stack context in
  let result' = result ^
    span "trace_step" (
      span "trace_step_op" (Sexp.to_string_hum (sexp_of_dwarf_op (List.hd ops))) ^
      span "trace_step_stack" (List.fold_left (fun acc e -> acc ^ html_of_stack_element e) "" stack')
    )
  in
  match ops' with
  | [] -> result' ^ span "trace_result" (html_of_location (Dwarf_evaluator.as_loc (List.hd stack')))
  | _ -> build_output_html_impl result' context' ops' stack'

let _ =
  let context = get "context" CoerceTo.textarea in
  let input = get "input" CoerceTo.textarea in
  let eval = get "eval" CoerceTo.button in
  let preprocessed = get "preprocessed" CoerceTo.element in
  let output = get "output" CoerceTo.element in
  let arguments = Url.Current.arguments in
  let initial_context = List.assoc_opt "context" arguments |> Option.value ~default:context_default in
  let initial_input = List.assoc_opt "input" arguments |> Option.value ~default:input_default in
  context##.innerHTML := (Js.string initial_context);
  input##.innerHTML := (Js.string initial_input);
  let render _ =
    let context_sexp_string = Js.to_string context##.value in
    let locexpr_sexp_string = preprocess (Js.to_string input##.value) in
    let output_html =
      try
        let ctx = (context_t_of_sexp (Parsexp.Single.parse_string_exn context_sexp_string)) in
        let locexpr = (locexpr_t_of_sexp (Parsexp.Single.parse_string_exn locexpr_sexp_string)) in
        build_output_html ctx locexpr
      with e -> Printexc.to_string e |> escapeHTML in
    preprocessed##.innerHTML := locexpr_sexp_string |> escapeHTML |> Js.string;
    (* FIXME: This cannot be passed through escapeHTML as we have built up
     an HTML document as a string. *)
    output##.innerHTML := Js.string output_html;
    Js._true in
  ignore (addEventListener eval Event.click (handler render) Js._false)
