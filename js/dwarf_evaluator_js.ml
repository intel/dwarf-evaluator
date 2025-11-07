open Dwarf_evaluator_sexp
open Js_of_ocaml
open Dom_html
open Sexplib
open Printf

(* Shorthand for coerced getElementById with assertion *)
let get id coerce_to = Option.get (getElementById_coerce id coerce_to)

(* Get the non-empty, non-comment part of line, or None if it doesn't exist *)
let noncomment_part line =
  List.nth_opt (String.split_on_char ';' line) 0
  |> Option.map String.trim
  |> function | Some "" -> None | s -> s

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

let span ?(cl="") children =
  let s = Dom_html.createSpan Dom_html.window##.document in
  if cl != "" then s##.classList##add (Js.string cl);
  children |> List.iter (Dom.appendChild s);
  s

let text s = Dom_html.window##.document##createTextNode (Js.string s)
let text_of_int i = text (string_of_int i)

let setChildren elem children =
  elem##.innerHTML := Js.string "";
  List.iter (Dom.appendChild elem) children

let rec elem_of_stack_element element =
  let children = match element with
  | Val v -> [span ~cl:"stack_element_kind" [text "Val"]; elem_of_value v]
  | Loc l -> [span ~cl:"stack_element_kind" [text "Loc"]; elem_of_location l]
  in
  span ~cl:"stack_element" children
and elem_of_value v = span ~cl:"value" [text_of_int v]
and elem_of_location (storage, offset) =
  span ~cl:"location" [
    span ~cl:"location_offset" [text_of_int offset];
    span ~cl:"location_storage" [elem_of_storage storage]
  ]
and elem_of_storage storage =
  match storage with
  | Mem aspace -> span ~cl:"storage" [text (sprintf "Mem(aspace=%d)" aspace)]
  | Reg number -> span ~cl:"storage" [text (sprintf "Reg(%d)" number)]
  | Undefined -> span ~cl:"storage" [text "Undef"]
  | ImpData data ->
      span ~cl:"storage" [text (String.escaped data |> sprintf {|Implicit(value="%s")|})]
  | ImpPointer loc -> span ~cl:"storage" [
      span [text "Implicit(pointer="];
      elem_of_location loc;
      span [text ")"]
    ]
  | Composite parts ->
      let sorted_parts = List.sort (fun (s1, _, _) (s2, _, _) -> s1 - s2) parts in
      span [
        span ~cl:"storage" [text "Composite"];
        span ~cl:"composite_parts" (List.map elem_of_part sorted_parts)
      ]
and elem_of_part (s, e, loc) =
  span ~cl:"composite_part" [
    span ~cl:"composite_part_start" [text_of_int s];
    span ~cl:"composite_part_end" [text_of_int e];
    elem_of_location loc
  ]

let rec build_output_elems context ops =
  build_output_elems_impl [] context ops []
and build_output_elems_impl result context ops stack =
  let (ops', stack', context') = Dwarf_evaluator.eval_one ops stack context in
  let result' = result @ [span ~cl:"trace_step" [
      span ~cl:"trace_step_op" [text (List.hd ops |> sexp_of_dwarf_op |> Sexp.to_string_hum)];
      span ~cl:"trace_step_stack" (List.map elem_of_stack_element stack')
    ]]
  in
  match ops' with
  | [] -> result' @ [span ~cl:"trace_result" [elem_of_stack_element (List.hd stack')]]
  | _ -> build_output_elems_impl result' context' ops' stack'

let _ =
  let context = get "context" CoerceTo.textarea in
  let input = get "input" CoerceTo.textarea in
  let eval = get "eval" CoerceTo.button in
  let preprocessed = get "preprocessed" CoerceTo.element in
  let output = get "output" CoerceTo.element in
  let arguments = Url.Current.arguments in
  let initial_context = List.assoc_opt "context" arguments |> Option.value ~default:"" in
  let initial_input = List.assoc_opt "input" arguments |> Option.value ~default:"" in
  context##.innerHTML := (Js.string initial_context);
  input##.innerHTML := (Js.string initial_input);
  let render _ =
    let context_sexp_string = Js.to_string context##.value in
    let locexpr_sexp_string = preprocess (Js.to_string input##.value) in
    let output_children =
      try
        let ctx = try
          context_t_of_sexp (Parsexp.Single.parse_string_exn context_sexp_string)
        with e -> failwith (sprintf "Parsing Context: %s" (Printexc.to_string e)) in
        let locexpr = try
          locexpr_t_of_sexp (Parsexp.Single.parse_string_exn locexpr_sexp_string)
        with e -> failwith (sprintf "Parsing Input: %s" (Printexc.to_string e)) in
        build_output_elems ctx locexpr
      with e -> [span [text (Printexc.to_string e)]] in
    setChildren preprocessed [span [text locexpr_sexp_string]];
    setChildren output output_children;
    Js._true in
  ignore (addEventListener eval Event.click (handler render) Js._false)
