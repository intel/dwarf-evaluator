(* Copyright (C) 2025 Intel Corporation
   SPDX-License-Identifier: MIT *)

(* A simplified implementation of DWARF specification for locations on
   stack.

   For simplicity, all sizes are in bytes; data access is at the
   granularity of bytes.  *)

(* Data are a sequence of bytes.  *)
type data = string

(* Evaluation context.
   The consumer provides the evaluation context.  *)
type context_item =
  | TargetMem of int * data   (* Address space, contents.  *)
  | TargetReg of int * data   (* Register num, contents.  *)
  | Lane of int               (* Selected lane.  *)
  | Object of location        (* Current object.  *)

(* Virtual storage.  *)
and storage =
  | Mem of int (* Address space.  *)
  | Reg of int (* Register number.  *)
  | Undefined
  | ImpData of data (* Implicit data.  *)
  | ImpPointer of location (* Location of the pointed-to object.  *)
  | Composite of (int * int * location) list (* Parts of the composite.  *)

(* Location is an offset into a storage.  *)
and location = storage * int

(* Context accessors for convenience.  *)
let rec mem_data context addr_space =
  match context with
  | [] -> failwith "memory not found in context"
  | TargetMem(sp, data)::context' when sp = addr_space -> data
  | _::context' -> mem_data context' addr_space

let rec reg_data context num =
  match context with
  | [] -> failwith "register not found in context"
  | TargetReg(n, data)::context' when n = num -> data
  | _::context' -> reg_data context' num

let rec lane context =
  match context with
  (* DWARF spec states "If the current program is not using a
     SIMD/SIMT execution model, the current lane is always 0."
     Therefore, return 0 if the lane is not explicitly specified in
     the context.  *)
  | [] -> 0
  | Lane(n)::context' -> n
  | _::context' -> lane context'

let rec objekt context =
  match context with
  | [] -> failwith "object not found in context"
  | Object(loc)::context' -> loc
  | _::context' -> objekt context'


(* Element kinds for the DWARF expression evaluation stack.
   A stack is simply a list of stack elements.  *)
type stack_element =
  | Val of int
  | Loc of location

type dwarf_op =
  | DW_OP_const4s of int
  | DW_OP_lit0  | DW_OP_lit1  | DW_OP_lit2  | DW_OP_lit3
  | DW_OP_lit4  | DW_OP_lit5  | DW_OP_lit6  | DW_OP_lit7
  | DW_OP_lit8  | DW_OP_lit9  | DW_OP_lit10 | DW_OP_lit11
  | DW_OP_lit12 | DW_OP_lit13 | DW_OP_lit14 | DW_OP_lit15
  | DW_OP_lit16 | DW_OP_lit17 | DW_OP_lit18 | DW_OP_lit19
  | DW_OP_lit20 | DW_OP_lit21 | DW_OP_lit22 | DW_OP_lit23
  | DW_OP_lit24 | DW_OP_lit25 | DW_OP_lit26 | DW_OP_lit27
  | DW_OP_lit28 | DW_OP_lit29 | DW_OP_lit30 | DW_OP_lit31

  | DW_OP_plus
  | DW_OP_mul
  | DW_OP_dup
  | DW_OP_drop
  | DW_OP_pick of int
  | DW_OP_over
  | DW_OP_swap
  | DW_OP_rot
  | DW_OP_push_lane
  | DW_OP_regval of int

  | DW_OP_lt
  | DW_OP_eq
  | DW_OP_skip of int (* Number of operators to skip.  *)
  | DW_OP_bra of int (* Number of operators to skip.  *)
  | DW_OP_call of (dwarf_op list)

  | DW_OP_addr of int
  | DW_OP_regx of int
  | DW_OP_reg0  | DW_OP_reg1  | DW_OP_reg2  | DW_OP_reg3
  | DW_OP_reg4  | DW_OP_reg5  | DW_OP_reg6  | DW_OP_reg7
  | DW_OP_reg8  | DW_OP_reg9  | DW_OP_reg10 | DW_OP_reg11
  | DW_OP_reg12 | DW_OP_reg13 | DW_OP_reg14 | DW_OP_reg15
  | DW_OP_reg16 | DW_OP_reg17 | DW_OP_reg18 | DW_OP_reg19
  | DW_OP_reg20 | DW_OP_reg21 | DW_OP_reg22 | DW_OP_reg23
  | DW_OP_reg24 | DW_OP_reg25 | DW_OP_reg26 | DW_OP_reg27
  | DW_OP_reg28 | DW_OP_reg29 | DW_OP_reg30 | DW_OP_reg31

  | DW_OP_breg of int * int
  | DW_OP_undefined
  | DW_OP_implicit_value of int * data
  | DW_OP_stack_value
  | DW_OP_implicit_pointer of (dwarf_op list) * int
  | DW_OP_composite
  | DW_OP_piece of int
  | DW_OP_overlay
  | DW_OP_push_object_location

  | DW_OP_deref
  | DW_OP_offset

(* What is the size of a virtual storage?  *)
let data_size storage context =
  match storage with
  | Mem(addr_space) -> Int.max_int
  | Reg(n) -> String.length (reg_data context n)
  | Undefined -> Int.max_int
  | ImpData(data) -> String.length data
  | ImpPointer(pointee_loc) ->
     (* Size of an implicit pointer storage is the size
        of the pointer that was optimized away.  In this
        implementation, we assume pointers are 32b.  *)
     4
  | Composite(parts) -> (* The largest "end" marker in the parts.  *)
     List.fold_left (fun max (s, e, loc) -> if e > max then e else max) 0 parts

(* Error kinds.  *)
exception NotImplementedYet
exception OutOfBounds of location
exception UndefinedData of int
exception ReadOnlyData of storage

(* Utility operations.  *)

(* Find the part in a composite that contains the given offset.
   Return the location and the adjusted offset.  *)
let find_part parts offset : location =
  let part = List.find (fun (s, e, loc) -> s <= offset && offset < e) parts
  in let (s, e, (part_storage, part_offset)) = part
     in (part_storage, (part_offset + offset - s))

(* Remove parts that don't cover any data.
   Merge consecutive parts that retrieve data from the same storage.  *)
let simplify parts =
  let filter acc (s, e, loc) =
    if s < e then (s, e, loc)::acc else acc
  in
  let merge acc (s, e, (st, off)) =
    match acc with
    | [] -> [(s, e, (st, off))]
    | (s', e', (st', off'))::acc' ->
       if e == s' && st == st' && off' == s' then
         (s, e', (st, off))::acc'
       else
         (s, e, (st, off))::acc
  in
  let filtered = List.fold_left filter [] parts in
  let merged = List.fold_left merge [] filtered in
  List.rev merged

let rec read_one_byte context (location: location) =
  let (storage, offset) = location
  in if offset >= (data_size storage context) then
       raise (OutOfBounds location)
     else
       match storage with
       | Mem(addr_space) -> String.get (mem_data context addr_space) offset
       | Reg(n) -> String.get (reg_data context n) offset
       | Undefined -> raise (UndefinedData(offset))
       | ImpData(data) -> String.get data offset
       | ImpPointer(_, _) -> raise (UndefinedData(offset))
       | Composite(parts) -> read_one_byte context (find_part parts offset)

let fetch_data context (loc: location) length =
  let (storage, offset) = loc
  in String.init length (fun n -> read_one_byte context (storage, (offset + n)))

let fetch_int context (loc: location) =
  Int32.to_int (String.get_int32_ne (fetch_data context loc 4) 0)

let int_to_data n =
  let data = Bytes.create 4
  in Bytes.set_int32_ne data 0 (Int32.of_int n);
     String.of_bytes data

let ints_to_data ns =
  let data = Bytes.create (4 * List.length ns)
  in List.iteri (fun i n -> Bytes.set_int32_ne data (i * 4) (Int32.of_int n)) ns;
     String.of_bytes data

(* Discard n elements from the head of the given list.  *)
let rec discard n lst =
  if n == 0 then lst
  else discard (n - 1) (List.tl lst)

exception ConversionError of string * stack_element
exception EvalError of string * dwarf_op * (stack_element list)

let eval_error op stack =
  raise (EvalError("Cannot evaluate", op, stack))

(* Implicit conversion rules.  *)
let as_value element =
  match element with
  | Val(i) -> i
  | Loc(Mem(0), address) -> address
  | _ -> raise (ConversionError("Cannot convert to val", element))

let as_loc element =
  match element with
  | Loc(loc) -> loc
  | Val(i) -> (Mem 0, i)

(* Evaluate a single DWARF operator using the given stack.  *)
let rec eval op stack context =
  match op with
  | DW_OP_const4s(x) -> Val(x)::stack

  | DW_OP_lit0  -> Val(0)::stack
  | DW_OP_lit1  -> Val(1)::stack
  | DW_OP_lit2  -> Val(2)::stack
  | DW_OP_lit3  -> Val(3)::stack
  | DW_OP_lit4  -> Val(4)::stack
  | DW_OP_lit5  -> Val(5)::stack
  | DW_OP_lit6  -> Val(6)::stack
  | DW_OP_lit7  -> Val(7)::stack
  | DW_OP_lit8  -> Val(8)::stack
  | DW_OP_lit9  -> Val(9)::stack
  | DW_OP_lit10 -> Val(10)::stack
  | DW_OP_lit11 -> Val(11)::stack
  | DW_OP_lit12 -> Val(12)::stack
  | DW_OP_lit13 -> Val(13)::stack
  | DW_OP_lit14 -> Val(14)::stack
  | DW_OP_lit15 -> Val(15)::stack
  | DW_OP_lit16 -> Val(16)::stack
  | DW_OP_lit17 -> Val(17)::stack
  | DW_OP_lit18 -> Val(18)::stack
  | DW_OP_lit19 -> Val(19)::stack
  | DW_OP_lit20 -> Val(20)::stack
  | DW_OP_lit21 -> Val(21)::stack
  | DW_OP_lit22 -> Val(22)::stack
  | DW_OP_lit23 -> Val(23)::stack
  | DW_OP_lit24 -> Val(24)::stack
  | DW_OP_lit25 -> Val(25)::stack
  | DW_OP_lit26 -> Val(26)::stack
  | DW_OP_lit27 -> Val(27)::stack
  | DW_OP_lit28 -> Val(28)::stack
  | DW_OP_lit29 -> Val(29)::stack
  | DW_OP_lit30 -> Val(30)::stack
  | DW_OP_lit31 -> Val(31)::stack

  | DW_OP_plus ->
     (match stack with
      | e1::e2::stack' -> Val((as_value e1) + (as_value e2))::stack'
      | _ -> eval_error op stack)

  | DW_OP_mul ->
     (match stack with
      | e1::e2::stack' -> Val((as_value e1) * (as_value e2))::stack'
      | _ -> eval_error op stack)

  | DW_OP_dup ->
     (match stack with
      | e1::stack' -> e1::e1::stack'
      | _ -> eval_error op stack)

  | DW_OP_drop ->
     (match stack with
      | e1::stack' -> stack'
      | _ -> eval_error op stack)

  | DW_OP_pick(i) ->
     if i >= List.length stack then
       eval_error op stack
     else
       (List.nth stack i)::stack

  | DW_OP_over ->
     (match stack with
      | e1::e2::stack' -> e2::e1::e2::stack'
      | _ -> eval_error op stack)

  | DW_OP_swap ->
     (match stack with
      | e1::e2::stack' -> e2::e1::stack'
      | _ -> eval_error op stack)

  | DW_OP_rot ->
     (match stack with
      | e1::e2::e3::stack' -> e2::e3::e1::stack'
      | _ -> eval_error op stack)

  | DW_OP_push_lane -> Val(lane context)::stack

  | DW_OP_regval(r) ->
     (* This is a simplified version of DW_OP_regval_type
        where the type is implicitly an integer.  *)
     let data = reg_data context r in
     let as_int = Int32.to_int (String.get_int32_ne data 0) in
     Val(as_int)::stack

  | DW_OP_lt ->
     (match stack with
      | e1::e2::stack' ->
         if (as_value e2) < (as_value e1) then
           Val(1)::stack'
         else
           Val(0)::stack'
      | _ -> eval_error op stack)

  | DW_OP_eq ->
     (match stack with
      | e1::e2::stack' ->
         if (as_value e2) == (as_value e1) then
           Val(1)::stack'
         else
           Val(0)::stack'
      | _ -> eval_error op stack)

  | DW_OP_call(ops) ->
     eval_all ops stack context

  | DW_OP_addr(a) -> Loc(Mem 0, a)::stack

  | DW_OP_regx(n) -> Loc(Reg n, 0)::stack
  | DW_OP_reg0  -> Loc(Reg 0, 0)::stack
  | DW_OP_reg1  -> Loc(Reg 1, 0)::stack
  | DW_OP_reg2  -> Loc(Reg 2, 0)::stack
  | DW_OP_reg3  -> Loc(Reg 3, 0)::stack
  | DW_OP_reg4  -> Loc(Reg 4, 0)::stack
  | DW_OP_reg5  -> Loc(Reg 5, 0)::stack
  | DW_OP_reg6  -> Loc(Reg 6, 0)::stack
  | DW_OP_reg7  -> Loc(Reg 7, 0)::stack
  | DW_OP_reg8  -> Loc(Reg 8, 0)::stack
  | DW_OP_reg9  -> Loc(Reg 9, 0)::stack
  | DW_OP_reg10 -> Loc(Reg 10, 0)::stack
  | DW_OP_reg11 -> Loc(Reg 11, 0)::stack
  | DW_OP_reg12 -> Loc(Reg 12, 0)::stack
  | DW_OP_reg13 -> Loc(Reg 13, 0)::stack
  | DW_OP_reg14 -> Loc(Reg 14, 0)::stack
  | DW_OP_reg15 -> Loc(Reg 15, 0)::stack
  | DW_OP_reg16 -> Loc(Reg 16, 0)::stack
  | DW_OP_reg17 -> Loc(Reg 17, 0)::stack
  | DW_OP_reg18 -> Loc(Reg 18, 0)::stack
  | DW_OP_reg19 -> Loc(Reg 19, 0)::stack
  | DW_OP_reg20 -> Loc(Reg 20, 0)::stack
  | DW_OP_reg21 -> Loc(Reg 21, 0)::stack
  | DW_OP_reg22 -> Loc(Reg 22, 0)::stack
  | DW_OP_reg23 -> Loc(Reg 23, 0)::stack
  | DW_OP_reg24 -> Loc(Reg 24, 0)::stack
  | DW_OP_reg25 -> Loc(Reg 25, 0)::stack
  | DW_OP_reg26 -> Loc(Reg 26, 0)::stack
  | DW_OP_reg27 -> Loc(Reg 27, 0)::stack
  | DW_OP_reg28 -> Loc(Reg 28, 0)::stack
  | DW_OP_reg29 -> Loc(Reg 29, 0)::stack
  | DW_OP_reg30 -> Loc(Reg 30, 0)::stack
  | DW_OP_reg31 -> Loc(Reg 31, 0)::stack

  | DW_OP_breg(n, offset) ->
     let reg_contents = fetch_int context ((Reg n), 0)
     in let address = reg_contents + offset
        in Loc(Mem 0, address)::stack

  | DW_OP_undefined -> Loc(Undefined, 0)::stack

  | DW_OP_implicit_value(n, data) ->
     if String.length data == n then
       Loc(ImpData data, 0)::stack
     else
       eval_error op stack

  | DW_OP_stack_value ->
     (match stack with
      | e::stack' ->
         let data = int_to_data (as_value e)
         in Loc(ImpData data, 0)::stack'
      | _ -> eval_error op stack)

  | DW_OP_implicit_pointer(locexpr, offset) ->
     (match eval_all locexpr [] context with
      | result::_ ->
         let (storage, offset2) = as_loc result
         in Loc(ImpPointer(storage, offset2 + offset), 0)::stack
      | _ -> eval_error op stack)

  | DW_OP_composite -> Loc(Composite [], 0)::stack

  | DW_OP_piece(n) ->
     (match stack with
      | element::Loc(Composite(parts), off)::stack' ->
         let loc = as_loc element
         in let new_part = (match parts with
                            | [] -> (0, n, loc)
                            | (s, e, _)::_ -> (e, e + n, loc))
            in Loc(Composite(new_part::parts), off)::stack'

      | _ -> eval_error op stack)

  | DW_OP_push_object_location -> Loc(objekt context)::stack

  | DW_OP_overlay ->
     (match stack with
      | el1::el2::el3::el4::stack' ->
         let width = as_value el1 in
         let offset = as_value el2 in
         let overlay_loc = as_loc el3 in
         let (o_storage, o_offset) = overlay_loc in
         let base_loc = as_loc el4 in
         let (b_storage, b_offset) = base_loc in
         let b_storage_size = data_size b_storage context in
         let o_storage_size = data_size o_storage context in
         let overlay_start = offset + b_offset in
         if (width < 0
             || overlay_start < 0
             || width > o_storage_size - o_offset) then
           eval_error op stack
         else
           (* There are 4 kinds of parts that may occur in the
              resulting composite.  Although not all kinds will end
              up existing in the end result, to make the definition
              easier, we define all of them and then do elimination at
              the end.

              1. The part of the base storage up to the overlay.  If
              the overlay is beyond the base storage limits, this
              parts goes up to the end of the base storage.

              2. The expansion with undefined storage from the end of
              the base until the beginning of the overlay,
              when the overlay is to the right of the base.

              3. The overlay itself.

              4. The remaining data from the base storage up to its end.  *)

           let overlay_start = offset + b_offset in
           let overlay_end = overlay_start + width in
           let part1_end = Int.min overlay_start b_storage_size in
           let part1 = (0, part1_end, (b_storage, 0)) in
           let part2 = (part1_end, overlay_start, (Undefined, 0)) in
           let part3 = (overlay_start, overlay_end, overlay_loc) in
           let part4_loc = (b_storage, overlay_end) in
           let part4 = (overlay_end, b_storage_size, part4_loc) in
           let parts = simplify [part1; part2; part3; part4] in
           Loc(Composite parts, b_offset)::stack'

      | _ -> eval_error op stack)

  | DW_OP_deref ->
     (match stack with
      | element::stack' ->
         Val(fetch_int context (as_loc element))::stack'

      | _ -> eval_error op stack)

  | DW_OP_offset ->
     (match stack with
      | displacement::location::stack' ->
         let (storage, offset) = as_loc location
         in let new_offset = offset + (as_value displacement)
            in if (new_offset >= (data_size storage context)) then
                 raise (OutOfBounds (storage, offset))
               else
                 Loc(storage, new_offset)::stack'
      | _ -> eval_error op stack)

  (* Handled in the upper level.  *)
  | DW_OP_skip(n) | DW_OP_bra(n) -> stack

(* Evaluate the given list of DWARF operators using the given stack.  *)
and eval_all ops stack context =
  match ops with
  | [] -> stack

  | DW_OP_skip(n)::ops' ->
     (* DW_OP_skip is a control flow operator that requires access to
        the complete DWARF expression to be able skip a number of
        operators.  Hence, handle it here.  Without loss of
        generality, we support skipping forward only.  *)
     eval_all (discard n ops') stack context

  | DW_OP_bra(n)::ops' ->
     (match stack with
      | v::stack' ->
         if as_value v == 0 then
           eval_all ops' stack' context
         else
           eval_all (discard n ops') stack' context
      | _ -> eval_error (DW_OP_bra(n)) stack)

  | op::ops' -> eval_all ops' (eval op stack context) context

(* Evaluate the given list of DWARF operators using an initially empty
   stack, return the top element.  *)
let eval0 ops context =
  List.hd (eval_all ops [] context)

let eval_to_loc ops context =
  as_loc (eval0 ops context)

(**************)
(* Examples.  *)
(**************)

(* Consumer utility functions.  *)
(* ... *ptr ... *)
let rec dbg_deref (loc: location) context =
  match loc with
  | (ImpPointer(p_loc), 0) -> fetch_int context p_loc
  (* TODO: Handle the case when the data to fetch expands to multiple parts.  *)
  | (Composite parts, offset) -> dbg_deref (find_part parts offset) context
  | _ -> let address = fetch_int context loc
         in fetch_int context (Mem 0, address)

(* ... &x ... *)
let dbg_addr_of (loc: location) =
  match loc with
  | (Mem _, offset) -> offset
  | _ -> failwith "Cannot get address of that."

(* Sample contexts for testing purposes.  *)
let empty = []

let mem_contents =
  String.concat ""
    [(ints_to_data [100; 104; 108; 112; 116; 120]);
     "01234567XXXXCDEF"; (* Starting at offset 24. *)
    ]

let context = [TargetMem(0, mem_contents);
               TargetReg(0, int_to_data 1000);
               TargetReg(1, int_to_data 1001);
               TargetReg(2, int_to_data 1002);
               TargetReg(3, int_to_data 1003);
               TargetReg(4, ints_to_data [400; 401; 402; 403; 404; 405; 406; 407]);
               TargetReg(5, int_to_data 4); (* Pointer to memory #4.  *)
               TargetReg(6, "89AB");
               TargetReg(7, ints_to_data [700; 701; 702; 703; 704; 705; 706; 707]);
              ]

let num_pass = ref 0
let num_fail = ref 0
let test value expectation message =
  let result =
    if value = expectation then
      (num_pass := !num_pass + 1; "Pass")
    else
      (num_fail := !num_fail + 1; "FAIL")
  in
    Printf.printf "%s: %s\n" result message

(* Expect an evaluation error.  *)
let test_error lambda message =
  try
    let _ = lambda () in
    test 1 0 message
  with
  | EvalError _  -> test 1 1 message

(* Simple stack operations.  *)
let _ =
  test (eval_all [DW_OP_const4s 9;
                  DW_OP_const4s 5] [] context) [Val 5; Val 9] "DW_OP_const"

let _ =
  test (eval_all [DW_OP_lit9;
                  DW_OP_lit5;
                  DW_OP_dup] [] context) [Val 5; Val 5; Val 9] "DW_OP_dup"

let _ =
  test (eval_all [DW_OP_lit9;
                  DW_OP_lit5;
                  DW_OP_drop] [] context) [Val 9] "DW_OP_drop"

let _ =
  test (eval_all [DW_OP_lit9;
                  DW_OP_lit5;
                  DW_OP_lit3;
                  DW_OP_pick 2] [] context) [Val 9; Val 3; Val 5; Val 9] "DW_OP_pick"

let _ =
  test (eval_all [DW_OP_lit9;
                  DW_OP_lit5;
                  DW_OP_over] [] context) [Val 9; Val 5; Val 9] "DW_OP_over"

let _ =
  test (eval_all [DW_OP_lit9;
                  DW_OP_lit5;
                  DW_OP_swap] [] context) [Val 9; Val 5] "DW_OP_swap"

let _ =
  test (eval_all [DW_OP_lit3;
                  DW_OP_lit2;
                  DW_OP_lit1;
                  DW_OP_rot] [] context) [Val 2; Val 3; Val 1] "DW_OP_rot"

let _ =
  test (eval_all [DW_OP_lit0;
                  DW_OP_lit1;
                  DW_OP_lit2;
                  DW_OP_lit3;
                  DW_OP_lit4;
                  DW_OP_lit5;
                  DW_OP_lit10;
                  DW_OP_lit15;
                  DW_OP_lit31] [] context)
    [Val 31; Val 15; Val 10; Val 5; Val 4; Val 3; Val 2; Val 1; Val 0] "DW_OP_lit"

let _ =
  test (eval_all [DW_OP_push_lane] [] [Lane 5]) [Val 5] "DW_OP_push_lane"

let _ =
  test (eval_all [DW_OP_regval 2] [] context) [Val 1002] "DW_OP_regval"

let _ =
  test (eval_all [DW_OP_regx 123] [] context) [Loc(Reg 123, 0)] "DW_OP_regx"

(* Simple arithmethic exp test.  *)
let _ =
  test (eval0 [DW_OP_lit9;
               DW_OP_lit5;
               DW_OP_plus;
               DW_OP_lit3;
               DW_OP_mul] context)
    (Val 42)
    "arithmetic expr"

(* Relational operators.  *)
let _ =
  test (eval0 [DW_OP_lit9;
               DW_OP_lit5;
               DW_OP_lt] context) (Val 0) "DW_OP_lt 1"
let _ =
  test (eval0 [DW_OP_lit5;
               DW_OP_lit9;
               DW_OP_lt] context) (Val 1) "DW_OP_lt 2"
let _ =
  test (eval0 [DW_OP_lit9;
               DW_OP_lit5;
               DW_OP_eq] context) (Val 0) "DW_OP_eq 1"
let _ =
  test (eval0 [DW_OP_lit9;
               DW_OP_lit9;
               DW_OP_eq] context) (Val 1) "DW_OP_eq 2"

(* Control flow.  *)
let _ =
  test (eval0 [DW_OP_lit15;
               DW_OP_lit25;
               DW_OP_eq;
               DW_OP_bra 4;
               DW_OP_lit2;
               DW_OP_lit3;
               DW_OP_mul;
               DW_OP_skip 3;
               DW_OP_lit4;
               DW_OP_lit5;
               DW_OP_plus] context) (Val 6) "control flow 1"
let _ =
  test (eval0 [DW_OP_lit15;
               DW_OP_lit15;
               DW_OP_eq;
               DW_OP_bra 4;
               DW_OP_lit2;
               DW_OP_lit3;
               DW_OP_mul;
               DW_OP_skip 3;
               DW_OP_lit4;
               DW_OP_lit5;
               DW_OP_plus] context) (Val 9) "control flow 2"
let _ =
  test (eval0 [DW_OP_lit17;
               DW_OP_lit25;
               DW_OP_call [DW_OP_plus]] context) (Val 42) "DW_OP_call 1"
let _ =
  test (eval0 [DW_OP_lit17;
               DW_OP_lit25;
               DW_OP_call [DW_OP_plus];
               DW_OP_lit8;
               DW_OP_plus] context) (Val 50) "DW_OP_call 2"

(* x is an integer in memory.  *)
let _ =
  let x_address = 4 in
  let x_locexpr = [DW_OP_addr x_address] in
  let x_loc = eval_to_loc x_locexpr context in
  let x_val = fetch_int context x_loc in
  let addr_of_x = dbg_addr_of x_loc in
  test x_val 104 "value of x";
  test addr_of_x 4 "address of x"

(* y is an integer in register 1.  *)
let _ =
  let y_locexpr = [DW_OP_reg1] in
  let y_loc = eval_to_loc y_locexpr context in
  let y_val = fetch_int context y_loc in
  test y_val 1001 "value of y"

(* p is a pointer to x and is located in register 5.  *)
let _ =
  let p_locexpr = [DW_OP_reg5] in
  let p_loc = eval_to_loc p_locexpr context in
  let p_val = fetch_int context p_loc in
  let p_deref_val = dbg_deref p_loc context in
  test p_val 4 "value of p";
  test p_deref_val 104 "value of *p"

(* Use DW_OP_regval and deref p.  This also tests implicit conversion
   from a value to a memory location.  *)
let _ =
  let pointee_expr = [DW_OP_regval 5; DW_OP_deref] in
  let pointee_val = eval0 pointee_expr context in
  test pointee_val (Val 104) "DW_OP_deref a DW_OP_regval"

(* Use DW_OP_reg and double deref p for the same as above.  *)
let _ =
  let pointee_expr = [DW_OP_reg5; DW_OP_deref; DW_OP_deref] in
  let pointee_val = eval0 pointee_expr context in
  test pointee_val (Val 104) "double DW_OP_deref a DW_OP_reg"

(* ip is an implicit pointer to x.  We can deref, but we cannot
   read/write ip.  *)
let _ =
  let ip_locexpr = [DW_OP_implicit_pointer ([DW_OP_addr 4], 0)] in
  let ip_loc = eval_to_loc ip_locexpr context in
  let ip_deref_val = dbg_deref ip_loc context in
  test ip_deref_val 104 "value of *ip, pointing to memory"

(* ip is an implicit pointer to a variable that has been promoted to
   register 3.  *)
let _ =
  let ip_locexpr = [DW_OP_implicit_pointer ([DW_OP_reg3], 0)] in
  let ip_loc = eval_to_loc ip_locexpr context in
  let ip_deref_val = dbg_deref ip_loc context in
  test ip_deref_val 1003 "value of *ip, pointing to register"

(* v is a vectorized integer in register 4.  *)
let _ =
  let v_locexpr = [DW_OP_reg4;
                   DW_OP_push_lane;
                   DW_OP_lit4;
                   DW_OP_mul;
                   DW_OP_offset] in
  let v_loc = eval_to_loc v_locexpr (Lane(3)::context) in
  let v_val = fetch_int (Lane(3)::context) v_loc in
  test v_val 403 "value of v in lane 3";
  let v_loc = eval_to_loc v_locexpr (Lane(5)::context) in
  let v_val = fetch_int (Lane(5)::context) v_loc in
  test v_val 405 "value of v in lane 5"

(* q is an implicit value.  *)
let _ =
  let q_locexpr = [DW_OP_implicit_value (4, int_to_data 42)] in
  let q_loc = eval_to_loc q_locexpr empty in
  let q_val = fetch_int empty q_loc in
  test q_val 42 "implicit value"

(* q is a value computed in the DWARF stack.  *)
let _ =
  let q_locexpr = [DW_OP_lit14;
                   DW_OP_lit3;
                   DW_OP_mul;
                   DW_OP_stack_value] in
  let q_loc = eval_to_loc q_locexpr empty in
  let q_val = fetch_int empty q_loc in
  test q_val 42 "stack value"

(* z is located 12 bytes away from p's pointee.  *)
let _ =
  let z_locexpr = [DW_OP_breg(5, 12)] in
  let z_loc = eval_to_loc z_locexpr context in
  let z_addr = dbg_addr_of z_loc in
  let z_val = fetch_int context z_loc in
  test z_addr 16 "address of z";
  test z_val 116 "value of z"

(* Another approach for the same thing.  *)
let _ =
  let z_locexpr = [DW_OP_addr 0;
                   DW_OP_reg5;
                   DW_OP_deref;
                   DW_OP_offset;
                   DW_OP_lit12;
                   DW_OP_offset] in
  let z_loc = eval_to_loc z_locexpr context in
  let z_addr = dbg_addr_of z_loc in
  let z_val = fetch_int context z_loc in
  test z_addr 16 "address of z, take 2";
  test z_val 116 "value of z, take 2"

(* Suppose we have a struct as follows:

   struct {
     int m; // Located in memory at address 20.
     int *ptr; // Implicit pointer to "x" above.
     int r2; // Located in register 2.
     int r3; // Located in register 3.
     int d; // Implicit data known to be 333;
   } s;
*)

let _ =
  let s_locexpr = [DW_OP_composite;
                   DW_OP_addr 20;
                   DW_OP_piece 4;
                   DW_OP_implicit_pointer ([DW_OP_addr 4], 0);
                   DW_OP_piece 4;
                   DW_OP_reg2;
                   DW_OP_piece 4;
                   DW_OP_reg3;
                   DW_OP_piece 4;
                   DW_OP_implicit_value (4, int_to_data 333);
                   DW_OP_piece 4] in
  let s_loc = eval_to_loc s_locexpr context in
  (* ... s.m ... *)
  let s_m_locexpr = s_locexpr @ [DW_OP_lit0; DW_OP_offset] in
  let s_m_val = fetch_int context (eval_to_loc s_m_locexpr context) in
  test s_m_val 120 "value of s.m";
  (* ... s.r2 ... *)
  let s_r2_locexpr = s_locexpr @ [DW_OP_lit8; DW_OP_offset] in
  let s_r2_val = fetch_int context (eval_to_loc s_r2_locexpr context) in
  test s_r2_val 1002 "value of s.r2";
  (* ... s.r3 ... *)
  let s_r3_locexpr = s_locexpr @ [DW_OP_lit12; DW_OP_offset] in
  let s_r3_val = fetch_int context (eval_to_loc s_r3_locexpr context) in
  test s_r3_val 1003 "value of s.r3";
  (* ... s.d ... *)
  let s_d_locexpr = s_locexpr @ [DW_OP_lit16; DW_OP_offset] in
  let s_d_val = fetch_int context (eval_to_loc s_d_locexpr context) in
  test s_d_val 333 "value of s.d";
  (* ... *s.ptr ... *)
  let s_ptr_locexpr = s_locexpr @ [DW_OP_lit4; DW_OP_offset] in
  let s_ptr_loc = eval_to_loc s_ptr_locexpr context in
  let s_ptr_deref_val = dbg_deref s_ptr_loc context in
  test s_ptr_deref_val 104 "value of *s.ptr";
  (* ... this.r3 ...  *)
  let r3_data_member_locexpr = [DW_OP_push_object_location;
                                DW_OP_lit12;
                                DW_OP_offset] in
  let r3_data_member_loc = eval_to_loc r3_data_member_locexpr (Object(s_loc)::context) in
  let r3_data_member_val = fetch_int context r3_data_member_loc in
  test r3_data_member_val 1003 ".r3 with an object context"

(* An array, whose values are "0123456789ABCDEF".  The elements "89AB"
   are in register 6, the others are in the memory.  This example
   illustrates promotion of array elements to a vector register.  *)
let _ =
  let array_locexpr = [DW_OP_composite;
                       DW_OP_addr 24;
                       DW_OP_piece 8;
                       DW_OP_reg6;
                       DW_OP_piece 4;
                       DW_OP_addr (24 + 12);
                       DW_OP_piece 4] in
  let (storage, offset) = eval_to_loc array_locexpr context in
  let array_element i =
    fetch_data context (storage, offset + i) 1
  in
  test (array_element 0) "0" "array[0] in composite";
  test (array_element 3) "3" "array[3] in composite";
  test (array_element 7) "7" "array[7] in composite";
  test (array_element 8) "8" "array[8] in composite";
  test (array_element 9) "9" "array[9] in composite";
  test (array_element 10) "A" "array[10] in composite";
  test (array_element 11) "B" "array[11] in composite";
  test (array_element 12) "C" "array[12] in composite";
  test (array_element 15) "F" "array[15] in composite";
  ()

(* The same array example with an overlay.

  b_offset :    v
  b_storage: |..01234567XXXXCDEF...

  offset   :    |------>
  width    :           |----|
  o_offset :            v
  o_storage:           |89AB|

  c_offset :    v
  composite: |..0123456789ABCDEF...

 *)
let _ =
  let overlay_locexpr = [DW_OP_addr 24;
                         DW_OP_reg6;
                         DW_OP_lit8;
                         DW_OP_lit4;
                         DW_OP_overlay] in
  let (storage, offset) = eval_to_loc overlay_locexpr context in
  let array_element i =
    fetch_data context (storage, offset + i) 1
  in
  test (array_element 0) "0" "array[0] in overlay composite";
  test (array_element 3) "3" "array[3] in overlay composite";
  test (array_element 7) "7" "array[7] in overlay composite";
  test (array_element 8) "8" "array[8] in overlay composite";
  test (array_element 9) "9" "array[9] in overlay composite";
  test (array_element 10) "A" "array[10] in overlay composite";
  test (array_element 11) "B" "array[11] in overlay composite";
  test (array_element 12) "C" "array[12] in overlay composite";
  test (array_element 15) "F" "array[15] in overlay composite";
  ()

(* Overlay tests.  *)
let reg_size = data_size (Reg 1) context
let vreg_size = data_size (Reg 4) context

(*
  b_offset : 3       v
  b_storage:     |bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb|

  offset   : 5       |--->
  width    : 6           |------|

  o_offset : 7            v
  o_storage:      |yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy|

  c_offset :         v
  composite:     |bbbbbbbbyyyyyybbbbbbbbbbbbbbbbbb|
 *)
let _ =
  let overlay_locexpr = [DW_OP_reg4; DW_OP_lit3; DW_OP_offset;
                         DW_OP_reg7; DW_OP_lit7; DW_OP_offset;
                         DW_OP_lit5;
                         DW_OP_lit6;
                         DW_OP_overlay] in
  let overlay_loc = eval_to_loc overlay_locexpr context in
  test overlay_loc
    (Composite [(8 + 6, vreg_size, (Reg 4, 8 + 6));
                (8, 8 + 6, (Reg 7, 7));
                (0, 8, (Reg 4, 0))], 3)
    "overlay: base bigger than overlay"

(*
  b_offset : 3       v
  b_storage:     |bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb|

  offset   : 23      |--------------------->
  width    : 6                             |------|

  o_offset : 7                              v
  o_storage:                        |yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy|

  c_offset :         v
  composite:     |bbbbbbbbbbbbbbbbbbbbbbbbbbyyyyyy|
 *)
let _ =
  let overlay_locexpr = [DW_OP_reg4; DW_OP_lit3; DW_OP_offset;
                         DW_OP_reg7; DW_OP_lit7; DW_OP_offset;
                         DW_OP_lit23;
                         DW_OP_lit6;
                         DW_OP_overlay] in
  let overlay_loc = eval_to_loc overlay_locexpr context in
  test overlay_loc
    (Composite [(26, 32, (Reg 7, 7));
                (0, 26, (Reg 4, 0))], 3)
    "overlay: overlay ends at base's end"

(*
  b_offset : 0         v
  b_storage:          |bbbb|

  offset   : 0         |
  width    : 13       |-------------|

  o_offset : 7         v
  o_storage:   |yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy|

  c_offset :           v
  composite:          |yyyyyyyyyyyyy|
 *)
let _ =
  let width = 13 in
  let overlay_locexpr = [DW_OP_reg3;
                         DW_OP_reg4; DW_OP_lit7; DW_OP_offset;
                         DW_OP_lit0;
                         DW_OP_const4s width;
                         DW_OP_overlay] in
  let overlay_loc = eval_to_loc overlay_locexpr context in
  test overlay_loc
    (Composite [(0, width, (Reg 4, 7))], 0)
    "overlay: overlay bigger than base"

(*
  b_offset : 0   v
  b_storage:    |bbbb|

  offset   : 0   |
  width    : 4  |----|

  o_offset : 0   v
  o_storage:    |yyyy|

  c_offset :     v
  composite:    |yyyy|
 *)
let _ =
  let overlay_locexpr = [DW_OP_reg1;
                         DW_OP_reg2;
                         DW_OP_lit0;
                         DW_OP_const4s reg_size;
                         DW_OP_overlay] in
  let overlay_loc = eval_to_loc overlay_locexpr context in
  test overlay_loc
    (Composite [(0, reg_size, (Reg 2, 0))], 0)
    "overlay: base perfectly covered by overlay"

(*
  b_offset : 3       v
  b_storage:     |bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb|

  offset   : 31      |----------------------------->
  width    : 6                                     |------|

  o_offset : 7                                      v
  o_storage:                                |yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy|

  c_offset :         v
  composite:     |bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbUUyyyyyy|
 *)
let _ =
  let overlay_locexpr = [DW_OP_reg4; DW_OP_lit3; DW_OP_offset;
                         DW_OP_reg7; DW_OP_lit7; DW_OP_offset;
                         DW_OP_lit31;
                         DW_OP_lit6;
                         DW_OP_overlay] in
  let overlay_loc = eval_to_loc overlay_locexpr context in
  test overlay_loc
    (Composite [(34, 40, (Reg 7, 7));
                (32, 34, (Undefined, 0));
                (0, 32, (Reg 4, 0))], 3)
    "overlay: overlay after base with gap"

(*
  b_offset : 3       v
  b_storage:     |bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb|

  offset   : 29      |--------------------------->
  width    : 6                                   |------|

  o_offset : 7                                    v
  o_storage:                              |yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy|

  c_offset :         v
  composite:     |bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbyyyyyy|
 *)
let _ =
  let overlay_locexpr = [DW_OP_reg4; DW_OP_lit3; DW_OP_offset;
                         DW_OP_reg7; DW_OP_lit7; DW_OP_offset;
                         DW_OP_lit29;
                         DW_OP_lit6;
                         DW_OP_overlay] in
  let overlay_loc = eval_to_loc overlay_locexpr context in
  test overlay_loc
    (Composite [(32, 38, (Reg 7, 7));
                (0, 32, (Reg 4, 0))], 3)
    "overlay: overlay after base with zero gap";
  test (data_size (fst overlay_loc) context) (vreg_size + 6)
    "overlay: size of composite"

(*
  b_offset : 3       v
  b_storage:     |bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb|

  offset   : 26      |------------------------>
  width    : 6                                |------|

  o_offset : 7                                 v
  o_storage:                           |yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy|

  c_offset :         v
  composite:     |bbbbbbbbbbbbbbbbbbbbbbbbbbbbbyyyyyy|
 *)
let _ =
  let overlay_locexpr = [DW_OP_reg4; DW_OP_lit3; DW_OP_offset;
                         DW_OP_reg7; DW_OP_lit7; DW_OP_offset;
                         DW_OP_lit26;
                         DW_OP_lit6;
                         DW_OP_overlay] in
  let overlay_loc = eval_to_loc overlay_locexpr context in
  test overlay_loc
    (Composite [(29, 35, (Reg 7, 7));
                (0, 29, (Reg 4, 0))], 3)
    "overlay: registers with overlap"

(*
  b_offset : 3       v
  b_storage:     |bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb|

  offset   : 26      |------------------------>
  width    : 0                                ||

  o_offset : 7                                 v
  o_storage:                           |yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy|

  c_offset :         v
  composite:     |bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb|
 *)
let _ =
  let overlay_locexpr = [DW_OP_reg4; DW_OP_lit3; DW_OP_offset;
                         DW_OP_reg7; DW_OP_lit7; DW_OP_offset;
                         DW_OP_lit26;
                         DW_OP_lit0;
                         DW_OP_overlay] in
  let overlay_loc = eval_to_loc overlay_locexpr context in
  test overlay_loc
    (Composite [(0, 32, (Reg 4, 0))], 3)
    "overlay: width is nil"

(*
  b_offset : 0   v
  b_storage:    |bbbb|

  offset   : 0   |
  width    : 5  |-----|

  o_offset : 0   v
  o_storage:    |yyyy|

  composite:    N/A
 *)
let _ =
  let overlay_locexpr = [DW_OP_reg1;
                         DW_OP_reg2;
                         DW_OP_lit0;
                         DW_OP_const4s (reg_size + 1);
                         DW_OP_overlay] in
  test_error (fun () -> eval_to_loc overlay_locexpr context)
    "overlay: width is larger than overlay"

(*
  b_offset : 0   v
  b_storage:    |bbbb|

  offset   : 0   |
  width    : 5  |-----|

  o_offset : 2   v
  o_storage:  |yyyy|

  composite:    N/A
 *)
let _ =
  let overlay_locexpr = [DW_OP_reg1;
                         DW_OP_reg2; DW_OP_lit2; DW_OP_offset;
                         DW_OP_lit0;
                         DW_OP_const4s (reg_size - 1);
                         DW_OP_overlay] in
  test_error (fun () -> eval_to_loc overlay_locexpr context)
    "overlay: width is larger than overlay, non-zero overlay offset"

(*
  b_offset : 0   v
  b_storage:    ||

  offset   : 0   |
  width    : 4  |----|

  o_offset : 0   v
  o_storage:    |yyyy|

  c_offset :     v
  composite:    |yyyy|
 *)
let _ =
  let overlay_locexpr = [DW_OP_composite;
                         DW_OP_reg2;
                         DW_OP_lit0;
                         DW_OP_const4s reg_size;
                         DW_OP_overlay] in
  let overlay_loc = eval_to_loc overlay_locexpr context in
  test overlay_loc
    (Composite [(0, reg_size, (Reg 2, 0))], 0)
    "overlay: on empty base"

(*
  b_offset : 0   v
  b_storage:    ||

  offset   : 3   |->
  width    : 4     |----|

  o_offset :        v
  o_storage:       |yyyy|

  c_offset :     v
  composite:    |UUUyyyy|
 *)
let _ =
  let overlay_locexpr = [DW_OP_composite;
                         DW_OP_reg2;
                         DW_OP_lit3;
                         DW_OP_const4s reg_size;
                         DW_OP_overlay] in
  let overlay_loc = eval_to_loc overlay_locexpr context in
  test overlay_loc
    (Composite [(3, reg_size + 3, (Reg 2, 0));
                (0, 3, (Undefined, 0))], 0)
    "overlay: after empty base"

(*
  b_offset : 23                          v
  b_storage:     |bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb|

  offset   : -10               <---------|
  width    : 6                |------|

  o_offset : 7                 v
  o_storage:           |yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy|

  c_offset :                             v
  composite:     |bbbbbbbbbbbbbyyyyyybbbbbbbbbbbbb|
 *)
let _ =
  let overlay_locexpr = [DW_OP_reg4; DW_OP_lit23; DW_OP_offset;
                         DW_OP_reg7; DW_OP_lit7; DW_OP_offset;
                         DW_OP_const4s (-10);
                         DW_OP_lit6;
                         DW_OP_overlay] in
  let overlay_loc = eval_to_loc overlay_locexpr context in
  test overlay_loc
    (Composite [(19, 32, (Reg 4, 19));
                (13, 19, (Reg 7, 7));
                (0, 13, (Reg 4, 0))], 23)
    "overlay: negative offset, naive case"

(*
  b_offset : 23                          v
  b_storage:     |bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb|

  offset   : -23  <----------------------|
  width    : 6   |------|

  o_offset : 3    v
  o_storage:  |yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy|

  c_offset :                             v
  composite:     |yyyyyybbbbbbbbbbbbbbbbbbbbbbbbbb|
 *)
let _ =
  let overlay_locexpr = [DW_OP_reg4; DW_OP_lit23; DW_OP_offset;
                         DW_OP_reg7; DW_OP_lit3; DW_OP_offset;
                         DW_OP_const4s (-23);
                         DW_OP_lit6;
                         DW_OP_overlay] in
  let overlay_loc = eval_to_loc overlay_locexpr context in
  test overlay_loc
    (Composite [(6, 32, (Reg 4, 6));
                (0, 6, (Reg 7, 3))], 23)
    "overlay: negative offset, overlay starts at base beginning"

(*
  b_offset : 15                       v
  b_storage:          |bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb|

  offset   : -20  <-------------------|
  width    : 6   |------|

  o_offset : 3    v
  o_storage:  |yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy|

  composite: N/A
 *)
let _ =
  let overlay_locexpr = [DW_OP_reg4; DW_OP_lit15; DW_OP_offset;
                         DW_OP_reg7; DW_OP_lit3; DW_OP_offset;
                         DW_OP_const4s (-20);
                         DW_OP_lit6;
                         DW_OP_overlay] in
  test_error (fun () -> eval_to_loc overlay_locexpr context)
    "overlay: negative offset, overlay starts before base"

(****************************)
(* Print the final result.  *)
let _ =
  Printf.printf "*************************\n";
  Printf.printf "# of passes:\t %d\n" !num_pass;
  Printf.printf "# of failures:\t %d\n" !num_fail;
  Printf.printf "*************************\n";
