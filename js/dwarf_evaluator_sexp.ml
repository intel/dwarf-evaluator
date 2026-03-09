(* Copyright (C) 2025-2026 Advanced Micro Devices, Inc.
   SPDX-License-Identifier: MIT *)

open Sexplib.Std

(* Re-open relevant types in Dwarf_evaluator to derive sexp functions *)
type data = [%import: Dwarf_evaluator.data]
and context_item = [%import: Dwarf_evaluator.context_item]
and storage = [%import: Dwarf_evaluator.storage]
and location = [%import: Dwarf_evaluator.location]
and stack_element = [%import: Dwarf_evaluator.stack_element]
and dwarf_op = [%import: Dwarf_evaluator.dwarf_op]
[@@deriving sexp]

(* Add some types not present in Dwarf_evaluator but useful with sexp *)
type context_t = (context_item list) [@@deriving sexp]
type locexpr_t = (dwarf_op list) [@@deriving sexp]
type stack_t = (stack_element list) [@@deriving sexp]
