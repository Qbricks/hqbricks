(**************************************************************************)
(*  This file is part of HQbricks.                                        *)
(*                                                                        *)
(*  Copyright (C) 2026                                                    *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*    Université Paris-Saclay                                             *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 3.0.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 3.0                 *)
(*  for more details (enclosed in the file LICENSE).                      *)
(*                                                                        *)
(**************************************************************************)

open Hps

module Reg = struct
  let check_phase y_set p =
    not
    @@ Phase.exists
         (fun vs _ ->
           Var_set.exists
             (fun v -> match v with X _ -> false | Y yi -> Y_set.mem yi y_set)
             vs
           && Var_set.exists
                (fun v ->
                  match v with
                  | X _ -> false
                  | Y yi -> not @@ Y_set.mem yi y_set)
                vs)
         p

  let rec check_scalar y_set = function
    | Scalar.SMul (s1, s2) -> check_scalar y_set s1 && check_scalar y_set s2
    | _ as s ->
        let y_set_s = Scalar.find_all_y s in
        Y_set.subset y_set_s y_set || Y_set.disjoint y_set_s y_set

  let check_mem reg_name_set y_set m =
    not
    @@ Mem.exists
         (fun (qreg_name, _) ket ->
           (not @@ Reg_name_set.mem qreg_name reg_name_set)
           && Hket.exists
                (fun vs ->
                  Var_set.exists
                    (fun v ->
                      match v with X _ -> false | Y yi -> Y_set.mem yi y_set)
                    vs)
                ket)
         m

  let check_mem_stack ?(past_only = false) reg_name_set y_set ms =
    let ms = if past_only then List.drop 1 ms else ms in
    Mem_stack.for_all (fun m -> check_mem reg_name_set y_set m) ms

  let check_output ?(past_only = false) reg_name_set y_set o =
    check_mem reg_name_set y_set Output.(o.qmem)
    && check_mem_stack reg_name_set y_set Output.(o.cmem_stack) ~past_only

  let check ?(past_only = false) reg_name_set hps =
    let y_set =
      if past_only then Output.find_y_in_regs_past reg_name_set hps.output
      else Output.find_y_in_regs reg_name_set hps.output
    in
    if
      not @@ Reg_name_set.subset reg_name_set (Output.find_reg_names hps.output)
    then Error "a reg from the reg name set is not in the output"
    else if not @@ check_phase y_set hps.phase then
      Error "the phase contains a product with y from both side of the fact"
    else if not @@ check_scalar y_set hps.scalar then
      Error
        "the scalar product contains an element containing y from both side of \
         the fact"
    else if not @@ check_output reg_name_set y_set hps.output ~past_only then
      Error
        "the output contains an y from the reg_name_set in a reg which is not \
         in the reg_name_set"
    else Ok ()

  let partition_phase y_set p =
    Phase.partition
      (fun vs _ ->
        match
          Var_set.find_last_opt
            (fun v -> match v with X _ -> false | Y _ -> true)
            vs
        with
        | Some (Y yi) -> Y_set.mem yi y_set
        | _ -> not @@ Var_set.is_empty vs
        (* If there is an x and no y in the var set, the element goes to the y_set part *))
      p

  let rec partition_scalar_aux y_set = function
    | Scalar.SMul (sl, sr) ->
        let sll, slr = partition_scalar_aux y_set sl in
        let srl, srr = partition_scalar_aux y_set sr in
        (Scalar.SMul (sll, srl), Scalar.SMul (slr, srr))
    | _ as s -> (
        match Scalar.find_any_y s with
        | Some yi when Y_set.mem yi y_set -> (s, Scalar.one)
        | Some _ -> (Scalar.one, s)
        | None when Scalar.contains_any_x s -> (s, Scalar.one)
        | None -> (Scalar.one, s))

  let partition_scalar y_set s =
    let s1, s2 = partition_scalar_aux y_set s in
    (Scalar.simp s1, Scalar.simp s2)

  let partition_mem reg_name_set m =
    Mem.partition
      (fun (qreg_name, _) _ -> Reg_name_set.mem qreg_name reg_name_set)
      m

  let partition_mem_stack ?(past_only = false) reg_name_set ms =
    let present_ms1, present_ms2 =
      match ms with
      | m :: _ when past_only -> ([ Mem.empty ], [ m ])
      | m :: _ ->
          let m1, m2 = partition_mem reg_name_set m in
          ([ m1 ], [ m2 ])
      | _ -> ([], [])
    in
    let ms1, ms2 =
      List.fold_right
        (fun m (acc1, acc2) ->
          let m1, m2 = partition_mem reg_name_set m in
          (m1 :: acc1, m2 :: acc2))
        (List.drop 1 ms) ([], [])
    in
    ( Mem_stack.remove_trailing_voids (present_ms1 @ ms1),
      Mem_stack.remove_trailing_voids (present_ms2 @ ms2) )

  let partition_output ?(past_only = false) reg_name_set o =
    let qm1, qm2 =
      if past_only then (Mem.empty, Output.(o.qmem))
      else partition_mem reg_name_set Output.(o.qmem)
    in
    let cs1, cs2 =
      partition_mem_stack reg_name_set Output.(o.cmem_stack) ~past_only
    in
    Output.(make qm1 cs1, make qm2 cs2)

  let partition_support y_set s =
    Support.partition (fun yi -> Y_set.mem yi y_set) s

  let apply_unchecked ?(past_only = false) ?metrics reg_name_set hps =
    let y_set =
      if past_only then Output.find_y_in_regs_past reg_name_set hps.output
      else Output.find_y_in_regs reg_name_set hps.output
    in
    let p1, p2 = partition_phase y_set hps.phase in
    let s1, s2 = partition_scalar y_set hps.scalar in
    let o1, o2 = partition_output reg_name_set hps.output ~past_only in
    let su1, su2 = partition_support y_set hps.support in
    Metrics.add_rewrites_opt metrics 1;
    ( {
        phase = p1;
        scalar = s1;
        output = o1;
        support = su1;
        y_count = hps.y_count;
      },
      {
        phase = p2;
        scalar = s2;
        output = o2;
        support = su2;
        y_count = hps.y_count;
      } )

  let check_and_apply ?(past_only = false) ?metrics reg_name_set hps =
    match check reg_name_set hps ~past_only with
    | Error s -> Error s
    | Ok () -> Ok (apply_unchecked reg_name_set hps ~past_only ?metrics)
end

module Xy_vars = struct
  exception Fail of string

  let check_n n =
    if n < 1 then
      Error (Printf.sprintf "n should be greater than or equal to 1; n = %d" n)
    else Ok ()

  let check_var_set n f vs =
    match Var_set.choose_opt vs with
    | Some v ->
        let i = f v in
        if i >= n then
          raise
            (Fail
               (Printf.sprintf
                  "f v is greater than or equal to n; v = %s, f v = %d, n = %d"
                  (Var.to_string v) i n));
        if Var_set.exists (fun v -> f v <> i) vs then
          raise
            (Fail
               ("there is a term with variables from several sides of the \
                 fact: " ^ Var_set.to_string vs))
    | None -> ()

  let check_phase n f p =
    try
      Phase.iter (fun vs _ -> check_var_set n f vs) p;
      Ok ()
    with Fail msg -> Error ("phase: " ^ msg)

  let check_scalar n f s_list =
    try
      List.iter (fun s -> Scalar.find_all_xy s |> check_var_set n f) s_list;
      Ok ()
    with Fail msg -> Error ("scalar: " ^ msg)

  let check_mem n f m =
    Mem.iter (fun _ ket -> check_var_set n f (Hket.find_all_xy ket)) m

  let check_qmem n f m =
    try
      check_mem n f m;
      Ok ()
    with Fail msg -> Error ("qmem: " ^ msg)

  let check_cmem_stack n f ms =
    try
      List.iter (fun m -> check_mem n f m) ms;
      Ok ()
    with Fail msg -> Error ("cmem_stack: " ^ msg)

  let ( let* ) = Result.bind

  let check_output n f o =
    let* () = check_qmem n f Output.(o.qmem) in
    check_cmem_stack n f Output.(o.cmem_stack)

  let check_aux n f hps s_list =
    let* () = check_n n in
    let* () = check_phase n f hps.phase in
    let* () = check_scalar n f s_list in
    check_output n f hps.output

  let check n f hps =
    if n = 1 then Ok ()
    else check_aux n f hps (Scalar.to_product_list hps.scalar)

  let partition_phase f p hps_product =
    Phase.iter
      (fun vs dy ->
        let i = match Var_set.choose_opt vs with Some v -> f v | None -> 0 in
        hps_product.(i) <- add_phase (Var_set.to_list vs) dy hps_product.(i))
      p

  let partition_scalar f s_list hps_product =
    List.iter
      (fun s ->
        let i = match Scalar.find_any_xy s with Some v -> f v | None -> 0 in
        hps_product.(i) <- mul_scalar s hps_product.(i))
      s_list

  let partition_qmem f m hps_product =
    Mem.iter
      (fun reg_id k ->
        let i = match Hket.find_any_xy k with Some v -> f v | None -> 0 in
        hps_product.(i) <- add_qmem reg_id k hps_product.(i))
      m

  let partition_cmem f m hps_product =
    Mem.iter
      (fun reg_id k ->
        let i = match Hket.find_any_xy k with Some v -> f v | None -> 0 in
        hps_product.(i) <- add_cmem reg_id k hps_product.(i))
      m

  let partition_cmem_stack f cs hps_product =
    List.rev cs
    |> List.iter (fun m ->
           Array.map_inplace
             (fun hps -> add_empty_cmem_stack_elem hps)
             hps_product;
           partition_cmem f m hps_product)

  let partition_output f o hps_product =
    partition_qmem f Output.(o.qmem) hps_product;
    partition_cmem_stack f Output.(o.cmem_stack) hps_product

  let partition_support f su hps_product =
    Support.iter
      (fun yi ->
        let i = f (Var.Y yi) in
        hps_product.(i) <- add_support [ yi ] hps_product.(i))
      su

  let apply_unchecked_aux ?metrics n f hps s_list =
    let init_hps = Hps.one |> set_y_count hps.y_count |> set_cmem_stack [] in
    let hps_product = Array.make n init_hps in
    partition_phase f hps.phase hps_product;
    partition_scalar f s_list hps_product;
    partition_output f hps.output hps_product;
    partition_support f hps.support hps_product;
    Metrics.add_rewrites_opt metrics (n - 1);
    hps_product

  let apply_unchecked ?metrics n f hps =
    if n = 1 then [| hps |]
    else
      apply_unchecked_aux n f hps (Scalar.to_product_list hps.scalar) ?metrics

  let check_and_apply ?metrics n f hps =
    if n = 1 then Ok [| hps |]
    else
      let s_list = Scalar.to_product_list hps.scalar in
      match check_aux n f hps s_list with
      | Error s -> Error s
      | Ok () -> Ok (apply_unchecked_aux n f hps s_list ?metrics)

  let hashtbl_union ht1 ht2 =
    Hashtbl.iter (fun key _ -> Hashtbl.replace ht1 key ()) ht2

  let var_set_to_hashtbl vs =
    let ht = Hashtbl.create (Var_set.cardinal vs) in
    Var_set.iter (fun v -> Hashtbl.add ht v ()) vs;
    ht

  let add_var_set_to_separable_var_list vs separable_var_list =
    let rec fold_separable_var_list vs separable_var_list merged_vars
        new_separable_var_list =
      match separable_var_list with
      | _ when Var_set.is_empty vs ->
          (merged_vars :: new_separable_var_list) @ separable_var_list
      | [] -> merged_vars :: new_separable_var_list
      | vars :: separable_var_list ->
          let vs, is_sep =
            Var_set.fold
              (fun v (vs, is_sep) ->
                if Hashtbl.mem vars v then (Var_set.remove v vs, false)
                else (vs, is_sep))
              vs (vs, true)
          in
          if is_sep then
            fold_separable_var_list vs separable_var_list merged_vars
              (vars :: new_separable_var_list)
          else (
            hashtbl_union vars merged_vars;
            fold_separable_var_list vs separable_var_list vars
              new_separable_var_list)
    in
    fold_separable_var_list vs separable_var_list (var_set_to_hashtbl vs) []

  let add_phase_vars_to_separable_var_list p separable_var_list =
    Phase.fold
      (fun vs _ separable_var_list ->
        if Var_set.is_empty vs then separable_var_list
        else add_var_set_to_separable_var_list vs separable_var_list)
      p separable_var_list

  let add_scalar_vars_to_separable_var_list s_list separable_var_list =
    List.fold_left
      (fun separable_var_list s ->
        let vs = Scalar.find_all_xy s in
        if Var_set.is_empty vs then separable_var_list
        else add_var_set_to_separable_var_list vs separable_var_list)
      separable_var_list s_list

  let add_mem_vars_to_separable_var_list m separable_var_list =
    Mem.fold
      (fun _ ket separable_var_list ->
        let vs = Hket.find_all_xy ket in
        if Var_set.is_empty vs then separable_var_list
        else add_var_set_to_separable_var_list vs separable_var_list)
      m separable_var_list

  let add_mem_stack_vars_to_separable_var_list ms separable_var_list =
    List.fold_left
      (fun separable_var_list m ->
        add_mem_vars_to_separable_var_list m separable_var_list)
      separable_var_list ms

  let add_output_vars_to_separable_var_list o separable_var_list =
    let open Output in
    separable_var_list
    |> add_mem_vars_to_separable_var_list o.qmem
    |> add_mem_stack_vars_to_separable_var_list o.cmem_stack

  let separable_var_list_to_vars_i separable_var_list =
    let vars_i = Hashtbl.create 32 in
    List.iteri
      (fun i vars -> Hashtbl.iter (fun v _ -> Hashtbl.add vars_i v i) vars)
      separable_var_list;
    vars_i

  let find_and_apply_all ?metrics hps =
    let s_list = Scalar.to_product_list hps.scalar in
    let separable_var_list =
      []
      |> add_phase_vars_to_separable_var_list hps.phase
      |> add_scalar_vars_to_separable_var_list s_list
      |> add_output_vars_to_separable_var_list hps.output
    in
    let n = List.length separable_var_list in
    let vars_i = separable_var_list_to_vars_i separable_var_list in
    let f v = Hashtbl.find_opt vars_i v |> Option.value ~default:0 in
    let hps_product = apply_unchecked n f hps ?metrics in
    hps_product
end
