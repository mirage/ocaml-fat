(*
 * Copyright (C) 2011-2013 Citrix Systems Inc
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

type t = 
  | Free
  | Used of int
  | End
  | Bad

let to_string = function
  | Free -> "F"
  | Used x -> "U"
  | End -> "E"
  | Bad -> "B"

  let of_fat16 n fat =
    bitmatch fat with
      | { x: 16: littleendian, offset(16*n) } ->
	if x = 0 then Free
        else if x >= 0x0002 && x <= 0xffef then Used x
        else if x >= 0xfff8 && x <= 0xffff then End
        else Bad
      | { _ } -> Bad
  let to_fat16 n fat x =
    let x' = match x with
    | Free -> 0 | End -> 0xffff | Bad -> 0xfff7 | Used x -> x in
    let bs = BITSTRING {
      x' : 16 : littleendian
    } in
    Update.make (Int64.of_int (2 * n)) bs

  (* TESTING only *)
  let of_fat16 n fat =
    let x = of_fat16 n fat in
    let fat' = Update.apply fat (to_fat16 n fat x) in
    if Bitstring.compare fat fat' <> 0 then begin
      Printf.printf "before =\n";
      Bitstring.hexdump_bitstring stdout fat;
      Printf.printf "after =\n";
      Bitstring.hexdump_bitstring stdout fat';
    end;
    x
  let of_fat32 n fat =
    bitmatch fat with
      | { x: 32: littleendian, offset(32 * n) } ->
        if x = 0l then Free
        else if x >= 0x00000002l && x <= 0x0fffffefl then Used (Int32.to_int x)
        else if x >= 0x0ffffff8l && x <= 0x0fffffffl then End
        else Bad
      | { _ } -> Bad
  let to_fat32 n fat x =
    let x' = match x with
      | Free -> 0l | End -> 0x0fffffffl | Bad -> 0x0ffffff7l | Used x -> Int32.of_int x in
    let bs = BITSTRING {
      x' : 32 : littleendian
    } in
    Update.make (Int64.of_int (4 * n)) bs
  let of_fat12 n fat =
    (* 2 entries span groups of 3 bytes *)
    bitmatch fat with
      | { x: 16: littleendian, offset((3 * n)/2) } ->
        let x = if n mod 2 = 0 then x land 0xfff else x lsr 4 in
        if x = 0 then Free
        else if x >= 0x002 && x <= 0xfef then Used x
        else if x >= 0xff8 && x <= 0xfff then End
        else Bad
      | { _ } -> Bad
  let to_fat12 n fat x = failwith "Unimplemented"

(** Return the bitstring containing the nth FAT entry *)
let of_bitstring format =
  let open Fat_format in
  match format with
  | FAT16 -> of_fat16
  | FAT32 -> of_fat32
  | FAT12 -> of_fat12

(** Return the bitstring describing the FAT delta and the offset within
    the FAT table. *)
let to_bitstring format =
  let open Fat_format in
  match format with
  | FAT16 -> to_fat16
  | FAT32 -> to_fat32
  | FAT12 -> to_fat12

  module IntSet = Set.Make(struct type t = int let compare = compare end)

(** [follow_chain format fat cluster] returns the list of sectors containing
    data according to FAT [fat] which is of type [format]. *)
let follow_chain format fat cluster =
  (* the elements will be returned in order as 'list'; 'set' is used to
     check that we aren't going round in an infinite loop. *)
  let rec inner (list, set) = function
    | 0 -> list (* either zero-length chain if list = [] or corrupt file *)
    | 1 -> list (* corrupt file *)
    | i -> begin match of_bitstring format i fat with
      | End -> i :: list
      | Free | Bad -> list (* corrupt file *)
      | Used j ->
        if IntSet.mem i set
        then list (* infinite loop: corrupt file *)
        else inner (i :: list, IntSet.add i set) j
      end in
  List.rev (inner ([], IntSet.empty) cluster)

let initial = 2 (* first valid entry *)

(** [find_free_from boot format fat start] returns an unallocated cluster
    after [start] *)
let find_free_from boot format fat start =
  let n = Boot_sector.clusters boot in
  let rec inner i =
    if i = n then None
    else match of_bitstring format i fat with
    | Free -> Some i
    | _ -> inner (i + 1) in
  inner start

(** [extend boot format fat last n] allocates [n] free clusters to extend
    the chain whose current end is [last] *)
let extend boot format fat (last: int option) n =
  let rec inner acc start = function
    | 0 -> acc (* in reverse disk order *)
    | i ->
      match find_free_from boot format fat start with
      | None -> acc (* out of space *)
      | Some c -> inner (c :: acc) (c + 1) (i - 1) in
  let to_allocate = inner [] (match last with None -> initial | Some x -> x) n in
  if n = 0
  then [], []
  else
    if List.length to_allocate <> n
    then [], [] (* allocation failed *)
    else
      let final = List.hd to_allocate in
      let to_allocate = List.rev to_allocate in
      let updates = fst(List.fold_left (fun (acc, last) next ->
        (match last with
         | Some last ->
            to_bitstring format last fat (Used next) :: acc
         | None -> acc), Some next
        ) ([], last) to_allocate) in
      to_bitstring format final fat End :: updates (* reverse order *), to_allocate
