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
  | Free   -> "F"
  | Used _ -> "U"
  | End    -> "E"
  | Bad    -> "B"

type fat = Cstruct.t

let of_fat16 n fat =
  if Cstruct.len fat < (2 * n + 2)
  then Bad
  else
    let x = Cstruct.LE.get_uint16 fat (2 * n) in
    if x = 0 then Free
    else if x >= 0x0002 && x <= 0xffef then Used x
    else if x >= 0xfff8 && x <= 0xffff then End
    else Bad

let to_fat16 n fat x =
  let x' = match x with
    | Free -> 0 | End -> 0xffff | Bad -> 0xfff7 | Used x -> x in
  Cstruct.LE.set_uint16 fat (2 * n) x'

let of_fat32 n fat =
  if Cstruct.len fat < (4 * n + 4)
  then Bad
  else
    let x = Cstruct.LE.get_uint32 fat (4 * n) in
    if x = 0l then Free
    else if x >= 0x00000002l && x <= 0x0fffffefl then Used (Int32.to_int x)
    else if x >= 0x0ffffff8l && x <= 0x0fffffffl then End
    else Bad

let to_fat32 n fat x =
  let x' = match x with
    | Free -> 0l | End -> 0x0fffffffl | Bad -> 0x0ffffff7l | Used x -> Int32.of_int x in
  Cstruct.LE.set_uint32 fat (4 * n) x'

let of_fat12 _n _fat = failwith "Unimplemented"
let to_fat12 _n _fat _x = failwith "Unimplemented"

let unmarshal format =
  let open Fat_format in
  match format with
  | FAT16 -> of_fat16
  | FAT32 -> of_fat32
  | FAT12 -> of_fat12

let marshal format =
  let open Fat_format in
  match format with
  | FAT16 -> to_fat16
  | FAT32 -> to_fat32
  | FAT12 -> to_fat12

let cluster_0 format =
  let open Fat_format in
  Used ( (match format with
      | FAT16 -> 0xff00
      | FAT12 -> failwith "Unimplemented"
      | FAT32 -> 0x0fffff00) lor Fat_boot_sector.fat_id )

let cluster_1 format =
  let open Fat_format in
  Used ( match format with
      | FAT16 -> 0xffff
      | FAT12 -> 0xfff
      | FAT32 -> 0x0fffffff )

let make boot_sector format =
  let n = Fat_boot_sector.clusters boot_sector in
  let open Fat_format in
  let bytes_per_cluster = match format with
    | FAT16 -> 2
    | FAT32 -> 4
    | FAT12 -> failwith "Unimplemented" in
  let buf = Cstruct.create (n * bytes_per_cluster) in
  marshal format 0 buf (cluster_0 format);
  marshal format 1 buf (cluster_1 format);
  for i = 2 to n - 1 do
    marshal format i buf Free
  done;
  buf

let initial = 2 (* first valid entry *)

(** [find_free_from boot format fat start] returns an unallocated cluster
    after [start] *)
let find_free_from boot format fat start =
  let n = Fat_boot_sector.clusters boot in
  let rec inner i =
    if i = n then None
    else match unmarshal format i fat with
      | Free -> Some i
      | _ -> inner (i + 1) in
  inner start

module Chain = struct
  module IntSet = Set.Make(struct type t = int let compare = compare end)

  type t = int list

  let follow format fat cluster =
    (* the elements will be returned in order as 'list'; 'set' is used to
       check that we aren't going round in an infinite loop. *)
    let rec inner (list, set) = function
      | 0 -> list (* either zero-length chain if list = [] or corrupt file *)
      | 1 -> list (* corrupt file *)
      | i -> begin match unmarshal format i fat with
          | End -> i :: list
          | Free | Bad -> list (* corrupt file *)
          | Used j ->
            if IntSet.mem i set
            then list (* infinite loop: corrupt file *)
            else inner (i :: list, IntSet.add i set) j
        end in
    List.rev (inner ([], IntSet.empty) cluster)

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
    then []
    else
    if List.length to_allocate <> n
    then [] (* allocation failed *)
    else
      let final = List.hd to_allocate in
      let to_allocate = List.rev to_allocate in
      ignore(List.fold_left (fun last next ->
          (match last with
           | Some last ->
             marshal format last fat (Used next)
           | None -> ());
          Some next
        ) last to_allocate);
      marshal format final fat End;
      to_allocate

  let to_sectors boot clusters =
    List.concat (List.map (Fat_boot_sector.sectors_of_cluster boot) clusters)
end
