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

type data =
  | String of string
  | Cstruct of Cstruct.t

let bytes = function
  | String x -> String.length x
  | Cstruct x -> Cstruct.length x

let string_of_data = function
  | String x -> Printf.sprintf "(string of length %d)" (String.length x)
  | Cstruct x -> Printf.sprintf "(Cstruct.t of length %d)" (Cstruct.length x)

type t = {
  offset: int64;
  data: data
}

let to_string x = Printf.sprintf "Update.({ offset=%Ld data=%s })"
                    x.offset (string_of_data x.data)

let from_string offset x = { offset; data = String x }
let from_cstruct offset x = { offset; data = Cstruct x }

(* was: move *)
let shift offset x = { x with offset = Int64.add x.offset offset }

(** [total_length x] returns the minimum size of the buffer needed to apply
    this update. *)
let total_length x = Int64.(add x.offset (of_int (bytes x.data)))

let apply buf { offset; data } =
  let offset = Int64.to_int offset in
  match data with
  | String x -> Cstruct.blit_from_string x 0 buf offset (String.length x)
  | Cstruct x -> Cstruct.blit x 0 buf offset (Cstruct.length x)

let sub offset length =
  let offset = Int64.to_int offset in
  function
  | String x -> String (String.sub x offset length)
  | Cstruct x -> Cstruct (Cstruct.sub x offset length)

(** [clip x offset length] returns the fraction of the update between
    [offset] and [offset+length] in bytes *)
let clip x offset length =
  let new_offset = max x.offset offset in
  let drop_bytes_from_start = Int64.sub new_offset x.offset in
  let original_end = Int64.(add x.offset (of_int (bytes x.data))) in
  let proposed_end = Int64.(add offset (of_int length)) in
  let new_end = min original_end proposed_end in
  let new_length = Int64.(to_int (sub new_end new_offset)) in
  let data = sub drop_bytes_from_start new_length x.data in
  { offset = new_offset; data }

let is_empty x = bytes x.data = 0

(** [split x sector_size] returns [x] as a sequence of consecutive updates,
    each of which corresponds to a region of length [sector_size]. Note empty
    updates are omitted. *)
let split x sector_size =
  let starting_sector_offset = Int64.(mul (div x.offset (of_int sector_size)) (of_int sector_size)) in
  let rec inner acc start =
    if Int64.(add x.offset (of_int (bytes x.data))) <= start
    then List.rev acc
    else
      let this = clip x start sector_size in
      let new_start = Int64.(add start (of_int sector_size)) in
      inner (if is_empty this then acc else this :: acc) new_start
  in
  inner [] starting_sector_offset

(** [map xs offsets] takes a sequence of virtual sector updates (eg within the
    virtual address space of a file) and a sequence of physical offsets (eg the
    location of physical sectors on disk) and returns a sequence of physical
    sector updates. *)
let map xs sectors sector_size =
  let m = Fat_sector_map.make sectors in
  List.map (fun x ->
      { x with offset = Fat_sector_map.transform_offset m sector_size x.offset}
    ) xs
