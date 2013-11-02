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

type t = { offset: int64; data: Bitstring.t }

let to_string x = Printf.sprintf "Update[offset=%Ld length=%d]" x.offset (Bitstring.bitstring_length x.data / 8)

let hexdump x = Printf.printf "%s:\n%!" (to_string x); Bitstring.hexdump_bitstring stdout x.data

let make offset data = { offset = offset; data = data }
let move offset x = { x with offset = Int64.add x.offset offset }

(** [total_length x] returns the minimum size of the buffer needed to apply this update. *)
let total_length x = Int64.add x.offset (Int64.of_int (Bitstring.bitstring_length x.data / 8))

(** [bitstring_is_byte_aligned b] true if the data within [b] is byte aligned *)
let bitstring_is_byte_aligned (_, off, len) = off mod 8 = 0 && (len mod 8 = 0)

(** [bitstring_write src offset dest] modifies the bitstring [dest] by writing
 *     [src] at [offset] in [dest] *)
let bitstring_write ((src_s, src_off, src_len) as src) offset_bytes ((dest_s, dest_off, dest_len) as dest) =
  (* We don't expect to run off the end of the target bitstring *)
  assert (dest_len - offset_bytes * 8 - src_len >= 0);
  assert (bitstring_is_byte_aligned src);
  assert (bitstring_is_byte_aligned dest);
  String.blit src_s (src_off / 8) dest_s (dest_off / 8 + offset_bytes) (src_len / 8)

let apply bs x =
  let result = Bitstring.bitstring_of_string (Bitstring.string_of_bitstring bs) in
  bitstring_write x.data (Int64.to_int x.offset) result;
  result

(** [bitstring_clip s offset length] returns the sub-bitstring which exists
    between [offset] and [length] *)
let bitstring_clip (s_s, s_off, s_len) offset length =
  let s_end = s_off + s_len in
  let the_end = offset + length in
  let offset' = max s_off offset in
  let end' = min s_end the_end in
  let length' = max 0 (end' - offset') in
  s_s, offset', length'


(** [clip x offset length] returns the fraction of the update between
    [offset] and [offset+length] in bytes *)
let clip x offset length =
  let new_offset = max x.offset offset in
  let drop_bytes_from_start = Int64.(to_int(sub new_offset x.offset)) in
  let original_end = Int64.(add x.offset (of_int (Bitstring.bitstring_length x.data * 8))) in
  let proposed_end = Int64.(add offset (of_int length)) in
  let new_end = min original_end proposed_end in
  let new_length = Int64.(to_int(sub new_end new_offset)) in
  { offset = new_offset; data = bitstring_clip x.data (8 * drop_bytes_from_start) (8 * new_length) }

let is_empty x = Bitstring.equals Bitstring.empty_bitstring x.data

(** [split x sector_size] returns [x] as a sequence of consecutive updates,
    each of which corresponds to a region of length [sector_size]. Note empty
    updates are omitted. *)
let split x sector_size =
  let rec inner acc start =
    if Int64.(add x.offset (mul 8L (of_int (Bitstring.bitstring_length x.data)))) <= start
    then List.rev acc
    else
      let this = clip x start sector_size in
      let new_start = Int64.(add start (of_int sector_size)) in
      inner (if is_empty this then acc else this :: acc) new_start in
    inner [] 0L

(** [map_updates xs offsets] takes a sequence of virtual sector updates (eg within the
    virtual address space of a file) and a sequence of physical offsets (eg the
    location of physical sectors on disk) and returns a sequence of physical
    sector updates. *)
let map_updates xs sectors sector_size =
  let m = SectorMap.make sectors in
  List.map (fun x -> { x with offset = SectorMap.transform_offset m sector_size x.offset}) xs
