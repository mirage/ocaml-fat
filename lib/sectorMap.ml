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

module M = Map.Make(struct
  type t = int
  let compare (x: int) (y: int) = compare x y
end)
open M
type t = int M.t

let make sectors =
  fst (List.fold_left (fun (m, i) o -> add i o m, i + 1) (empty,0) sectors)

let find (x: t) sector =
  if not (mem sector x) then failwith "fault";
  find sector x

let transform_offset (x: t) sector_size vaddr =
  let s = Int64.of_int sector_size in
  let vsector = Int64.(div vaddr s) in
  let psector = find x (Int64.to_int vsector) in
  let voffset = Int64.(sub vaddr (mul vsector s)) in
  Int64.(add voffset (mul (of_int psector) s))

