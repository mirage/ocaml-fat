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

type byte_range = int * t * int

let byte_range bps start len =
  let start_sector = start / bps in
  let end_sector = (start + len - 1) / bps in
  let preceeding = start - (start_sector * bps) in
  let succeeding = bps - (start + len - end_sector * bps) in
  let rec loop acc i =
    if i > end_sector
    then acc
    else loop (add i i acc) (i + 1) in
  let t = loop empty start_sector in
  preceeding, t, succeeding

let clip (preceeding, _, succeeding) = function
  | [] -> []
  | [c] -> [Cstruct.sub c preceeding (Cstruct.len c - succeeding - preceeding)]
  | c :: cs ->
    let cs' = List.rev cs in
    let last = List.hd cs' in
    let middle' = List.tl cs' in
    let last = Cstruct.sub last 0 (Cstruct.len last - succeeding) in
    let head = Cstruct.sub c preceeding (Cstruct.len c - preceeding) in
    head :: (List.rev middle') @ [ last ]

let compose a b =
  map (fun x ->
    if not (mem x b)
    then failwith (Printf.sprintf "SectorMap.compose: missing mapping for %d" x)
    else find x b) a

let to_list m = List.rev (fold (fun _ x acc -> x :: acc) m [])

let to_string m = "[ " ^ (String.concat "; " (List.map string_of_int (to_list m))) ^ " ]"

let find (x: t) sector =
  if not (mem sector x) then failwith "fault";
  find sector x

let transform_offset (x: t) sector_size vaddr =
  let s = Int64.of_int sector_size in
  let vsector = Int64.(div vaddr s) in
  let psector = find x (Int64.to_int vsector) in
  let voffset = Int64.(sub vaddr (mul vsector s)) in
  Int64.(add voffset (mul (of_int psector) s))

