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

open Result

type t = {
  oem_name: string;
  bytes_per_sector: int;
  sectors_per_cluster: int;
  reserved_sectors: int;
  number_of_fats: int;
  number_of_root_dir_entries: int;
  total_sectors: int32;
  sectors_per_fat: int;
  hidden_preceeding_sectors: int32;
}

val sizeof: int

val marshal: Cstruct.t -> t -> unit

val unmarshal: Cstruct.t -> (t, string) result

val debug_print: t -> unit

val initial_cluster: t -> int
(** Return the sector number of the first cluster *)

val sectors_of_cluster: t -> int -> int list
(** Return a list of sectors corresponding to cluster n *)

val clusters: t -> int
(** Return the number of clusters *)

val detect_format: t -> Fat_format.t option
(* Choose between FAT12, FAT16 and FAT32 using heuristic from:
   http://averstak.tripod.com/fatdox/bootsec.htm *)

val sectors_of_fat: t -> int list

val sectors_of_root_dir: t -> int list
