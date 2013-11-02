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

let unmarshal bits =
  bitmatch bits with
  | { _: 24: string; (* JMP instruction *)
      oem_name: (8 * 8): string;
      bytes_per_sector: (2 * 8): littleendian;
      sectors_per_cluster: (1 * 8): littleendian;
      reserved_sectors: (2 * 8): littleendian;
      number_of_fats: (1 * 8): littleendian;
      number_of_root_dir_entries: (2 * 8): littleendian;
      total_sectors_small: (2 * 8): littleendian;
      media_descriptor: (1 * 8): littleendian;
      sectors_per_fat: (2 * 8): littleendian;
      sectors_per_track: (2 * 8): littleendian;
      heads: (2 * 8): littleendian;
      hidden_preceeding_sectors: (4 * 8): littleendian;
      total_sectors_large: (4 * 8): littleendian;
      0xaa55: 16: littleendian, offset(0x1fe * 8)
    } -> 
    {
      oem_name = oem_name;
      bytes_per_sector = bytes_per_sector;
      sectors_per_cluster = sectors_per_cluster;
      reserved_sectors = reserved_sectors;
      number_of_fats = number_of_fats;
      number_of_root_dir_entries = number_of_root_dir_entries;
      total_sectors = max (Int32.of_int total_sectors_small) total_sectors_large;
      sectors_per_fat = sectors_per_fat;
      hidden_preceeding_sectors = hidden_preceeding_sectors;
    }
  | { _ } -> failwith "Failed to read a boot sector"

let debug_print x =
  Printf.printf "OEM: [%s]\n" x.oem_name;
  Printf.printf "bytes_per_sector: %d\n" x.bytes_per_sector;
  Printf.printf "sectors_per_cluster: %d\n" x.sectors_per_cluster;
  Printf.printf "total_sectors: %ld\n" x.total_sectors;
  Printf.printf "reserved_sectors: %d\n" x.reserved_sectors;
  Printf.printf "number of FATs: %d\n" x.number_of_fats;
  Printf.printf "number_of_root_dir_entries: %d\n" x.number_of_root_dir_entries;
  Printf.printf "hidden_preceeding_sectors: %ld\n" x.hidden_preceeding_sectors;
  ()

let ints start length =
  let rec enumerate start length acc = match length with
  | 0 -> acc
  | _ -> enumerate (start + 1) (length - 1) (start :: acc) in
  List.rev (enumerate start length [])

(** Return the sector number of the first cluster *)
let initial_cluster x =
  let root_start = x.reserved_sectors + x.number_of_fats * x.sectors_per_fat in
  root_start + (x.number_of_root_dir_entries * 32) / x.bytes_per_sector

(** Return a list of sectors corresponding to cluster n *)
let sectors_of_cluster x n =
  (* NB clusters 0 and 1 are not on disk *)
  ints (initial_cluster x + x.sectors_per_cluster * (n - 2)) x.sectors_per_cluster

(** Return the number of clusters *)
let clusters x =
  let cluster_start = initial_cluster x in
  2 + (Int32.to_int (Int32.div (Int32.sub x.total_sectors (Int32.of_int cluster_start)) (Int32.of_int x.sectors_per_cluster)))

(* Choose between FAT12, FAT16 and FAT32 using heuristic from:
   http://averstak.tripod.com/fatdox/bootsec.htm *)
let detect_format x =
  let open Fat_format in
  let number_of_clusters = clusters x in
  if number_of_clusters < 4087 then Some FAT12
  else if number_of_clusters < 65527 then Some FAT16
  else if number_of_clusters < 268435457 then Some FAT32
  else None

let sectors_of_fat x =
  ints x.reserved_sectors x.sectors_per_fat

let sectors_of_root_dir x =
  let start = x.reserved_sectors + x.sectors_per_fat * x.number_of_fats in
  let length = (x.number_of_root_dir_entries * 32) / x.bytes_per_sector in
  ints start length
