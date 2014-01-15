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
  bytes_per_sector: int; (* usually 512 *)
  sectors_per_cluster: int; (* 1, 2, 4, 8, 16, 32, 64, 128 *)
  reserved_sectors: int; (* at least 1, usually 32 for FAT32 *)
  number_of_fats: int; (* usually 2; sometimes 1 for RAM disks *)
  number_of_root_dir_entries: int; (* 0 on FAT32 *)
  total_sectors: int32;
  sectors_per_fat: int;
  hidden_preceeding_sectors: int32;
}

let default_oem_name = "ocamlfat"

cstruct t {
  uint8_t jump_instruction[3];
  uint8_t oem_name[8];
  uint16_t bytes_per_sector;
  uint8_t sectors_per_cluster;
  uint16_t reserved_sectors;
  uint8_t number_of_fats;
  uint16_t number_of_root_dir_entries;
  uint16_t total_sectors_small;
  uint8_t media_descriptor;
  uint16_t sectors_per_fat;
  uint16_t sectors_per_track;
  uint16_t heads;
  uint32_t hidden_preceeding_sectors;
  uint32_t total_sectors_large;
  uint8_t boot_code[474];
  uint16_t signature
} as little_endian

let sizeof = sizeof_t

let _ = assert(sizeof = 512)

let fat_id = 0xf8 (* fixed disk *)

let marshal (buf: Cstruct.t) t =
  for i = 0 to Cstruct.len buf - 1 do
    Cstruct.set_uint8 buf i 0
  done;
  set_t_oem_name t.oem_name 0 buf;
  set_t_bytes_per_sector buf t.bytes_per_sector;
  set_t_sectors_per_cluster buf t.sectors_per_cluster;
  set_t_reserved_sectors buf t.reserved_sectors;
  set_t_number_of_fats buf t.number_of_fats;
  set_t_number_of_root_dir_entries buf t.number_of_root_dir_entries;
  set_t_total_sectors_small buf 0; (* means use total_sectors_large *)
  set_t_media_descriptor buf fat_id;
  set_t_sectors_per_fat buf t.sectors_per_fat;
  set_t_sectors_per_track buf 0; (* not used *)
  set_t_heads buf 0; (* not used *)
  set_t_hidden_preceeding_sectors buf t.hidden_preceeding_sectors;
  set_t_total_sectors_large buf t.total_sectors;
  (* no boot code yet *)
  set_t_signature buf 0xaa55

let unmarshal (buf: Cstruct.t) : (t, string) result =
  ( if Cstruct.len buf < sizeof
    then `Error (Printf.sprintf "boot sector too small: %d < %d" (Cstruct.len buf) sizeof)
    else `Ok () ) >>= fun () ->
  let signature = get_t_signature buf in
  ( if signature <> 0xaa55
    then `Error (Printf.sprintf "boot sector signature invalid: %04x <> %04x" signature 0xaa55)
    else `Ok () ) >>= fun () ->
  let oem_name = Cstruct.to_string (get_t_oem_name buf) in
  let bytes_per_sector = get_t_bytes_per_sector buf in
  let sectors_per_cluster = get_t_sectors_per_cluster buf in
  let reserved_sectors = get_t_reserved_sectors buf in
  let number_of_fats = get_t_number_of_fats buf in
  let number_of_root_dir_entries = get_t_number_of_root_dir_entries buf in
  let total_sectors_small = get_t_total_sectors_small buf in
  let sectors_per_fat = get_t_sectors_per_fat buf in
  let hidden_preceeding_sectors = get_t_hidden_preceeding_sectors buf in
  let total_sectors_large = get_t_total_sectors_large buf in
  `Ok {
    oem_name; bytes_per_sector; sectors_per_cluster;
    reserved_sectors; number_of_fats; number_of_root_dir_entries;
    total_sectors = max (Int32.of_int total_sectors_small) total_sectors_large;
    sectors_per_fat; hidden_preceeding_sectors;
  }

let debug_print x =
  Printf.printf "OEM: [%s]\n" x.oem_name;
  Printf.printf "bytes_per_sector: %d\n" x.bytes_per_sector;
  Printf.printf "sectors_per_cluster: %d\n" x.sectors_per_cluster;
  Printf.printf "sectors_per_fat: %d\n" x.sectors_per_fat;
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
let format_of_clusters number_of_clusters =
  let open Fat_format in
  if number_of_clusters < 4087 then Some FAT16 (* FAT12 *)
  else if number_of_clusters < 65527 then Some FAT16
  else if number_of_clusters < 268435457 then Some FAT32
  else None

exception Unknown_FAT_cluster_type

let detect_format x = match format_of_clusters (clusters x) with
  | None -> `Error "unknown cluster type"
  | Some x -> `Ok x

let make size =
  let bytes_per_sector = 512 in
  (* XXX: need to choose this intelligently based on the disk size *)
  let sectors_per_cluster = 4 in
  let total_sectors = Int64.(to_int32 (div (add 511L size) 512L)) in
  let total_clusters = Int32.(to_int (div (add 3l total_sectors) (of_int sectors_per_cluster))) in
  let open Fat_format in
  match format_of_clusters total_clusters with
  | Some FAT12 | Some FAT32 | None ->
    failwith "unimplemented"
  | Some FAT16 ->
    let sectors_per_fat = ((total_clusters * 2) + 511) / 512 in
    let reserved_sectors = 4 in
    let number_of_fats = 1 in
    let number_of_root_dir_entries = 512 in
    let hidden_preceeding_sectors = 0l in
    { oem_name = default_oem_name;
      bytes_per_sector; sectors_per_cluster; total_sectors;
      sectors_per_fat; reserved_sectors; number_of_fats;
      number_of_root_dir_entries; hidden_preceeding_sectors }

let sectors_of_fat x =
  ints x.reserved_sectors x.sectors_per_fat

let sectors_of_root_dir x =
  let start = x.reserved_sectors + x.sectors_per_fat * x.number_of_fats in
  let length = (x.number_of_root_dir_entries * 32) / x.bytes_per_sector in
  ints start length
