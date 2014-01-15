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

type datetime = {
  year: int;
  month: int;
  day: int;
  hours: int;
  mins: int;
  secs: int;
  ms: int;
}

val epoch: datetime

type lfn = {
  lfn_deleted: bool;
  lfn_last: bool; (** marks the highest sequence number *)
  lfn_seq: int;
  lfn_checksum: int;
  lfn_utf16_name: string
}
(** Long filename entry: the same size as an original DOS disk entry *)

type dos = {
  filename: string; (** 8 chars *)
  ext: string;      (** 3 chars *)
  deleted: bool;
  read_only: bool;
  hidden: bool;
  system: bool;
  volume: bool;
  subdir: bool;
  archive: bool;
  create: datetime;
  access: datetime;
  modify: datetime;
  start_cluster: int;
  file_size: int32;
}
(** A DOS disk entry *)

type single_entry =
  | Dos of dos
  | Lfn of lfn
  | End
  (** Useful for streaming entries to/from the disk *)

type r = {
  utf_filename: string;
  dos: int * dos;
  lfns: (int * lfn) list;
}
(** A high-level directory entry, complete with reconstructed UTF name and
    offsets of each individual entry on the disk *)

val fake_root_entry: r
(* Make the tree more uniform by creating a "fake root" node above the
   root directory entries *)

val file_size_of: r -> int32
val deleted: r -> bool
val filename_of: r -> string

val to_single_entries: r -> single_entry list

val legal_dos_char: char -> bool
val legal_dos_string: string -> bool

val is_legal_dos_name: string -> bool

val dos_name_of_filename: string -> string * string

val compute_checksum: dos -> int
(** Returns the checksum corresponding to the 8.3 DOS filename *)

val make: ?read_only:bool -> ?system:bool -> ?subdir:bool -> string -> r

val to_string: r -> string
(** [to_string r] returns a canonical version of the name in UTF-8 *)

val to_pretty_string: r -> string
(** [to_pretty_string r] returns a pretty version of the filename,
    containing both legacy DOS, extra UTF16, size and time components. *)

val int_to_hms: int -> int * int * int
val hms_to_int: int * int * int -> int

val int_of_time: datetime -> int

val time_of_int: int -> int -> int -> datetime

val int_of_date: datetime -> int

(** [unmarshal slot] parses a single directory entry from [slot] *)
val unmarshal: Cstruct.t -> single_entry

(** [marshal slot single_entry] writes [single_entry] into [slot] *)
val marshal: Cstruct.t -> single_entry -> unit

(** the size in bytes of a single_entry *)
val sizeof: int

(** [blocks bits] returns the directory chopped into individual bitstrings,
    each one containing a possible name (fragment) *)
val blocks: Cstruct.t -> (int * Cstruct.t) list

(** [fold f initial bits] folds [f acc offset dir_entry] across all the
    reconstructed directory entries contained in bits. *)
val fold: ('a -> int -> r -> 'a) -> 'a -> Cstruct.t -> 'a

(** [list bits] returns a list of valid (not deleted) directory entries
    contained within the directory [bits] *)
val list: Cstruct.t -> r list

(** [next bits] returns the bit offset of a free directory slot. Note this
    function does not recycle deleted elements. *)
val next: Cstruct.t -> int option

(** [add block t] return the update required to add [t] to the directory
    [block]. Note the update may be beyond the end of [block] indicating
    more space needs to be allocated. *)
val add: Cstruct.t -> r -> Update.t list

val name_match: string -> r -> bool

(** [find name list] returns [Some d] where [d] is a name with
    name [name] (or None) *)
val find: string -> r list -> r option

(** [remove buf filename] erases any entries corresponding to [filename]
    from [buf] *)
val remove: Cstruct.t -> string -> Update.t list

(** [modify buf filename file_size start_cluster] changes any entry
    corresponding to [filename] in [buf] to have [file_size] and
    [start_cluster] *)
val modify: Cstruct.t -> string -> int32 -> int -> Update.t list
