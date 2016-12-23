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

type t
(** Instances of SectorMap will map virtual sectors within a file to physical
    sector numbers on disk *)

val make: int list -> t

type byte_range = int * t * int
(** A range of bytes represented by a [preceeding, t, succeeding] where
    [t] represents a sequence of sectors, [preceeding] is the number of
    unnecessary bytes in the first sector and [succeeding] is the number
    of unnecessary bytes in the last sector. *)

val byte_range: int -> int -> int -> byte_range
(** [byte_range bytes_per_sector byte_start byte_len] returns
    [preceeding_bytes, t, succeeding_bytes] where [t] is the identity
    map for all sectors in the range [floor(byte_start/bytes_per_sector)]
    to [ceiling((byte_start + byte_len)/bytes_per_sector].
    [preceeding_bytes] is the number of bytes before [byte_start] in the
    first sector; [succeeding_bytes] is the number of bytes affter
    [byte_start+byte_len] in the last sector. *)

val clip: byte_range -> Cstruct.t list -> Cstruct.t list
(** [clip byte_range sectors] removes unnecessary bytes from [sectors] *)

val compose: t -> t -> t
(** [compose a b] returns (x, y) for all x\in domain(a) where a[x],y \in b
    In particular if we want to read an object in a file represented by
    [a], and [b] represents the mapping of virtual sector in the file to
    physical sector on the disk, then [compose a b] represents the physical
    sectors of the object on the disk. *)

val to_list: t -> int list
(** [to_list t] returns the values of [t], ordered by keys *)

val to_string: t -> string
(** [to_string t] returns a printable version of [t] *)

val find: t -> int -> int
(** [find x sector] returns the physical address on disk corresponding to the
    virtual sector [sector] according to SectorMap [x] *)

val transform_offset: t -> int -> int64 -> int64
(** [transform_offset x sector_size vaddr] returns the physical address on disk
    corresponding to virtual address [vaddr] according to SectorMap [x] *)
