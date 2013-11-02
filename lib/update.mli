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
(** an update to a buffer: write [data] at [offset] *)

val to_string: t -> string

val hexdump: t -> unit

val make: int64 -> Bitstring.t -> t

val move: int64 -> t -> t

val total_length: t -> int64
(** [total_length x] returns the minimum size of the buffer needed to apply this update. *)

val apply: Bitstring.t -> t -> Bitstring.t

val clip: t -> int64 -> int -> t
(** [clip x offset length] returns the fraction of the update between
    [offset] and [offset+length] in bytes *)

val is_empty: t -> bool

val split: t -> int -> t list
(** [split x sector_size] returns [x] as a sequence of consecutive updates,
    each of which corresponds to a region of length [sector_size]. Note empty
    updates are omitted. *)

val map_updates: t list -> int list -> int -> t list
(** [map_updates xs offsets] takes a sequence of virtual sector updates (eg within the
    virtual address space of a file) and a sequence of physical offsets (eg the
    location of physical sectors on disk) and returns a sequence of physical
    sector updates. *)
