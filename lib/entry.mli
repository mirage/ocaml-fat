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

type t = 
  | Free
  | Used of int (** points to the next in the chain *)
  | End         (** end of a chain *)
  | Bad         (** bad sector or illegal FAT entry *)

val to_string: t -> string

val of_fat16: int -> Bitstring.t -> t

val to_fat16: int -> Bitstring.t -> t -> Update.t

val of_fat32: int -> Bitstring.t -> t

val to_fat32: int -> Bitstring.t -> t -> Update.t

val of_fat12: int -> Bitstring.t -> t

val to_fat12: int -> Bitstring.t -> t -> Update.t

val of_bitstring: Fat_format.t -> int -> Bitstring.t -> t
(** Return the bitstring containing the nth FAT entry *)

val to_bitstring: Fat_format.t -> int -> Bitstring.t -> t -> Update.t
(** Return the bitstring describing the FAT delta and the offset within
    the FAT table. *)
(*
module IntSet : Set.S with type t := int
*)

val follow_chain: Fat_format.t -> Bitstring.t -> int -> int list
(** [follow_chain format fat cluster] returns the list of sectors containing
    data according to FAT [fat] which is of type [format]. *)

val initial: int
(** first valid entry *)

val find_free_from: Boot_sector.t -> Fat_format.t -> Bitstring.t -> int -> int option
(** [find_free_from boot format fat start] returns an unallocated cluster
    after [start] *)

val extend: Boot_sector.t -> Fat_format.t -> Bitstring.t -> int option -> int -> Update.t list * int list
(** [extend boot format fat last n] allocates [n] free clusters to extend
    the chain whose current end is [last] *)
