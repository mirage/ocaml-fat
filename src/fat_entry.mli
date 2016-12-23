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

(** a whole File Allocation Table *)
type fat = Cstruct.t

val make: Fat_boot_sector.t -> Fat_format.t -> Cstruct.t
(** [make boot_sector format] creates an empty FAT given the parameters
    in [boot_sector] and the [format] *)

(** [find_free_from boot format fat start] returns an unallocated cluster
    after [start] *)
val find_free_from: Fat_boot_sector.t -> Fat_format.t -> fat -> int -> int option

(** first valid entry *)
val initial: int

(** [unmarshal format n fat] return the [n]th [fat] entry in [format] *)
val unmarshal: Fat_format.t -> int -> fat -> t

(** [marhsal format n fat v] update the [n]th [fat] entry in [format] with [v] *)
val marshal: Fat_format.t -> int -> fat -> t -> unit

module Chain : sig

  type t = int list
  (** A sequence of clusters containing file data *)

  (** [follow_chain format fat cluster] returns the list of sectors containing
      data according to FAT [fat] which is of type [format]. *)
  val follow: Fat_format.t -> fat -> int -> t

  (** [extend boot format fat last n] allocates [n] free clusters to extend
      the chain whose current end is [last] *)
  val extend: Fat_boot_sector.t -> Fat_format.t -> fat -> int option -> int -> t

  val to_sectors: Fat_boot_sector.t -> t -> int list
  (** [to_sectors boot t] converts the chain [t] into a sequence of sectors *)
end
