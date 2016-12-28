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

(** FAT32 implementation *)

[@@@ocaml.warning "-34"]

(** FIXME: this should be in io-page *)
module type IO_PAGE = sig

  val get_buf : ?n:int -> unit -> Cstruct.t
  (** [get_buf ~n ()] allocates and returns a memory block of [n] pages,
      represented as a {!Cstruct.t}. If there is not enough memory,
      an [Out_of_memory] exception is raised. *)

end

(** The main FS interface. *)
module FS (B: V1_LWT.BLOCK) (M: IO_PAGE): sig

  type error = [
    | V1.Fs.error
    | `Block_read of B.error
  ]
  type write_error = [
    | error
    | V1.Fs.write_error
    | `Directory_not_empty
    | `Block_write of B.write_error
    | `Exn of exn
  ]
  include V1_LWT.FS with type error := error and type write_error := write_error
  val connect : B.t -> t Lwt.t
  val format : B.t -> int64 -> (t, write_error) Result.result Lwt.t

end

(** An in-memory FAT implementation. *)
module MemFS (M: IO_PAGE): sig

  type error = [
    | V1.Fs.error
    | `Block_read of Mirage_block.error
  ]
  type write_error = [
    | error
    | V1.Fs.write_error
    | `Directory_not_empty
    | `Block_write of Mirage_block.write_error
    | `Exn of exn
  ]
  include V1_LWT.FS with type error := error and type write_error := write_error
  val connect : string -> t Lwt.t
  val format : string -> int64 -> (t, write_error) Result.result Lwt.t

end
