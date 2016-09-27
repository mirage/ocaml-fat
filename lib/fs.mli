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

type block_error = [ `Unknown of string | `Unimplemented | `Is_read_only | `Disconnected ]

val string_of_block_error: block_error -> string

type filesystem_error = [
  | `Not_a_directory of string             (** named directory entry is not a directory *)
  | `Is_a_directory of string              (** named directory entry is a directory *)
  | `Directory_not_empty of string         (** can't delete non-empty directories *)
  | `No_directory_entry of string * string (** named directory entry doesn't exist *)
  | `File_already_exists of string         (** named directory entry does exist *)
  | `No_space                              (** there's no space left for new blocks *)
  | `Format_not_recognised of string       (** the block device isn't formatted with FAT *)
  | `Unknown_error of string               (** some unknown error occurred *)
  | `Block_device of block_error           (** the underying block device returned an error *)
]

val string_of_filesystem_error: filesystem_error -> string

module Make (B: V1.BLOCK
  with type 'a io = 'a Lwt.t
  and type page_aligned_buffer = Cstruct.t)(M: S.IO_PAGE) : sig
  include V1.FS
    with type 'a io = 'a Lwt.t
    and type block_device_error = B.error
    and type page_aligned_buffer = Cstruct.t
  val connect : B.t -> t io
end
