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

module type BLOCK_DEVICE = V1.BLOCK.CLIENT
with type page_aligned_buffer = Cstruct.t
and type 'a io = 'a Lwt.t

module type IO_PAGE = V1.IO_PAGE

module Error = struct

  let to_string = function
    | `Not_a_directory x ->
      Printf.sprintf "Not_a_directory %s" x
    | `Is_a_directory x->
      Printf.sprintf "Is_a_directory %s" x
    | `Directory_not_empty x ->
      Printf.sprintf "Directory_not_empty %s" x
    | `No_directory_entry (x, y) ->
      Printf.sprintf "No_directory_entry %s %s" x y
    | `File_already_exists x ->
      Printf.sprintf "File_already_exists %s" x
    | `No_space ->
      Printf.sprintf "No_space"
    | `Format_not_recognised details ->
      Printf.sprintf "Format_not_recognised: %s" details
    | `Unknown_error x ->
      Printf.sprintf "Unknown_error: %s" x
    | `Block_device _ ->
      Printf.sprintf "Block device error"
end

module type FS = sig

  type block_device_error

  type error = [
    | `Not_a_directory of string
    | `Is_a_directory of string
    | `Directory_not_empty of string
    | `No_directory_entry of string * string
    | `File_already_exists of string
    | `No_space
    | `Format_not_recognised of string
    | `Unknown_error of string
    | `Block_device of block_device_error
  ]

  include V1.DEVICE with
    type error := error

  type stat = {
    filename: string;
    read_only: bool;
    directory: bool;
    size: int64;
  }

  val format: t -> int64 -> [ `Ok of unit | `Error of error ] io
  (** [format size] creates an empty filesystem of size [size] *)

  val create: t -> string -> [ `Ok of unit | `Error of error ] io

  val mkdir: t -> string -> [ `Ok of unit | `Error of error ] io

  val destroy: t -> string -> [ `Ok of unit | `Error of error ] io
  (** [destroy t path] removes a [path] on filesystem [t] *)

  val stat: t -> string -> [ `Ok of stat | `Error of error ] io
  (** [stat fs f] returns information about file [f] on filesystem [fs] *)

  val listdir: t -> string -> [ `Ok of string list | `Error of error ] io
  (** [listdir fs dir] returns the names of files within the directory [dir]
      or the error `Not_a_directory *)

  val write: t -> string -> int -> Cstruct.t -> [ `Ok of unit | `Error of error ] io
  (** [write fs f offset data] writes [data] at [offset] in file [f] on
      filesystem [fs] *)

  val read: t -> string -> int -> int -> [ `Ok of Cstruct.t list | `Error of error ] io
  (** [read fs f offset length] reads up to [length] bytes from file [f] on
      filesystem [fs]. If less data is returned than requested, this indicates
      end-of-file. *)
end

