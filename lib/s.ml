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

module type BLOCK_DEVICE = sig

  (** Abstract type of a block device instance. *)
  type t

  (** Abstract type for a blocking IO operation *)
  type 'a io

  (** Abstract type for a page-aligned memory buffer *)
  type page_aligned_buffer

  (** IO operation errors *)
  type error =
    | Unknown of string (** an undiagnosed error *)
    | Unimplemented     (** operation not yet implemented in the code *)
    | Is_read_only      (** you cannot write to a read/only instance *)

  (** Characteristics of the block device. Note some devices may be able
      to make themselves bigger over time. *)
  type info = {
    read_write: bool;    (** True if we can write, false if read/only *)
    sector_size: int;    (** Octets per sector *)
    size_sectors: int64; (** Total sectors per device *)
  }

  (** Connect to a named block device *)
  val connect: string -> [ `Error of error | `Ok of t ] io

  (** Query the characteristics of a specific block device *)
  val get_info: t -> info io

  (** [read device sector_start buffers] returns a blocking IO operation which
      attempts to fill [buffers] with data starting at [sector_start].
      Each of [buffers] must be a whole number of sectors in length. *)
  val read: t -> int64 -> page_aligned_buffer list -> [ `Error of error | `Ok of unit ] io

  (** [write device sector_start buffers] returns a blocking IO operation which
      attempts to write the data contained within [buffers] to [t] starting
      at [sector_start]. If an error occurs then the write may have partially
      succeeded.
      Each of [buffers] must be a whole number of sectors in length. *)
  val write: t -> int64 -> page_aligned_buffer list -> [ `Error of error | `Ok of unit ] io
end

module Error = struct
  type t =
    | Not_a_directory of Path.t
    | Is_a_directory of Path.t
    | Directory_not_empty of Path.t
    | No_directory_entry of Path.t * string
    | File_already_exists of string
    | No_space
    | Unknown_error of string

  let to_string = function
    | Not_a_directory x ->
      Printf.sprintf "Not_a_directory %s" (Path.to_string x)
    | Is_a_directory x->
      Printf.sprintf "Is_a_directory %s" (Path.to_string x)
    | Directory_not_empty x ->
      Printf.sprintf "Directory_not_empty %s" (Path.to_string x)
    | No_directory_entry (x, y) ->
      Printf.sprintf "No_directory_entry %s %s" (Path.to_string x) y
    | File_already_exists x ->
      Printf.sprintf "File_already_exists %s" x
    | No_space ->
      Printf.sprintf "No_space"
    | Unknown_error x ->
      Printf.sprintf "Unknown_error: %s" x
end

module Stat = struct 
  type t =  
    | File of Name.r 
    | Dir of Name.r * (Name.r list) (** the directory itself and its immediate children *) 
end 

module type FS = sig
  type fs

  type block_device

  type 'a io

  val make: block_device -> int64 -> fs io
  (** [make size] creates a filesystem of size [size] *)

  val openfile: block_device -> fs io

  type file

  val create: fs -> Path.t -> [ `Ok of unit | `Error of Error.t ] io

  val mkdir: fs -> Path.t -> [ `Ok of unit | `Error of Error.t ] io

  val destroy: fs -> Path.t -> [ `Ok of unit | `Error of Error.t ] io
  (** [destroy fs path] removes a [path] on filesystem [fs] *)

  val file_of_path: fs -> Path.t -> file
  (** [file_of_path fs path] returns a [file] corresponding to [path] on
       filesystem [fs] *)

  val stat: fs -> Path.t -> [ `Ok of Stat.t | `Error of Error.t ] io
  (** [stat fs f] returns information about file [f] on filesystem [fs] *)

  val write: fs -> file -> int -> Cstruct.t -> [ `Ok of unit | `Error of Error.t ] io
  (** [write fs f offset data] writes [data] at [offset] in file [f] on
      filesystem [fs] *)

  val read: fs -> file -> int -> int -> [ `Ok of Cstruct.t list | `Error of Error.t ] io
  (** [read fs f offset length] reads up to [length] bytes from file [f] on
      filesystem [fs]. If less data is returned than requested, this indicates
      end-of-file. *)
end

