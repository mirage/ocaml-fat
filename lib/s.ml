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

module type IO = sig
  type 'a t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t

  val read_sector: Cstruct.t -> int -> unit
  val write_sector: Cstruct.t -> int -> unit
end

module Error = struct
  type t =
    | Not_a_directory of Path.t
    | Is_a_directory of Path.t
    | Directory_not_empty of Path.t
    | No_directory_entry of Path.t * string
    | File_already_exists of string
    | No_space

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
end

module Stat = struct 
  type t =  
    | File of Name.r 
    | Dir of Name.r * (Name.r list) (** the directory itself and its immediate children *) 
end 

module type FS = sig
  type fs

  val make: int64 -> fs
  (** [make size] creates a filesystem of size [size] *)

  val openfile: unit -> fs

  type file

  val create: fs -> Path.t -> (unit, Error.t) Result.result

  val mkdir: fs -> Path.t -> (unit, Error.t) Result.result

  val destroy: fs -> Path.t -> (unit, Error.t) Result.result
  (** [destroy fs path] removes a [path] on filesystem [fs] *)

  val file_of_path: fs -> Path.t -> file
  (** [file_of_path fs path] returns a [file] corresponding to [path] on
       filesystem [fs] *)

  val stat: fs -> Path.t -> (Stat.t, Error.t) Result.result
  (** [stat fs f] returns information about file [f] on filesystem [fs] *)

  val write: fs -> file -> int -> Cstruct.t -> (unit, Error.t) Result.result
  (** [write fs f offset data] writes [data] at [offset] in file [f] on
      filesystem [fs] *)

  val read: fs -> file -> int -> int -> (Cstruct.t list, Error.t) Result.result
  (** [read fs f offset length] reads up to [length] bytes from file [f] on
      filesystem [fs]. If less data is returned than requested, this indicates
      end-of-file. *)
end

