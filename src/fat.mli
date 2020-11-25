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

type stat = {
  filename: string; (** Filename within the enclosing directory *)
  read_only: bool;  (** True means the contents are read-only *)
  directory: bool;  (** True means the entity is a directory; false means a file *)
  size: int64;      (** Size of the entity in bytes *)
}
(** The type for Per-file/directory statistics. *)

module Make (B: Mirage_block.S): sig

  type error = [
    | `Is_a_directory
    | `No_directory_entry
    | `Not_a_directory
    | `Block_read of B.error
  ]
  val pp_error: error Fmt.t
  (** [pp_error] is the pretty-printer for errors. *)

  type write_error = [
    | error
    | `Is_a_directory
    | `No_directory_entry
    | `Not_a_directory
    | `File_already_exists
    | `No_directory_entry
    | `No_space
    | `Directory_not_empty
    | `Block_write of B.write_error
    | `Exn of exn
  ]
  val pp_write_error: write_error Fmt.t
  (** [pp_write_error] is the pretty-printer for write errors. *)

  include Mirage_device.S

  val connect : B.t -> t Lwt.t
  val format : B.t -> int64 -> (t, write_error) result Lwt.t

  val read: t -> string -> int -> int ->
    (Cstruct.t list, error) result Lwt.t
  (** [read t key offset length] reads up to [length] bytes from the
      value associated with [key]. If less data is returned than
      requested, this indicates the end of the value. *)

  val size: t -> string -> (int64, error) result Lwt.t
  (** Get the value size. *)

  val create: t -> string -> (unit, write_error) result Lwt.t
  (** [create t path] creates an empty file at [path]. If [path]
      contains directories that do not yet exist, [create] will
      attempt to create them. *)

  val mkdir: t -> string -> (unit, write_error) result Lwt.t
  (** [mkdir t path] creates an empty directory at [path].  If [path]
      contains intermediate directories that do not yet exist, [mkdir]
      will create them.  If a directory already exists at [path],
      [mkdir] returns [`Ok ()] and takes no action. *)

  val destroy: t -> string -> (unit, write_error) result Lwt.t
  (** [destroy t path] removes a [path] (which may be a file or an
      empty directory) on filesystem [t]. *)

  val stat: t -> string -> (stat, error) result Lwt.t
  (** [stat t path] returns information about file or directory at
      [path]. *)

  val listdir: t -> string -> (string list, error) result Lwt.t
  (** [listdir t path] returns the names of files and subdirectories
      within the directory [path]. *)

  val write: t -> string -> int -> Cstruct.t ->
    (unit, write_error) result Lwt.t
  (** [write t path offset data] writes [data] at [offset] in file
      [path] on filesystem [t].

      If [path] contains directories that do not exist, [write] will
      attempt to create them.  If [path] already exists, [write] will
      overwrite existing information starting at [off].*)

end

module KV_RO(B: Mirage_block.S): sig
  include Mirage_kv.RO

  val connect: B.t -> t Lwt.t
  (** [connect block] creates a key=value store over a FAT filesystem on
      the [block] device> *)
end
(** KV_RO is a read-only key=value store backed by a simple FAT filesystem
    on a block device. *)
