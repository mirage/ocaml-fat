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
open Lwt
open Fat
open S

module Block = Mirage_block.Block
module Filesystem = Fs.Make(Block)
open Common

exception Block_error of Block.error

let (>>|=) m f = m >>= function
  | `Error e -> fail (Block_error e)
  | `Ok x -> f x

let create common filename size =
  let t =
    ( if Sys.file_exists filename
      then fail (Failure (Printf.sprintf "%s already exists" filename))
      else return () ) >>= fun () ->
    Lwt_unix.openfile filename [ Unix.O_CREAT; Unix.O_RDWR ] 0o0644 >>= fun fd ->

    Lwt_unix.LargeFile.lseek fd Int64.(sub size 512L) Lwt_unix.SEEK_CUR >>= fun _ ->
    let message = "All work and no play makes Dave a dull boy.\n" in
    let sector = Mirage_block.Block.Memory.alloc 512 in
    for i = 0 to 511 do
      Cstruct.set_char sector i (message.[i mod (String.length message)])
    done;
    Block.really_write fd sector >>= fun () ->
    Lwt_unix.close fd >>= fun () ->

    Block.connect filename >>|= fun device ->
    if common.verb then Printf.printf "Created %s\n%!" filename;
    Filesystem.make device size >>= fun _ ->
    if common.verb then Printf.printf "Filesystem of size %Ld created\n%!" size;
    return () in
  try
    Lwt_main.run t;
    `Ok ()
  with Filesystem.Block_device_error `Is_read_only ->
    `Error(false, "File is read only")
  | Filesystem.Block_device_error `Unimplemented ->
    `Error(false, "Block operation is unimplemented")
  | Filesystem.Block_device_error (`Unknown x) ->
    `Error(false, x)
  | Filesystem.Block_device_error _ ->
    `Error(false, "Unknown block device error")

let add common filename files =
  Printf.fprintf stderr "add %s <- [ %s ]\n%!" filename (String.concat "; " files);
  `Ok ()
