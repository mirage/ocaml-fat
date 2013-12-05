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
module Filesystem = Fs.Make(Block)(Io_page)
open Common
let f = Filesystem.openfile

exception Block_error of Block.error

let (>>|=) m f = m >>= function
  | `Error e -> fail (Block_error e)
  | `Ok x -> f x

let alloc bytes =
  let pages = Io_page.(to_cstruct (get ((bytes + 4095) / 4096))) in
  Cstruct.sub pages 0 bytes

let with_file flags filename f =
  Lwt_unix.openfile filename flags 0o0 >>= fun file ->
  catch (fun () -> f file >>= fun x -> Lwt_unix.close file >>= fun () -> return x) (fun x -> Lwt_unix.close file >>= fun () -> fail x)

let copy_file_in fs outside inside =
  Lwt_unix.lstat outside >>= fun stats ->
  let block_size = 1024 * 1024 in
  let block = alloc block_size in
  with_file [ Unix.O_RDONLY ] outside
    (fun ifd ->
      let rec loop offset remaining =
        if remaining = 0
        then return ()
        else
          let this = min remaining block_size in
          let frag = Cstruct.sub block 0 this in
          Block.really_read ifd frag >>= fun () ->
          Filesystem.write fs (Filesystem.file_of_path fs inside) offset frag >>= function
          | `Ok () -> loop (offset + this) (remaining - this)
          | `Error _ -> failwith "some error" in
      loop 0 stats.Lwt_unix.st_size
    )

let run t =
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


let create common filename size =
  let t =
    ( if Sys.file_exists filename
      then fail (Failure (Printf.sprintf "%s already exists" filename))
      else return () ) >>= fun () ->
    Lwt_unix.openfile filename [ Unix.O_CREAT; Unix.O_RDWR ] 0o0644 >>= fun fd ->

    Lwt_unix.LargeFile.lseek fd Int64.(sub size 512L) Lwt_unix.SEEK_CUR >>= fun _ ->
    let message = "All work and no play makes Dave a dull boy.\n" in
    let sector = alloc 512 in
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
  run t

let add common filename files =
  (* the first entry in [files] is the image name *)
  let files = List.tl files in
  Printf.fprintf stderr "add %s <- [ %s ]\n%!" filename (String.concat "; " files);
  let t =
    Block.connect filename >>|= fun device ->
    if common.verb then Printf.printf "Opened %s\n%!" filename;
    Filesystem.openfile device >>= fun fs ->
    let rec copyin outside_path inside_path file =
      let outside_path = Filename.concat outside_path file in
      let inside_path = Path.concat inside_path file in
      Lwt_unix.stat outside_path >>= fun stats ->
      match stats.Lwt_unix.st_kind with
      | Lwt_unix.S_REG ->
        Printf.fprintf stderr "copyin %s to %s\n%!" outside_path (Path.to_string inside_path);
        ( Filesystem.create fs inside_path >>= function
          | `Error _ -> fail (Failure (Printf.sprintf "Failed to create %s" (Path.to_string inside_path)))
          | `Ok () -> return () ) >>= fun () ->
        copy_file_in fs outside_path inside_path
      | Lwt_unix.S_DIR ->
        let children = Array.to_list (Sys.readdir outside_path) in
        ( Filesystem.mkdir fs inside_path >>= function
          | `Error _ -> fail (Failure (Printf.sprintf "Failed to mkdir %s" (Path.to_string inside_path)))
          | `Ok () -> return () ) >>= fun () ->
        Lwt_list.iter_s (copyin outside_path inside_path) children
      | _ ->
        Printf.fprintf stderr "Skipping file: %s\n%!" outside_path;
        return () in
    Lwt_list.iter_s (copyin "" (Path.of_string "/")) files in
  run t
