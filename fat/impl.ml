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

module Filesystem = Fs.Make(Block)(Io_page)
open Common

(* Default policy when we hit a block- or fs-level error which we don't
   have specific code to handle is to 'fail' the Lwt thread with
   the exception Failure "user-readable message". *)

let (>>|=) m f = m >>= function
  | Result.Error (`Msg m) -> fail (Failure m)
  | Result.Error `Unimplemented -> fail (Failure "Block layer unimplemented")
  | Result.Error `Disconnected -> fail (Failure "Block layer disconnected")
  | Result.Error `Is_read_only -> fail (Failure "Block layer is read only")
  | Result.Ok x -> f x

let (>>*=) m f = m >>= function
  | Result.Error (`Msg m) -> fail (Failure m)
  | Result.Error `Directory_not_empty -> fail (Failure "Directory not empty")
  | Result.Error `File_already_exists -> fail (Failure "File already exists")
  | Result.Error `Is_a_directory -> fail (Failure "Is a directory")
  | Result.Error `No_directory_entry -> fail (Failure "No directory entry")
  | Result.Error `No_space -> fail (Failure "No space")
  | Result.Error `Not_a_directory -> fail (Failure "Not a directory")
  | Result.Ok x -> f x

let rec iter_s f = function
  | [] -> Lwt.return (Result.Ok ())
  | x :: xs ->
    f x >>= function
    | Result.Error e -> Lwt.return (Result.Error e)
    | Result.Ok () -> iter_s f xs

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
          Filesystem.write fs inside offset frag >>= function
          | Result.Ok () -> loop (offset + this) (remaining - this)
          | Result.Error e ->
            let b = Buffer.create 20 in
            let ppf = Format.formatter_of_buffer b in
            let k ppf = Format.pp_print_flush ppf (); fail (Failure (Buffer.contents b)) in
            Format.kfprintf k ppf "%a while copying %s -> %s, at offset %d with %d bytes remaining"
              Mirage_pp.pp_fs_write_error e outside inside offset remaining in
      loop 0 stats.Lwt_unix.st_size
    )

let run t =
  try
    Lwt_main.run t;
    `Ok ()
  with
  | Failure x -> `Error(false, x)

let buffered common filename =
  if common.unbuffered
  then filename
  else "buffered:" ^ filename

let create common filename size =
  let size =
    let sz = parse_size size in
    max Int64.(add (mul 512L 48L) 1L) sz
  in
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

    Block.connect (buffered common filename) >>= fun device ->
    Filesystem.format device size >>*= fun _fs ->
    if common.verb then Printf.printf "Filesystem of size %Ld created\n%!" size;
    return () in
  run t

let add common filename files =
  (* the first entry in [files] is the image name *)
  let files = List.tl files in
  Printf.fprintf stderr "add %s <- [ %s ]\n%!" filename (String.concat "; " files);
  let t =
    Block.connect (buffered common filename) >>= fun device ->
    if common.verb then Printf.printf "Opened %s\n%!" filename;
    Filesystem.connect device >>|= fun fs ->
    let rec copyin outside_path inside_path file =
      let outside_path = Filename.concat outside_path file in
      let inside_path = Filename.concat inside_path file in
      Lwt_unix.stat outside_path >>= fun stats ->
      match stats.Lwt_unix.st_kind with
      | Lwt_unix.S_REG ->
        Printf.fprintf stderr "copyin %s to %s\n%!" outside_path inside_path;
        Filesystem.create fs inside_path >>*= fun _ ->
        Filesystem.listdir fs (Filename.dirname inside_path) >>*= fun xs ->
        if not(List.mem (Filename.basename inside_path) xs)
        then fail (Failure (Printf.sprintf "listdir(%s) = [ %s ], doesn't include '%s'(%d)"
                              (Filename.dirname inside_path)
                              (String.concat ", " (List.map (fun x -> Printf.sprintf "'%s'(%d)" x (String.length x)) xs))
                              (Filename.basename inside_path)
                              (String.length (Filename.basename inside_path))
                           ))
        else copy_file_in fs outside_path inside_path
      | Lwt_unix.S_DIR ->
        let children = Array.to_list (Sys.readdir outside_path) in
        Filesystem.mkdir fs inside_path >>*= fun _ ->
        Lwt_list.iter_s (copyin outside_path inside_path) children
      | _ ->
        Printf.fprintf stderr "Skipping file: %s\n%!" outside_path;
        return () in
    Lwt_list.iter_s (copyin "" "/") files in
  run t

let list common filename =
  let t =
    Block.connect (buffered common filename) >>= fun device ->
    Filesystem.connect device >>|= fun fs ->
    let rec loop curdir =
      Filesystem.listdir fs curdir >>*= fun children ->
      Lwt_list.iter_s
        (fun child ->
           let path = Filename.concat curdir child in
           Filesystem.stat fs path >>*= fun stats ->
           Printf.printf "%s (%s)(%Ld bytes)\n" path
             (if stats.Filesystem.directory then "DIR" else "FILE") stats.Filesystem.size;
           if stats.Filesystem.directory
           then loop path
           else return ()
        ) children in
    loop "/" in
  run t

let cat common filename path =
  let t =
    Block.connect (buffered common filename) >>= fun device ->
    Filesystem.connect device >>|= fun fs ->
    let rec loop offset =
      Filesystem.read fs path offset 1024 >>*= fun bufs ->
      List.iter (fun x -> print_string (Cstruct.to_string x)) bufs;
      let copied = List.fold_left (+) 0 (List.map Cstruct.len bufs) in
      if copied < 1024
      then return ()
      else loop (offset + copied) in
    loop 0 in
  run t
