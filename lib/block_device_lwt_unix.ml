(*
 * Copyright (C) 2013 Citrix Systems Inc
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

(* Block device based on Lwt_unix *)

type 'a t = 'a Lwt.t
let ( >>= ) = Lwt.( >>= )
let return = Lwt.return

(* NB not page aligned *)
type page_aligned_buffer = Cstruct.t
let alloc = Cstruct.create

type error =
  | Unknown of string
  | Unimplemented
  | Is_read_only


type info = {
  read_write: bool;
  sector_size: int;
  size_sectors: int64;
}

type device = {
  fd: Lwt_unix.file_descr;
  info: info;
}

let connect filename =
  Lwt_unix.openfile filename [ Unix.O_RDWR ] 0o0 >>= fun fd ->
  Lwt_unix.LargeFile.fstat fd >>= fun stats ->
  let info = {
    read_write = true;
    sector_size = 512;
    size_sectors = Int64.(div stats.Unix.LargeFile.st_size 512L);
  } in
  return (`Ok { fd; info })

let get_info { info } = info

let complete op fd buffer =
  let open Lwt in
  let ofs = buffer.Cstruct.off in
  let len = buffer.Cstruct.len in
  let buf = buffer.Cstruct.buffer in
  let rec loop acc fd buf ofs len =
    op fd buf ofs len >>= fun n ->
    let len' = len - n in
    let acc' = acc + n in
    if len' = 0 || n = 0
    then return acc'
    else loop acc' fd buf (ofs + n) len' in
  loop 0 fd buf ofs len >>= fun n ->
  if n = 0 && len <> 0
  then fail End_of_file
  else return ()

let really_read = complete Lwt_bytes.read
let really_write = complete Lwt_bytes.write

let rec read x sector_start buffers = match buffers with
  | [] -> return (`Ok ())
  | b :: bs ->
    Lwt_unix.LargeFile.lseek x.fd Int64.(mul sector_start (of_int x.info.sector_size)) Unix.SEEK_SET >>= fun _ ->
    really_read x.fd b >>= fun () ->
    read x Int64.(add sector_start (div (of_int (Cstruct.len b)) 512L)) bs

let rec write x sector_start buffers = match buffers with
  | [] -> return (`Ok ())
  | b :: bs ->
    Lwt_unix.LargeFile.lseek x.fd Int64.(mul sector_start (of_int x.info.sector_size)) Unix.SEEK_SET >>= fun _ ->
    really_write x.fd b >>= fun () ->
    write x Int64.(add sector_start (div (of_int (Cstruct.len b)) 512L)) bs

