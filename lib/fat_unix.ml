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

type 'a io = 'a (* no support for concurrent IO *)
let ( >>= ) x f = f x
let return x = x

(* NB not page-aligned *)
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
  fd: Unix.file_descr;
  info: info;
}

let connect filename =
  let fd = Unix.openfile filename [ Unix.O_RDWR ] 0o0 in
  let stats = Unix.LargeFile.fstat fd in
  let info = {
    read_write = true;
    sector_size = 512;
    size_sectors = Int64.(div stats.Unix.LargeFile.st_size 512L);
  } in
  return (`Ok { fd; info })

let get_info { info } = info

let rec really_read fd string off n =
  if n=0 then () else
    let m = Unix.read fd string off n in
    if m = 0 then begin
      for i = 0 to String.length string - 1 do
        string.[i] <- '\000'
      done;
      ()
    end else
    really_read fd string (off+m) (n-m)

let rec read x sector_start buffers = match buffers with
  | [] -> return (`Ok ())
  | b :: bs ->
    (* XXX: need a Cstruct read/write API *)
    ignore(Unix.LargeFile.lseek x.fd Int64.(mul sector_start (of_int x.info.sector_size)) Unix.SEEK_SET);
    let results = String.create (Cstruct.len b) in
    really_read x.fd results 0 (Cstruct.len b);
    Cstruct.blit_from_string results 0 b 0 (Cstruct.len b);
    read x Int64.(add sector_start (of_int (Cstruct.len b))) bs

let rec write x sector_start buffers = match buffers with
  | [] -> return (`Ok ())
  | b :: bs ->
    (* XXX: need a Cstruct read/write API *)
    ignore(Unix.LargeFile.lseek x.fd Int64.(mul sector_start (of_int x.info.sector_size)) Unix.SEEK_SET);
    let string = Cstruct.to_string b in
    let m = Unix.write x.fd string 0 (String.length string) in
    if m <> (String.length string) then failwith (Printf.sprintf "short write: expected=%d written=%d" (String.length string) m);
    write x Int64.(add sector_start (of_int (Cstruct.len b))) bs
