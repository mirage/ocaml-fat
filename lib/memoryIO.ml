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

type 'a io = 'a Lwt.t

type id = string

(* NB not actually page-aligned *)
type page_aligned_buffer = Cstruct.t

let alloc = Cstruct.create

type error = [
  | `Unknown of string
  | `Unimplemented
  | `Is_read_only
  | `Disconnected ]

type info = {
  read_write: bool;
  sector_size: int;
  size_sectors: int64;
}

module Int64Map = Map.Make(Int64)

type t = {
  mutable map: page_aligned_buffer Int64Map.t;
  info: info;
  id: id;
}

let id t = t.id

let devices = Hashtbl.create 1

let get_info { info } = return info

let connect name =
  if Hashtbl.mem devices name
  then return (`Ok (Hashtbl.find devices name))
  else
    let map = Int64Map.empty in
    let info = {
      read_write = true;
      sector_size = 512;
      size_sectors = 32768L; (* 16 MiB *)
    } in
    let device = { map; info; id = name } in
    Hashtbl.replace devices name device;
    return (`Ok device)

let disconnect t =
  t.map <- Int64Map.empty;
  return ()

let rec read x sector_start buffers = match buffers with
  | [] -> return (`Ok ())
  | b :: bs ->
    if Int64Map.mem sector_start x.map
    then Cstruct.blit (Int64Map.find sector_start x.map) 0 b 0 512
    else begin
      for i = 0 to 511 do
        Cstruct.set_uint8 b i 0
      done
    end;
    read x (Int64.succ sector_start)
      (if Cstruct.len b > 512
       then (Cstruct.shift b 512) :: bs
       else bs)

let rec write x sector_start buffers = match buffers with
  | [] -> return (`Ok ())
  | b :: bs ->
    if Cstruct.len b = 512 then begin
      x.map <- Int64Map.add sector_start b x.map;
      write x (Int64.succ sector_start) bs
    end else begin
      x.map <- Int64Map.add sector_start (Cstruct.sub b 0 512) x.map;
      write x (Int64.succ sector_start) (Cstruct.shift b 512 :: bs)
    end
