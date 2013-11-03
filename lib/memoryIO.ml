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

type 'a t = 'a

let ( >>= ) x f = f x

let return x = x

module IntMap = Map.Make(struct
  type t = int
  let compare (x: int) (y: int) = compare x y
end)

let map = ref IntMap.empty

let read_sector buf n =
  if IntMap.mem n !map
  then Cstruct.blit (IntMap.find n !map) 0 buf 0 512
  else
    for i = 0 to 511 do
      Cstruct.set_uint8 buf i 0
    done

let write_sector buf n =
  let sector = Cstruct.create 512 in
  Cstruct.blit buf 0 sector 0 512;
  map := IntMap.add n sector !map

