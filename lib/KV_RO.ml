(*
 * Copyright (C) 2013 Anil Madhavapeddy <anil@recoil.org>
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

open V1
open Lwt

module Make(FS: FS with
       type 'a io = 'a Lwt.t
  ) = struct

  type t = FS.t
  type +'a io = 'a Lwt.t
  type id = FS.id
  type page_aligned_buffer = FS.page_aligned_buffer

  type error = Unknown_key of string

  let connect id =
    FS.connect id
    >>= function
    | `Error _ -> return (`Error (Unknown_key "connect"))
    | `Ok t -> return (`Ok t)

  let disconnect id =
    FS.disconnect id

  let id t = FS.id t

  let read t name off len =
    FS.read t name off len
    >>= function
    | `Error _ -> return (`Error (Unknown_key name))
    | `Ok l -> return (`Ok l)

  let size t name =
    FS.stat t name
    >>= function
    | `Error _ -> return (`Error (Unknown_key name))
    | `Ok stat -> return (`Ok (stat.FS.size))
end
