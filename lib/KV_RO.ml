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
  type page_aligned_buffer = FS.page_aligned_buffer

  let connect t = return t

  type error = [ V1.Kv_ro.error | `FS of FS.error ]
  type write_error = FS.write_error

  let pp_error ppf = function
    | #V1.Kv_ro.error as e -> Mirage_pp.pp_kv_ro_error ppf e
    | `FS e -> FS.pp_error ppf e

  let write_error = FS.pp_write_error

  let disconnect t =
    FS.disconnect t

  let mem t name =
    FS.stat t name >|= function
    | Result.Ok _ -> Result.Ok true
    | Result.Error `Not_a_directory
    | Result.Error `No_directory_entry -> Result.Ok false
    | Result.Error e -> Result.Error (`FS e)

  let read t name off len =
    FS.read t name (Int64.to_int off) (Int64.to_int len) >|= function
    | Result.Error `Not_a_directory | Result.Error `No_directory_entry ->
      Result.Error (`Unknown_key name)
    | Result.Error e -> Result.Error (`FS e)
    | Result.Ok l -> Result.Ok l

  let size t name =
    FS.stat t name >|= function
    | Result.Error `Not_a_directory
    | Result.Error `No_directory_entry -> Result.Error (`Unknown_key name)
    | Result.Error e -> Result.Error (`FS e)
    | Result.Ok stat -> Result.Ok (stat.FS.size)

end
