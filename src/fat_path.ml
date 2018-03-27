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

type t = string list (* stored in reverse order *)
let of_string_list x = List.rev x
let to_string_list x = List.rev x

let directory = List.tl
let filename = List.hd

let to_string p = "/" ^ (String.concat "/" (to_string_list p))

let slash = Re.Str.regexp_string "/"

let of_string s = if s = "/" || s = "" then [] else of_string_list (Re.Str.split slash s)

let concat path x = x :: path

let cd path x = of_string x @ (if x <> "" && x.[0] = '/' then [] else path)
let is_root p = p = []
