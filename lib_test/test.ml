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

open OUnit
open Lwt
open Fat
open Fat_lwt

let read_sector filename =
  Lwt_unix.openfile filename [ Lwt_unix.O_RDONLY ] 0o0 >>= fun fd ->
  let buf = Cstruct.create 512 in
  really_read fd buf >>= fun () ->
  Lwt_unix.close fd >>= fun () ->
  return buf

let test_parse_boot_sector () =
  let t =
    let open Boot_sector in
    read_sector "lib_test/bootsector.dat" >>= fun bytes ->
    let x = match unmarshal bytes with
    | Result.Error x -> failwith x
    | Result.Ok x -> x in
    assert_equal "mkdosfs\000" x.oem_name;
    assert_equal ~printer:string_of_int 512 x.bytes_per_sector;
    assert_equal ~printer:string_of_int 4 x.sectors_per_cluster;
    assert_equal ~printer:string_of_int 4 x.reserved_sectors;
    assert_equal ~printer:string_of_int 2 x.number_of_fats;
    assert_equal ~printer:string_of_int 512 x.number_of_root_dir_entries;
    assert_equal ~printer:Int32.to_string 30720l x.total_sectors;
    assert_equal ~printer:string_of_int 32 x.sectors_per_fat;
    assert_equal ~printer:Int32.to_string 0l x.hidden_preceeding_sectors;
    return () in
  Lwt_main.run t

let _ =
  let verbose = ref false in
  Arg.parse [
    "-verbose", Arg.Unit (fun _ -> verbose := true), "Run in verbose mode";
  ] (fun x -> Printf.fprintf stderr "Ignoring argument: %s" x)
    "Test FAT filesystem";

  let suite = "fat" >::: [
    "test_parse_boot_sector" >:: test_parse_boot_sector;
  ] in
  run_test_tt ~verbose:!verbose suite

