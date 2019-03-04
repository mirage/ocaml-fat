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

open Lwt.Infix
open Block
open Mirage_fs

module MemFS = Fat.FS(Mirage_block_lwt.Mem)

let fail fmt = Fmt.kstrf Lwt.fail_with fmt

let (>>*=) m f = m >>= function
  | Error e -> fail "%a" MemFS.pp_write_error (e :> MemFS.write_error)
  | Ok x    -> f x

let alloc bytes =
  let pages = Io_page.(to_cstruct (get ((bytes + 4095) / 4096))) in
  Cstruct.sub pages 0 bytes

let read_sector filename =
  Lwt_unix.openfile filename [ Lwt_unix.O_RDONLY ] 0o0 >>= fun fd ->
  let buf = alloc 512 in
  really_read fd buf >>= fun () ->
  Lwt_unix.close fd >|= fun () ->
  buf

let read_whole_file filename =
  Lwt_unix.openfile filename [ Lwt_unix.O_RDONLY ] 0o0 >>= fun fd ->
  let size = (Unix.stat filename).Unix.st_size in
  let buf = alloc size in
  really_read fd buf >>= fun () ->
  Lwt_unix.close fd >|= fun () ->
  buf

let checksum_test () =
  let open Fat_name in
  let checksum_tests = [
    make "MAKEFILE", 193;
    make "FAT.ML", 223;
  ] in
  List.iter (fun (d, expected) ->
      let d = snd d.dos in
      let checksum = compute_checksum d in
      Alcotest.(check int) __LOC__ expected checksum
    ) checksum_tests

let print_int_list xs = "[" ^ ( String.concat "; " (List.map string_of_int xs) ) ^ "]"

let test_root_list () =
  let open Fat_name in
  let t =
    read_whole_file "root.dat" >>= fun bytes ->
    let all = list bytes in
    Alcotest.(check int) __LOC__ 5 (List.length all);
    let x = List.nth all 1 in
    let utf_filename =
      "l\000o\000w\000e\000r\000.\000t\000x\000t\000\000\000\255\255\255\255\255\255"
    in
    Alcotest.(check string) __LOC__ utf_filename x.utf_filename;
    let a, b = x.dos in
    Alcotest.(check int)    __LOC__ 192 a;
    Alcotest.(check string) __LOC__ "LOWER" b.filename;
    Alcotest.(check string) __LOC__ "TXT" b.ext;
    Alcotest.(check bool)   __LOC__ false b.deleted;
    Alcotest.(check bool)   __LOC__ false b.read_only;
    Alcotest.(check bool)   __LOC__ false b.hidden;
    Alcotest.(check bool)   __LOC__ false b.system;
    Alcotest.(check bool)   __LOC__ false b.volume;
    Alcotest.(check bool)   __LOC__ false b.subdir;
    Alcotest.(check bool)   __LOC__ true b.archive;
    Alcotest.(check int)    __LOC__ 0 b.start_cluster;
    Alcotest.(check int32)  __LOC__ 0l b.file_size;
    Alcotest.(check int)    __LOC__ 2013 b.create.year;
    Alcotest.(check int)    __LOC__ 11 b.create.month;
    Alcotest.(check int)    __LOC__ 2 b.create.day;
    Alcotest.(check int)    __LOC__ 16 b.create.hours;
    Alcotest.(check int)    __LOC__ 58 b.create.mins;
    Alcotest.(check int)    __LOC__ 52 b.create.secs;
    Alcotest.(check int)    __LOC__ 100 b.create.ms;
    let lfns = x.lfns in
    Alcotest.(check int)    __LOC__ 1 (List.length lfns);
    let a, b = List.hd lfns in
    Alcotest.(check int)    __LOC__ 160 a;
    Alcotest.(check bool)   __LOC__ false b.lfn_deleted;
    Alcotest.(check bool)   __LOC__ true b.lfn_last;
    Alcotest.(check int)    __LOC__ 1 b.lfn_seq;
    Alcotest.(check int)    __LOC__ 252 b.lfn_checksum;
    Alcotest.(check string) __LOC__ utf_filename b.lfn_utf16_name;
    Lwt.return ()
  in
  Lwt_main.run t

let fat_format = Alcotest.testable (Fmt.of_to_string Fat_format.to_string) (=)

let test_chains () =
  let t =
    let open Fat_boot_sector in
    read_sector "bootsector.dat" >>= fun bytes ->
    let boot = match unmarshal bytes with
      | Error x -> failwith x
      | Ok x -> x in
    Alcotest.(check (result fat_format string)) __LOC__
      (Ok Fat_format.FAT16) (Fat_boot_sector.detect_format boot);
    read_whole_file "root.dat" >>= fun bytes ->
    let open Fat_name in
    let all = list bytes in
    read_whole_file "fat.dat" >>= fun fat ->

    let expected = [0; 0; 0; 2235; 3] in
    let actual = List.map (fun x -> (snd (x.dos)).start_cluster) all in
    Alcotest.(check (list int)) __LOC__ expected actual;
    Alcotest.(check (list int)) __LOC__ []
      (Fat_entry.Chain.follow Fat_format.FAT16 fat 0);

    Alcotest.(check (list int)) __LOC__  [2235]
      (Fat_entry.Chain.follow Fat_format.FAT16 fat 2235);
    let rec ints last x = if x = last then [x] else x :: (ints last (x + 1)) in
    let expected = ints 2234 3 in
    Alcotest.(check (list int)) __LOC__ expected
      (Fat_entry.Chain.follow Fat_format.FAT16 fat 3);
    Lwt.return ()
  in
  Lwt_main.run t

let test_parse_boot_sector () =
  let t =
    let open Fat_boot_sector in
    read_sector "bootsector.dat" >>= fun bytes ->
    let x = match unmarshal bytes with
      | Error x -> failwith x
      | Ok x -> x in
    let check x =
      Alcotest.(check string) __LOC__ "mkdosfs\000" x.oem_name;
      Alcotest.(check int)    __LOC__ 512 x.bytes_per_sector;
      Alcotest.(check int)    __LOC__ 4 x.sectors_per_cluster;
      Alcotest.(check int)    __LOC__ 4 x.reserved_sectors;
      Alcotest.(check int)    __LOC__ 2 x.number_of_fats;
      Alcotest.(check int)    __LOC__ 512 x.number_of_root_dir_entries;
      Alcotest.(check int32)  __LOC__ 30720l x.total_sectors;
      Alcotest.(check int)    __LOC__ 32 x.sectors_per_fat;
      Alcotest.(check int32)  __LOC__ 0l x.hidden_preceeding_sectors;
      let sectors_of_fat =
        [4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20; 21;
         22; 23; 24; 25; 26; 27; 28; 29; 30; 31; 32; 33; 34; 35]
      in
      let sectors_of_root_dir =
        [68; 69; 70; 71; 72; 73; 74; 75; 76; 77; 78; 79; 80; 81; 82; 83; 84;
         85; 86; 87; 88; 89; 90; 91; 92; 93; 94; 95; 96; 97; 98; 99]
      in
      Alcotest.(check (list int)) __LOC__ sectors_of_fat
        (Fat_boot_sector.sectors_of_fat x);
      Alcotest.(check (list int)) __LOC__ sectors_of_root_dir
        (Fat_boot_sector.sectors_of_root_dir x) in
    check x;
    let buf = alloc sizeof in
    marshal buf x;
    let x = match unmarshal buf with
      | Error x -> failwith x
      | Ok x -> x in
    check x;
    Lwt.return ()
  in
  Lwt_main.run t

let kib = 1024L
let mib = Int64.mul kib 1024L

module FsError = struct
  let (>>=) x f = x >>= function
    | `Ok x -> f x
    | `Error error ->
      let b = Buffer.create 20 in
      let ppf = Format.formatter_of_buffer b in
      let k ppf = Format.pp_print_flush ppf (); fail "%s" (Buffer.contents b) in
      Fmt.kpf k ppf "%a" Mirage_fs.pp_write_error error
end

let format () =
  Mirage_block_lwt.Mem.connect "" >>= fun t ->
  MemFS.format t (Int64.mul 16L mib)

let test_create () =
  let t =
    format () >>*= fun fs ->
    let filename = "HELLO.TXT" in
    MemFS.create fs filename >>*= fun () ->
    MemFS.stat fs "/" >>*= function
    | { directory = true; _ } ->
      let file = "/" in
      MemFS.listdir fs file >>*= fun names ->
      Alcotest.(check (list string)) __LOC__ [ filename ] names;
      Lwt.return ()
    | { directory = false; _ } ->
      fail "Not a directory"
  in
  Lwt_main.run t

let make_pattern tag length =
  (* if the tag is smaller than length then truncate it *)
  let tag = if String.length tag > length then String.sub tag 0 length else tag in
  assert (String.length tag <= length);
  let buffer = alloc length in
  Cstruct.blit_from_string tag 0 buffer 0 (String.length tag);
  for i = String.length tag to Cstruct.len buffer - 1 do
    Cstruct.set_char buffer i tag.[i mod (String.length tag)]
  done;
  buffer

let sector_aligned_writes = [
  0, 512;
  512, 512;
  4096, 512;
]

let sector_misaligned_writes = [
  1, 1;
  512, 1;
  256, 512;
]

let interesting_writes = sector_aligned_writes @ sector_misaligned_writes

let interesting_filenames = [
  "HELLO.TXT";
  "/FOO/BAR.TXT";
]

let test_listdir () =
  let t =
    format () >>*= fun fs ->
    let filename = "hello" in
    MemFS.create fs filename >>*= fun () ->
    MemFS.listdir fs "/" >>*= fun all ->
    if List.mem filename all
    then Lwt.return ()
    else fail "Looking for '%s' in directory, contents [ %s ]" filename
        (String.concat ", " (List.map (fun x ->
             Printf.sprintf "'%s'(%d)" x (String.length x)) all))
  in
  Lwt_main.run t

let test_listdir_subdir () =
  let t =
    format () >>*= fun fs ->
    let dirname = "hello" in
    MemFS.mkdir fs dirname >>*= fun () ->
    MemFS.listdir fs "/" >>*= fun all ->
    ( if List.mem dirname all
      then Lwt.return ()
      else fail "Looking for '%s' in / directory, contents [ %s ]" dirname
          (String.concat ", " (List.map (fun x ->
               Printf.sprintf "'%s'(%d)" x (String.length x)) all)))
    >>= fun () ->
    let filename = "there" in
    let path = Filename.concat dirname filename in
    MemFS.create fs path >>*= fun () ->
    MemFS.listdir fs dirname >>*= fun all ->
    ( if List.mem filename all
      then Lwt.return ()
      else fail "Looking for '%s' in %s directory, contents [ %s ]"
          filename dirname
          (String.concat ", " (List.map (fun x ->
               Printf.sprintf "'%s'(%d)" x (String.length x)) all)))
  in
  Lwt_main.run t

let test_read () =
  let t =
    format () >>*= fun fs ->
    let filename = "hello" in
    let length = 512 in
    MemFS.create fs filename >>*= fun () ->
    let buffer = make_pattern "basic writing test " length in
    MemFS.write fs filename 0 buffer >>*= fun () ->
    MemFS.read fs filename 0 length >>*= fun buffers ->
    let count buffers = List.fold_left (+) 0 (List.map Cstruct.len buffers) in
    Alcotest.(check int) __LOC__ length (count buffers);
    MemFS.read fs filename 0 (length * 2) >>*= fun buffers ->
    Alcotest.(check int) __LOC__ length (count buffers);
    MemFS.read fs filename 1 (length * 2) >>*= fun buffers ->
    Alcotest.(check int) __LOC__ (length - 1) (count buffers);
    MemFS.read fs filename 1 (length - 2) >>*= fun buffers ->
    Alcotest.(check int) __LOC__ (length - 2) (count buffers);
    MemFS.read fs filename length length >>*= fun buffers ->
    Alcotest.(check int) __LOC__ 0 (count buffers);
    Lwt.return ()
  in
  Lwt_main.run t

(* Very simple, easy sector-aligned writes. Tests that
   read(write(data)) = data; and that files are extended properly *)
let test_write ((filename: string), (_offset, length)) () =
  let t =
    format () >>*= fun fs ->
    let open Lwt in
    ( match List.rev (Fat_path.to_string_list (Fat_path.of_string filename)) with
      | [] -> assert false
      | [ _ ] -> return ()
      | _ :: dir ->
        List.fold_left (fun current dir ->
            current >>= fun current ->
            MemFS.mkdir fs (Fat_path.(to_string (of_string_list (current @ [dir]))))
            >>*= fun () ->
            return (current @ [dir])
          ) (return []) (List.rev dir) >>= fun _ ->
        return () )
    >>= fun () ->
    MemFS.create fs filename >>*= fun () ->
    let buffer = make_pattern "basic writing test " length in
    MemFS.write fs filename 0 buffer >>*= fun () ->
    MemFS.read fs filename 0 512 >>*= fun buffers ->
    let cstruct =
      Alcotest.testable
        (fun ppf x ->
           Fmt.pf ppf "\"%s\"(%d)" (Cstruct.to_string x) (Cstruct.len x)
        ) Cstruct.equal
    in
    Alcotest.(check cstruct) __LOC__ buffer (List.hd buffers);
    return ()
  in
  Lwt_main.run t

let test_destroy () =
  let t =
    Mirage_block_lwt.Mem.connect "" >>= fun t ->
    MemFS.format t 0x100000L >>*= fun fs ->
    MemFS.create fs "/data" >>*= fun () ->
    MemFS.destroy fs "/data" >>*= fun () ->
    MemFS.listdir fs "/" >>*= function
    | []    -> Lwt.return ()
    | items ->
      List.iter (Printf.printf "Item: %s\n") items;
      Alcotest.fail "Items present after destroy!"
  in
  Lwt_main.run t

let rec allpairs xs ys = match xs with
  | []      -> []
  | x :: xs -> List.map (fun y -> x, y) ys @ (allpairs xs ys)

let write_tests =
  List.map (fun ((filename, (off, len)) as x) ->
      Printf.sprintf "write to %s at %d length %d" filename off len,
      `Quick,
      (test_write x)
    ) (allpairs interesting_filenames interesting_writes)

let suite = [
  "parse_boot_sector", `Quick, test_parse_boot_sector;
  "checksum"         , `Quick, checksum_test;
  "root_list"        , `Quick, test_root_list;
  "chains"           , `Quick, test_chains;
  "create"           , `Quick, test_create;
  "listdir"          , `Quick, test_listdir;
  "listdir_subdir"   , `Quick, test_listdir_subdir;
  "read"             , `Quick, test_read;
  "destroy"          , `Quick, test_destroy;
  ] @ write_tests

let () =
  Alcotest.run "fat" ["tests", suite]
