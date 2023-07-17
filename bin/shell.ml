(* This is a toplevel-like test program *)
open Lwt.Infix

module Test = Fat.Make(Block)

let with_file flags filename f =
  Lwt_unix.openfile filename flags 0o0 >>= fun file ->
  Lwt.catch (fun () ->
      f file >>= fun x ->
      Lwt_unix.close file >|= fun () ->
      x
    ) (fun x ->
      Lwt_unix.close file >>= fun () ->
      Lwt.fail x)

let fail fmt = Fmt.kstr Lwt.fail_with fmt

let (>>|=) m f = m >>= function
  | Error e -> fail "%a" Test.pp_error e
  | Ok x    -> f x

let (>>*=) m f = m >>= function
  | Error e -> fail "%a" Test.pp_write_error (e :> Test.write_error)
  | Ok x    -> f x

let main filename _create_size =
  Block.connect filename >>= fun device ->
  Test.connect device >>= fun fs ->
  let open Test in
  let module Path = Fat_path in
  let cwd = ref (Path.of_string "/") in

  let do_dir dir =
    let path = Path.cd !cwd dir in
    stat fs (Path.to_string path) >>*= function
    | { directory = true; _ } ->
      let file = Path.to_string path in
      listdir fs file >>*= fun xs ->
      Printf.printf "Directory for A:%s\n\n" (Path.to_string path);
      List.iter (fun x -> Printf.printf "%s\n" x) xs;
      Printf.printf "%9d files\n%!" (List.length xs);
      Lwt.return ()
    | { directory = false; _ } ->
      Printf.printf "Not a directory.\n%!";
      Lwt.return ()
  in

  let do_type file =
    let path = Path.cd !cwd file in
    stat fs (Path.to_string path) >>*= function
    | { directory = true; _ } ->
      Printf.printf "Is a directory.\n%!";
      Lwt.return ()
    | { directory = false; size; _ } ->
      let file_size = Int64.to_int size in
      read fs (Path.to_string path) 0 file_size >>*= fun datas ->
      let n = ref 0 in
      List.iter (fun buf ->
          Printf.printf "%s" (Cstruct.to_string buf);
          n := !n + (Cstruct.length buf)
        ) datas;
      Printf.printf "\n%!";
      if !n <> file_size
      then Printf.printf "Short read; expected %d got %d\n%!" file_size !n;
      Lwt.return ()
  in

  let do_del file =
    let path = Path.cd !cwd file in
    destroy fs (Path.to_string path) >>*= fun () -> Lwt.return ()
  in

  let do_cd dir =
    let path = Path.cd !cwd dir in
    stat fs (Path.to_string path) >>*= function
    | { directory = true; _ } ->
      cwd := path;
      Lwt.return ()
    | { directory = false; _ } ->
      Printf.printf "Not a directory.\n%!";
      Lwt.return ()
  in

  let do_touch x =
    let path = Path.cd !cwd x in
    create fs (Path.to_string path) >>*= fun () -> Lwt.return ()
  in

  let do_mkdir x =
    let path = Path.cd !cwd x in
    mkdir fs (Path.to_string path) >>*= fun () -> Lwt.return ()
  in

  let do_rmdir x =
    let path = Path.cd !cwd x in
    destroy fs (Path.to_string path) >>*= fun () -> Lwt.return ()
  in

  let copy_file_in outside inside =
    Lwt_unix.lstat (Path.to_string outside) >>= fun stats ->
    let block_size = 1024 * 1024 in
    let block = Cstruct.create block_size in
    with_file [ Unix.O_RDONLY ] (Path.to_string outside)
      (fun ifd ->
         let rec loop offset remaining =
           let this = min remaining block_size in
           let frag = Cstruct.sub block 0 this in
           Block.really_read ifd frag >>= fun () ->
           write fs (Path.to_string inside) offset frag >>*= fun () ->
           loop (offset + this) (remaining - this) in
         loop 0 stats.Lwt_unix.st_size
      )
  in

  (* True if string 'x' starts with prefix 'prefix' *)
  let startswith prefix x =
    let x_l = String.length x and prefix_l = String.length prefix in
    prefix_l <= x_l && String.sub x 0 prefix_l  = prefix
  in

  let parse_path x =
    (* return a pair of (outside filesystem bool, absolute path) *)
    let is_outside = startswith "u:" x in
    (* strip off the drive prefix *)
    let x' = if is_outside then String.sub x 2 (String.length x - 2) else x in
    let is_absolute = x' <> "" && x'.[0] = '/' in
    let abspath =
      if is_absolute
      then Path.of_string x'
      else
        let wd = if is_outside then Path.of_string (Unix.getcwd ()) else !cwd in
        Path.cd wd x' in
    is_outside, abspath
  in

  let do_copy x y =
    let x_outside, x_path = parse_path x in
    let y_outside, y_path = parse_path y in
    match x_outside, y_outside with
    | true, false ->
      copy_file_in x_path y_path
    | _, _ -> failwith "Unimplemented"
  in

  let deltree x =
    let rec inner path =
      stat fs (Path.to_string path) >>*= function
      | { directory = true; _ } ->
        let file = Path.to_string path in
        listdir fs file >>*= fun xs ->
        Lwt_list.iter_s
          (fun dir ->
             inner (Path.cd path dir)
          ) xs >>= fun () ->
        destroy fs (Path.to_string path) >>*= fun () -> Lwt.return ()
      | { directory = false; _ } ->
        destroy fs (Path.to_string path) >>*= fun () -> Lwt.return ()
    in
    inner (snd(parse_path x))
  in

  let split_ws s =
    let non_empty_hd = function "" :: tl -> tl | tl -> tl in
    let a = non_empty_hd (String.split_on_char ' ' s) in
    List.rev (non_empty_hd (List.rev a))
  in

  let rec loop () =
    Printf.printf "A:%s> %!" (Path.to_string !cwd);
    match split_ws (input_line stdin) with
    | [ "dir" ] -> do_dir "" >>= loop
    | [ "dir"; path ] -> do_dir path >>= loop
    | [ "cd"; path ] -> do_cd path >>= loop
    | [ "type"; path ] -> do_type path >>= loop
    | [ "touch"; path ] -> do_touch path >>= loop
    | [ "mkdir"; path ] -> do_mkdir path >>= loop
    | [ "rmdir"; path ] -> do_rmdir path >>= loop
    | [ "copy"; a; b ] -> do_copy a b >>= loop
    | [ "deltree"; a ] -> deltree a >>= loop
    | [ "del"; a ] -> do_del a >>= loop
    | [ "exit" ] -> Lwt.return () (* terminate loop *)
    | [] -> loop ()
    | cmd :: _ -> Printf.printf "Unknown command: %s\n%!" cmd; loop ()
  in
  loop ()

let () =
  let usage () =
    Printf.fprintf stderr "Usage:\n";
    Printf.fprintf stderr "  %s -fs <filesystem>\n" Sys.argv.(0);
    exit 1 in
  let filename = ref "" in
  let create_size = ref None in
  Arg.parse
    [ ("-fs"    , Arg.Set_string filename, "Filesystem to open");
      ("-create", Arg.Int (fun x -> create_size := Some x),
       "Create using the given number of MiB")]
    (fun x -> Printf.fprintf stderr "Skipping unknown argument: %s\n" x)
    "Examine the contents of a fat filesystem";
  if !filename = "" then usage ();
  Lwt_main.run (main !filename !create_size)
