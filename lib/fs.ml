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
open S
open Mirage_block
open Result

type fs = {
  boot: Boot_sector.t;
  format: Fat_format.t;
  fat: Entry.fat;
  root: Cstruct.t;
}

(*
type block_error = [ `Unknown of string | `Unimplemented | `Is_read_only | `Disconnected ]

let string_of_block_error = function
  | `Unknown x -> Printf.sprintf "Unknown block error: %s" x
  | `Unimplemented -> "Block device function is not implemented"
  | `Is_read_only -> "Block device is read only"
  | `Disconnected -> "Block device has been disconnected"

type filesystem_error = [
  | `Not_a_directory of string
  | `Is_a_directory of string
  | `Directory_not_empty of string
  | `No_directory_entry of string * string
  | `File_already_exists of string
  | `No_space
  | `Format_not_recognised of string
  | `Unknown_error of string
  | `Block_device of block_error
]

let string_of_filesystem_error = function
| `Not_a_directory x -> Printf.sprintf "%s is not a directory" x
| `Is_a_directory x -> Printf.sprintf "%s is a directory" x
| `Directory_not_empty x -> Printf.sprintf "The directory %s is not empty" x
| `No_directory_entry (x, y) -> Printf.sprintf "The directory %s contains no entry called %s" x y
| `File_already_exists x -> Printf.sprintf "The filename %s already exists" x
| `No_space -> "There is insufficient free space to complete this operation"
| `Format_not_recognised x -> Printf.sprintf "This disk is not formatted with %s" x
| `Unknown_error x -> Printf.sprintf "Unknown error: %s" x
| `Block_device x -> string_of_block_error x
*)

module Make (B: BLOCK_DEVICE
  with type 'a io = 'a Lwt.t
  and type page_aligned_buffer = Cstruct.t)(M: IO_PAGE) = struct
  type t = {
    device: B.t;
    fs: fs;
  }

  type 'a io = 'a Lwt.t

  type id = B.t

  let id t = t.device

  type page_aligned_buffer = Cstruct.t

  type stat = {
    filename: string;
    read_only: bool;
    directory: bool;
    size: int64;
  }

  module BlockError = struct
    let (>>=) m f = m >>= function
    | Error (`Msg m) -> Lwt.return (Error (`Msg m))
    | Error (#Mirage_block.write_error as e) ->
      Lwt.return (Error (`Msg (Fmt.strf "%a" B.pp_write_error e)))
    | Ok x -> f x
  end

  let (>>|=) m f = m >>= function
    | Error (`Msg m) -> Lwt.return (Error (`Msg m))
    | Error (#Mirage_block.write_error as e) ->
      Lwt.return (Error (`Msg (Fmt.strf "%a" B.pp_write_error e)))
    | Ok x -> f x

  let rec iter_s f = function
    | [] -> Lwt.return (Ok ())
    | x :: xs ->
      f x >>= function
      | Error e -> Lwt.return (Error e)
      | Ok () -> iter_s f xs

  let alloc bytes =
    let pages = M.get_buf ~n:((bytes + 4095) / 4096) () in
    Cstruct.sub pages 0 bytes

  (* TODO: this function performs extra data copies *)
  let read_sectors bps device xs: (Cstruct.t, [> `Msg of string ]) Result.result Lwt.t=
    let buf = alloc (List.length xs * 512) in
    let rec split buf =
      if Cstruct.len buf = 0 then []
      else if Cstruct.len buf <= 512 then [ buf ]
      else Cstruct.sub buf 0 512 :: (split (Cstruct.shift buf 512)) in

    let page = alloc 4096 in
    B.get_info device >>= fun info ->
    let rec loop = function
      | [] -> return (Ok ())
      | (sector, buffer) :: xs ->
        let offset = sector * bps in
        let sector' = offset / info.sector_size in
        B.read device (Int64.of_int sector') [ page ] >>|= fun () ->
        Cstruct.blit page (offset mod info.B.sector_size) buffer 0 512;
        loop xs in
    loop (List.combine xs (split buf)) >>|= fun () ->
    return (Ok buf)

  let write_update device fs ({ Update.offset = offset; data = data } as update) =
    B.get_info device >>= fun info ->
    let bps = fs.boot.Boot_sector.bytes_per_sector in
    let sector_number = Int64.(div offset (of_int bps)) in
    let sector_offset = Int64.(sub offset (mul sector_number (of_int bps))) in
    (* number of 512-byte FAT sectors per physical disk sectors *)
    let sectors_per_block = info.B.sector_size / bps in
    let page = alloc 4096 in
    let sector_number' = Int64.(div sector_number (of_int sectors_per_block)) in
    B.read device sector_number' [ page ] >>|= fun () ->
    let sector = Cstruct.sub page (Int64.(to_int (rem sector_number (of_int sectors_per_block))) * bps) bps in
    Update.apply sector { update with Update.offset = sector_offset };
    B.write device sector_number' [ page ] >>|= fun () ->
    return (Ok ())

  let make size =
    let open Rresult in
    let boot = Boot_sector.make size in
    Boot_sector.detect_format boot >>= fun format ->

    let fat = Entry.make boot format in

    let root_sectors = Boot_sector.sectors_of_root_dir boot in
    let root = alloc (List.length root_sectors * 512) in
    for i = 0 to Cstruct.len root - 1 do
      Cstruct.set_uint8 root i 0
    done;
    let fs = { boot = boot; format = format; fat = fat; root = root } in
    Ok fs

  let format device size =

    (match make size with
     | Ok x -> return x
     | Error x -> fail (Failure x)
    )
    >>= fun fs ->
    let sector = alloc 512 in
    Boot_sector.marshal sector fs.boot;

    let to_string list =
      String.concat ", " (List.map (fun e -> Printf.sprintf "%d" e) list)
    in

    let fat_sectors = Boot_sector.sectors_of_fat fs.boot in
    let fat_writes = Update.(
        let updates = split (from_cstruct 0L fs.fat) 512 in
        map updates fat_sectors 512
      )
    in

    let root_sectors = Boot_sector.sectors_of_root_dir fs.boot in
    let root_writes = Update.(map (split (from_cstruct 0L fs.root) 512) root_sectors 512) in

    let t = { device; fs } in
    write_update device fs (Update.from_cstruct 0L sector) >>|= fun () ->
    iter_s (write_update device fs) fat_writes >>|= fun () ->
    iter_s (write_update device fs) root_writes >>|= fun () ->
    return (Ok t)

  let connect device =
    let (>>!=) x f =
      x >>= function | Ok y -> f y | Error e -> fail @@ (Failure (Format.asprintf "Block error: %a" Mirage_pp.pp_block_error e))
    in
    let page = alloc 4096 in
    B.get_info device >>= fun info ->
    let sector = Cstruct.sub page 0 info.B.sector_size in
    B.read device 0L [ sector ] >>!= fun () ->
    ( match Boot_sector.unmarshal sector with
      | Error reason -> fail @@ Failure ("error reading first sector of block device: " ^ reason)
      | Ok boot ->
        match Boot_sector.detect_format boot with
        | Error reason -> fail @@ Failure ("error detecting the format of block device: " ^ reason)
        | Ok format ->
          read_sectors boot.Boot_sector.bytes_per_sector device (Boot_sector.sectors_of_fat boot) >>!= fun fat ->
          read_sectors boot.Boot_sector.bytes_per_sector device (Boot_sector.sectors_of_root_dir boot) >>!= fun root ->
          return { boot; format; fat; root } )
    >>= fun fs -> return { device; fs }

  let disconnect _ = return ()

  type file = string

  type find =
    | Dir of Name.r list
    | File of Name.r

  let sectors_of_file fs { Name.start_cluster = cluster; file_size = file_size; subdir = subdir } =
    Entry.Chain.(to_sectors fs.boot (follow fs.format fs.fat cluster))

  let read_whole_file device fs { Name.dos = _, ({ Name.file_size = file_size; subdir = subdir } as f) } =
    read_sectors fs.boot.Boot_sector.bytes_per_sector device (sectors_of_file fs f)

  (** [find device fs path] returns a [find_result] corresponding to the object
      stored at [path] *)
  let find device fs path =
    let readdir = function
      | Dir ds -> return (Ok ds)
      | File d ->
        read_whole_file device fs d >>|= fun buf ->
        return (Ok (Name.list buf)) in
    let rec inner sofar current = function
      | [] ->
        begin match current with
          | Dir ds -> return (Ok (Dir ds))
          | File { Name.dos = _, { Name.subdir = true } } ->
            readdir current >>|= fun names ->
            return (Ok (Dir names))
          | File ( { Name.dos = _, { Name.subdir = false } } as d ) ->
            return (Ok (File d))
        end
      | p :: ps ->
        readdir current >>|= fun entries ->
        begin match Name.find p entries, ps with
          | Some { Name.dos = _, { Name.subdir = false } }, _ :: _ ->
            return (Error `Not_a_directory)
          | Some d, _ ->
            inner (p::sofar) (File d) ps
          | None, _ ->
            return (Error `No_directory_entry)
        end in
    inner [] (Dir (Name.list fs.root)) (Path.to_string_list path)

  module Location = struct
    (* Files and directories are stored in a location *)
    type t =
      | Chain of int list (* a chain of clusters *)
      | Rootdir           (* the root directory area *)

    let to_string = function
      | Chain xs -> Printf.sprintf "Chain [ %s ]" (String.concat "; " (List.map string_of_int xs))
      | Rootdir -> "Rootdir"

    (** [chain_of_file device fs path] returns [Some chain] where [chain] is the chain
        corresponding to [path] or [None] if [path] cannot be found or if it
        is / and hasn't got a chain. *)
    let chain_of_file device fs path =
      if Path.is_root path then return (Ok None)
      else
        let parent_path = Path.directory path in
        find device fs parent_path >>= fun entry ->
        match entry with
        | Ok (Dir ds) ->
          begin match Name.find (Path.filename path) ds with
            | None -> assert false
            | Some f ->
              let start_cluster = (snd f.Name.dos).Name.start_cluster in
              return (Ok (Some(Entry.Chain.follow fs.format fs.fat start_cluster)))
          end
        | _ -> return (Ok None)

    (* return the storage location of the object identified by [path] *)
    let of_file device fs path =
      chain_of_file device fs path >>|= function
      | None -> return (Ok Rootdir)
      | Some c -> return (Ok (Chain c))

    let to_sectors fs = function
      | Chain clusters -> Entry.Chain.to_sectors fs.boot clusters
      | Rootdir -> Boot_sector.sectors_of_root_dir fs.boot

  end

  let (>>||=) m f = m >>= function
    | Error (`Msg m) -> Lwt.return (Error (`Msg m))
    | Error `Is_a_directory -> Lwt.return (Error `Is_a_directory)
    | Error `Not_a_directory -> Lwt.return (Error `Not_a_directory)
    | Error `Directory_not_empty -> Lwt.return (Error `Directory_not_empty)
    | Error `File_already_exists -> Lwt.return (Error `File_already_exists)
    | Error `No_directory_entry -> Lwt.return (Error `No_directory_entry)
    | Error `No_space -> Lwt.return (Error `No_space)
    | Ok x -> f x

  (** [write_to_location device fs path location update] applies [update]
      to the data stored in the object at [path] which is currently
      stored at [location]. If [location] is a chain of clusters then
      it will be extended. *)
  let rec write_to_location device fs path location update =
    let bps = fs.boot.Boot_sector.bytes_per_sector in
    let spc = fs.boot.Boot_sector.sectors_per_cluster in
    let updates = Update.split update bps in

    let sectors = Location.to_sectors fs location in
    (* This would be a good point to see whether we need to allocate
       new sectors and do that too. *)
    let current_bytes = bps * (List.length sectors) in
    let bytes_needed = max 0L (Int64.(sub (Update.total_length update) (of_int current_bytes))) in
    let clusters_needed =
      let bpc = Int64.of_int(spc * bps) in
      Int64.(to_int(div (add bytes_needed (sub bpc 1L)) bpc)) in
    ( match location, bytes_needed > 0L with
      | Location.Rootdir, true ->
        return (Error `No_space)
      | (Location.Rootdir | Location.Chain _), false ->
        let writes = Update.map updates sectors bps in
        iter_s (write_update device fs) writes >>|= fun () ->
        if location = Location.Rootdir then Update.apply fs.root update;
        return (Ok location)
      | Location.Chain cs, true ->
        let last = if cs = [] then None else Some (List.hd (List.rev cs)) in
        let new_clusters = Entry.Chain.extend fs.boot fs.format fs.fat last clusters_needed in
        let fat_sectors = Boot_sector.sectors_of_fat fs.boot in
        let new_sectors = Entry.Chain.to_sectors fs.boot new_clusters in
        let data_writes = Update.map updates (sectors @ new_sectors) bps in
        iter_s (write_update device fs) data_writes >>|= fun () ->
        let fat_writes = Update.(map (split (from_cstruct 0L fs.fat) bps) fat_sectors bps) in
        iter_s (write_update device fs) fat_writes >>|= fun () ->
        return (Ok (Location.Chain (cs @ new_clusters))) )  >>||= fun location ->
    match location with
    | Location.Chain [] ->
      (* In the case of a previously empty file (location = Chain []), we
         have extended the chain (location = Chain (_ :: _)) so it's safe to
         call List.hd *)
      assert false
    | Location.Chain (start_cluster :: _) ->
      update_directory_containing device fs path
        (fun bits ds ->
           let filename = Path.filename path in
           match Name.find filename ds with
           | None ->
             return (Error `No_directory_entry)
           | Some d ->
             let file_size = Name.file_size_of d in
             let new_file_size = max file_size (Int32.of_int (Int64.to_int (Update.total_length update))) in
             let updates = Name.modify bits filename new_file_size start_cluster in
             return (Ok updates)
        )
    | Location.Rootdir -> return (Ok ()) (* the root directory itself has no attributes *)

  and update_directory_containing device fs path f =
    let parent_path = Path.directory path in
    find device fs parent_path >>||= function
      | File _ -> Lwt.return (Error `Not_a_directory)
      | Dir ds ->
        Location.of_file device fs parent_path >>|= fun location ->
        let sectors = Location.to_sectors fs location in
        read_sectors fs.boot.Boot_sector.bytes_per_sector device sectors >>= function
        | Error (`Msg m) -> return (Error (`Msg m))
        | Ok contents ->
        f contents ds >>||= fun updates ->
        iter_s (write_to_location device fs parent_path location) updates >>||= fun () ->
        return (Ok ())

  let create_common x path dir_entry =
    let path = Path.of_string path in
    let filename = Path.filename path in
    update_directory_containing x.device x.fs path
      (fun contents ds ->
         if Name.find filename ds <> None
         then return (Error `File_already_exists)
         else return (Ok (Name.add contents dir_entry))
      )

  let wrap f : ('a, [> `Msg of string]) Result.result Lwt.t =
    catch f
      (fun e -> return (Error (`Msg (Printexc.to_string e))))

  (** [write x f offset buf] writes [buf] at [offset] in file [f] on
      filesystem [x] *)
  let write x f offset buf =
    let f = Path.of_string f in
    wrap (fun () ->
        (* u is the update, in file co-ordinates *)
        let u = Update.from_cstruct (Int64.of_int offset) buf in
        (* all data is either in the root directory or in a chain of clusters.
           Note even subdirectories are stored in chains of clusters. *)
        Location.of_file x.device x.fs f >>||= fun location ->
        write_to_location x.device x.fs f location u)

  (** [create x path] create a zero-length file at [path] *)
  let create x path =
    wrap (fun () -> create_common x path (Name.make (Filename.basename path)))

  (** [mkdir x path] create an empty directory at [path] *)
  let mkdir x path =
    wrap (fun () -> create_common x path (Name.make ~subdir:true (Filename.basename path)))

  (** [destroy x path] deletes the entry at [path] *)
  let destroy x path  =
    let path = Path.of_string path in
    let filename = Path.filename path in
    let do_destroy () =
      update_directory_containing x.device x.fs path
        (fun contents ds ->
           (* XXX check for nonempty *)
           (* XXX delete chain *)
           if Name.find filename ds = None
           then Lwt.return (Error `No_directory_entry)
           else Lwt.return (Ok (Name.remove contents filename))
        ) in
    find x.device x.fs path >>= function
    | Error x -> return (Error x)
    | Ok (File _) -> do_destroy ()
    | Ok (Dir []) -> do_destroy ()
    | Ok (Dir (_::_)) -> return (Error `Directory_not_empty)

  let stat x path =
    let path = Path.of_string path in
    let entry_of_file f = f in
    find x.device x.fs path >>= function
    | Error x -> return (Error x)
    | Ok (File f) ->
      let r = entry_of_file f in
      return (Ok {
          filename = r.Name.utf_filename;
          read_only = (snd r.Name.dos).Name.read_only;
          directory = false;
          size = Int64.of_int32 ((snd r.Name.dos).Name.file_size);
        })
    | Ok (Dir ds) ->
      if Path.is_root path
      then return (Ok {
          filename = "/";
          read_only = false;
          directory = true;
          size = 0L;
        })
      else
        let filename = Path.filename path in
        let parent_path = Path.directory path in
        find x.device x.fs parent_path >>= function
        | Error x -> return (Error x)
        | Ok (File _) -> assert false (* impossible by initial match *)
        | Ok (Dir ds) ->
          begin match Name.find filename ds with
            | None -> assert false (* impossible by initial match *)
            | Some f ->
              let r = entry_of_file f in
              return (Ok {
                  filename = r.Name.utf_filename;
                  read_only = (snd r.Name.dos).Name.read_only;
                  directory = true;
                  size = Int64.of_int32 ((snd r.Name.dos).Name.file_size);
                })
          end

  let size x path =
    stat x path >>= function
    | Ok s -> return (Ok s.size)
    | Error _ as e -> return e

  let listdir x path =
    let path = Path.of_string path in
    find x.device x.fs path >>= function
    | Ok (File _) -> return (Error `Not_a_directory)
    | Ok (Dir ds) ->
      return (Ok (List.map Name.to_string ds))
    | Error x -> return (Error x)

  let read_file device fs { Name.dos = _, ({ Name.file_size = file_size } as f) } the_start length =
    let bps = fs.boot.Boot_sector.bytes_per_sector in
    let the_file = SectorMap.make (sectors_of_file fs f) in
    (* If the file is small, truncate length *)
    let length = max 0 (min length (Int32.to_int file_size - the_start)) in
    if length = 0
    then return (Ok [])
    else
      let preceeding, requested, succeeding = SectorMap.byte_range bps the_start length in
      let to_read = SectorMap.compose requested the_file in
      read_sectors fs.boot.Boot_sector.bytes_per_sector device (SectorMap.to_list to_read) >>= function
      | Error e -> Lwt.return (Error e)
      | Ok buffer ->
        let buffer = Cstruct.sub buffer preceeding (Cstruct.len buffer - preceeding - succeeding) in
        return (Ok [ buffer ])

  let read x path the_start length =
    let path = Path.of_string path in
    find x.device x.fs path >>= function
    | Ok (Dir _) -> return (Error `Is_a_directory)
    | Ok (File f) ->
      read_file x.device x.fs f the_start length
    | Error x -> return (Error x)
end
