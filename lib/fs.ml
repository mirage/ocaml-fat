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

type fs = {
  boot: Boot_sector.t;
  format: Fat_format.t;
  fat: Entry.fat;
  root: Cstruct.t;
}

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

module Make (B: BLOCK_DEVICE
  with type 'a io = 'a Lwt.t
  and type page_aligned_buffer = Cstruct.t)(M: IO_PAGE
  with type buf = Cstruct.t): (FS
  with type id = B.t
  and type 'a io = 'a Lwt.t
  and type block_device_error = B.error
  and type page_aligned_buffer = Cstruct.t
) = struct
  type t = {
    device: B.t;
    mutable fs: fs option; (* a filesystem is either formatted or not *)
  }

  type 'a io = 'a Lwt.t

  type id = B.t

  let id t = t.device

  type block_device_error = B.error

  type error = filesystem_error

  type page_aligned_buffer = Cstruct.t

  type stat = {
    filename: string;
    read_only: bool;
    directory: bool;
    size: int64;
  }

  exception Block_device_error of B.error
  let (>>|=) m f = m >>= function
    | `Error e -> fail (Block_device_error e)
    | `Ok x -> f x

  let alloc bytes =
    let pages = M.(to_cstruct (get ((bytes + 4095) / 4096))) in
    Cstruct.sub pages 0 bytes

  (* TODO: this function performs extra data copies *)
  let read_sectors bps device xs =
    let buf = alloc (List.length xs * 512) in
    let rec split buf =
      if Cstruct.len buf = 0 then []
      else if Cstruct.len buf <= 512 then [ buf ]
      else Cstruct.sub buf 0 512 :: (split (Cstruct.shift buf 512)) in

    let page = alloc 4096 in
    B.get_info device >>= fun info ->
    let rec loop = function
      | [] -> return ()
      | (sector, buffer) :: xs ->
        let offset = sector * bps in
        let sector' = offset / info.B.sector_size in
        B.read device (Int64.of_int sector') [ page ] >>|= fun () ->
        Cstruct.blit page (offset mod info.B.sector_size) buffer 0 512;
        loop xs in
    loop (List.combine xs (split buf)) >>= fun () ->
    return buf

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
    return ()

  let make size =
    let open Result in
    let boot = Boot_sector.make size in
    Boot_sector.detect_format boot >>= fun format ->

    let fat = Entry.make boot format in

    let root_sectors = Boot_sector.sectors_of_root_dir boot in
    let root = alloc (List.length root_sectors * 512) in
    for i = 0 to Cstruct.len root - 1 do
      Cstruct.set_uint8 root i 0
    done;
    let fs = { boot = boot; format = format; fat = fat; root = root } in
    `Ok fs

  let format t size =
    let device = t.device in

    ( match make size with
      | `Ok x -> return x
      | `Error x -> fail (Failure x) ) >>= fun fs ->

    let sector = alloc 512 in
    Boot_sector.marshal sector fs.boot;

    let fat_sectors = Boot_sector.sectors_of_fat fs.boot in
    let fat_writes = Update.(map (split (from_cstruct 0L fs.fat) 512) fat_sectors 512) in

    let root_sectors = Boot_sector.sectors_of_root_dir fs.boot in
    let root_writes = Update.(map (split (from_cstruct 0L fs.root) 512) root_sectors 512) in

    t.fs <- Some fs;
    write_update device fs (Update.from_cstruct 0L sector) >>= fun () ->
    Lwt_list.iter_s (write_update device fs) fat_writes >>= fun () ->
    Lwt_list.iter_s (write_update device fs) root_writes >>= fun () ->
    return (`Ok ())

  let connect device =
    let page = alloc 4096 in
    B.get_info device >>= fun info ->
    let sector = Cstruct.sub page 0 info.B.sector_size in
    B.read device 0L [ sector ] >>|= fun () ->
    ( match Boot_sector.unmarshal sector with
      | `Error _ -> return None
      | `Ok boot ->
        match Boot_sector.detect_format boot with
        | `Error reason -> return None
        | `Ok format ->
          read_sectors boot.Boot_sector.bytes_per_sector device (Boot_sector.sectors_of_fat boot) >>= fun fat ->
          read_sectors boot.Boot_sector.bytes_per_sector device (Boot_sector.sectors_of_root_dir boot) >>= fun root ->
          return (Some { boot; format; fat; root }) ) >>= fun fs ->
    return (`Ok { device; fs })

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
  let find device fs path : [ `Ok of find | `Error of error ] io =
    let readdir = function
      | Dir ds -> return ds
      | File d ->
        read_whole_file device fs d >>= fun buf ->
        return (Name.list buf) in
    let rec inner sofar current = function
      | [] ->
        begin match current with
          | Dir ds -> return (`Ok (Dir ds))
          | File { Name.dos = _, { Name.subdir = true } } ->
            readdir current >>= fun names ->
            return (`Ok (Dir names))
          | File ( { Name.dos = _, { Name.subdir = false } } as d ) ->
            return (`Ok (File d))
        end
      | p :: ps ->
        readdir current >>= fun entries ->
        begin match Name.find p entries, ps with
          | Some { Name.dos = _, { Name.subdir = false } }, _ :: _ ->
            return (`Error (`Not_a_directory (Path.(to_string (of_string_list (List.rev (p :: sofar)))))))
          | Some d, _ ->
            inner (p::sofar) (File d) ps
          | None, _ ->
            return (`Error(`No_directory_entry (Path.(to_string (of_string_list (List.rev sofar))), p)))
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
      if Path.is_root path then return None
      else
        let parent_path = Path.directory path in
        find device fs parent_path >>= fun entry ->
        match entry with
        | `Ok (Dir ds) ->
          begin match Name.find (Path.filename path) ds with
            | None -> assert false
            | Some f ->
              let start_cluster = (snd f.Name.dos).Name.start_cluster in
              return (Some(Entry.Chain.follow fs.format fs.fat start_cluster))
          end
        | _ -> return None

    (* return the storage location of the object identified by [path] *)
    let of_file device fs path =
      chain_of_file device fs path >>= function
      | None -> return Rootdir
      | Some c -> return (Chain c)

    let to_sectors fs = function
      | Chain clusters -> Entry.Chain.to_sectors fs.boot clusters
      | Rootdir -> Boot_sector.sectors_of_root_dir fs.boot

  end

  exception Fs_error of error

  let (>>|=) m f = m >>= function
    | `Error e -> fail (Fs_error e)
    | `Ok x -> f x

  (** [write_to_location device fs path location update] applies [update]
      to the data stored in the object at [path] which is currently
      stored at [location]. If [location] is a chain of clusters then
      it will be extended. *)
  let rec write_to_location device fs path location update : unit io =
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
        fail (Fs_error `No_space)
      | (Location.Rootdir | Location.Chain _), false ->
        let writes = Update.map updates sectors bps in
        Lwt_list.iter_s (write_update device fs) writes >>= fun () ->
        if location = Location.Rootdir then Update.apply fs.root update;
        return location
      | Location.Chain cs, true ->
        let last = if cs = [] then None else Some (List.hd (List.rev cs)) in
        let new_clusters = Entry.Chain.extend fs.boot fs.format fs.fat last clusters_needed in
        let fat_sectors = Boot_sector.sectors_of_fat fs.boot in
        let new_sectors = Entry.Chain.to_sectors fs.boot new_clusters in
        let data_writes = Update.map updates (sectors @ new_sectors) bps in
        Lwt_list.iter_s (write_update device fs) data_writes >>= fun () ->
        let fat_writes = Update.(map (split (from_cstruct 0L fs.fat) bps) fat_sectors bps) in
        Lwt_list.iter_s (write_update device fs) fat_writes >>= fun () ->
        return (Location.Chain (cs @ new_clusters)))  >>= fun location ->
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
             fail (Fs_error (`No_directory_entry (Path.to_string (Path.directory path), Path.filename path)))
           | Some d ->
             let file_size = Name.file_size_of d in
             let new_file_size = max file_size (Int32.of_int (Int64.to_int (Update.total_length update))) in
             let updates = Name.modify bits filename new_file_size start_cluster in
             return updates
        )
    | Location.Rootdir -> return () (* the root directory itself has no attributes *)

  and update_directory_containing device fs path f =
    let parent_path = Path.directory path in
    find device fs parent_path >>= function
      | `Error (x: error) -> fail (Fs_error x)
      | `Ok (File _) -> fail (Fs_error (`Not_a_directory (Path.to_string parent_path)))
      | `Ok (Dir ds) ->
        Location.of_file device fs parent_path >>= fun location ->
        let sectors = Location.to_sectors fs location in
        read_sectors fs.boot.Boot_sector.bytes_per_sector device sectors >>= fun contents ->
    f contents ds >>= fun updates ->
        Lwt_list.iter_s (write_to_location device fs parent_path location) updates

  let if_formatted x f = match x.fs with
    | Some fs -> f fs
    | None -> fail (Fs_error (`Format_not_recognised "FAT"))

  let create_common x path dir_entry =
    let path = Path.of_string path in
    if_formatted x (fun fs ->
        let filename = Path.filename path in
        update_directory_containing x.device fs path
          (fun contents ds ->
             if Name.find filename ds <> None
             then fail (Fs_error (`File_already_exists filename))
             else return (Name.add contents dir_entry)
          )
      )

  let wrap f : [ `Ok of unit | `Error of error ] io =
    catch
      (fun () -> f () >>= fun x -> return (`Ok x))
      (function
       | Fs_error (`Not_a_directory x)         -> return (`Error (`Not_a_directory x))
       | Fs_error (`Directory_not_empty x)     -> return (`Error (`Directory_not_empty x))
       | Fs_error (`File_already_exists x)     -> return (`Error (`File_already_exists x))
       | Fs_error (`Format_not_recognised x)   -> return (`Error (`Format_not_recognised x))
       | Fs_error (`Is_a_directory x)          -> return (`Error (`Is_a_directory x))
       | Fs_error (`No_directory_entry (x, y)) -> return (`Error (`No_directory_entry (x, y)))
       | Fs_error `No_space                    -> return (`Error `No_space)
       | Fs_error (`Unknown_error x)           -> return (`Error (`Unknown_error x))
       | Fs_error (`Block_device x)            -> return (`Error (`Block_device x))
       | Block_device_error `Unimplemented     -> return (`Error (`Block_device `Unimplemented))
       | Block_device_error (`Unknown x)       -> return (`Error (`Block_device (`Unknown x)))
       | Block_device_error `Is_read_only      -> return (`Error (`Block_device `Is_read_only))
       | Block_device_error `Disconnected      -> return (`Error (`Block_device `Disconnected))
       | e                                     -> return (`Error (`Unknown_error (Printexc.to_string e))))

  (** [write x f offset buf] writes [buf] at [offset] in file [f] on
      filesystem [x] *)
  let write x f offset buf : [ `Ok of unit | `Error of error ] io =
    let f = Path.of_string f in
    if_formatted x (fun fs ->
        wrap (fun () ->
            (* u is the update, in file co-ordinates *)
            let u = Update.from_cstruct (Int64.of_int offset) buf in
            (* all data is either in the root directory or in a chain of clusters.
               Note even subdirectories are stored in chains of clusters. *)
            Location.of_file x.device fs f >>= fun location ->
            write_to_location x.device fs f location u)
      )

  (** [create x path] create a zero-length file at [path] *)
  let create x path : [ `Ok of unit | `Error of error ] io =
    wrap (fun () -> create_common x path (Name.make (Filename.basename path)))

  (** [mkdir x path] create an empty directory at [path] *)
  let mkdir x path : [ `Ok of unit | `Error of error ] io =
    wrap (fun () -> create_common x path (Name.make ~subdir:true (Filename.basename path)))

  (** [destroy x path] deletes the entry at [path] *)
  let destroy x path : [ `Ok of unit | `Error of error ] io =
    let path = Path.of_string path in
    if_formatted x (fun fs ->
        let filename = Path.filename path in
        let do_destroy () =
          update_directory_containing x.device fs path
            (fun contents ds ->
               (* XXX check for nonempty *)
               (* XXX delete chain *)
               if Name.find filename ds = None
               then fail (Fs_error (`No_directory_entry(Path.(to_string (directory path)), filename)))
               else return (Name.remove contents filename)
            ) >>= fun () -> return (`Ok ()) in
        find x.device fs path >>= function
        | `Error x -> return (`Error x)
        | `Ok (File _) -> do_destroy ()
        | `Ok (Dir []) -> do_destroy ()
        | `Ok (Dir (_::_)) -> return (`Error(`Directory_not_empty(Path.to_string path)))
      )

  let stat x path =
    let path = Path.of_string path in
    let entry_of_file f = f in
    if_formatted x (fun fs ->
        find x.device fs path >>= function
        | `Error x -> return (`Error x)
        | `Ok (File f) ->
          let r = entry_of_file f in
          return (`Ok {
              filename = r.Name.utf_filename;
              read_only = (snd r.Name.dos).Name.read_only;
              directory = false;
              size = Int64.of_int32 ((snd r.Name.dos).Name.file_size);
            })
        | `Ok (Dir ds) ->
          if Path.is_root path
          then return (`Ok {
              filename = "/";
              read_only = false;
              directory = true;
              size = 0L;
            })
          else
            let filename = Path.filename path in
            let parent_path = Path.directory path in
            find x.device fs parent_path >>= function
            | `Error x -> return (`Error x)
            | `Ok (File _) -> assert false (* impossible by initial match *)
            | `Ok (Dir ds) ->
              begin match Name.find filename ds with
                | None -> assert false (* impossible by initial match *)
                | Some f ->
                  let r = entry_of_file f in
                  return (`Ok {
                      filename = r.Name.utf_filename;
                      read_only = (snd r.Name.dos).Name.read_only;
                      directory = true;
                      size = Int64.of_int32 ((snd r.Name.dos).Name.file_size);
                    })
              end
      )

  let size x path =
    stat x path >>|= fun s ->
    return (`Ok s.size)

  let listdir x path =
    let path = Path.of_string path in
    if_formatted x (fun fs ->
        find x.device fs path >>= function
        | `Ok (File _) -> return (`Error (`Not_a_directory (Path.to_string path)))
        | `Ok (Dir ds) ->
          return (`Ok (List.map Name.to_string ds))
        | `Error x -> return (`Error x)
      )

  let read_file device fs { Name.dos = _, ({ Name.file_size = file_size } as f) } the_start length =
    let bps = fs.boot.Boot_sector.bytes_per_sector in
    let the_file = SectorMap.make (sectors_of_file fs f) in
    (* If the file is small, truncate length *)
    let length = max 0 (min length (Int32.to_int file_size - the_start)) in
    if length = 0
    then return []
    else
      let preceeding, requested, succeeding = SectorMap.byte_range bps the_start length in
      let to_read = SectorMap.compose requested the_file in
      read_sectors fs.boot.Boot_sector.bytes_per_sector device (SectorMap.to_list to_read) >>= fun buffer ->
      let buffer = Cstruct.sub buffer preceeding (Cstruct.len buffer - preceeding - succeeding) in
      return [ buffer ]

  let read x path the_start length =
    let path = Path.of_string path in
    if_formatted x (fun fs ->
        find x.device fs path >>= function
        | `Ok (Dir _) -> return (`Error (`Is_a_directory (Path.to_string path)))
        | `Ok (File f) ->
          read_file x.device fs f the_start length >>= fun buffer ->
          return (`Ok buffer)
        | `Error x -> return (`Error x)
      )
end
