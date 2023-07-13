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

 open Lwt.Infix
 open Mirage_block
 
 type stat = {
   filename: string;
   read_only: bool;
   directory: bool;
   size: int64;
 }
 
 type fs = {
   boot  : Fat_boot_sector.t;
   format: Fat_format.t;
   fat   : Fat_entry.fat;
   root  : Cstruct.t;
 }
 
 module Make (B: Mirage_block.S) = struct
   type t = {
     device: B.t;
     fs: fs;
   }
 
   type error = [
     | `Is_a_directory
     | `No_directory_entry
     | `Not_a_directory
     | `Block_read of B.error
   ]
 
   type write_error = [
     | error
     | `Is_a_directory
     | `No_directory_entry
     | `Not_a_directory
     | `File_already_exists
     | `No_directory_entry
     | `No_space
     | `Directory_not_empty
     | `Block_write of B.write_error
     | `Exn of exn
   ]
 
   let pp_error ppf = function
   | `Is_a_directory      -> Fmt.string ppf "is a directory"
   | `Not_a_directory     -> Fmt.string ppf "is not a directory"
   | `No_directory_entry  -> Fmt.string ppf "a directory in the path does not exist"
     | `Block_read err       -> B.pp_error ppf err
 
   let pp_write_error ppf = function
     | #error as e                 -> pp_error ppf e
     | `Directory_not_empty -> Fmt.string ppf "directory is not empty"
     | `File_already_exists -> Fmt.string ppf "file already exists"
     | `No_space            -> Fmt.string ppf "device has no more free space"
     | `Block_write err            -> B.pp_write_error ppf err
     | `Exn e                      -> Fmt.exn ppf e
 
   let rec iter_s f = function
     | [] -> Lwt.return (Ok ())
     | x :: xs ->
       f x >>= function
       | Error e -> Lwt.return (Error e)
       | Ok () -> iter_s f xs
 
   let (>>*=) x f = x >>= function Ok m -> f m | Error e -> Lwt.return @@ Error e
   let (>|*=) x f = x >|= function Ok m -> f m | Error e -> Error e
 
   let alloc bytes =
     let pages = Io_page.get_buf ~n:((bytes + 4095) / 4096) () in
     Cstruct.sub pages 0 bytes
 
   (* TODO: this function performs extra data copies *)
   let read_sectors bps device xs =
     let buf = alloc (List.length xs * bps) in
     let rec split buf =
       if Cstruct.length buf = 0 then []
       else if Cstruct.length buf <= bps then [ buf ]
       else Cstruct.sub buf 0 bps :: (split (Cstruct.shift buf bps))
     in
     let page = alloc bps in
     B.get_info device >>= fun {sector_size; _} ->
     let rec loop = function
       | []                     -> Lwt.return (Ok ())
       | (sector, buffer) :: xs ->
         let offset = sector * bps in
         let sector' = offset / sector_size in
         Cstruct.memset page 0;
         B.read device (Int64.of_int sector') [ page ] >>= function
         | Error e -> Lwt.return (Error (`Block_read e))
         | Ok () ->
           Cstruct.blit page (offset mod sector_size) buffer 0 bps;
           loop xs
     in
     loop (List.combine xs (split buf)) >|*= fun () ->
     Ok buf
 
   let write_update device fs update =
     let overwrite_sector ~block_number ~sector_offset ~sector_number
         ~sectors_per_block ~bps page
       =
       let sector =
         Cstruct.sub page
           (Int64.(to_int (rem sector_number (of_int sectors_per_block))) * bps)
           bps
       in
       Fat_update.apply sector { update with Fat_update.offset = sector_offset };
       B.write device block_number [ page ] >>= function
       | Error e -> Lwt.return @@ Error (`Block_write e)
       | Ok () -> Lwt.return @@ Ok ()
     in
     B.get_info device >>= fun info ->
     let offset = update.Fat_update.offset in
     let bps = fs.boot.Fat_boot_sector.bytes_per_sector in
     let sector_number = Int64.(div offset (of_int bps)) in
     let sector_offset = Int64.(sub offset (mul sector_number (of_int bps))) in
     (* number of 512-byte FAT sectors per physical disk sectors *)
     let sectors_per_block = info.sector_size / bps in
     let page = alloc 4096 in
     let block_number = Int64.(div sector_number (of_int sectors_per_block)) in
     B.read device block_number [ page ] >>= function
     | Error e -> Lwt.return @@ Error (`Block_read e)
     | Ok () ->
       overwrite_sector ~block_number ~sector_offset ~sector_number
         ~sectors_per_block ~bps page
 
   let make size =
     let open Rresult in
     let boot = Fat_boot_sector.make size in
     Fat_boot_sector.detect_format boot >>= fun format ->
     let fat = Fat_entry.make boot format in
     let root_sectors = Fat_boot_sector.sectors_of_root_dir boot in
     let root = alloc (List.length root_sectors * 512) in
     for i = 0 to Cstruct.length root - 1 do Cstruct.set_uint8 root i 0 done;
     let fs = { boot = boot; format = format; fat = fat; root = root } in
     Ok fs
 
   let format device size =
     (match make size with Ok x -> Lwt.return x | Error x -> Lwt.fail_with x)
     >>= fun fs ->
     let sector = alloc 512 in
     Fat_boot_sector.marshal sector fs.boot;
     let fat_sectors = Fat_boot_sector.sectors_of_fat fs.boot in
     let fat_writes = Fat_update.(
         let updates = split (from_cstruct 0L fs.fat) 512 in
         map updates fat_sectors 512
       )
     in
     let root_sectors = Fat_boot_sector.sectors_of_root_dir fs.boot in
     let root_writes =
       Fat_update.(map (split (from_cstruct 0L fs.root) 512) root_sectors 512)
     in
     let t = { device; fs } in
     write_update device fs (Fat_update.from_cstruct 0L sector) >>*= fun () ->
     iter_s (write_update device fs) fat_writes >>*= fun () ->
     iter_s (write_update device fs) root_writes >|*= fun () ->
     Ok t
 
   let connect device =
     let get_fs sector =
       match Fat_boot_sector.unmarshal sector with
       | Error reason ->
         Fmt.kstr Lwt.fail_with
           "error unmarshalling first sector of block device: %s" reason
       | Ok boot ->
         match Fat_boot_sector.detect_format boot with
         | Error reason ->
           Fmt.kstr Lwt.fail_with
             "error detecting the format of block device: %s" reason
         | Ok format -> Lwt.return (boot, format)
     in
     let page = alloc 4096 in
     B.get_info device >>= fun info ->
     let sector = Cstruct.sub page 0 info.sector_size in
     (B.read device 0L [ sector ] >>= function
       | Error e -> Lwt.return (Error (`Block_read e))
       | Ok () ->
         let open Fat_boot_sector in
         get_fs sector >>= fun (boot, format) ->
         read_sectors boot.bytes_per_sector device (sectors_of_fat boot)
         >>*= fun fat ->
         read_sectors boot.bytes_per_sector device (sectors_of_root_dir boot)
         >|*= fun root ->
         Ok { device; fs = { boot; format; fat; root } })
     >>= function
     | Ok t    -> Lwt.return t
     | Error e ->
       Fmt.kstr Lwt.fail_with "error reading essential sectors: %a" pp_error e
 
   let disconnect _ = Lwt.return ()
 
   type find =
     | Dir of Fat_name.r list
     | File of Fat_name.r
 
   let sectors_of_file fs name =
     let cluster = name.Fat_name.start_cluster in
     Fat_entry.Chain.(to_sectors fs.boot (follow fs.format fs.fat cluster))
 
   let read_whole_file device fs name =
     let f = snd name.Fat_name.dos in
     read_sectors fs.boot.Fat_boot_sector.bytes_per_sector device
       (sectors_of_file fs f)
 
   (** [find device fs path] returns a [find_result] corresponding to
       the object stored at [path]
       XXX: doesn't handle the cases where path is: /a/../b or /a/./b *)
   let find device fs path =
     let readdir = function
       | Dir ds -> Lwt.return (Ok ds)
       | File d ->
         read_whole_file device fs d >|*= fun buf ->
         Ok (Fat_name.list buf)
     in
     let rec inner sofar current = function
       | [] ->
         (match current with
          | Dir ds -> Lwt.return (Ok (Dir ds))
          | File ({Fat_name.dos = _, {Fat_name.is_dot = true; _}; _} as d) ->
            Lwt.return (Ok (File d))
          | File ({Fat_name.dos = _, {Fat_name.is_dotdot = true; _}; _} as d) ->
            Lwt.return (Ok (File d))
          | File {Fat_name.dos = _, {Fat_name.subdir = true; _}; _} ->
            readdir current >|*= fun names ->
            Ok (Dir names)
          | File ({Fat_name.dos = _, {Fat_name.subdir = false; _}; _} as d) ->
            Lwt.return (Ok (File d)))
       | p :: ps ->
         readdir current >>*= fun entries ->
         match Fat_name.find p entries, ps with
         | Some {Fat_name.dos = _, {Fat_name.subdir = false; _}; _}, _ :: _ ->
           Lwt.return (Error `Not_a_directory)
         | Some d, _ -> inner (p::sofar) (File d) ps
         | None, _   -> Lwt.return (Error `No_directory_entry)
     in
     inner [] (Dir (Fat_name.list fs.root)) (Fat_path.to_string_list path)
 
   module Location = struct
 
     (* Files and directories are stored in a location *)
     type t =
       | Chain of int list (* a chain of clusters *)
       | Rootdir           (* the root directory area *)
 
     (** [chain_of_file device fs path] returns [Some chain] where
         [chain] is the chain corresponding to [path] or [None] if
         [path] cannot be found or if it is / and hasn't got a
         chain. *)
     let chain_of_file device fs path =
       if Fat_path.is_root path then Lwt.return (Ok None)
       else
         let parent_path = Fat_path.directory path in
         find device fs parent_path >|= fun entry ->
         match entry with
         | Ok (Dir ds) ->
           begin match Fat_name.find (Fat_path.filename path) ds with
             | None -> assert false
             | Some f ->
               let start_cluster = (snd f.Fat_name.dos).Fat_name.start_cluster in
               Ok (Some(Fat_entry.Chain.follow fs.format fs.fat start_cluster))
           end
         | _ -> Ok None
 
     (* return the storage location of the object identified by [path] *)
     let of_file device fs path =
       chain_of_file device fs path >|*= function
       | None   -> Ok Rootdir
       | Some c -> Ok (Chain c)
 
     let to_sectors fs = function
       | Chain clusters -> Fat_entry.Chain.to_sectors fs.boot clusters
       | Rootdir        -> Fat_boot_sector.sectors_of_root_dir fs.boot
 
   end
 
   (** [write_to_location device fs path location update] applies
       [update] to the data stored in the object at [path] which is
       currently stored at [location]. If [location] is a chain of
       clusters then it will be extended. *)
   let rec write_to_location device fs path location update =
     let bps = fs.boot.Fat_boot_sector.bytes_per_sector in
     let spc = fs.boot.Fat_boot_sector.sectors_per_cluster in
     let updates = Fat_update.split update bps in
     let sectors = Location.to_sectors fs location in
     (* This would be a good point to see whether we need to allocate
        new sectors and do that too. *)
     let current_bytes = bps * (List.length sectors) in
     let bytes_needed =
       max 0L (Int64.(sub (Fat_update.total_length update) (of_int current_bytes)))
     in
     let clusters_needed =
       let bpc = Int64.of_int(spc * bps) in
       Int64.(to_int(div (add bytes_needed (sub bpc 1L)) bpc)) in
     let resolve_location = function
       | Location.Rootdir, true -> Lwt.return (Error `No_space)
       | (Location.Rootdir | Location.Chain _), false ->
         let writes = Fat_update.map updates sectors bps in
         iter_s (write_update device fs) writes >|*= fun () ->
         if location = Location.Rootdir then Fat_update.apply fs.root update;
         Ok location
       | Location.Chain cs, true ->
         let last = if cs = [] then None else Some (List.hd (List.rev cs)) in
         let new_clusters =
           Fat_entry.Chain.extend fs.boot fs.format fs.fat last clusters_needed
         in
         let fat_sectors = Fat_boot_sector.sectors_of_fat fs.boot in
         let new_sectors = Fat_entry.Chain.to_sectors fs.boot new_clusters in
         let data_writes = Fat_update.map updates (sectors @ new_sectors) bps in
         iter_s (write_update device fs) data_writes >>*= fun () ->
         let fat_writes =
           Fat_update.(map (split (from_cstruct 0L fs.fat) bps) fat_sectors bps)
         in
         iter_s (write_update device fs) fat_writes >|*= fun () ->
         Ok (Location.Chain (cs @ new_clusters))
     in
     resolve_location (location, bytes_needed > 0L) >>*= function
     | Location.Chain [] ->
       (* In the case of a previously empty file (location = Chain []),
          we have extended the chain (location = Chain (_ :: _)) so
          it's safe to call List.hd *)
       assert false
     | Location.Chain (start_cluster :: _) ->
       update_directory_containing device fs path
         (fun bits ds ->
            let filename = Fat_path.filename path in
            match Fat_name.find filename ds with
            | None ->
              Lwt.return (Error `No_directory_entry)
            | Some d ->
              let file_size = Fat_name.file_size_of d in
              let new_file_size =
                max file_size
                  (Int32.of_int (Int64.to_int (Fat_update.total_length update)))
              in
              let updates =
                Fat_name.modify bits filename new_file_size start_cluster
              in
              Lwt.return (Ok updates)
         )
     | Location.Rootdir ->
       Lwt.return (Ok ()) (* the root directory itself has no attributes *)
 
   and update_directory_containing device fs path f =
     let parent_path = Fat_path.directory path in
     find device fs parent_path >>*= function
     | File _ -> Lwt.return (Error `Not_a_directory)
     | Dir ds ->
       Location.of_file device fs parent_path >>*= fun location ->
       let sectors = Location.to_sectors fs location in
       read_sectors fs.boot.Fat_boot_sector.bytes_per_sector device sectors
       >>*= fun contents ->
       f contents ds >>*= fun updates ->
       iter_s (write_to_location device fs parent_path location) updates
       >|*= fun () ->
       Ok ()
 
   let create_common x path dir_entry =
     let path = Fat_path.of_string path in
     let filename = Fat_path.filename path in
     update_directory_containing x.device x.fs path
       (fun contents ds ->
          if Fat_name.find filename ds <> None
          then Lwt.return (Error `File_already_exists)
          else Lwt.return (Ok (Fat_name.add contents dir_entry))
       )
 
   let wrap f = Lwt.catch f  (fun e -> Lwt.return (Error (`Exn e)))
 
   (** [write x f offset buf] writes [buf] at [offset] in file [f] on
       filesystem [x] *)
   let write x f offset buf =
     let f = Fat_path.of_string f in
     wrap (fun () ->
         (* u is the update, in file co-ordinates *)
         let u = Fat_update.from_cstruct (Int64.of_int offset) buf in
         (* all data is either in the root directory or in a chain of
            clusters.  Note even subdirectories are stored in chains of
            clusters. *)
         Location.of_file x.device x.fs f >>*= fun location ->
         write_to_location x.device x.fs f location u)
 
   (** [create x path] create a zero-length file at [path] *)
   let create x path =
     wrap (fun () -> create_common x path (Fat_name.make (Filename.basename path)))
 
   (** [mkdir x path] create an empty directory at [path] *)
   let mkdir x path =
     wrap (fun () ->
         create_common x path (Fat_name.make ~subdir:true (Filename.basename path))
       )
 
   (** [destroy x path] deletes the entry at [path] *)
   let destroy x path: (unit, write_error) result Lwt.t  =
     let path = Fat_path.of_string path in
     let filename = Fat_path.filename path in
     let do_destroy () =
       update_directory_containing x.device x.fs path
         (fun contents ds ->
            (* XXX check for nonempty *)
            (* XXX delete chain *)
            if Fat_name.find filename ds = None
            then Lwt.return (Error `No_directory_entry)
            else Lwt.return (Ok (Fat_name.remove contents filename))
         ) in
     find x.device x.fs path >>= function
     | Error x         -> Lwt.return (Error (x :> write_error))
     | Ok (File _)     -> do_destroy ()
     | Ok (Dir [])     -> do_destroy ()
     | Ok (Dir (_::_)) -> Lwt.return (Error `Directory_not_empty)
 
   let stat x path =
     let path = Fat_path.of_string path in
     let entry_of_file f = f in
     find x.device x.fs path >>= function
     | Error x -> Lwt.return (Error x)
     | Ok (File f) ->
       let r = entry_of_file f in
       Lwt.return (Ok {
           filename = r.Fat_name.utf_filename;
           read_only = (snd r.Fat_name.dos).Fat_name.read_only;
           directory = false;
           size = Int64.of_int32 ((snd r.Fat_name.dos).Fat_name.file_size);
         })
     | Ok (Dir _) ->
       if Fat_path.is_root path
       then Lwt.return (Ok {
           filename = "/";
           read_only = false;
           directory = true;
           size = 0L;
         })
       else
         let filename = Fat_path.filename path in
         let parent_path = Fat_path.directory path in
         find x.device x.fs parent_path >|= function
         | Error x     -> Error x
         | Ok (File _) -> assert false (* impossible by initial match *)
         | Ok (Dir ds) ->
           match Fat_name.find filename ds with
           | None -> assert false (* impossible by initial match *)
           | Some f ->
             let r = entry_of_file f in
             Ok {
               filename = r.Fat_name.utf_filename;
               read_only = (snd r.Fat_name.dos).Fat_name.read_only;
               directory = true;
               size = Int64.of_int32 ((snd r.Fat_name.dos).Fat_name.file_size);
             }
 
   let size x path =
     stat x path >|= function
     | Ok s         -> Ok s.size
     | Error _ as e -> e
 
   let listdir x path =
     let path = Fat_path.of_string path in
     find x.device x.fs path >|= function
     | Ok (File _)  -> Error `Not_a_directory
     | Ok (Dir ds)  -> Ok (List.map Fat_name.to_string ds)
     | Error _ as e -> e
 
   let read_file device fs
       {Fat_name.dos = _, ({Fat_name.file_size = file_size; _} as f); _}
       the_start length
     =
     let bps = fs.boot.Fat_boot_sector.bytes_per_sector in
     let the_file = Fat_sector_map.make (sectors_of_file fs f) in
     (* If the file is small, truncate length *)
     let length = max 0 (min length (Int32.to_int file_size - the_start)) in
     if length = 0
     then Lwt.return (Ok [])
     else
       let preceeding, requested, succeeding =
         Fat_sector_map.byte_range bps the_start length
       in
       let to_read = Fat_sector_map.compose requested the_file in
       read_sectors fs.boot.Fat_boot_sector.bytes_per_sector device
         (Fat_sector_map.to_list to_read)
       >|= function
       | Error _ as e -> e
       | Ok buffer    ->
         let buffer =
           Cstruct.sub buffer preceeding
             (Cstruct.length buffer - preceeding - succeeding)
         in
         Ok [ buffer ]
 
   let read x path the_start length =
     let path = Fat_path.of_string path in
     find x.device x.fs path >>= function
     | Ok (Dir _)  -> Lwt.return (Error `Is_a_directory)
     | Ok (File f) -> read_file x.device x.fs f the_start length
     | Error x     -> Lwt.return (Error x)
 end
 
 module KV_RO(B: Mirage_block.S) = struct
   module FS = Make(B)
   type t = FS.t
   type error = [ Mirage_kv.error | `FS of FS.error ]
   type key = Mirage_kv.Key.t
 
   let pp_error ppf = function
     | #Mirage_kv.error as e -> Mirage_kv.pp_error ppf e
     | `FS e                 -> FS.pp_error ppf e
 
   let disconnect t = FS.disconnect t
 
   let exists t key =
     let name = Mirage_kv.Key.to_string key in
     FS.stat t name >|= function
     | Ok s ->
       Ok (Some (if s.directory then `Dictionary else `Value))
     | Error `Not_a_directory
     | Error `No_directory_entry -> Ok None
     | Error e -> Error (`FS e)
 
   let get t key =
     let name = Mirage_kv.Key.to_string key in
     FS.stat t name >>= function
     | Error `Is_a_directory -> Lwt.return (Error (`Value_expected key))
     | Error `No_directory_entry -> Lwt.return (Error (`Not_found key))
     | Error e -> Lwt.return (Error (`FS e))
     | Ok s ->
       FS.read t name 0 (Int64.to_int s.size) >|= function
       | Error e -> Error (`FS e)
       | Ok l -> Ok Cstruct.(to_string (concat l))
   
   let get_partial t key ~offset ~length =
     let name = Mirage_kv.Key.to_string key in
     FS.stat t name >>= function
     | Error `Is_a_directory -> Lwt.return (Error (`Value_expected key))
     | Error `No_directory_entry -> Lwt.return (Error (`Not_found key))
     | Error e -> Lwt.return (Error (`FS e))
     | Ok s ->
       let file_size = Int64.to_int s.size in
       let start_offset = Optint.Int63.to_int offset in
       let actual_length = min length (file_size - start_offset) in
       if start_offset > file_size then
         Lwt.return (Ok "")
       else
         FS.read t name start_offset actual_length >|= function
         | Error e -> Error (`FS e)
         | Ok l -> Ok Cstruct.(to_string (concat l)) 
 
   let list t key =
     let name = Mirage_kv.Key.to_string key in
     let dict_or_value fn =
       FS.stat t Mirage_kv.Key.(to_string (key / fn)) >|= function
       | Error e -> Error (`FS e)
       | Ok s -> Ok (if s.directory then `Dictionary else `Value)
     in
     FS.listdir t name >>= function
     | Error `Not_a_directory -> Lwt.return (Error (`Dictionary_expected key))
     | Error `No_directory_entry -> Lwt.return (Error (`Not_found key))
     | Error e -> Lwt.return (Error (`FS e))
     | Ok files ->
       Lwt_list.fold_left_s (fun acc f ->
           match acc with
           | Error e -> Lwt.return (Error e)
           | Ok acc -> dict_or_value f >|= function
             | Error e -> Error e
             | Ok t -> Ok ((Mirage_kv.Key.add key f, t) :: acc))
         (Ok []) files        
 
   let last_modified t key = 
     let name = Mirage_kv.Key.to_string key in
     FS.stat t name >>= function
     | Error `Is_a_directory -> Lwt.return (Error (`Value_expected key))
     | Error `No_directory_entry -> Lwt.return (Error (`Not_found key))
     | Error e -> Lwt.return (Error (`FS e))
     | Ok _s -> Lwt.return (Ok (Ptime.epoch))
 
   let digest t key =
     get t key >|= function
     | Error e -> Error e
     | Ok data -> Ok (Digest.string data)    
 
   let size t key =
     let name = Mirage_kv.Key.to_string key in
     FS.stat t name >>= function
     | Error `Is_a_directory -> Lwt.return (Error (`Value_expected key))
     | Error `No_directory_entry -> Lwt.return (Error (`Not_found key))
     | Error e -> Lwt.return (Error (`FS e))
     | Ok s -> Lwt.return (Ok (Optint.Int63.of_int64 s.size))
 
   let connect t = FS.connect t
 end