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

module type BLOCK = sig
  type 'a t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t

  val read_sector: Cstruct.t -> int -> unit
  val write_sector: Cstruct.t -> int -> unit
end

type error =
  | Not_a_directory of Path.t
  | Is_a_directory of Path.t
  | Directory_not_empty of Path.t
  | No_directory_entry of Path.t * string
  | File_already_exists of string
  | No_space

open Result
let iter f xs = List.fold_left (fun r x -> match r with Error _ -> r | _ -> f x) (Ok ()) xs

module Stat = struct
  type t = 
    | File of Name.r
    | Dir of Name.r * (Name.r list) (** the directory itself and its immediate children *)
end

module type FS = sig
  type fs
  val make: unit -> fs

  type file

  val create: fs -> Path.t -> (unit, error) result

  val mkdir: fs -> Path.t -> (unit, error) result

  (** [destroy fs path] removes a [path] on filesystem [fs] *)
  val destroy: fs -> Path.t -> (unit, error) result

  (** [file_of_path fs path] returns a [file] corresponding to [path] on
      filesystem [fs] *)
  val file_of_path: fs -> Path.t -> file

  (** [stat fs f] returns information about file [f] on filesystem [fs] *)
  val stat: fs -> Path.t -> (Stat.t, error) result

  (** [write fs f offset data] writes [data] at [offset] in file [f] on
      filesystem [fs] *)
  val write: fs -> file -> int -> Cstruct.t -> (unit, error) result

  (** [read fs f offset length] reads up to [length] bytes from file [f] on
      filesystem [fs]. If less data is returned than requested, this indicates
      end-of-file. *)
  val read: fs -> file -> int -> int -> (Cstruct.t list, error) result
end

module FATFilesystem = functor(B: BLOCK) -> struct
  type fs = {
    boot: Boot_sector.t;
    format: Fat_format.t; (** FAT12, 16 or 32 *)
    fat: Entry.fat;       (** contains the whole FAT *)
    root: Cstruct.t;      (** contains the root directory *)
  }

  let read_sectors xs =
    let buf = Cstruct.create (List.length xs * 512) in
    let (_: int) = List.fold_left (fun i n ->
      let sector = Cstruct.sub buf (i * 512) 512 in
      B.read_sector sector n;
      i + 1
    ) 0 xs in
    buf

  let make () =
    let sector = Cstruct.create 512 in
    B.read_sector sector 0;
    let boot = match Boot_sector.unmarshal sector with
    | Ok x -> x
    | Error x -> failwith x in
    let format = match Boot_sector.detect_format boot with
    | None -> failwith "Failed to detect FAT format"
    | Some format -> format in
    let fat = read_sectors (Boot_sector.sectors_of_fat boot) in
    let root = read_sectors (Boot_sector.sectors_of_root_dir boot) in
    { boot = boot; format = format; fat = fat; root = root }

  type file = Path.t
  let file_of_path fs x = x

  type find =
    | Dir of Name.r list
    | File of Name.r

  let sectors_of_chain x chain =
    List.concat (List.map (Boot_sector.sectors_of_cluster x.boot) chain)
       
  let sectors_of_file x { Name.start_cluster = cluster; file_size = file_size; subdir = subdir } =
    let chain = Entry.follow_chain x.format x.fat cluster in
    sectors_of_chain x chain

  let read_whole_file x { Name.dos = _, ({ Name.file_size = file_size; subdir = subdir } as f) } =
    read_sectors (sectors_of_file x f)

  let write_update x ({ Update.offset = offset; data = data } as update) =
    let bps = x.boot.Boot_sector.bytes_per_sector in
    let sector_number = Int64.(div offset (of_int bps)) in
    let sector_offset = Int64.(sub offset (mul sector_number (of_int bps))) in
    let sector = Cstruct.create 512 in
    B.read_sector sector (Int64.to_int sector_number);
    Update.apply sector { update with Update.offset = sector_offset };
    B.write_sector sector (Int64.to_int sector_number)

  (** [find x path] returns a [find_result] corresponding to the object
      stored at [path] *)
  let find x path : (find, error) result =
    let readdir = function
      | Dir ds -> ds
      | File d -> Name.list (read_whole_file x d) in
    let rec inner sofar current = function
    | [] ->
      begin match current with
      | Dir ds -> Ok (Dir ds)
      | File { Name.dos = _, { Name.subdir = true } } -> Ok (Dir (readdir current))
      | File ( { Name.dos = _, { Name.subdir = false } } as d ) -> Ok (File d)
      end
    | p :: ps ->
      let entries = readdir current in
      begin match Name.find p entries, ps with
      | Some { Name.dos = _, { Name.subdir = false } }, _ :: _ ->
        Error (Not_a_directory (Path.of_string_list (List.rev (p :: sofar))))
      | Some d, _ ->
        inner (p::sofar) (File d) ps
      | None, _ ->
        Error(No_directory_entry (Path.of_string_list (List.rev sofar), p))
      end in
    inner [] (Dir (Name.list x.root)) (Path.to_string_list path)

  (** Updates to files and directories involve writing to the following disk areas: *)
  type location =
    | Chain of int list (** write to a file/directory stored in a chain *)
    | Rootdir           (** write to the root directory area *)


  (** [chain_of_file x path] returns [Some chain] where [chain] is the chain
      corresponding to [path] or [None] if [path] cannot be found or if it
      is / and hasn't got a chain. *)
  let chain_of_file x path =
    if Path.is_root path then None
    else
      let parent_path = Path.directory path in
      match find x parent_path with
	| Ok (Dir ds) ->
	  begin match Name.find (Path.filename path) ds with
	    | None -> assert false
	    | Some f ->
	      let start_cluster = (snd f.Name.dos).Name.start_cluster in
	      Some(Entry.follow_chain x.format x.fat start_cluster)
	  end
	| _ -> None

  (** [write_to_location x path location update] applies [update] to the data given by
      [location]. This will also allocate any additional clusters necessary. *)
  let rec write_to_location x path location update : (unit, error) result =
    let bps = x.boot.Boot_sector.bytes_per_sector in
    let spc = x.boot.Boot_sector.sectors_per_cluster in
    let updates = Update.split update bps in
    let sectors = match location with 
      | Chain clusters -> sectors_of_chain x clusters
      | Rootdir -> Boot_sector.sectors_of_root_dir x.boot in
    (* This would be a good point to see whether we need to allocate
       new sectors and do that too. *)
    let current_bytes = bps * (List.length sectors) in
    let bytes_needed = max 0L (Int64.(sub (Update.total_length update) (of_int current_bytes))) in
    let clusters_needed =
      let bpc = Int64.of_int(spc * bps) in
      Int64.(to_int(div (add bytes_needed (sub bpc 1L)) bpc)) in
    match location, bytes_needed > 0L with
      | Rootdir, true ->
	Error No_space
      | (Rootdir | Chain _), false ->
	let writes = Update.map updates sectors bps in
	List.iter (write_update x) writes;
	if location = Rootdir then Update.apply x.root update;
	Ok ()
      | Chain cs, true ->
	let last = if cs = [] then None else Some (List.hd (List.tl cs)) in
	let new_clusters = Entry.extend x.boot x.format x.fat last clusters_needed in
	let fat_sectors = Boot_sector.sectors_of_fat x.boot in
	let new_sectors = sectors_of_chain x new_clusters in
	let data_writes = Update.map updates (sectors @ new_sectors) bps in
	List.iter (write_update x) data_writes;
        let fat_writes = Update.(map (split (from_cstruct 0L x.fat) bps) fat_sectors bps) in
	List.iter (write_update x) fat_writes;
	update_directory_containing x path
	  (fun bits ds ->
	    let enoent = Error(No_directory_entry (Path.directory path, Path.filename path)) in
	    let filename = Path.filename path in
	    match Name.find filename ds with
	      | None ->
		enoent
	      | Some d ->
		let file_size = Name.file_size_of d in
		let new_file_size = max file_size (Int32.of_int (Int64.to_int (Update.total_length update))) in
		let start_cluster = List.hd (cs @ new_clusters) in
                Ok (Name.modify bits filename new_file_size start_cluster)
	  )

  and update_directory_containing x path f =
    let parent_path = Path.directory path in
    match find x parent_path with
      | Error x -> Error x
      | Ok (File _) -> Error(Not_a_directory parent_path)
      | Ok (Dir ds) ->
	let sectors, location = match (chain_of_file x parent_path) with
	  | None -> Boot_sector.sectors_of_root_dir x.boot, Rootdir
	  | Some c -> sectors_of_chain x c, Chain c in
	let contents = read_sectors sectors in
	begin match f contents ds with
	  | Error x -> Error x
	  | Ok updates ->
                          (*
            (* Rewrite all sectors *)
            let updates = Update.(map (split (from_cstruct 0L contents) 512) sectors 512) in
  *)
	    begin match iter (write_to_location x parent_path location) updates with
	      | Ok () -> Ok ()
	      | Error x -> Error x
	    end
	end

  (** [write x f offset bs] writes bitstring [bs] at [offset] in file [f] on
      filesystem [x] *)
  let write x f offset bs =
    let u = Update.from_cstruct (Int64.of_int offset) bs in
    let location = match chain_of_file x f with
      | None -> Rootdir
      | Some c -> Chain (sectors_of_chain x c) in
    write_to_location x f location u

  let create_common x path dir_entry =
    let filename = Path.filename path in
    update_directory_containing x path
      (fun contents ds ->
	if Name.find filename ds <> None
	then Error (File_already_exists filename)
	else Ok (Name.add contents dir_entry)
      )

  (** [create x path] create a zero-length file at [path] *)
  let create x path : (unit, error) result =
    create_common x path (Name.make (Path.filename path))

  (** [mkdir x path] create an empty directory at [path] *)
  let mkdir x path : (unit, error) result =
    create_common x path (Name.make ~subdir:true (Path.filename path))

  (** [destroy x path] deletes the entry at [path] *)
  let destroy x path : (unit, error) result =
    let filename = Path.filename path in
    let do_destroy () =
      update_directory_containing x path
	(fun contents ds ->
	(* XXX check for nonempty *)
	(* XXX delete chain *)
	  if Name.find filename ds = None
	  then Error (No_directory_entry(Path.directory path, filename))
	  else Ok (Name.remove contents filename)
	) in
    match find x path with
      | Error x -> Error x
      | Ok (File _) -> do_destroy ()
      | Ok (Dir []) -> do_destroy ()
      | Ok (Dir (_::_)) -> Error(Directory_not_empty(path))

  let stat x path =
    let entry_of_file f = f in
    match find x path with
      | Error x -> Error x
      | Ok (File f) -> Ok (Stat.File (entry_of_file f))
      | Ok (Dir ds) ->
	let ds' = List.map entry_of_file ds in
	if Path.is_root path
	then Ok (Stat.Dir (entry_of_file Name.fake_root_entry, ds'))
	else
	  let filename = Path.filename path in
	  let parent_path = Path.directory path in
	  match find x parent_path with
	    | Error x -> Error x
	    | Ok (File _) -> assert false (* impossible by initial match *)
	    | Ok (Dir ds) ->
	      begin match Name.find filename ds with
		| None -> assert false (* impossible by initial match *)
		| Some f ->
		  Ok (Stat.Dir (entry_of_file f, ds'))
	      end

  let bitstring_clip (s_s, s_off, s_len) offset length =
    let s_end = s_off + s_len in
    let the_end = offset + length in
    let offset' = max s_off offset in
    let end' = min s_end the_end in
    let length' = max 0 (end' - offset') in
    s_s, offset', length'

  let read_file x { Name.dos = _, ({ Name.file_size = file_size } as f) } the_start length =
    let bps = x.boot.Boot_sector.bytes_per_sector in
    let sm = SectorMap.make (sectors_of_file x f) in
    (* Clip [length] so that the region is within [file_size] *)
    let length = min (the_start + length) (Int32.to_int file_size) - the_start in
    (* Compute the list of sectors from the_start to length inclusive *)
    let the_end = the_start + length in
    let start_sector = the_start / bps in
    let rec inner acc sector bytes_read =
      if bytes_read >= length
      then List.rev acc
      else
        let data = Cstruct.create bps in
        B.read_sector data (SectorMap.find sm sector);
        (* consider whether this sector needs to be clipped to be within the range *)
	let needed_start = max 0 (the_start - sector * bps) in
	let needed_trim_from_end = max 0 ((sector + 1) * bps - the_end) in
	let needed_length = bps - needed_start - needed_trim_from_end in
        let data =
	  if needed_length <> bps
          then Cstruct.sub data needed_start needed_length
          else data in
        inner (data :: acc) (sector + 1) (bytes_read + needed_length) in
    inner [] start_sector 0
      
  let read x path the_start length =
    match find x path with
      | Ok (Dir _) -> Error (Is_a_directory path)
      | Ok (File f) -> Ok (read_file x f the_start length)
      | Error x -> Error x

end
