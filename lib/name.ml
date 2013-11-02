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

type datetime = {
  year: int;
  month: int;
  day: int;
  hours: int;
  mins: int;
  secs: int;
  ms: int;
}

let epoch = {
  year = 1980; month = 0; day = 0;
  hours = 0; mins = 0; secs = 0; ms = 0;
}

(** Long filename entry: the same size as an original DOS disk entry *)
type lfn = {
  lfn_deleted: bool;
  lfn_last: bool; (** marks the highest sequence number *)
  lfn_seq: int;
  lfn_checksum: int;
  lfn_utf16_name: string
}

(** A DOS disk entry *)
type dos = {
  filename: string; (** 8 chars *)
  ext: string;      (** 3 chars *)
  deleted: bool;
  read_only: bool;
  hidden: bool;
  system: bool;
  volume: bool;
  subdir: bool;
  archive: bool;
  create: datetime;
  access: datetime;
  modify: datetime;
  start_cluster: int;
  file_size: int32;
}

(** Useful for streaming entries to/from the disk *)
type single_entry =
| Dos of dos
| Lfn of lfn
| End

(** A high-level directory entry, complete with reconstructed UTF name and
    offsets of each individual entry on the disk *)
type r = {
  utf_filename: string;
  dos: int * dos;
  lfns: (int * lfn) list;
}

(* Make the tree more uniform by creating a "fake root" node above the
   root directory entries *)
let fake_root_entry = {
  utf_filename = "";
  dos = 0, {
    filename = ""; ext = ""; deleted = false; read_only = false;
    hidden = false; system = false; volume = false; subdir = true; archive = false;
    create = epoch; access = epoch; modify = epoch; start_cluster = 0; file_size = 0l
  };
  lfns = []
}

let remove_padding x =
  let rec inner = function
    | -1 -> x
    | n when x.[n] = ' ' -> inner (n-1)
    | n -> String.sub x 0 (n + 1) in
  inner (String.length x - 1)

let file_size_of r = (snd r.dos).file_size
let deleted r = (snd r.dos).deleted
let filename_of r =
  if r.lfns <> []
  then r.utf_filename
  else
    let d = snd(r.dos) in
    (remove_padding d.filename) ^ "." ^ (remove_padding d.ext)

let to_single_entries r =
  List.rev ((Dos (snd r.dos)) :: (List.map (fun l -> Lfn (snd l)) r.lfns))

let legal_dos_char = function
  | 'A' .. 'Z'
  | '0' .. '9'
  | ' '
  | '!' | '#' | '$' | '%' | '&' | '\'' | '(' | ')' 
  | '-' | '@' | '^' | '_' | '`' | '{'  | '}' | '~' -> true
  | c -> int_of_char c >= 128

let legal_dos_string x =
  try
    for i = 0 to String.length x - 1 do
      if not(legal_dos_char x.[i]) then raise Not_found
    done;
    true
  with Not_found -> false

let dot = Re_str.regexp_string "."
let is_legal_dos_name filename = match Re_str.split dot filename with
  | [ one ] -> String.length one <= 8 && (legal_dos_string one)
  | [ one; two ] -> String.length one <= 8 && (String.length two <= 3) && (legal_dos_string one) && (legal_dos_string two)
  | _ -> false

let add_padding p n x =
  if String.length x >= n then x
  else
    let y = String.make n p in
    String.blit x 0 y 0 (String.length x);
    y

let dos_name_of_filename filename =
  if is_legal_dos_name filename
  then match Re_str.split dot filename with
    | [ one ] -> add_padding ' ' 8 one, "   "
    | [ one; two ] -> add_padding ' ' 8 one, add_padding ' ' 3 two
    | _ -> assert false (* implied by is_legal_dos_name *)
  else
    let all = String.uppercase (Digest.to_hex (Digest.string filename)) in
    let base = String.sub all 0 8 in
    let ext = String.sub all 8 3 in
    base, ext

let ascii_to_utf16 x =
  let l = String.length x in
  (* round up to next multiple of 13 *)
  let padto = (l + 1 + 12) / 13 * 13 in
  let total = max (l + 1) padto in (* NULL *)
  let results = String.make (total * 2) (char_of_int 0xff) in
  for i = 0 to l - 1 do
    results.[i*2] <- x.[i];
    results.[i*2+1] <- char_of_int 0;
  done;
  results.[l*2] <- char_of_int 0;
  results.[l*2+1] <- char_of_int 0;
  results

(** Returns the checksum corresponding to the 8.3 DOS filename *)
let compute_checksum x =
  let y = add_padding ' ' 8 x.filename ^ (add_padding ' ' 3 x.ext) in
  let rec inner sum i =
    if i = String.length y then sum
    else
      (* In [*] below: note the algorithm given by wikipedia uses arithmetic
         modulo 256 to make things less obvious than using natural numbers. *)
      let sum' = (sum land 1) lsl 7 + ((sum (* [*] *) land 0xff) lsr 1) + (int_of_char y.[i]) in
      inner sum' (i + 1) in
  (inner 0 0) land 0xff

let make ?(read_only=false) ?(system=false) ?(subdir=false) filename =
  (* entries with file size 0 should have start cluster 0 *)
  let start_cluster = 0 and file_size = 0l in
  let filename', ext = dos_name_of_filename filename in
  let dos = {
    filename = filename';
    ext = ext;
    deleted = false;
    read_only = read_only;
    hidden = false;
    system = system;
    volume = false;
    subdir = subdir;
    archive = false;
    create = epoch;
    access = epoch;
    modify = epoch;
    start_cluster = start_cluster;
    file_size = file_size
  } in
  let checksum = compute_checksum dos in
  let lfns =
    if is_legal_dos_name filename
    then []
    else
      (* chop filename into 13 character / 26 byte chunks *)
      let padded_utf16 = ascii_to_utf16 filename in
      let rec inner acc seq i =
        let last = i + 26 = String.length padded_utf16 in
        let finished = i + 26 > String.length padded_utf16 in
        if finished then acc
        else
          let chunk = String.sub padded_utf16 i 26 in
          let lfn = {
            lfn_deleted = false;
            lfn_last = last;
            lfn_seq = seq;
            lfn_checksum = checksum;
            lfn_utf16_name = chunk;
          } in
          inner (lfn :: acc) (seq + 1) (i + 26) in
        inner [] 1 0 in
  {
    utf_filename = filename;
    dos = 0, dos;
    lfns = List.map (fun l -> 0, l) lfns
  }

  let _ =
    let checksum_tests = [
      make "MAKEFILE", 193;
      make "FAT.ML", 223;
    ] in
    List.iter (fun (d, expected) ->
      let d = snd d.dos in
      let checksum = compute_checksum d in
      if checksum <> expected then failwith (Printf.sprintf "checksum_tests: %s.%s expected=%d actual=%d" d.filename d.ext expected checksum)
    ) checksum_tests

let to_string x =
  let trim_utf16 x =
    let chars = ref (String.length x / 2) in
    for i = 0 to String.length x / 2 - 1 do
      let a = int_of_char x.[i * 2] and b = int_of_char x.[i * 2 + 1] in
      if a = 0xff && b = 0xff && i < !chars then chars := i
    done;
    String.sub x 0 (!chars * 2) in
  let d = snd x.dos in
  Printf.sprintf "%-8s %-3s %10s %04d-%02d-%02d  %02d:%02d  %s"
    d.filename d.ext
    (if d.subdir then "<DIR>     " else (Printf.sprintf "%10ld" d.file_size))
    d.create.year d.create.month d.create.day
    d.create.hours d.create.mins
    (trim_utf16 x.utf_filename)

let int_to_hms time =
  let hours = ((time lsr 11) land 0b11111) in
  let mins = (time lsr 5) land 0b111111 in
  let secs = (time land 0b11111) * 2 in
  hours, mins, secs

let hms_to_int (hours, mins, secs) =
  let h = (hours land 0b11111) lsl 11 in
  let m = (mins land 0b111111) lsl 5 in
  let s = ((secs/2) land 0b11111) in
  h lor m lor s

let int_of_time time = hms_to_int (time.hours, time.mins, time.secs)

let time_of_int date time ms =
  let day = date land 0b11111 in
  let month = (date lsr 5) land 0b1111 in
  let year = (date lsr 9) + 1980 in
  let hours, mins, secs = int_to_hms time in
  { day = day; month = month; year = year;
    hours = hours; mins = mins; secs = secs; ms = ms }

let int_of_date x =
  let d = x.day land 0b11111 in
  let m = (x.month land 0b1111) lsl 5 in
  let y = (x.year - 1980) lsl 9 in
  d lor m lor y

let of_bitstring bits =
  bitmatch bits with
  | { seq: 8;
      utf1: (10 * 8): string;
      0x0f: 8;
      0: 8;
      checksum: 8;
      utf2: (12 * 8): string;
      0: 16;
      utf3: (4 * 8): string
    } ->
    Lfn {
      lfn_deleted = seq land 0x80 = 0x80;
      lfn_last = seq land 0x40 = 0x40;
      lfn_seq = seq land 0x3f;
      lfn_checksum = checksum;
      lfn_utf16_name = utf1 ^ utf2 ^ utf3;
    }
    | { filename: (8 * 8): string;
        ext: (3 * 8): string;
        _: 1; (* unused *)
        _: 1; (* device *)
        archive: 1;
        subdir: 1;
        volume: 1;
        system: 1;
        hidden: 1;
        read_only: 1;
        _: 8; (* reserved *)
        create_time_ms: 8; (* high precision create time 0-199 in units of 10ms *)
        create_time: 16: littleendian;
	create_date: 16: littleendian;
	last_access_date: 16: littleendian;
	ea_index: 16: littleendian;
	last_modify_time: 16: littleendian;
	last_modify_date: 16: littleendian;
	start_cluster: 16: littleendian;
	file_size: 32: littleendian
      } ->
        let x = int_of_char filename.[0] in
        if x = 0
        then End
        else
          let deleted = x = 0xe5 in
          filename.[0] <- char_of_int (if x = 0x05 then 0xe5 else x);
          Dos {
            filename = remove_padding filename;
            ext = remove_padding ext;
            read_only = read_only;
            deleted = deleted;
            hidden = hidden;
            system = system;
            volume = volume;
            subdir = subdir;
            archive = archive;
            create = time_of_int create_date create_time create_time_ms;
            access = time_of_int last_access_date 0 0;
            modify = time_of_int last_modify_date last_modify_time 0;
            start_cluster = start_cluster;
            file_size = file_size
          }
    | { _ } ->
      let (s, off, len) = bits in
      if len = 0
      then End
      else failwith (Printf.sprintf "Not a dir entry off=%d len=%d" off len)

    let to_bitstring = function
      | End ->
	let zeroes = String.make 32 (char_of_int 0) in
	BITSTRING {
	  zeroes: (32 * 8): string
	}
      | Lfn l ->
	let seq = l.lfn_seq lor (if l.lfn_last then 0x40 else 0) lor (if l.lfn_deleted then 0x80 else 0) in
	let utf = add_padding (char_of_int 0xff) 26 l.lfn_utf16_name in
	let utf1 = String.sub utf 0 10 in
	let utf2 = String.sub utf 10 12 in
	let utf3 = String.sub utf 22 4 in
	let checksum = l.lfn_checksum in
	BITSTRING {
	  seq: 8;
          utf1: (10 * 8): string;
          0x0f: 8;
          0: 8;
          checksum: 8;
          utf2: (12 * 8): string;
          0: 16;
          utf3: (4 * 8): string
	}
      | Dos x ->
	let filename = add_padding ' ' 8 x.filename in
	let y = int_of_char filename.[0] in
        filename.[0] <- char_of_int (if y = 0xe5 then 0x05 else y);
	if x.deleted then filename.[0] <- char_of_int 0xe5;
	let ext = add_padding ' ' 3 x.ext in
	let create_time_ms = x.create.ms in
	let create_time = int_of_time x.create in
	let create_date = int_of_date x.create in
	let last_access_date = int_of_date x.access in
	let last_modify_time = int_of_time x.modify in
	let last_modify_date = int_of_date x.modify in
	BITSTRING {
	  filename: (8 * 8): string;
          ext: (3 * 8): string;
          false: 1; (* unused *)
          false: 1; (* device *)
          x.archive: 1;
          x.subdir: 1;
          x.volume: 1;
          x.system: 1;
          x.hidden: 1;
          x.read_only: 1;
          0: 8; (* reserved *)
          create_time_ms: 8; (* high precision create time 0-199 in units of 10ms *)
          create_time: 16: littleendian;
          create_date: 16: littleendian;
	  last_access_date: 16: littleendian;
	  0: 16: littleendian;
	  last_modify_time: 16: littleendian;
	  last_modify_date: 16: littleendian;
	  x.start_cluster: 16: littleendian;
	  x.file_size: 32: littleendian
	}

let entry_size = 32 (* bytes *)

(** [bitstring_chop n b] splits [b] into a list of bitstrings, all but possibly
    the last of size [n] *)
let bitstring_chop n bits =
  let module B = Bitstring in
  let rec inner acc bits =
    if B.bitstring_length bits <= n then bits :: acc
    else inner (B.takebits n bits :: acc) (B.dropbits n bits) in
  List.rev (inner [] bits)

(** [blocks bits] returns the directory chopped into individual bitstrings,
    each one containing a possible Dir_entry (fragment) *)
let blocks bits =
  let list = bitstring_chop (8 * entry_size) bits in
  List.rev (fst (List.fold_left (fun (acc, offset) bs -> ((offset, bs)::acc, offset + entry_size)) ([], 0) list))

(** [fold f initial bits] folds [f acc offset dir_entry] across all the
    reconstructed directory entries contained in bits. *)
let fold f initial bits =
  (* Stop as soon as we find a None *)
  let rec inner lfns acc = function
    | [] -> acc
    | (offset, b) :: bs ->
      begin match of_bitstring b with
      | Dos { deleted = true }
      | Lfn { lfn_deleted = true } -> inner lfns acc bs
      | Lfn lfn -> inner ((offset, lfn) :: lfns) acc bs
      | Dos d ->
        let expected_checksum = compute_checksum d in
	      (* TESTING ONLY *)
              let b' = to_bitstring (Dos d) in
	      if Bitstring.compare b b' <> 0 then begin
                Printf.printf "On disk:\n";
		Bitstring.hexdump_bitstring stdout b;
		Printf.printf "Regenerated:\n";
		Bitstring.hexdump_bitstring stdout b'
	      end;
        (* reconstruct UTF text from LFNs *)
        let lfns = List.sort (fun a b -> compare (snd a).lfn_seq (snd b).lfn_seq) lfns in
        List.iter
          (fun (_, l) -> if l.lfn_checksum <> expected_checksum then begin
             Printf.printf "Filename: %s.%s; expected_checksum = %d; actual = %d\n%!" d.filename d.ext expected_checksum l.lfn_checksum
          end) lfns;
        let utfs = List.rev (List.fold_left (fun acc (_, lfn) -> lfn.lfn_utf16_name :: acc) [] lfns) in
        let reconstructed = {
          utf_filename = String.concat "" utfs;
          dos = offset, d;
          lfns = lfns;
        } in
        let acc' = f acc offset reconstructed in
        inner [] acc' bs
      | End -> acc
      end in
    inner [] initial (blocks bits)

(** [list bits] returns a list of valid (not deleted) directory entries
    contained within the directory [bits] *)
let list = fold (fun acc _ d -> d :: acc) []

(** [next bits] returns the bit offset of a free directory slot. Note this
    function does not recycle deleted elements. *)
let next bits =
   let rec inner offset = function
     | [] -> None
     | b :: bs ->
       begin match of_bitstring b with
       | End -> Some offset
       | _ -> inner (8 * 32 + offset) bs
       end in
   inner 0 (bitstring_chop (8 * 32) bits)

(** [add block t] return the update required to add [t] to the directory [block].
    Note the update may be beyond the end of [block] indicating more space needs 
    to be allocated. *)
let add block r =
  let after_block = Bitstring.bitstring_length block in
  let next_bit = match next block with
  | Some b -> b
  | None -> after_block in
  let dir_entries = to_single_entries r in
  let bits = Bitstring.concat (List.map to_bitstring dir_entries) in
   [ Update.make (Int64.of_int (next_bit / 8)) bits ]

let name_match name x =
  let utf_name = ascii_to_utf16 name in
  let d = snd x.dos in
  let d_filename = remove_padding d.filename in
  let d_ext = remove_padding d.ext in
  if is_legal_dos_name name
  then begin
    let filename, ext = dos_name_of_filename name in
    let filename = remove_padding filename and ext = remove_padding ext in
    filename = d_filename && ext = d_ext
  end else
    utf_name = x.utf_filename || name = x.utf_filename

(** [find name list] returns [Some d] where [d] is a Dir_entry.t with
    name [name] (or None) *)
let find name list =
  try Some (List.find (name_match name) list) with Not_found -> None

let remove block filename =
  match find filename (list block) with
  | Some r ->
    let offsets = fst r.dos :: (List.map fst r.lfns) in
    List.rev (List.fold_left
      (fun acc offset ->
        let b = Bitstring.takebits (8 * entry_size) (Bitstring.dropbits (8 * offset) block) in
        let update = match of_bitstring b with
        | Lfn lfn ->
          let lfn' = { lfn with lfn_deleted = true } in
          Update.make (Int64.of_int offset) (to_bitstring (Lfn lfn'))
        | Dos dos ->
          let dos' = { dos with deleted = true } in
          Update.make (Int64.of_int offset) (to_bitstring (Dos dos'))
        | End -> assert false
        in
        update :: acc
      ) [] offsets)
  | None -> [] (* no updates implies nothing to remove *)

let modify block filename file_size start_cluster =
  fold (fun acc offset x ->
    if name_match filename x
    then
    let offset, dos = x.dos in
    let dos' = { dos with file_size = file_size; start_cluster = start_cluster } in
    (Update.make (Int64.of_int offset) (to_bitstring (Dos dos'))) :: acc
    else acc
  ) [] block
