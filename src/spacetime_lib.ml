(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2015--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Raw_spacetime_lib

module Position = struct

  type t = Printexc.location

  let filename { Printexc.filename } = filename

  let line_number { Printexc.line_number } = line_number

  let start_char { Printexc.start_char } = start_char

  let end_char { Printexc.end_char } = end_char

  let print ppf t =
    let open Printexc in
    Format.fprintf ppf "%s{%d:%d-%d}"
      t.filename t.line_number t.start_char t.end_char

end

module Location = struct

  type t =
    { address: Int64.t;
      symbol: string option;
      position : Position.t option;
      foreign : bool; }

  let address { address } = address

  let symbol { symbol } = symbol

  let position { position } = position

  let foreign { foreign } = foreign

  let compare t1 t2 =
    Int64.compare t1.address t2.address

  let create_ocaml ?executable ~frame_table pc =
    let address = Program_counter.OCaml.to_int64 pc in
    let foreign = false in
    let symbol =
      match executable with
      | None -> None
      | Some elf_locations ->
        Elf_locations.function_at_pc elf_locations ~program_counter:address
    in
    let position =
      try
        let slot = Frame_table.find_exn pc frame_table in
        Printexc.Slot.location slot
      with Not_found -> None
    in
    { address; symbol; position; foreign; }

  let create_foreign ?executable pc =
    let program_counter = Program_counter.Foreign.to_int64 pc in
    let position =
      match executable with
      | None -> None
      | Some elf_locations ->
        match Elf_locations.resolve elf_locations ~program_counter with
        | None -> None
        | Some (filename, line_number) ->
          let location =
            { Printexc.
              filename;
              line_number;
              start_char = -1;
              end_char = -1;
            }
          in
          Some location
    in
    let symbol =
      match executable with
      | None -> None
      | Some elf_locations ->
        Elf_locations.function_at_pc elf_locations ~program_counter
    in
    { address = program_counter;
      symbol;
      position;
      foreign = true;
    }

  let print ppf t =
    match t.position with
    | Some pos -> Position.print ppf pos
    | None ->
        match t.symbol with
        | Some symbol -> Format.fprintf ppf "%s" symbol
        | None -> Format.fprintf ppf "%Ld" t.address

end

module Backtrace = struct

  type t = Location.t list

  let rec compare b1 b2 =
    match b1, b2 with
    | [], [] -> 0
    | l1 :: b1, l2 :: b2 ->
      let c = Location.compare l1 l2 in
      if c <> 0 then c
      else compare b1 b2
    | _ :: _, [] -> 1
    | [], _ :: _ -> -1

  let rec print ppf = function
    | [] -> ()
    | [loc] -> Location.print ppf loc
    | loc :: res ->
      Format.fprintf ppf "%a %a"
        Location.print loc
        print res

end

module Small_count : sig

  type t

  val max_allocations : int

  val max_words : int

  val max_blocks : int

  val create : allocations:int -> words:int -> blocks:int -> t

  val allocations : t -> int

  val words : t -> int

  val blocks : t -> int

end = struct

  type t = int

  let allocations_size = Sys.int_size / 2
  let words_size = (Sys.int_size - allocations_size) / 2
  let blocks_size = Sys.int_size - allocations_size - words_size

  let max_allocations = ((1 lsl allocations_size) - 1)
  let max_words = ((1 lsl words_size) - 1)
  let max_blocks = ((1 lsl blocks_size) - 1)

  let blocks_shift = 0
  let words_shift = blocks_shift + blocks_size
  let allocations_shift = words_shift + words_size

  let allocations_mask = max_allocations lsl allocations_shift
  let words_mask = max_words lsl words_shift
  let blocks_mask = max_blocks lsl blocks_shift

  let create ~allocations ~words ~blocks =
    allocations lsl allocations_shift
    lor words lsl words_shift
    lor blocks lsl blocks_shift

  let allocations t =
    (t land allocations_mask) lsr allocations_shift

  let words t =
    (t land words_mask) lsr words_shift

  let blocks t =
    (t land blocks_mask) lsr blocks_shift

end

module Annotation_data = struct

  type t =
    | Nothing
    | Alloc of { allocations : int; }
    | Small of { counts : Small_count.t; }
    | Large of { blocks : int;
                 words : int;
                 allocations : int; }

  let create ~blocks ~words =
    if words <= 0 && blocks <= 0 then Nothing
    else if words <= Small_count.max_words
         && blocks <= Small_count.max_blocks then
      Small { counts = Small_count.create ~allocations:0 ~words ~blocks }
    else Large { blocks; words; allocations = 0 }

  let set_allocations t ~allocations =
    if allocations <= 0 then t
    else begin
      match t with
      | Nothing -> Alloc { allocations }
      | Alloc _ -> assert false
      | Small { counts } ->
        let words = Small_count.words counts in
        let blocks = Small_count.blocks counts in
        if allocations <= Small_count.max_allocations then begin
          let counts = Small_count.create ~allocations ~words ~blocks in
          Small { counts }
        end else begin
          Large { words; blocks; allocations }
        end
      | Large { words; blocks; allocations = _ } ->
        Large { words; blocks; allocations }
    end

end


module Entry = struct

  type t =
    | Alloc of { backtrace : Backtrace.t;
                 allocations : int; }
    | Small of { backtrace : Backtrace.t;
                 counts : Small_count.t; }
    | Large of { backtrace : Backtrace.t;
                 blocks : int;
                 words : int;
                 allocations : int; }

  let create ~backtrace ~data =
    match data with
    | Annotation_data.Nothing -> assert false
    | Annotation_data.Alloc { allocations } ->
      Alloc { backtrace; allocations }
    | Annotation_data.Small { counts } ->
      Small { backtrace; counts }
    | Annotation_data.Large { blocks; words; allocations } ->
      Large { backtrace; blocks; words; allocations }

  let backtrace = function
    | Alloc { backtrace } -> backtrace
    | Small { backtrace } -> backtrace
    | Large { backtrace } -> backtrace

  let blocks = function
    | Alloc _ -> 0
    | Small { counts } -> Small_count.blocks counts
    | Large { blocks } -> blocks

  let words = function
    | Alloc _ -> 0
    | Small { counts } -> Small_count.words counts
    | Large { words } -> words

  let allocations = function
    | Alloc { allocations } -> allocations
    | Small { counts } -> Small_count.allocations counts
    | Large { allocations } -> allocations

end

module Stats = struct

  type t =
    { gc: Gc_stats.t;
      words_scanned : int;
      words_scanned_with_profinfo : int; }

  let minor_words t = Gc_stats.minor_words t.gc
  let promoted_words t = Gc_stats.promoted_words t.gc
  let major_words t = Gc_stats.major_words t.gc
  let minor_collections t = Gc_stats.minor_collections t.gc
  let major_collections t = Gc_stats.major_collections t.gc
  let heap_words t = Gc_stats.heap_words t.gc
  let heap_chunks t = Gc_stats.heap_chunks t.gc
  let compactions t = Gc_stats.compactions t.gc
  let top_heap_words t = Gc_stats.top_heap_words t.gc

  let words_scanned { words_scanned } = words_scanned

  let words_scanned_with_profinfo { words_scanned_with_profinfo } =
    words_scanned_with_profinfo

end

module Snapshot = struct

  type t =
    { time : float;
      stats : Stats.t;
      entries : Entry.t list;
      raw : Heap_snapshot.t;
    }

  let time { time } = time

  let stats { stats } = stats

  let entries { entries } = entries

  let create ~snapshot ~entries =
    let time = Heap_snapshot.timestamp snapshot in
    let gc = Heap_snapshot.gc_stats snapshot in
    let words_scanned = Heap_snapshot.words_scanned snapshot in
    let words_scanned_with_profinfo =
      Heap_snapshot.words_scanned_with_profinfo snapshot
    in
    let stats =
      { Stats.gc; words_scanned; words_scanned_with_profinfo; }
    in
    { time; stats; entries; raw = snapshot; }

  let raw t = t.raw
end

module Series = struct

  type t = Snapshot.t list

  let rec fold_ocaml_indirect_calls ?executable ~frame_table ~shape_table
            visited f backtrace acc callee =
    let node = Trace.OCaml.Indirect_call_point.Callee.callee_node callee in
    let acc =
      fold_node ?executable ~frame_table ~shape_table visited f backtrace
        acc node
    in
    let next = Trace.OCaml.Indirect_call_point.Callee.next callee in
    match next with
    | None -> acc
    | Some callee ->
      fold_ocaml_indirect_calls ?executable ~frame_table ~shape_table
        visited f backtrace acc callee

  and fold_ocaml_fields ?executable ~frame_table ~shape_table
        visited f backtrace acc field =
    let acc =
      match Trace.OCaml.Field.classify field with
      | Trace.OCaml.Field.Allocation alloc ->
        let pc = Trace.OCaml.Allocation_point.program_counter alloc in
        let loc = Location.create_ocaml ?executable ~frame_table pc in
        let annot = Trace.OCaml.Allocation_point.annotation alloc in
        f (loc :: backtrace) annot acc
      | Trace.OCaml.Field.Direct_call (Trace.OCaml.Field.To_ocaml call) ->
        let site = Trace.OCaml.Direct_call_point.call_site call in
        let loc = Location.create_ocaml ?executable ~frame_table site in
        let node = Trace.OCaml.Direct_call_point.callee_node call in
        fold_ocaml_node ?executable ~frame_table ~shape_table
          visited f (loc :: backtrace) acc node
      | Trace.OCaml.Field.Direct_call (Trace.OCaml.Field.To_foreign call) ->
        let site = Trace.OCaml.Direct_call_point.call_site call in
        let loc = Location.create_ocaml ?executable ~frame_table site in
        let node = Trace.OCaml.Direct_call_point.callee_node call in
        fold_foreign_node ?executable ~frame_table ~shape_table
          visited f (loc :: backtrace) acc node
      | Trace.OCaml.Field.Direct_call
          (Trace.OCaml.Field.To_uninstrumented _) ->
        acc
      | Trace.OCaml.Field.Indirect_call call ->
        let site = Trace.OCaml.Indirect_call_point.call_site call in
        let loc = Location.create_ocaml ?executable ~frame_table site in
        let callee = Trace.OCaml.Indirect_call_point.callees call in
        match callee with
        | None -> acc
        | Some callee ->
          fold_ocaml_indirect_calls ?executable ~frame_table ~shape_table
            visited f (loc :: backtrace) acc callee
    in
    match Trace.OCaml.Field.next field with
    | None -> acc
    | Some field ->
      fold_ocaml_fields ?executable ~frame_table ~shape_table
        visited f backtrace acc field

  and fold_ocaml_node ?executable ~frame_table ~shape_table visited f
        backtrace acc node =
    if Trace.Node.Set.mem (Trace.Node.of_ocaml_node node) !visited then acc
    else begin
      visited := Trace.Node.Set.add (Trace.Node.of_ocaml_node node) !visited;
      let acc =
         fold_ocaml_node
           ?executable ~frame_table ~shape_table visited f backtrace acc
           (Trace.OCaml.Node.next_in_tail_call_chain node)
      in
      let acc =
        match Trace.OCaml.Node.fields node ~shape_table with
        | None -> acc
        | Some fields ->
          fold_ocaml_fields
            ?executable ~frame_table ~shape_table visited f backtrace acc fields
      in
      acc
    end

  and fold_foreign_fields ?executable ~frame_table ~shape_table
        visited f backtrace acc field =
    let acc =
      match Trace.Foreign.Field.classify field with
      | Trace.Foreign.Field.Allocation alloc ->
        let pc = Trace.Foreign.Allocation_point.program_counter alloc in
        let loc = Location.create_foreign ?executable pc in
        let annot = Trace.Foreign.Allocation_point.annotation alloc in
        f (loc :: backtrace) annot acc
      | Trace.Foreign.Field.Call call ->
        let site = Trace.Foreign.Call_point.call_site call in
        let loc = Location.create_foreign ?executable site in
        let node = Trace.Foreign.Call_point.callee_node call in
        fold_node ?executable ~frame_table ~shape_table
          visited f (loc :: backtrace) acc node
    in
    match Trace.Foreign.Field.next field with
    | None -> acc
    | Some field ->
      fold_foreign_fields ?executable ~frame_table ~shape_table
        visited f backtrace acc field

  and fold_foreign_node ?executable ~frame_table ~shape_table visited f
        backtrace acc node =
    match Trace.Foreign.Node.fields node with
    | None -> acc
    | Some field ->
      fold_foreign_fields ?executable ~frame_table ~shape_table
        visited f backtrace acc field

  and fold_node ?executable ~frame_table ~shape_table visited f backtrace
        acc node =
    match Trace.Node.classify node with
    | Trace.Node.OCaml node ->
      fold_ocaml_node ?executable ~frame_table ~shape_table visited f
        backtrace acc node
    | Trace.Node.Foreign node ->
      fold_foreign_node ?executable ~frame_table ~shape_table visited f
        backtrace acc node

  let fold_trace ?executable ~frame_table ~shape_table f acc trace =
    match Trace.root trace with
    | None -> acc
    | Some node ->
      let visited = ref Trace.Node.Set.empty in
      fold_node ?executable ~frame_table ~shape_table visited f [] acc node

  let fold_traces ?executable ~series ~frame_table ~shape_table f acc =
    let num_threads = Heap_snapshot.Series.num_threads series in
    let rec loop acc n =
      if n >= num_threads then acc
      else begin
        let normal =
          Heap_snapshot.Series.trace series Heap_snapshot.Series.Normal n
        in
        let acc =
          match normal with
          | None -> acc
          | Some trace ->
            fold_trace ?executable ~frame_table ~shape_table f acc trace
        in
        let finaliser =
          Heap_snapshot.Series.trace series Heap_snapshot.Series.Finaliser n
        in
        let acc =
          match finaliser with
          | None -> acc
          | Some trace ->
            fold_trace ?executable ~frame_table ~shape_table f acc trace
        in
        loop acc (n + 1)
      end
    in
    loop acc 0

  let data_table ~snapshots =
    let num_snapshots = List.length snapshots in
    let tbl = Hashtbl.create 42 in
    List.iteri
      (fun idx snapshot ->
         let entries = Heap_snapshot.entries snapshot in
         let length = Heap_snapshot.Entries.length entries in
         for entry = 0 to length - 1 do
           let blocks = Heap_snapshot.Entries.num_blocks entries entry in
           let words =
             Heap_snapshot.Entries.num_words_including_headers entries entry
           in
           if blocks > 0 || words > 0 then begin
             let annotation =
               Heap_snapshot.Entries.annotation entries entry
             in
             let array =
               match Hashtbl.find tbl annotation with
               | array -> array
               | exception Not_found ->
                 let array =
                   Array.make num_snapshots Annotation_data.Nothing
                 in
                 Hashtbl.add tbl annotation array;
                 array
             in
             let data = Annotation_data.create ~words ~blocks in
             array.(idx) <- data
           end
         done)
      snapshots;
    List.iteri
      (fun idx snapshot ->
         let rec loop next =
           match next with
           | None -> ()
           | Some allocation ->
             let allocations =
               Heap_snapshot.Total_allocation.num_words_including_headers
                 allocation
             in
             if allocations > 0 then begin
               let annotation =
                 Heap_snapshot.Total_allocation.annotation allocation
               in
               let array =
                 match Hashtbl.find tbl annotation with
                 | array -> array
                 | exception Not_found ->
                   let array =
                     Array.make num_snapshots Annotation_data.Nothing
                   in
                   Hashtbl.add tbl annotation array;
                   array
               in
               let data =
                 Annotation_data.set_allocations array.(idx) ~allocations
               in
               array.(idx) <- data
             end;
             let next = Heap_snapshot.Total_allocation.next allocation in
             loop next
         in
         loop (Heap_snapshot.total_allocations snapshot))
      snapshots;
    tbl

  let create ?executable path =
    let series = Heap_snapshot.Series.read ~path in
    let executable =
      match executable with
      | None -> None
      | Some executable ->
        Some (Elf_locations.create ~elf_executable:executable)
    in
    let frame_table = Heap_snapshot.Series.frame_table series in
    let shape_table = Heap_snapshot.Series.shape_table series in
    let length = Heap_snapshot.Series.num_snapshots series in
    let snapshots =
      let rec loop acc n =
        if n < 0 then List.rev acc
        else begin
          let snapshot = Heap_snapshot.Series.snapshot series ~index:n in
          loop (snapshot :: acc) (n - 1)
        end
      in
      loop [] (length - 1)
    in
    let data_table = data_table ~snapshots in
    let init =
      List.mapi
        (fun idx snapshot -> idx, snapshot, [])
        snapshots
    in
    let accumulate backtrace annot accs =
      List.map
        (fun ((idx, snapshot, entries) as acc) ->
           match Hashtbl.find data_table annot with
           | exception Not_found -> acc
           | array ->
             match array.(idx) with
             | Annotation_data.Nothing -> acc
             | data ->
               let entry = Entry.create ~backtrace ~data in
               let entries = entry :: entries in
               idx, snapshot, entries)
        accs
    in
    let snapshots =
      fold_traces ?executable ~series ~frame_table ~shape_table accumulate init
    in
    let snapshots =
      List.map
        (fun (_, snapshot, entries) -> Snapshot.create ~snapshot ~entries)
        snapshots
    in
    snapshots

end
