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
      position : Position.t list;
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
        let slots = Frame_table.find_exn pc frame_table in
        List.fold_right (fun slot acc ->
            match Printexc.Slot.location slot with
            | Some location -> location::acc
            | None -> acc)
          slots
          []
      with Not_found -> []
    in
    { address; symbol; position; foreign; }

  let create_foreign ?executable pc =
    let program_counter = Program_counter.Foreign.to_int64 pc in
    let position =
      match executable with
      | None -> []
      | Some elf_locations ->
        match Elf_locations.resolve elf_locations ~program_counter with
        | None -> []
        | Some (filename, line_number) ->
          let location =
            { Printexc.
              filename;
              line_number;
              start_char = -1;
              end_char = -1;
            }
          in
          [location]
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
    | [] ->
      begin match t.symbol with
      | Some symbol -> Format.fprintf ppf "%s" symbol
      | None -> Format.fprintf ppf "%Ld" t.address
      end
    | locations ->
      Format.fprintf ppf "%a" (Format.pp_print_list Position.print) locations

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
    { time; stats; entries; }

end

module Series = struct

  type t = Snapshot.t list

  let iter_opt f = function
    | None -> ()
    | Some x -> f x

  let rec iter_ocaml_indirect_calls ?executable ~frame_table ~shape_table
            visited f backtrace callee =
    let node = Trace.OCaml.Indirect_call_point.Callee.callee_node callee in
    iter_node ?executable ~frame_table ~shape_table visited f backtrace node;
    let next = Trace.OCaml.Indirect_call_point.Callee.next callee in
    iter_opt
      (iter_ocaml_indirect_calls ?executable ~frame_table ~shape_table
         visited f backtrace)
      next

  and iter_ocaml_field_classification ?executable ~frame_table ~shape_table
        visited f backtrace classification =
    match classification with
    | Trace.OCaml.Field.Allocation alloc ->
      let pc = Trace.OCaml.Allocation_point.program_counter alloc in
      let loc = Location.create_ocaml ?executable ~frame_table pc in
      let annot = Trace.OCaml.Allocation_point.annotation alloc in
      f (loc :: backtrace) annot
    | Trace.OCaml.Field.Direct_call (Trace.OCaml.Field.To_ocaml call) ->
      let site = Trace.OCaml.Direct_call_point.call_site call in
      let loc = Location.create_ocaml ?executable ~frame_table site in
      let node = Trace.OCaml.Direct_call_point.callee_node call in
      iter_ocaml_node ?executable ~frame_table ~shape_table
        visited f (loc :: backtrace) node
    | Trace.OCaml.Field.Direct_call (Trace.OCaml.Field.To_foreign call) ->
      let site = Trace.OCaml.Direct_call_point.call_site call in
      let loc = Location.create_ocaml ?executable ~frame_table site in
      let node = Trace.OCaml.Direct_call_point.callee_node call in
      iter_foreign_node ?executable ~frame_table ~shape_table
        visited f (loc :: backtrace) node
    | Trace.OCaml.Field.Direct_call
        (Trace.OCaml.Field.To_uninstrumented _) -> ()
    | Trace.OCaml.Field.Indirect_call call ->
      let site = Trace.OCaml.Indirect_call_point.call_site call in
      let loc = Location.create_ocaml ?executable ~frame_table site in
      let callee = Trace.OCaml.Indirect_call_point.callees call in
      iter_opt
        (iter_ocaml_indirect_calls ?executable ~frame_table ~shape_table
           visited f (loc :: backtrace))
        callee

  and iter_ocaml_fields ?executable ~frame_table ~shape_table
        visited f backtrace field =
    iter_ocaml_field_classification ?executable ~frame_table ~shape_table
        visited f backtrace (Trace.OCaml.Field.classify field);
    iter_opt
      (iter_ocaml_fields ?executable ~frame_table ~shape_table
        visited f backtrace)
      (Trace.OCaml.Field.next field)

  and iter_ocaml_node ?executable ~frame_table ~shape_table visited f
        backtrace node =
    if Trace.Node.Set.mem (Trace.Node.of_ocaml_node node) !visited then ()
    else begin
      visited := Trace.Node.Set.add (Trace.Node.of_ocaml_node node) !visited;
      iter_ocaml_node
        ?executable ~frame_table ~shape_table visited f backtrace
        (Trace.OCaml.Node.next_in_tail_call_chain node);
      iter_opt
        (iter_ocaml_fields
          ?executable ~frame_table ~shape_table visited f backtrace)
        (Trace.OCaml.Node.fields node ~shape_table)
    end

  and iter_foreign_field_classification ?executable ~frame_table ~shape_table
        visited f backtrace classification =
    match classification with
    | Trace.Foreign.Field.Allocation alloc ->
      let pc = Trace.Foreign.Allocation_point.program_counter alloc in
      let loc = Location.create_foreign ?executable pc in
      let annot = Trace.Foreign.Allocation_point.annotation alloc in
      f (loc :: backtrace) annot
    | Trace.Foreign.Field.Call call ->
      let site = Trace.Foreign.Call_point.call_site call in
      let loc = Location.create_foreign ?executable site in
      let node = Trace.Foreign.Call_point.callee_node call in
      iter_node ?executable ~frame_table ~shape_table
        visited f (loc :: backtrace) node

  and iter_foreign_fields ?executable ~frame_table ~shape_table
        visited f backtrace field =
    iter_foreign_field_classification ?executable ~frame_table ~shape_table
        visited f backtrace (Trace.Foreign.Field.classify field);
    iter_opt
      (iter_foreign_fields ?executable ~frame_table ~shape_table
         visited f backtrace)
      (Trace.Foreign.Field.next field)

  and iter_foreign_node ?executable ~frame_table ~shape_table visited f
        backtrace node =
    iter_opt
      (iter_foreign_fields ?executable ~frame_table ~shape_table
         visited f backtrace)
      (Trace.Foreign.Node.fields node)

  and iter_node ?executable ~frame_table ~shape_table visited f backtrace
        node =
    match Trace.Node.classify node with
    | Trace.Node.OCaml node ->
      iter_ocaml_node ?executable ~frame_table ~shape_table visited f
        backtrace node
    | Trace.Node.Foreign node ->
      iter_foreign_node ?executable ~frame_table ~shape_table visited f
        backtrace node

  let iter_trace ?executable ~frame_table ~shape_table f trace =
    match Trace.root trace with
    | None -> ()
    | Some node ->
      let visited = ref Trace.Node.Set.empty in
      iter_node ?executable ~frame_table ~shape_table visited f [] node

  let iter_traces ?executable ~series ~frame_table ~shape_table f =
    let num_threads = Heap_snapshot.Series.num_threads series in
    let rec loop n =
      if n >= num_threads then ()
      else begin
        let normal =
          Heap_snapshot.Series.trace series Heap_snapshot.Series.Normal n
        in
        iter_opt
          (iter_trace ?executable ~frame_table ~shape_table f)
          normal;
        let finaliser =
          Heap_snapshot.Series.trace series Heap_snapshot.Series.Finaliser n
        in
        iter_opt
          (iter_trace ?executable ~frame_table ~shape_table f)
          finaliser;
        loop (n + 1)
      end
    in
    loop 0

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
    let num_snapshots = List.length snapshots in
    let entries = Array.make num_snapshots [] in
    let accumulate backtrace annot =
      match Hashtbl.find data_table annot with
      | exception Not_found -> ()
      | data ->
        for i = 0 to num_snapshots - 1 do
          match data.(i) with
          | Annotation_data.Nothing -> ()
          | data ->
              let entry = Entry.create ~backtrace ~data in
              entries.(i) <- entry :: entries.(i)
        done
    in
    iter_traces ?executable ~series ~frame_table ~shape_table accumulate;
    let snapshots =
      List.map2
        (fun snapshot entries -> Snapshot.create ~snapshot ~entries)
        snapshots (Array.to_list entries)
    in
    snapshots

end
