(* Executes a script of sequence operations and prints a canonical trace.
   A Racket driver executes the same script; the two traces must agree. *)

(* Capacities come from the environment so that the same script can be run at
   several tree shapes, matching what the Racket side is configured with. *)
let getenv_int name default =
  match Sys.getenv_opt name with Some s -> int_of_string s | None -> default

module Settings = struct
  let k0 = getenv_int "SEK_LEAF" 128
  let k1 = getenv_int "SEK_NODE" 16
  let[@inline] capacity depth = if depth = 0 then k0 else k1
  let overwrite_empty_slots = getenv_int "SEK_OVERWRITE" 1 <> 0
  let threshold = getenv_int "SEK_THRESHOLD" 32
  let check_iterator_validity = getenv_int "SEK_CHECKITER" 1 <> 0
end

module S = Sek.Make (Settings)
module E = S.Ephemeral
module P = S.Persistent

let nslots = 6
let default = 0

let es : int E.t array = Array.init nslots (fun _ -> E.create default)
let ps : int P.t array = Array.init nslots (fun _ -> P.create default)

let buf = Buffer.create (1 lsl 16)
let out fmt = Printf.ksprintf (fun s -> Buffer.add_string buf s; Buffer.add_char buf '\n') fmt

let show_list l = String.concat "," (List.map string_of_int l)

let side = function 0 -> S.front | _ -> S.back
let dir = function 0 -> S.forward | _ -> S.backward

let protect f =
  match f () with
  | v -> v
  | exception S.Empty -> "empty"
  | exception Not_found -> "notfound"
  | exception Invalid_argument _ -> "invalid"

(* the state of every slot, so that divergence shows up at the step that
   causes it rather than many steps later *)
let dump_state () =
  for i = 0 to nslots - 1 do
    out "  e%d=[%s]" i (show_list (E.to_list es.(i)))
  done;
  for i = 0 to nslots - 1 do
    out "  p%d=[%s]" i (show_list (P.to_list ps.(i)))
  done

let iter_to_list d i =
  let it = E.Iter.create (dir d) es.(i) in
  let acc = ref [] in
  while not (E.Iter.finished it) do
    acc := E.Iter.get_and_move (dir d) it :: !acc
  done;
  List.rev !acc

let piter_to_list d i =
  let it = P.Iter.create (dir d) ps.(i) in
  let acc = ref [] in
  while not (P.Iter.finished it) do
    acc := P.Iter.get_and_move (dir d) it :: !acc
  done;
  List.rev !acc

let run line =
  match String.split_on_char ' ' (String.trim line) with
  | [] | [""] -> ()
  | cmd :: args ->
    let a n = int_of_string (List.nth args n) in
    out "%s" line;
    (match cmd with
     (* ---- ephemeral core *)
     | "ecreate" -> es.(a 0) <- E.create default
     | "eclear" -> E.clear es.(a 0)
     | "epush" -> E.push (side (a 1)) es.(a 0) (a 2)
     | "epop" -> out "-> %s" (protect (fun () -> string_of_int (E.pop (side (a 1)) es.(a 0))))
     | "epeek" -> out "-> %s" (protect (fun () -> string_of_int (E.peek (side (a 1)) es.(a 0))))
     | "eget" -> out "-> %s" (protect (fun () -> string_of_int (E.get es.(a 0) (a 1))))
     | "eset" -> out "-> %s" (protect (fun () -> E.set es.(a 0) (a 1) (a 2); "ok"))
     | "elen" -> out "-> %d" (E.length es.(a 0))
     | "eempty" -> out "-> %b" (E.is_empty es.(a 0))
     (* ---- ephemeral structure *)
     | "eassign" -> E.assign es.(a 0) es.(a 1)
     | "ecopy" -> es.(a 0) <- E.copy es.(a 1)
     | "ecopyshare" -> es.(a 0) <- E.copy ~mode:`Share es.(a 1)
     | "eappend" -> E.append (side (a 2)) es.(a 0) es.(a 1)
     | "econcat" -> es.(a 0) <- E.concat es.(a 1) es.(a 2)
     | "esplit" ->
       let s1, s2 = E.split es.(a 2) (a 3) in
       es.(a 0) <- s1; es.(a 1) <- s2
     | "ecarve" -> es.(a 0) <- E.carve (side (a 2)) es.(a 1) (a 3)
     | "etake" -> E.take (side (a 1)) es.(a 0) (a 2)
     | "edrop" -> E.drop (side (a 1)) es.(a 0) (a 2)
     | "esub" -> es.(a 0) <- E.sub es.(a 1) (a 2) (a 3)
     | "efill" -> E.fill es.(a 0) (a 1) (a 2) (a 3)
     | "eblit" -> E.blit es.(a 1) (a 2) es.(a 0) (a 3) (a 4)
     (* ---- conversions *)
     | "esnap" -> ps.(a 0) <- S.snapshot es.(a 1)
     | "esnapclear" -> ps.(a 0) <- S.snapshot_and_clear es.(a 1)
     | "eedit" -> es.(a 0) <- S.edit ps.(a 1)
     (* ---- ephemeral derived *)
     | "edump" -> out "-> [%s]" (show_list (E.to_list es.(a 0)))
     | "edumpiter" -> out "-> [%s]" (show_list (iter_to_list (a 1) (a 0)))
     | "efold" ->
       out "-> %d" (E.fold_left (fun acc x -> (acc * 3) + x) 7 es.(a 0));
       out "-> %d" (E.fold_right (fun x acc -> (acc * 3) + x) es.(a 0) 7)
     | "emap" -> es.(a 0) <- E.map default (fun x -> (x * 2) + 1) es.(a 1)
     | "emapi" -> es.(a 0) <- E.mapi default (fun i x -> (i * 100) + x) es.(a 1)
     | "efilter" -> es.(a 0) <- E.filter (fun x -> x mod 3 <> 0) es.(a 1)
     | "efiltermap" ->
       es.(a 0) <- E.filter_map default (fun x -> if x mod 2 = 0 then Some (x / 2) else None) es.(a 1)
     | "erev" -> es.(a 0) <- E.rev es.(a 1)
     | "esort" -> E.sort compare es.(a 0)
     | "euniq" -> es.(a 0) <- E.uniq compare es.(a 1)
     | "emerge" ->
       E.sort compare es.(a 1); E.sort compare es.(a 2);
       es.(a 0) <- E.merge compare es.(a 1) es.(a 2)
     | "epart" ->
       let s1, s2 = E.partition (fun x -> x mod 2 = 0) es.(a 2) in
       es.(a 0) <- s1; es.(a 1) <- s2
     | "ezip" ->
       let n = min (E.length es.(a 1)) (E.length es.(a 2)) in
       let z = E.zip (E.sub es.(a 1) 0 n) (E.sub es.(a 2) 0 n) in
       out "-> [%s]" (String.concat "," (List.map (fun (x, y) -> Printf.sprintf "%d/%d" x y) (E.to_list z)))
     | "efind" ->
       out "-> %s" (protect (fun () -> string_of_int (E.find (dir (a 1)) (fun x -> x > (a 2)) es.(a 0))))
     | "eforall" -> out "-> %b" (E.for_all (fun x -> x < (a 1)) es.(a 0))
     | "eexists" -> out "-> %b" (E.exists (fun x -> x > (a 1)) es.(a 0))
     | "emem" -> out "-> %b" (E.mem (a 1) es.(a 0))
     | "eequal" -> out "-> %b" (E.equal ( = ) es.(a 0) es.(a 1))
     | "ecompare" -> out "-> %d" (compare (E.compare compare es.(a 0) es.(a 1)) 0)
     (* ---- persistent core *)
     | "pcreate" -> ps.(a 0) <- P.create default
     | "ppush" -> ps.(a 0) <- P.push (side (a 1)) ps.(a 0) (a 2)
     | "ppop" ->
       out "-> %s"
         (protect (fun () ->
              let x, s = P.pop (side (a 1)) ps.(a 0) in
              ps.(a 0) <- s; string_of_int x))
     | "ppeek" -> out "-> %s" (protect (fun () -> string_of_int (P.peek (side (a 1)) ps.(a 0))))
     | "pget" -> out "-> %s" (protect (fun () -> string_of_int (P.get ps.(a 0) (a 1))))
     | "pset" ->
       out "-> %s" (protect (fun () -> ps.(a 0) <- P.set ps.(a 0) (a 1) (a 2); "ok"))
     | "plen" -> out "-> %d" (P.length ps.(a 0))
     | "pconcat" -> ps.(a 0) <- P.concat ps.(a 1) ps.(a 2)
     | "psplit" ->
       let s1, s2 = P.split ps.(a 2) (a 3) in
       ps.(a 0) <- s1; ps.(a 1) <- s2
     | "ptake" -> ps.(a 0) <- P.take (side (a 2)) ps.(a 1) (a 3)
     | "pdrop" -> ps.(a 0) <- P.drop (side (a 2)) ps.(a 1) (a 3)
     | "psub" -> ps.(a 0) <- P.sub ps.(a 1) (a 2) (a 3)
     | "pdump" -> out "-> [%s]" (show_list (P.to_list ps.(a 0)))
     | "pdumpiter" -> out "-> [%s]" (show_list (piter_to_list (a 1) (a 0)))
     | "pmap" -> ps.(a 0) <- P.map default (fun x -> (x * 2) + 1) ps.(a 1)
     | "pfilter" -> ps.(a 0) <- P.filter (fun x -> x mod 3 <> 0) ps.(a 1)
     | "prev" -> ps.(a 0) <- P.rev ps.(a 1)
     | "psort" -> ps.(a 0) <- P.sort compare ps.(a 1)
     | "pfold" ->
       out "-> %d" (P.fold_left (fun acc x -> (acc * 3) + x) 7 ps.(a 0));
       out "-> %d" (P.fold_right (fun x acc -> (acc * 3) + x) ps.(a 0) 7)
     | "pequal" -> out "-> %b" (P.equal ( = ) ps.(a 0) ps.(a 1))
     (* ---- iterator walk with jumps and reaches *)
     | "piterwalk" ->
       let it = P.Iter.create S.forward ps.(a 0) in
       let n = P.length ps.(a 0) in
       let acc = ref [] in
       List.iteri
         (fun k target ->
            let target = if n = 0 then -1 else ((target mod (n + 2)) - 1) in
            P.Iter.reach it target;
            acc := (k, P.Iter.index it,
                    (if P.Iter.finished it then "-" else string_of_int (P.Iter.get it)))
                   :: !acc)
         (List.map (fun i -> (a 1) + (i * 7)) [0; 1; 2; 3; 4; 5; 6; 7]);
       out "-> %s"
         (String.concat ";"
            (List.rev_map (fun (k, i, v) -> Printf.sprintf "%d:%d:%s" k i v) !acc))
     | "eitersweep" ->
       (* rewrite every element through a writable iterator *)
       let it = E.Iter.create S.forward es.(a 0) in
       while not (E.Iter.finished it) do
         E.Iter.set it (E.Iter.get it + (a 1));
         E.Iter.move S.forward it
       done
     | "echeck" -> E.check es.(a 0)
     | "pcheck" -> P.check ps.(a 0)
     | _ -> out "-> UNKNOWN"
     );
    dump_state ()

let () =
  (try
     while true do
       run (input_line stdin)
     done
   with End_of_file -> ());
  print_string (Buffer.contents buf)
