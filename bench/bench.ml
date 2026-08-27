(* The same benchmark scenarios as bench/main.rkt, run against the authors'
   OCaml library.  Printing the two side by side separates the cost of the
   data structure from the cost of the language it is written in.

   Usage:  ./bench-ocaml [--quick] [scenario ...]   (default: all)  *)

module E = Sek.Ephemeral
module P = Sek.Persistent

let default = 0

(* ------------------------------------------------------------------ harness *)

let quick = ref false
let scale n = if !quick then max 1 (n / 8) else n

let now () = Unix.gettimeofday () *. 1000.0

(* Repeat thunk until at least 150ms have elapsed, then report the best of
   three trials, in nanoseconds per operation. *)
let measure ops thunk =
  let once reps =
    Gc.compact ();
    let start = now () in
    for _ = 1 to reps do thunk () done;
    now () -. start
  in
  let rec calibrate reps =
    let elapsed = once reps in
    if elapsed < 150.0 && reps < 10_000_000 then
      calibrate (reps * max 2 (int_of_float (ceil (150.0 /. Float.max elapsed 0.05))))
    else reps
  in
  let reps = calibrate 1 in
  let best = ref infinity in
  for _ = 1 to 3 do best := Float.min !best (once reps) done;
  !best *. 1e6 /. float_of_int (reps * ops)

let fmt = function
  | None -> "-"
  | Some x ->
    if x >= 1000.0 then Printf.sprintf "%.0f" x
    else if x >= 100.0 then Printf.sprintf "%.1f" x
    else Printf.sprintf "%.2f" x

let pad s w = s ^ String.make (max 1 (w - String.length s)) ' '
let rpad s w = String.make (max 1 (w - String.length s)) ' ' ^ s

let table title cols rows =
  Printf.printf "\n%s\n" title;
  print_string (pad "" 24);
  List.iter (fun c -> print_string (rpad c 12)) cols;
  print_newline ();
  List.iter
    (fun (name, vals) ->
       print_string (pad name 24);
       List.iter (fun v -> print_string (rpad (fmt v) 12)) vals;
       print_newline ())
    rows;
  flush stdout

(* A row of the table: apply f to each column parameter. *)
let row name f xs = (name, List.map (fun x -> Some (f x)) xs)
let row_opt name f xs = (name, List.map f xs)

let sizes_s () = if !quick then [ 10; 1000; 100000 ] else [ 10; 1000; 100000; 1000000 ]
let sizes_m () = if !quick then [ 100; 10000; 100000 ] else [ 100; 10000; 1000000 ]
let big_n () = if !quick then 200000 else 1000000
let show_ints l = List.map string_of_int l

let e_of n = E.init default n (fun i -> i)
let p_of n = P.init default n (fun i -> i)
let a_of n = Array.init n (fun i -> i)
let l_of n = List.init n (fun i -> i)

(* --------------------------------------------------------------- scenarios *)

(* Figures 17 and 18: repeat "n pushes then n pops" until [total] pushes. *)
let stack_like title side_push side_pop total =
  let e_run n =
    let rounds = max 1 (total / n) in
    measure (2 * rounds * n) (fun () ->
        let s = E.create default in
        for _ = 1 to rounds do
          for k = 0 to n - 1 do E.push side_push s k done;
          for _ = 1 to n do ignore (E.pop side_pop s) done
        done)
  in
  let p_run n =
    let rounds = max 1 (total / n) in
    measure (2 * rounds * n) (fun () ->
        let s = ref (P.create default) in
        for _ = 1 to rounds do
          for k = 0 to n - 1 do s := P.push side_push !s k done;
          for _ = 1 to n do
            let _, t = P.pop side_pop !s in
            s := t
          done
        done)
  in
  let l_run n =
    if side_push != Sek.front || side_pop != Sek.front then None
    else
      let rounds = max 1 (total / n) in
      Some (measure (2 * rounds * n) (fun () ->
          let s = ref [] in
          for _ = 1 to rounds do
            for k = 0 to n - 1 do s := k :: !s done;
            for _ = 1 to n do
              match !s with [] -> assert false | _ :: t -> s := t
            done
          done))
  in
  let sizes = sizes_s () in
  table title (show_ints sizes)
    [ row "eseq" e_run sizes; row "pseq" p_run sizes; row_opt "list" l_run sizes ]

let scenario_stack () =
  stack_like "stack: push-back / pop-back, ns per operation"
    Sek.back Sek.back (scale 2000000)

let scenario_front_stack () =
  stack_like "front stack: push-front / pop-front, ns per operation"
    Sek.front Sek.front (scale 2000000)

let scenario_queue () =
  stack_like "queue: push-back / pop-front, ns per operation"
    Sek.back Sek.front (scale 2000000)

let scenario_traversal () =
  let sizes = sizes_m () in
  let e_run n = let s = e_of n in measure n (fun () -> ignore (E.fold_left ( + ) 0 s)) in
  let p_run n = let s = p_of n in measure n (fun () -> ignore (P.fold_left ( + ) 0 s)) in
  let it_run n =
    let s = p_of n in
    measure n (fun () ->
        let it = P.Iter.create Sek.forward s in
        let acc = ref 0 in
        while not (P.Iter.finished it) do
          acc := !acc + P.Iter.get_and_move Sek.forward it
        done;
        ignore !acc)
  in
  let a_run n = let a = a_of n in measure n (fun () -> ignore (Array.fold_left ( + ) 0 a)) in
  let l_run n = let l = l_of n in measure n (fun () -> ignore (List.fold_left ( + ) 0 l)) in
  table "traversal: fold over the whole sequence, ns per element" (show_ints sizes)
    [ row "eseq" e_run sizes; row "pseq" p_run sizes;
      row "  pseq via iter" it_run sizes;
      row "  array" a_run sizes; row "  list" l_run sizes ]

let random_indices n k = Array.init k (fun _ -> Random.int n)

let scenario_random_access () =
  let sizes = sizes_m () in
  let k = scale 100000 in
  let e_run n =
    let s = e_of n and ix = random_indices n k in
    measure k (fun () ->
        let acc = ref 0 in
        Array.iter (fun j -> acc := !acc + E.get s j) ix;
        ignore !acc)
  in
  let p_run n =
    let s = p_of n and ix = random_indices n k in
    measure k (fun () ->
        let acc = ref 0 in
        Array.iter (fun j -> acc := !acc + P.get s j) ix;
        ignore !acc)
  in
  let a_run n =
    let a = a_of n and ix = random_indices n k in
    measure k (fun () ->
        let acc = ref 0 in
        Array.iter (fun j -> acc := !acc + a.(j)) ix;
        ignore !acc)
  in
  table "random access: get at random indices, ns per read" (show_ints sizes)
    [ row "eseq" e_run sizes; row "pseq" p_run sizes; row "  array" a_run sizes ]

let scenario_hops () =
  let n = big_n () in
  let k = scale 100000 in
  let strides = [ 1; 8; 64; 4096 ] in
  let dests stride =
    let pos = ref (Random.int n) in
    Array.init k (fun _ ->
        pos := (!pos + stride) mod n;
        !pos)
  in
  let ixs = List.map dests strides in
  let e_run ix =
    let s = e_of n in
    measure k (fun () ->
        let acc = ref 0 in
        Array.iter (fun j -> acc := !acc + E.get s j) ix;
        ignore !acc)
  in
  let p_run ix =
    let s = p_of n in
    measure k (fun () ->
        let acc = ref 0 in
        Array.iter (fun j -> acc := !acc + P.get s j) ix;
        ignore !acc)
  in
  let it_run ix =
    let s = p_of n in
    let it = P.Iter.create Sek.forward s in
    measure k (fun () ->
        let acc = ref 0 in
        Array.iter (fun j ->
            P.Iter.reach it j;
            acc := !acc + P.Iter.get it) ix;
        ignore !acc)
  in
  let a_run ix =
    let a = a_of n in
    measure k (fun () ->
        let acc = ref 0 in
        Array.iter (fun j -> acc := !acc + a.(j)) ix;
        ignore !acc)
  in
  table
    (Printf.sprintf "hops over %d elements: get at a fixed stride, ns per read" n)
    (List.map (fun d -> "+" ^ string_of_int d) strides)
    [ row "eseq" e_run ixs; row "pseq" p_run ixs;
      row "pseq iterator" it_run ixs; row "  array" a_run ixs ]

let scenario_update () =
  let sizes = sizes_m () in
  let k = scale 100000 in
  let e_run n =
    let s = e_of n and ix = random_indices n k in
    measure k (fun () -> Array.iter (fun j -> E.set s j 0) ix)
  in
  let p_run n =
    let s0 = p_of n and ix = random_indices n k in
    measure k (fun () ->
        let s = ref s0 in
        Array.iter (fun j -> s := P.set !s j 0) ix)
  in
  let a_run n =
    let a = a_of n and ix = random_indices n k in
    measure k (fun () -> Array.iter (fun j -> a.(j) <- 0) ix)
  in
  table "update: set at random indices, ns per write" (show_ints sizes)
    [ row "eseq" e_run sizes; row "pseq" p_run sizes; row "  array" a_run sizes ]

let scenario_construction () =
  let sizes = sizes_m () in
  table "construction: n elements from scratch, ns per element" (show_ints sizes)
    [ row "eseq" (fun n -> measure n (fun () -> ignore (E.init default n (fun i -> i)))) sizes;
      row "pseq" (fun n -> measure n (fun () -> ignore (P.init default n (fun i -> i)))) sizes;
      row "  array" (fun n -> measure n (fun () -> ignore (Array.init n (fun i -> i)))) sizes;
      row "  list" (fun n -> measure n (fun () -> ignore (List.init n (fun i -> i)))) sizes ]

let scenario_concat () =
  let sizes = sizes_m () in
  let p_run n =
    let a = p_of (n / 2) and b = p_of (n / 2) in
    measure 1 (fun () -> ignore (P.concat a b))
  in
  let l_run n =
    let a = l_of (n / 2) and b = l_of (n / 2) in
    measure 1 (fun () -> ignore (a @ b))
  in
  table "concat: append two sequences of n/2 elements, ns per concatenation"
    (show_ints sizes)
    [ row "pseq" p_run sizes; row "  list" l_run sizes ]

let scenario_split () =
  let sizes = sizes_m () in
  let p_run n =
    let k = if n > 50000 then 100 else 1000 in
    let s = p_of n and ix = random_indices n k in
    measure k (fun () -> Array.iter (fun j -> ignore (P.split s j)) ix)
  in
  table "split: split at a random index, ns per split" (show_ints sizes)
    [ row "pseq" p_run sizes ]

let scenario_snapshot () =
  let sizes = sizes_m () in
  let snap_run n =
    let e = e_of n in
    let keep = ref (P.create default) in
    measure 1 (fun () ->
        E.set e 0 1;
        keep := Sek.snapshot e)
  in
  table "snapshot: one change plus one snapshot of an n-element sequence, ns"
    (show_ints sizes) [ row "eseq" snap_run sizes ];
  let n = scale 100000 in
  let periods = [ 1; 10; 1000; 100000 ] in
  let e_run m =
    measure n (fun () ->
        let e = E.create default in
        let keep = ref [] in
        for k = 0 to n - 1 do
          E.push Sek.back e k;
          if k mod m = 0 then keep := Sek.snapshot e :: !keep
        done;
        ignore !keep)
  in
  let p_run _ =
    measure n (fun () ->
        let s = ref (P.create default) in
        for k = 0 to n - 1 do s := P.push Sek.back !s k done)
  in
  table
    (Printf.sprintf "snapshot: %d pushes, taking a snapshot every m of them, ns per push" n)
    (List.map (fun m -> "m=" ^ string_of_int m) periods)
    [ row "eseq" e_run periods; row "pseq (persistent)" p_run periods ]

let scenario_fill () =
  let n = big_n () in
  let ks = if !quick then [ 10; 1000 ] else [ 10; 1000; 100000 ] in
  let fill_run k = let s = e_of n in measure k (fun () -> E.fill s 0 k 0) in
  let set_run k =
    let s = e_of n in
    measure k (fun () -> for j = 0 to k - 1 do E.set s j 0 done)
  in
  let a_run k =
    let a = a_of n in
    measure k (fun () -> for j = 0 to k - 1 do a.(j) <- 0 done)
  in
  table
    (Printf.sprintf "fill: overwrite k consecutive elements of %d, ns per element" n)
    (show_ints ks)
    [ row "eseq (fill)" fill_run ks; row "eseq (set loop)" set_run ks;
      row "  array" a_run ks ]

let scenario_transient () =
  let n = big_n () in
  let counts = [ 1; 10; 1000; 100000 ] in
  let ix = Array.init 100000 (fun _ -> Random.int n) in
  let p = p_of n in
  let edit_run m =
    measure m (fun () ->
        let e = Sek.edit p in
        for k = 0 to m - 1 do
          E.set e ix.(k mod 100000) 0
        done;
        ignore (Sek.snapshot e))
  in
  let pset_run m =
    measure m (fun () ->
        let s = ref p in
        for k = 0 to m - 1 do
          s := P.set !s ix.(k mod 100000) 0
        done)
  in
  table
    (Printf.sprintf
       "transient: edit, m in-place updates, snapshot, over %d elements, ns per update" n)
    (List.map (fun m -> "m=" ^ string_of_int m) counts)
    [ row "sek edit/snapshot" edit_run counts;
      row "sek persistent set" pset_run counts ]

let scenario_filter () =
  let sizes = sizes_m () in
  let keep x = x mod 3 = 0 in
  let p_run n = let s = p_of n in measure n (fun () -> ignore (P.filter keep s)) in
  let e_run n = let s = e_of n in measure n (fun () -> ignore (E.filter keep s)) in
  let l_run n = let l = l_of n in measure n (fun () -> ignore (List.filter keep l)) in
  table "filter: keep one element in three, ns per input element" (show_ints sizes)
    [ row "pseq" p_run sizes; row "eseq" e_run sizes; row "  list" l_run sizes ]

let scenarios =
  [ "stack", scenario_stack;
    "front-stack", scenario_front_stack;
    "queue", scenario_queue;
    "traversal", scenario_traversal;
    "random-access", scenario_random_access;
    "hops", scenario_hops;
    "update", scenario_update;
    "construction", scenario_construction;
    "concat", scenario_concat;
    "split", scenario_split;
    "snapshot", scenario_snapshot;
    "transient", scenario_transient;
    "filter", scenario_filter;
    "fill", scenario_fill ]

let () =
  let args = List.tl (Array.to_list Sys.argv) in
  if List.mem "--quick" args then quick := true;
  let named = List.filter (fun a -> String.length a > 0 && a.[0] <> '-') args in
  let chosen = if named = [] then List.map fst scenarios else named in
  Printf.printf "sek benchmarks -- OCaml %s%s\n" Sys.ocaml_version
    (if !quick then " (quick)" else "");
  List.iter
    (fun name ->
       match List.assoc_opt name scenarios with
       | Some run -> run ()
       | None ->
         Printf.printf "\nno such scenario: %s\navailable: %s\n" name
           (String.concat " " (List.map fst scenarios)))
    chosen
