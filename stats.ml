open ExtLib

(*
Implements various functions in order to facilitate the statistical analysis of a neural network
*)

let (|>) x f = f x

let (+|) = Vector.add_float
let (-|) = Vector.sub_float

type succs2 = bool array
type succs3 = bool option array

exception Wrong_size

let mean a =
	let len = float_of_int (Array.length a) in
	(Array.fold_left (+.) 0. a) /. len

let mean_error desired results =
	mean (desired -| results)

let mk_perc n len =
	100. *. float_of_int(n) /. float_of_int(len)

let check_size a1 a2 =
	if (Array.length a1) <> (Array.length a2) then 
		raise Wrong_size
	else Array.length a1

let count_cond cond a =
	Array.fold_left (fun acc x -> if cond x then acc + 1 else acc) 0 a

let count_true a =
	Array.fold_left (fun acc x -> if x then acc + 1 else acc) 0 a

let succ2 results2 desired =
	let n = Array.length results2 in
	let n_succ =
		Array.map2 (=) results2 desired
	    |> count_true in
	let n_fail =
		n - n_succ in
	n, n_succ, n_fail

let succ3 results3 desired =
	let n = Array.length results3 in
	let eq3 x y = match x with
		| None -> false
		| Some(b) -> b = y in
	let n_succ = 
		Array.map2 eq3 results3 desired
		|> count_true in
	let n_inde = count_cond ((=) None) results3 in
	let n_fail = n - n_succ - n_inde in
	n, n_succ, n_fail, n_inde

let perc2 results2 desired =
	let n, n_succ, n_fail = succ2 results2 desired in
	mk_perc n_succ n, mk_perc n_fail n

let perc3 results3 desired =
	let n, n_succ, n_fail, n_inde = succ3 results3 desired in
	mk_perc n_succ (n-n_inde), mk_perc n_fail (n-n_inde), mk_perc n_inde n

let sens2 results2 desired =
	let nb_pos = count_true desired in
	let nb_true_pos = ref 0 in
	Array.iter2
		(fun x y -> if x && y then incr nb_true_pos)
		results2 desired;
	mk_perc !nb_true_pos nb_pos

let sens3 results3 desired =
	let and3 x y =
		(x = Some(true)) && y in
	let nb_pos = count_true desired in
	let nb_true_pos = ref 0 in
	Array.iter2
		(fun x y -> if and3 x y then incr nb_true_pos)
		results3 desired;
	mk_perc !nb_true_pos nb_pos

let spec2 results2 desired =
	let nb_neg = count_cond not desired in
	let nb_true_neg = ref 0 in
	Array.iter2
		(fun x y -> if not(x) && not(y) then incr nb_true_neg)
		results2 desired;
	mk_perc !nb_true_neg nb_neg

let spec3 results3 desired =
	let nb_neg = count_cond not desired in
	let nb_true_neg = ref 0 in
	Array.iter2
		(fun x y -> if (x = (Some false)) && not(y) then incr nb_true_neg)
		results3 desired;
	mk_perc !nb_true_neg nb_neg

(*

let count2_pos results desired =
	let true_pos = Array.map2 (&&) results desired in
	let nb_true = count_true results in
	let nb_true_pos = count_true true_pos in
	let nb_false_pos = nb_true - nb_true_pos in
	nb_true, nb_true_pos, nb_false_pos

let count3_pos results desired =
	let and_3 x y =
		(x = Some(true)) && y in
	let f_t x y =
		(x = Some(false)) && y in
	let true_pos = Array.map2 and_3 results desired in
	let false_pos = Array.map2 f_t results desired in
	let nb_true = count_true desired in
	let nb_true_pos = count_true true_pos in
	let nb_false_pos = count_true false_pos in
	nb_true, nb_true_pos, nb_false_pos
	
let perc2_pos results desired ?(count2_pos = count2_pos results desired) () =
	let n_t, n_tp, n_fp = count2_pos in
	mk_perc n_tp n_t, mk_perc n_fp n_t
	
let perc3_pos results desired ?(count3_pos = count3_pos results desired) () =
	let n_t, n_tp, n_fp = count3_pos in
	mk_perc n_tp n_t, mk_perc n_fp n_t
*)
