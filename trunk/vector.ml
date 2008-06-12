open Printf

exception Wrong_dimension of string

let (|>) x f = f x

let dim v =
	Array.length v

let same_dim v1 v2 =
	(dim v1) = (dim v2)

let create = Array.create

let map = Array.map

let init = Array.init

let modif f v =
	for i=0 to (dim v) - 1 do
		Array.unsafe_set v i (f (Array.get v i))
	done

let modifi f v =
	for i=0 to (dim v) - 1 do
		Array.unsafe_set v i (f i (Array.get v i))
	done

let map2 f v1 v2 =
	let len = dim v1 in
	if (dim v2) <> len then
		raise (Wrong_dimension("map2"));
	let res = create len 0. in
	for i=0 to len-1 do
		let s = f (Array.unsafe_get v1 i) (Array.unsafe_get v2 i) in
		Array.unsafe_set res i s
	done;
	res

let add_float v1 v2 =
	let len = dim v1 in
	if (dim v2) <> len then
		raise (Wrong_dimension("add_float"));
	let res = create len 0. in
	for i=0 to len-1 do
		let s = (Array.unsafe_get v1 i) +. (Array.unsafe_get v2 i) in
		Array.unsafe_set res i s
	done;
	res

let sub_float v1 v2 =
	let len = dim v1 in
	if (dim v2) <> len then
		raise (Wrong_dimension("sub_float"));
	let res = create len 0. in
	for i=0 to len-1 do
		let s = (Array.unsafe_get v1 i) -. (Array.unsafe_get v2 i) in
		Array.unsafe_set res i s
	done;
	res

let modif_sub ~on:x y =
	let len = dim x in
	if (dim y) <> len then
		raise (Wrong_dimension("modif_sub"));
	for i=0 to len-1 do
		Array.unsafe_set x i ((Array.unsafe_get x i) -. (Array.unsafe_get y i))
	done

let scal_mul s =
	Array.map (fun x -> x *. s)

let scal_div s =
	Array.map (fun x -> x /. s)

let print_float v =
	Array.iter (printf "%3.3f ") v

let println_float v =
	print_float v;
	printf "\n"

let float_to_string v =
	v |> Array.map (sprintf "%3.3f ") |> Array.to_list |> String.concat ""

let check_same_dim v1 v2 =
	if not (same_dim v1 v2) then raise (Wrong_dimension("check_same_dim"))

let float_to_file vec file =
	let chan = open_out file in
	fprintf chan "%s\n" (float_to_string vec);
	close_out chan
