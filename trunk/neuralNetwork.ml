open Printf
open Matrix

exception Wrong_input
exception Wrong_input_size

let (|>) x f = f x

let (+|) = Vector.add_float
let (-|) = Vector.sub_float

type vector = float array

type layer = {
	weights : float matrix;
	bias : vector;
}

type threshold = {
	f : float -> float;
	d : float -> float;
}

type activation =
	  Linear | Sigmoid
	| Custom of threshold

class virtual neuralNetwork value_input_size value_output_size =
	object (self)
		val input_size:int = value_input_size
		val output_size:int = value_output_size
		method getInputSize = input_size
		method getOutputSize = output_size
		method checkInputSize (input:float array) =
			if (Array.length input) <> input_size then
				(raise Wrong_input_size)
		method virtual feed : float array -> float array
		method virtual print : unit -> unit
		method virtual toString : unit -> string
		method virtual writeFile : ?comment:string -> string -> unit
	end

(*Printing fuctions*)
let string_of_array a =
	let strings = Array.map string_of_float a in
	String.concat ";" (Array.to_list strings)

let string_of_weights w =
	let strings = Array.map string_of_array (to_array w) in
	String.concat "|" (Array.to_list strings)

let shuffle a =
    Random.self_init();
    let len = Array.length a in
    for i = len downto 1 do
	(*swap rand i-1*)
	let rand = Random.int i in
	let temp = a.(rand) in
	a.(rand) <- a.(i-1);
	a.(i-1) <- temp
    done

let iter_random f a =
    let len = Array.length a in
    let indices = Array.init len (fun i -> i) in
    shuffle indices;
    Array.iter (fun i -> f a.(i)) indices

let array_map2 f a1 a2 =
	let len = Array.length a1 in
	if len <> Array.length a2 then
		raise (Invalid_argument "Array.map2");
	if len = 0 then [||]
	else (
		let r = Array.create len (f (Array.unsafe_get a1 0) (Array.unsafe_get a2 0)) in
		for i = 1 to len - 1 do
			Array.unsafe_set r i (f (Array.unsafe_get a1 i) (Array.unsafe_get a2 i))
		done;
		r
	)