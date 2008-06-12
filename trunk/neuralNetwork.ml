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
	end

(*Printing fuctions*)
let string_of_array a =
	let strings = Array.map string_of_float a in
	String.concat ";" (Array.to_list strings)

let string_of_weights w =
	let strings = Array.map string_of_array (to_array w) in
	String.concat "|" (Array.to_list strings)
