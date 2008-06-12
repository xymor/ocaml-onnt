open NeuralNetwork
open Printf
open Matrix
open ExtLib

exception Wrong_input_size
exception Wrong_init_sizes

let sigmoid x = 1. /. (1. +. exp(-. x))
let sigmoid' x = sigmoid(x) *. (1. -. sigmoid(x))
let sigderiv y = y *. (1. -. y)

let feed_layer input layer =
	let potential = (FloatMatrix.mul_vect layer.weights input) in
	Vector.modif_sub ~on:potential layer.bias;
	Vector.modif sigmoid potential;
	potential

let feed_and_sum_layer input layer =
	let sum = (FloatMatrix.mul_vect layer.weights input) in
	Vector.modif_sub ~on:sum layer.bias;
	let potential = Array.map sigmoid sum in
	(sum, potential)

class sigmoidMlPerceptron input_size output_size init_weights init_biases =
	object (self)
	inherit neuralNetwork input_size output_size as network
	
		val layers_nb =
			Array.length init_weights
		
		val layers =
			(*let layers_nb = Array.length init_weights in
			let init_layer i =
				{ weights = init_weights.(i); bias = init_biases.(i) } in
			Array.init layers_nb init_layer*)
			Array.map2
				(fun w b -> { weights = w; bias = b})
				init_weights init_biases
		
		method getLayersNb () = layers_nb
		
		method print () =
			printf "Sigmoid multilayer perceptron : %d layers\n" layers_nb;
			printf "input_size = %d, output_size = %d\n" input_size output_size;
			for i=0 to layers_nb - 1 do
				printf "Layer %d weights : \n%s" (i+1) (FloatMatrix.to_string layers.(i).weights);
				printf "bias : %s\n" (Vector.float_to_string layers.(i).bias)
			done
		
		method toString () =
			let out = IO.output_string () in
			IO.printf out "Sigmoid multilayer perceptron : %d layers\n" layers_nb;
			IO.printf out "input_size = %d, output_size = %d\n" input_size output_size;
			for i=0 to layers_nb - 1 do
				IO.printf out "Layer %d weights : \n%s" (i+1) (FloatMatrix.to_string layers.(i).weights);
				IO.printf out "bias : %s\n" (Vector.float_to_string layers.(i).bias)
			done;
			IO.close_out out
			
		method feed input =
			self#checkInputSize input;
			Array.fold_left feed_layer input layers
		
		method feedLayersResults input =
			network#checkInputSize input;
			let results = Array.create layers_nb [||] in
			let temp = ref input in
			for i=0 to layers_nb - 1 do
				temp := feed_layer !temp layers.(i);
				results.(i) <- !temp
			done;
			results
		
		method getResultsErrors input desired =
			let results = self#feedLayersResults input in
			let output = results.(layers_nb - 1) in
			let errors = Array.create layers_nb [||] in
			(*errors.(layers_nb-1) <- Array.init output_size
				(fun i ->
					(desired.(i) -. output.(i)) *. sigderiv(output.(i)));*)
			errors.(layers_nb-1) <-
				Array.map2 (fun d o -> (d -. o) *. sigderiv(o)) desired output;
			for index=2 to layers_nb do
				let k = layers_nb - index in
				let weights = layers.(k+1).weights in
				let layer_size = Matrix.col_dim layers.(k).weights in
				errors.(k) <- Array.init layer_size 
					(fun i ->
						let error = ref 0. in
						for j=0 to (Matrix.col_dim weights) - 1 do
							error := !error +. (Matrix.get weights j i) *. errors.(k+1).(j)
						done;
						error := !error *. (sigderiv results.(k).(i));
						!error)
			done;
			(results, errors)
		
		method learn rate input desired =
			let (results, errors) = self#getResultsErrors input desired in

		    (*Weights and bias rectifications*)
			for k=0 to layers_nb - 1 do
				let prev_results = if k=0 then input else results.(k-1) in
				
				FloatMatrix.modifij
					(fun i j x -> x +. rate *. errors.(k).(i) *. prev_results.(j))
					layers.(k).weights;
					
				Vector.modifi
					(fun i x -> x -. rate *. errors.(k).(i))
					layers.(k).bias
			done
		
		method copy () =
			let copy_layer l =
				{ weights = FloatMatrix.copy l.weights;
				  bias = Array.copy l.bias; } in
			let new_layers = Array.map copy_layer layers in
			{< layers = new_layers >}
		
		method writeFile ?(comment="") file_name =
			let channel = open_out file_name in
			if comment <> "" then fprintf channel "#%s\n" comment; 
			fprintf channel "type:SigmoidMlPerceptron\n";
			fprintf channel "input:%d\n" input_size;
			fprintf channel "output:%d\n" output_size;
			fprintf channel "layers:%d\n" layers_nb;
			for i=0 to layers_nb - 1 do
				fprintf channel "weights%d:%s\n" (i+1) (string_of_weights layers.(i).weights)
			done;
			for i=0 to layers_nb - 1 do
				fprintf channel "bias%d:%s\n" (i+1) (string_of_array layers.(i).bias)
			done;
			close_out channel
		end

type t = sigmoidMlPerceptron

let newSameValues input_size output_size (layers_sizes : int array) weights_value
bias_value =
	let layers_nb = Array.length layers_sizes in
	if layers_sizes.(layers_nb - 1) <> output_size then
		(raise Wrong_init_sizes);
	let init_weights i =
		let hidden_size = if i=0 then input_size else layers_sizes.(i-1) in
		Matrix.create layers_sizes.(i) hidden_size weights_value in
    let init_bias i =
		Array.create layers_sizes.(i) bias_value in
	let weights = Array.init layers_nb init_weights in
	let biases = Array.init layers_nb init_bias in
	let perceptron = new sigmoidMlPerceptron input_size output_size weights biases in
	perceptron

let newRandom input_size output_size (layers_sizes : int array) weights_range
bias_range =
	Random.self_init ();
	let init_bias i =
		let value = Random.float bias_range in
		if Random.bool () then value else -1. *. value in
	let layers_nb = Array.length layers_sizes in
	if layers_sizes.(layers_nb - 1) <> output_size then
		(raise Wrong_init_sizes);
	let init_weights i =
		let hidden_size = if i=0 then input_size else layers_sizes.(i-1) in
		FloatMatrix.random layers_sizes.(i) hidden_size weights_range in
    let init_bias i =
		Array.init layers_sizes.(i) init_bias in
	let weights = Array.init layers_nb init_weights in
	let biases = Array.init layers_nb init_bias in
	let perceptron = new sigmoidMlPerceptron input_size output_size weights biases in
	perceptron
