open NeuralNetwork
open Printf
open Matrix

exception Wrong_input_size
exception Wrong_init_sizes

let feed_layer f input layer =
	let activation = (FloatMatrix.mul_vect layer.weights input) -| layer.bias in
	Array.map f activation

let feed_and_sum_layer f input layer =
	let sum = (FloatMatrix.mul_vect layer.weights input) -| layer.bias in
	(sum, Array.map f sum)

class mlPerceptron input_size output_size init_weights init_biases activation =
	object (self)
	inherit neuralNetwork input_size output_size as network
	
		val layers_nb =
			Array.length init_weights
		
		val threshold = match activation with
			| Linear ->
				{ f = (fun x -> x); d = (fun x -> 1.) }
			| Sigmoid ->
				let f x = 1. /. (1. +. exp(-. x)) in
				{ f = f;
				  d = (fun x -> f(x) *. (1. -. f(x)));
				}
			| Custom(activation) ->
				activation
		
		val layers =
			let layers_nb = Array.length init_weights in
			let init_layer i =
				{ weights = init_weights.(i); bias = init_biases.(i) } in
			Array.init layers_nb init_layer
		
		method getLayersNb () = layers_nb
		
		method print () =
			printf "Multilayer perceptron : %d layers\n" layers_nb;
			printf "input_size = %d, output_size = %d\n" input_size output_size;
			for i=0 to layers_nb - 1 do
				printf "Layer %d weights : \n%s" (i+1) (FloatMatrix.to_string layers.(i).weights);
				printf "bias : %s\n" (Vector.float_to_string layers.(i).bias)
			done
		
		method toString () =
			let b = Buffer.create 80 in
			bprintf b "Multilayer perceptron : %d layers\n" layers_nb;
			bprintf b "input_size = %d, output_size = %d\n" input_size output_size;
			for i=0 to layers_nb - 1 do
				bprintf b "Layer %d weights : \n%s" (i+1) (FloatMatrix.to_string layers.(i).weights);
				bprintf b "bias : %s\n" (Vector.float_to_string layers.(i).bias)
			done;
			Buffer.contents b
		
		method feed input =
			self#checkInputSize input;
			Array.fold_left (feed_layer threshold.f) input layers
		
		method private feedLayersResults input =
			network#checkInputSize input;
			(*let results = Array.unfoldi layers_nb (fun i a -> feed_layer f a layers.(i)) input*)
			let results = Array.create layers_nb [||] in
			let temp = ref input in
			for i=0 to layers_nb - 1 do
				temp := feed_layer threshold.f !temp layers.(i);
				results.(i) <- !temp
			done;
			results
		
		method private feedLayersSumsResults input =
			self#checkInputSize input;
			let sums = Array.create layers_nb [||] in
			let results = Array.create layers_nb [||] in
			let temp = ref input in
			for i=0 to layers_nb - 1 do
				let (sum,result) = feed_and_sum_layer threshold.f !temp layers.(i) in
				sums.(i) <- sum;
				results.(i) <- result;
				temp := result
			done;
			(sums,results)
		
		method private getResultsErrors input desired =
			printf "get_errors\n";
			printf "output_size : %d\n" output_size;
			printf "size of results : %d\n" (Array.length (self#feedLayersResults input));
			let (sums, results) = self#feedLayersSumsResults input in
			let output = results.(layers_nb - 1) in
			let output_sum = sums.(layers_nb - 1) in
			let errors = Array.create layers_nb [||] in
			
			(*Error on the output layer*)
			printf "Error on the input layer\n";
			errors.(layers_nb-1) <- Array.init output_size
				(fun i ->
					(desired.(i) -. output.(i)) *. threshold.d(output_sum.(i)));
				
			(*Errors computation*)
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
						error := !error *. (threshold.d sums.(k).(i));
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
			fprintf channel "MlPerceptron\n";
			fprintf channel "%d\n" input_size;
			fprintf channel "%d\n" output_size;
			fprintf channel "%d\n" layers_nb;
			for i=0 to layers_nb - 1 do
				fprintf channel "%s\n" (string_of_weights layers.(i).weights)
			done;
			for i=0 to layers_nb - 1 do
				fprintf channel "%s\n" (string_of_array layers.(i).bias)
			done;
			fprintf channel "linear";
			close_out channel
	end

type t = mlPerceptron

let create input_size output_size (layers_sizes : int array) weight bias activation =
	let layers_nb = Array.length layers_sizes in
	if layers_sizes.(layers_nb - 1) <> output_size then
		(raise Wrong_init_sizes);
	let init_weights i =
		let hidden_size = if i=0 then input_size else layers_sizes.(i-1) in
		FloatMatrix.create layers_sizes.(i) hidden_size weight in
    let init_bias i =
		Array.create layers_sizes.(i) bias in
	let weights = Array.init layers_nb init_weights in
	let biases = Array.init layers_nb init_bias in
	let perceptron = new mlPerceptron input_size output_size weights biases activation in
	perceptron
 
let random input_size output_size (layers_sizes : int array) weights_range bias_range activation =
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
	let perceptron = new mlPerceptron input_size output_size weights biases activation in
	perceptron
