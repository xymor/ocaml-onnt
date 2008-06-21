open NeuralNetwork
open Printf
open Matrix

class adaline input_size output_size weights bias =
	object (self)
	inherit neuralNetwork input_size output_size as network
	
		val weights = weights 
		val bias = bias
		
		method getLayersNb () = 1
		
		method feed input =
			let output = (FloatMatrix.mul_vect weights input) in
			Vector.modif_sub ~on:output bias;
			output
		
		method learn rate input reference =
			let output = self#feed input in
			let error = reference -| output in
			
			FloatMatrix.modifij
				(fun i j x -> x +. rate *. error.(i) *. input.(j))
				weights;
			
			Vector.modifi
				(fun i x -> x -. rate *. error.(i))
				bias
		
		method print () =
			printf "Adaline perceptron\n";
			printf "input_size = %d, output_size = %d\n" input_size output_size;
			printf "weights : \n%s" (FloatMatrix.to_string weights);
			printf "bias : %s\n" (Vector.float_to_string bias)
		
		method toString () =
			let b = Buffer.create 80 in
			bprintf b "Adaline perceptron\n";
			bprintf b "input_size = %d, output_size = %d\n" input_size output_size;
			bprintf b "weights : \n%s" (FloatMatrix.to_string weights);
			bprintf b "bias : %s\n" (Vector.float_to_string bias);
			Buffer.contents b
			
		method copy () =
			let new_weights = FloatMatrix.copy weights in
			let new_bias = Array.copy bias in
			{< weights = new_weights; bias = new_bias >}
		
		method writeFile ?(comment="") file_name =
			let channel = open_out file_name in
			if comment <> "" then fprintf channel "#%s\n" comment; 
			fprintf channel "Adaline\n";
			fprintf channel "%d\n" input_size;
			fprintf channel "%d\n" output_size;
			fprintf channel "%d\n" 1;
			fprintf channel "%s\n" (string_of_weights weights);
			fprintf channel "%s\n" (string_of_array bias);
			fprintf channel "linear";
			close_out channel
	end

type t = adaline

let create input_size output_size weight bias =
	let weights = Matrix.create output_size input_size weight in
	let bias = Vector.create output_size bias in
	new adaline input_size output_size weights bias

let init input_size output_size init_weights init_bias =
	let weights = Matrix.init output_size input_size init_weights in
	let bias = Vector.init output_size init_bias in
	new adaline input_size output_size weights bias

