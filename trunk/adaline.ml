open NeuralNetwork
open Printf
open Matrix

class adaline input_size output_size weights bias =
	object (self)
	inherit neuralNetwork input_size output_size as network
	
		val weights = weights 
		val bias = bias
		
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
			let out = IO.output_string () in
			IO.printf out "Adaline perceptron\n";
			IO.printf out "input_size = %d, output_size = %d\n" input_size output_size;
			IO.printf out "weights : \n%s" (FloatMatrix.to_string weights);
			IO.printf out "bias : %s\n" (Vector.float_to_string bias);
			IO.close_out out
	end

type t = adaline

let newInit input_size output_size init_weights init_bias =
	let weights = Matrix.init output_size input_size init_weights in
	let bias = Vector.init output_size init_bias in
	new adaline input_size output_size weights bias
