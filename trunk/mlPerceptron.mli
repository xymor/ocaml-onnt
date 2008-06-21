(** This module implements a multilayer perceptron*)

exception Wrong_input_size
exception Wrong_init_sizes
 
val feed_layer : (float -> float) -> float array -> NeuralNetwork.layer -> float array
(** [feed_layer threshold_function input layer]
	returns the output of the layer*)

val feed_and_sum_layer : (float -> float) -> float array -> NeuralNetwork.layer -> float array * float array
(** [feed_and_sum_layer threshold_function input layer]
	returns (activation, output)
	where activation : product of input and layer
		  output : array of the output values*)

class mlPerceptron :
int -> int -> float Matrix.matrix array -> NeuralNetwork.vector array -> NeuralNetwork.activation ->
	object ('a)
	
	val threshold : NeuralNetwork.threshold
	(** Record containing the threshold function and its derivative*)

	val input_size : int
	val layers : NeuralNetwork.layer array
	val layers_nb : int
	val output_size : int
	method checkInputSize : float array -> unit
	method copy : unit -> 'a
	
	method feed : float array -> float array
	(** processes the input values, returns the output *)
	
	(*method feedLayersResults : float array -> float array array
	(** processes the input values, returns the successive results of the layers *)*)
	
	method private feedLayersSumsResults :
		float array -> float array array * float array array
	method getInputSize : int
	method getLayersNb : unit -> int
	method getOutputSize : int
	
	(*method getResultsErrors :
		float array -> float array -> float array array * float array array*)

	method learn : float -> float array -> float array -> unit
	method print : unit -> unit
	method toString : unit -> string
	method writeFile : ?comment:string -> string -> unit
end
(** Implements a mutable sigmoid perceptron object *)
				
type t = mlPerceptron
				
val create :
int ->	int -> int array ->	float -> float -> NeuralNetwork.activation -> mlPerceptron
(** [create input_size output_size layers_sizes weight bias activation]
	Initiates a sigmoid perceptron where
		layers_sizes : array of the sizes of the successive layers
	All weights are initialized to weight, all biases to bias*)
	
val random :
int -> int -> int array -> float -> float -> NeuralNetwork.activation -> mlPerceptron
(** [newRandom input_size output_size layers_sizes weight_range bias_range]
	Initiates a sigmoid perceptron where
		layers_sizes : array of the sizes of the successive layers
		weights_range : range of the random weights values around zero
		bias_range ; range of the biases values around zero*)
