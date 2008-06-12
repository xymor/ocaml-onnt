(** This module implements a multilayer perceptron whose threshold function is the sigmoid *)

exception Wrong_input_size
exception Wrong_init_sizes

val sigmoid : float -> float
(** Sigmoid function : 1 / ( 1 + exp(-x)).
	Used as an activation function in the neural network*)

val sigmoid' : float -> float
(** Derivative of the sigmoid function *)

val sigderiv : float -> float
(** [sigderiv(y) = sigmoid'(x)] where y = sigmoid(x) *)

val feed_layer : float array -> NeuralNetwork.layer -> float array
(** [feed_layer input layer]
	@return output of the layer*)

val feed_and_sum_layer : float array -> NeuralNetwork.layer -> float array * float array
(** [feed_and_sum_layer input layer]
	@return (activation, output)
	where
		- activation : product of input and layer
		- output : array of the output values*)

class sigmoidMlPerceptron :
  int ->
  int ->
  float Matrix.matrix array ->
  NeuralNetwork.vector array ->
  object ('a)
    val input_size : int
    val layers : NeuralNetwork.layer array
    val layers_nb : int
    val output_size : int
	
    method checkInputSize : float array -> unit
	
    method feed : float array -> float array
    (** [feed input ]
	processes the input values, returns the output *)
	
    method feedLayersResults : float array -> float array array
    (** [feed input]
	processes the input values
	@return successive results of each layers (last element is the output)*)
	
    method getInputSize : int
    method getLayersNb : unit -> int
    method getOutputSize : int
    method getResultsErrors :
      float array -> float array -> float array array * float array array
	
    method learn : float -> float array -> float array -> unit
	(** [learn learning_rate input desired]
		where desired are the expected values *)
	
    method print : unit -> unit
	method toString : unit -> string
	method copy : unit -> 'a
    method writeFile : ?comment:string -> string -> unit
  end
  (** [new sigmoidMlPerceptron input_size output_size weights biases]  
	  A mutable sigmoid perceptron object *)

type t = sigmoidMlPerceptron

val newSameValues :
  int -> int -> int array -> float -> float -> sigmoidMlPerceptron
(** [newSameValues input_size output_size layers_sizes weight bias]
	Initiates a sigmoid perceptron where
		layers_sizes : array of the sizes of the successive layers
	All weights are initialized to weight, all biases to bias*)

val newRandom :
  int -> int -> int array -> float -> float -> sigmoidMlPerceptron
(** [newRandom input_size output_size layers_sizes weight_range bias_range]
	Initiates a sigmoid perceptron where
		layers_sizes : array of the sizes of the successive layers
		weights_range : range of the random weights values around zero
		bias_range ; range of the biases values around zero*)
