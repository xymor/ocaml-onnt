(** Defines types, the interface of a feedforward neural network and useful functions*)

exception Wrong_input
exception Wrong_input_size

val ( |> ) : 'a -> ('a -> 'b) -> 'b
(** Pipe operator *)

val ( +| ) : float array -> float array -> float array
(** Vectors addition *)

val ( -| ) : float array -> float array -> float array
(** Vectors substraction *)

type vector = float array

type layer = { weights : float Matrix.matrix; bias : vector; }
(**Implements a neurons layer *)

type threshold = {
	f : float -> float;
	d : float -> float;
}

type activation =
	  Linear | Sigmoid
	| Custom of threshold


class virtual neuralNetwork :
  int ->
  int ->
  object ('a)
    val input_size : int
    val output_size : int
    method checkInputSize : float array -> unit
	(**Checks if a vector is of the correct size*)
    method virtual feed : float array -> float array
	(**[feed input]
	@return output of the network*)
    method getInputSize : int
	(**@return expected size of an input vector*)
    method getOutputSize : int
	(**@return size of the output vector*)
	method virtual feed : float array -> float array
	method virtual print : unit -> unit
	method virtual toString : unit -> string
	method virtual writeFile : ?comment:string -> string -> unit
  end

(** [new neuralNetwork input_size output_size]
Implements a virtual neural network class *)

val string_of_array : float array -> string
(**@return string representation of an array in order to write a file*) 

val string_of_weights : float Matrix.matrix -> string
(**@return string representation of a matrix of weights in order to write a file*) 

val iter_random : ('a -> unit) -> ('a array) -> unit
val array_map2 : ('a -> 'b -> 'c) -> ('a array ) -> ('b array) -> ('c array)

val learn_random_base :
  < learn : float -> float array -> float array -> unit; .. > -> float -> (float array * float array) array -> unit
(** [learn_random_base pct rate base]
	base is an array of tuples : (input, desired output)
	pct is a perceptron object
	Learning process of pct on the learning base in random order*)