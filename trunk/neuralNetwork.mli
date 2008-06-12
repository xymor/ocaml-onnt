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

class virtual neuralNetwork :
  int ->
  int ->
  object
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
  end

(** [new neuralNetwork input_size output_size]
Implements a virtual neural network class *)

val string_of_array : float array -> string
(**@return string representation of an array in order to write a file*) 

val string_of_weights : float Matrix.matrix -> string
(**@return string representation of a matrix of weights in order to write a file*) 
