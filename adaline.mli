class adaline :
  int ->
  int ->
  float Matrix.matrix ->
  float array ->
  object
    val input_size : int
    val output_size : int
    val weights : float Matrix.matrix
	val bias : float array
    method checkInputSize : float array -> unit
    method feed : float array -> float array
    method getInputSize : int
    method getOutputSize : int
    method learn : float -> float array -> float array -> unit
	method print : unit -> unit
	method toString : unit -> string
  end

type t = adaline

val newInit : int -> int -> (int -> int -> float) -> (int -> float) -> adaline 
