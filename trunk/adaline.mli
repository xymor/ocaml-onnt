class adaline :
  int ->
  int ->
  float Matrix.matrix ->
  float array ->
  object ('a)
    val input_size : int
    val output_size : int
    val weights : float Matrix.matrix
	val bias : float array
	method getLayersNb : unit -> int
    method checkInputSize : float array -> unit
    method feed : float array -> float array
    method getInputSize : int
    method getOutputSize : int
    method learn : float -> float array -> float array -> unit
	method print : unit -> unit
	method toString : unit -> string
	method copy : unit -> 'a
    method writeFile : ?comment:string -> string -> unit
  end

type t = adaline

val create : int -> int -> float -> float -> adaline 
val init : int -> int -> (int -> int -> float) -> (int -> float) -> adaline 
