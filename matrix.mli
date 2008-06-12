type 'a matrix
exception Not_a_matrix
exception Not_same_dimension
exception Empty_matrix
exception Wrong_dimension
external to_array : 'a matrix -> 'a array array = "%identity"
module Matrix :
  sig
    val of_array : 'a array array -> 'a matrix
    val copy : 'a matrix -> 'a matrix
    val col_dim : 'a matrix -> int
    val row_dim : 'a matrix -> int
    val unsafe_set : 'a matrix -> int -> int -> 'a -> unit
	val get : 'a matrix -> int -> int -> 'a
    val unsafe_get : 'a matrix -> int -> int -> 'a
    val create : int -> int -> 'a -> 'a matrix
    val init : int -> int -> (int -> int -> 'a) -> 'a matrix
    val iter : ('a -> unit) -> 'a matrix -> unit
    val iterij : (int -> int -> 'a -> unit) -> 'a matrix -> unit
    val map : ('a -> 'b) -> 'a matrix -> 'b matrix
    val mapij : (int -> int -> 'a -> 'b) -> 'a matrix -> 'b matrix
    val modif : ('a -> 'a) -> 'a matrix -> unit
    val modifij : (int -> int -> 'a -> 'a) -> 'a matrix -> unit
    val map2 :
      ('a -> 'b -> 'c) -> 'a matrix -> 'b matrix -> 'c matrix
    val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b matrix -> 'a
    val fold_right : ('a -> 'b -> 'b) -> 'a matrix -> 'b -> 'b
    val identity : int -> float matrix
    val transpose : 'a matrix -> 'a matrix
    val random_int : int -> int -> int -> int matrix
    val print_int : int matrix -> unit
    val println_int : int matrix -> unit
  end
module FloatMatrix :
  sig
    val of_array : float array array -> float matrix
    val copy : float matrix -> float matrix
    val col_dim : 'a array -> int
    val row_dim : float matrix -> int
    val unsafe_set : float matrix -> int -> int -> float -> unit
    val unsafe_get : float matrix -> int -> int -> float
    val create : int -> int -> float -> float matrix
    val init : int -> int -> (int -> int -> float) -> float matrix
    val iter : (float -> unit) -> float array array -> unit
    val iterij : (int -> int -> float -> unit) -> float matrix -> unit
    val map : (float -> float) -> float matrix -> float matrix
    val mapij : (int -> int -> float -> float) -> float matrix -> float matrix
    val modif : (float -> float) -> float matrix -> unit
    val modifij : (int -> int -> float -> float) -> float matrix -> unit
    val map2 :
      (float -> float -> float) -> float matrix -> float matrix -> float matrix
    val fold_left :  ('b -> float -> 'b) -> 'b -> float matrix -> 'b
    val fold_right : (float -> 'b -> 'b) -> float matrix -> 'b -> 'b
    val identity : int -> float matrix
    val transpose : float matrix -> float matrix
    val random : int -> int -> float -> float matrix
    val print : float matrix -> unit
    val println : float matrix -> unit
    val mul : float matrix -> float matrix -> float matrix
    val null_of_same_dim : float matrix -> float matrix
    val add : float matrix -> float matrix -> float matrix
    val sub : float matrix -> float matrix -> float matrix
    val modif_add : float matrix -> on:float matrix -> unit
    val modif_sub : float matrix -> on:float matrix -> unit
    val scal_mul : float -> float matrix -> float matrix
	val mul_vect : float matrix -> float array -> float array
	val to_string : float matrix -> string
  end
