(** Useful fonctions to read perceptrons from a file *)

exception Wrong_type
exception Wrong_function

type networkType = MlPerceptron | SigmoidMlPerceptron | Adaline
val read_network_type : string -> networkType
val read_activation : string -> NeuralNetwork.activation
val get_value : string -> string
val split_elem : string -> string array
val split_arrays : string -> string array
val weights_of_string : string -> float Matrix.matrix
val bias_of_string : string -> float array

(**Reads a perceptron file*)
val openFile : string -> MlPerceptron.t
