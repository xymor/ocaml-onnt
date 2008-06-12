(** Useful fonctions to read perceptrons from a file *)

exception Wrong_type
exception Wrong_function

type networkType = MlPerceptron | SigmoidMlPerceptron
val read_network_type : string -> networkType
val read_function_deriv : string -> (float -> float) * (float -> float)
val get_value : string -> string
val split_elem : string -> string array
val split_arrays : string -> string array
val weights_of_string : string -> float Matrix.matrix
val bias_of_string : string -> float array

(**Reads a perceptron file*)
val openFile : string -> SigmoidMlPerceptron.sigmoidMlPerceptron
