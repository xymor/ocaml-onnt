open NeuralNetwork
open MlPerceptron
open SigmoidMlPerceptron
open Matrix

exception Wrong_type
exception Wrong_function

type networkType = MlPerceptron | SigmoidMlPerceptron

module String =
struct
	include String
 	let suffix s i =
		try
			String.sub s i ((String.length s) - i)
		with Invalid_argument("String.sub") -> ""
	let split c s =
		let rec split_from n =
			try
				let p = String.index_from s n c in
				(String.sub s n (p-n)) :: (split_from (p+1))
			with Not_found -> [ suffix s n ]
		in if s="" then [] else split_from 0
end

(*PRINTING FUNCTIONS*)

let read_network_type s =
	match s with
		| "MlPerceptron" ->	MlPerceptron
		| "SigmoidMlPerceptron" -> SigmoidMlPerceptron
		| _ -> raise Wrong_type

let read_function_deriv s =
	match s with
		| "linear" -> ( (fun x -> x), (fun x -> 1.) )
		| "sigmoid" -> (sigmoid, sigmoid')
		| _ -> raise Wrong_function

(*READING A FILE*)

let get_value s = s |> String.split ':' |> fun l -> List.nth l 1
let split_elem s = s |> String.split ';' |> Array.of_list
let split_arrays s = s |> String.split '|' |> Array.of_list

let weights_of_string s =
	s |> split_arrays |> Array.map split_elem
		|> Array.map (Array.map float_of_string)
		|> FloatMatrix.of_array

let bias_of_string s =
	s |> split_elem |> Array.map float_of_string

let openFile file =
	let channel = open_in file in
	
	let rec new_line() =
		let line = input_line channel in
		if line = "" || line.[0] = '#' then
			new_line()
		else line in
	
	let first_line = new_line () in
	let network_type = read_network_type (get_value first_line) in
	let input = int_of_string (get_value (new_line ())) in
	let output = int_of_string (get_value (new_line ())) in
	let layers_nb = int_of_string (get_value (new_line ())) in
	let weights =
		Array.init layers_nb (fun _ -> new_line())
		|> Array.map get_value
		|> Array.map weights_of_string in
	let bias =
		Array.init layers_nb (fun _ -> new_line())
		|> Array.map get_value
		|> Array.map bias_of_string in
	close_in channel;
	match network_type with
		| SigmoidMlPerceptron ->
			  close_in channel;
			  new sigmoidMlPerceptron input output weights bias
		| MlPerceptron ->
			  let (f,deriv) = read_function_deriv (get_value(new_line ())) in
				  close_in channel;
			  new mlPerceptron input output weights bias f deriv
