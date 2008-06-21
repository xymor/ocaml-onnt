open Printf
open ExtLib

exception No_test

let (|>) x f = f x

(*Testing adaline (see learnsc32.pdf)*)
let adaline_test () =
	let adaline = Adaline.newInit 3 1 (fun i j -> 0.) (fun i -> 0.) in
	let inputs = [|
		[|1.;1.;1.|];
		[|1.;1.;-1.|];
		[|1.;-1.;1.|];
		[|1.;-1.;-1.|];
		[|-1.;1.;1.|];
		[|-1.;1.;-1.|];
		[|-1.;-1.;1.|];
		[|-1.;-1.;-1.|]
	|] in
	let desired = [|
		[|1.|];
		[|-1.|];
		[|1.|];
		[|-1.|];
		[|1.|];
		[|-1.|];
		[|1.|];
		[|1.|]
	|] in
	Array.iter2
		(fun input desired ->
			adaline#learn 0.1 input desired;
			adaline#print ();
			printf "\n")
		inputs desired

let print_result pct a b =
    let input = [|a;b|] |> Array.map float_of_int in
    let output = (pct#feed input).(0) in
    printf "%d, %d : %3.2f\n" a b output

let xor_sigmoidMlPerceptron_test () =
    let pct = SigmoidMlPerceptron.newRandom 2 1 [| 2; 2; 1 |] 1. 1. in
    let learn_base = [|
	[| 1.; 1.|], [| 0.|];
	[| 1.; 0.|], [| 1.|];
	[| 0.; 1.|], [| 1.|];
	[| 0.; 0.|], [| 0.|]
    |] in
    let learn rate (input, desired) =
	pct#learn rate input desired in
    for i = 1 to 5000 do
	NeuralNetwork.iter_random (learn 0.5) learn_base
    done;
    print_result pct 1 1;
    print_result pct 1 0;
    print_result pct 0 1;
    print_result pct 0 0

let _ =
  match Sys.argv.(1) with
    | "1" -> adaline_test ()
    | "2" -> xor_sigmoidMlPerceptron_test ()
    | _ -> raise No_test