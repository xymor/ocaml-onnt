open Printf
open ExtLib

exception No_test

let (|>) x f = f x

(*Testing adaline (see learnsc32.pdf)*)
let adaline_test () =
	let adaline = Adaline.create 3 1 0. 0. in
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

(*let print_result pct a b =
    let input = [|a;b|] |> Array.map float_of_int in
    let output = (pct#feed input).(0) in
    printf "%d, %d : %3.2f\n" a b output*)

let xor_sigmoid_test () =
    let pct = SigmoidMlPerceptron.random 2 1 [| 2; 2; 1 |] 1. 1. in
    let learn_base = [|
	[| 1.; 1.|], [| 0.|];
	[| 1.; 0.|], [| 1.|];
	[| 0.; 1.|], [| 1.|];
	[| 0.; 0.|], [| 0.|]
    |] in
    for i = 1 to 5000 do
	NeuralNetwork.learn_random_base pct 0.5 learn_base
    done;
    printf "0,0 : %3.2f\n" (pct#feed [|0.;0.|]).(0);
    printf "1,0 : %3.2f\n" (pct#feed [|1.;0.|]).(0);
    printf "0,1 : %3.2f\n" (pct#feed [|0.;1.|]).(0);
    printf "1,1 : %3.2f\n" (pct#feed [|1.;1.|]).(0)

let _ =
    if Array.length Sys.argv = 1 then
	printf "Specify a test : adaline, xor\n"
    else
	match Sys.argv.(1) with
	    | "adaline" -> adaline_test ()
	    | "xor" -> xor_sigmoid_test ()
	    | _ -> raise No_test