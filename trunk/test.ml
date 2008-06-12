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

let ml_perceptron_test () =
    let perceptron = SigmoidMlPerceptron.newSameValues 2 1 [|3;1|] 0. 0. in
    let vec_of_ints a b =
        [|a;b|] |> Array.map float_of_int in
    let print_result a b =
        let input = vec_of_ints a b in
        let output = (perceptron#feed input).(0) in
        printf "%d, %d : %3.2f\n" a b output in
    for i=0 to 1000 do
        perceptron#learn 0.1 [|1.;1.|] [|0.|];
        perceptron#learn 0.1 [|0.;0.|] [|0.|];
        perceptron#learn 0.1 [|1.;0.|] [|1.|];
        perceptron#learn 0.1 [|0.;1.|] [|1.|];
        print_result 0 0;
        print_result 1 0;
        print_result 0 1;
        print_result 1 1
    done;
    perceptron#print()

let xor_random_sigmoidMlPerceptron_test () =
    let perceptron = SigmoidMlPerceptron.newRandom 2 1 [| 3; 3; 1 |] 1. 1. in
    perceptron#print();
    let vec_of_ints a b =
        [|a;b|] |> Array.map float_of_int in
    let print_result a b =
        let input = vec_of_ints a b in
        let output = (perceptron#feed input).(0) in
        printf "%d, %d : %3.2f\n" a b output in
    for i = 1 to 100000 do
        perceptron#learn 0.001 [| 1.; 1.|] [| 0.|];
        perceptron#learn 0.001 [| 0.; 0.|] [| 0.|];
        perceptron#learn 0.001 [| 1.; 0.|] [| 1.|];
        perceptron#learn 0.001 [| 0.; 1.|] [| 1.|]
    done;
    print_result 1 1;
    print_result 1 0;
    print_result 1 0;
    print_result 0 1;
    perceptron#print()

let _ =
  match Sys.argv.(1) with
    | "1" -> adaline_test ()
    | "2" -> ml_perceptron_test ()
    | "3" -> xor_random_sigmoidMlPerceptron_test ()
    | _ -> raise No_test
