# Ocaml Neural Network Toolkit #
This project uses the ocaml language ([ocaml homepage](http://caml.inria.fr/)).
It aims to implement the common neural network models in ocaml (perceptron, adaline, multilayer perceptrons...).

## XOR Example ##

We want to create a perceptron that performs a XOR.

A perceptron takes an array of float values, returns another.
To represent boolean values, we take :
  * false -> 0
  * true  -> 1

After compilation, you can run the ocaml top with the neural network libraries :
  * ./onnt.top

You can then use the following script to create the perceptron :

```
(*Creation of multi-layer perceptron with :
  2 inputs, 1 output, 2 layers and weights, biases randomized between -1 and 1 *)
let pct = SigmoidMlPerceptron.random 2 1 [| 2; 1 |] 1. 1.;;
```

```
(*Learning base : array of (input, desired output) tuples*)
let learn_base = [|
    [| 1.; 1.|], [| 0.|];
    [| 1.; 0.|], [| 1.|];
    [| 0.; 1.|], [| 1.|];
    [| 0.; 0.|], [| 0.|]
|];;
```

```
(*Learning process on the tuples in random order, 5000 times, at a learning rate of 0.5 *)
for i = 1 to 5000 do
    NeuralNetwork.learn_random_base pct 0.5 learn_base
done;;
```

```
(* We can now test the perceptron *)
printf "0,0 : %3.2f\n" (pct#feed [|0.;0.|]).(0);
printf "1,0 : %3.2f\n" (pct#feed [|1.;0.|]).(0);
printf "0,1 : %3.2f\n" (pct#feed [|0.;1.|]).(0);
printf "1,1 : %3.2f\n" (pct#feed [|1.;1.|]).(0);;
```

The perceptron will usually yield these values :
  * 0,0 : 0.02
  * 1,0 : 0.97
  * 0,1 : 0.97
  * 1,1 : 0.02
Since the perceptron is randomized, the learning process sometimes does not succeed and must be re-run.

author : Jean-Denis Koeck