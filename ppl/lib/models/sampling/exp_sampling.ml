open Effppl.Infer
open Effppl.Print

let f1 () = 
	let* x1 = exp 1. in
	x1
;;

let x = get_samples f1 5 0.02 10000 in
print_statistics x;
print_to_file x "./exp_samples.txt";

