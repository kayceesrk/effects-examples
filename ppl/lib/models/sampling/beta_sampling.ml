open Effppl.Infer
open Effppl.Print

let f1 () = 
	let* x1 = beta 2. 1. in
	x1
;;

let x = get_samples f1 5 0.002 5000 in
print_statistics x;
print_to_file x "./beta_samples.txt";
