open Effppl.Infer
open Effppl.Print

let f1 () = 
	let* x1 = chi2 4. in
	x1
;;

let x = get_samples f1 3 0.05 2000 in
print_statistics x;
print_to_file x "./chi_samples.txt";

