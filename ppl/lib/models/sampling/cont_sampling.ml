open Effppl.Infer
open Effppl.Print

let f1 () = 
	let* x1 = beta 1. 1. in
	let* x2 = (mk 2.) +. ((mk 3.) *.x1) in 
	x2
;;


(* let f2 () = 
	let* x1 = beta 1. 1. in
	let v1 = 
	if((get x1) > 0.5) then
		(mk 1.0)
	else
		(mk 0.0)
	in v1
;; *)

let x = get_samples f1 5 0.02 5000 in
print_statistics x;
print_to_file x "./cont_samples.txt";

