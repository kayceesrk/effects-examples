open Effppl.Infer
open Effppl.Print

let f1 () = 
	let* x1 = normal 0. 1. in
	let* x2 = normal 0. 1. in
	(mk 2.0)*.x1 +. x1*.x2
;;
(*
	if X ~ N(m1, s1) and Y ~ N(m2, s2) 
	we know that the resulting distribution is also normal
	furthermore the resulting distribution will have
	mu = m1 + m2
	and 
	s^2 = s1^2 + s2^2
*)

let (grd , smp, _) = grad f1 in 
print_list grd;
print_normal_list smp;


(* let x = get_samples f1 3 0.02 10000 in
let ax = Array.of_list x in
let am = (Owl_stats.mean ax) in
let ast = (Owl_stats.median ax) in
Printf.printf "Mean = %f \n" am;
Printf.printf "Median = %f \n" ast; *) 
(*
	We therefore expect for large enough samples 
	mean = 2
	std.dev. = 5
	Since this is sampling its bound to be slightly inaccurate.
*)


(* let l1 = [0.; 0.] in  
let l2 = [1. ;1.] in  
let m = 2. in 
let x1 = listadd' l1 l2 m 0 in 
print_normal_list x1; *)