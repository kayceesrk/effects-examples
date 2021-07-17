open Effppl.Infer;;
open Effppl.Print;; 

let f1 () =
	let* x = uniform 0. 1. in 

	let* x1 = uniform 0. 1. in 
	let* x2 = uniform 0. 1. in 
	let* x3 = uniform 0. 1. in 

	let x1' = if((get x) > (get x1)) then 1.0 else 0.0 in 
	let x2' = if((get x) > (get x2)) then 1.0 else 0.0 in 
	let x3' = if((get x) > (get x3)) then 1.0 else 0.0 in 

	Printf.printf "%f \n" (get x);

	let* total = (mk x1') +. (mk x2') +. (mk x3')  in 
	observe total (fun x-> if x=2.0 then 0.0 else -1000.0);

	total
;;

let epochs = Stdlib.int_of_string (Sys.argv.(1)) in
let fils = (hmc f1 2 0.00001 epochs) in
let mcl = List.map (fun ls -> (List.nth ls 0)) fils in 
let sma =  Array.of_list mcl in
let mm = Owl_stats.mean sma in 
Printf.printf "%d %f\n" epochs mm;   
print_to_file mcl "coin_weights.txt"
