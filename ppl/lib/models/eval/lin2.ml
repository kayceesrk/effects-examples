open Effppl;;
open Effppl.Primitive;;
open Effppl.Infer;;
open Effppl.Print;; 

(*
	The linear regression model
*)
let lin obs_points ax ay () =
	let* m0 = normal 0.0 3.0 in
	let* m1 = normal 0.0 3.0 in
	let* c = normal 1. 5. in 

	for i = 0 to (obs_points-1) do 
		observe (mk ay.(i) -. c -. m0*.mk ax.(i).(0)-. m1*.mk ax.(i).(1)) (logpdf Primitive.(normal 0. 3.))
	done ;
	m0
;; 

(*setting constants*)
let epochs = Stdlib.int_of_string (Sys.argv.(1)) in
let points = Stdlib.int_of_string (Sys.argv.(2)) in
let dim = 2 in 

Printf.printf "epochs = %d\n" epochs;

let ax = Array.make_matrix points dim 0. in 
let ay = Array.make points 0. in 

for i = 0 to points - 1 do
	ay.(i) <- Primitive.sample (Primitive.normal 0. 1.)
done;

for i = 0 to points - 1 do
	for j = 0 to dim - 1 do
		ax.(i).(j) <- sample Primitive.(continuous_uniform 0. 10.)
	done
done;

for i = 0 to points - 1 do 
	for j = 0 to dim - 1 do 
		ay.(i) <- (Float.add ay.(i) (Float.mul 1.0 ax.(i).(j)))
	done
done;

(* for i = 0 to points - 1 do 
	for j = 0 to dim - 1 do 
		Printf.printf "%f " ax.(i).(j)
	done;
	print_endline ""
done;

for i = 0 to points - 1 do
	Printf.printf "%f " ay.(i)
done; *)

let fils = (hmc (lin points ax ay) 4 0.125 epochs) in 
let mcl = List.map (fun ls -> (List.nth ls 0, List.nth ls 1)) fils in 
let sm =  List.map (fun (ax, _) -> ax) mcl in  
let sc =  List.map (fun (_, ay) -> ay) mcl in  

print_statistics sm;
(* print_normal_list sm; *)

(* print_endline "The constant when sampled generated(original was 2):- "; *)
print_statistics sc;
	
