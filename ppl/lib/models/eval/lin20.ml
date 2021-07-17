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
	let* m2 = normal 0.0 3.0 in
	let* m3 = normal 0.0 3.0 in
	let* m4 = normal 0.0 3.0 in
	let* m5 = normal 0.0 3.0 in
	let* m6 = normal 0.0 3.0 in
	let* m7 = normal 0.0 3.0 in
	let* m8 = normal 0.0 3.0 in
	let* m9 = normal 0.0 3.0 in
	let* m10 = normal 0.0 3.0 in
	let* m11 = normal 0.0 3.0 in
	let* m12 = normal 0.0 3.0 in
	let* m13 = normal 0.0 3.0 in
	let* m14 = normal 0.0 3.0 in
	let* m15 = normal 0.0 3.0 in
	let* m16 = normal 0.0 3.0 in
	let* m17 = normal 0.0 3.0 in
	let* m18 = normal 0.0 3.0 in
	let* m19 = normal 0.0 3.0 in
	let* c = normal 1.0 5.0 in
	for i = 0 to (obs_points-1) do 
		observe (mk ay.(i) -. c -. m0*.mk ax.(i).(0)-. m1*.mk ax.(i).(1)-. m2*.mk ax.(i).(2)-. m3*.mk ax.(i).(3)-. m4*.mk ax.(i).(4)-. m5*.mk ax.(i).(5)-. m6*.mk ax.(i).(6)-. m7*.mk ax.(i).(7)-. m8*.mk ax.(i).(8)-. m9*.mk ax.(i).(9)-. m10*.mk ax.(i).(10)-. m11*.mk ax.(i).(11)-. m12*.mk ax.(i).(12)-. m13*.mk ax.(i).(13)-. m14*.mk ax.(i).(14)-. m15*.mk ax.(i).(15)-. m16*.mk ax.(i).(16)-. m17*.mk ax.(i).(17)-. m18*.mk ax.(i).(18)-. m19*.mk ax.(i).(19)) (logpdf Primitive.(normal 0. 3.))
	done ;
	m0
;; 

(*setting constants*)
let epochs = Stdlib.int_of_string (Sys.argv.(1)) in
let points = Stdlib.int_of_string (Sys.argv.(2)) in
let dim = 20 in 

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
		ay.(i) <- (Float.add ay.(i) (Float.mul 2.0 ax.(i).(j)))
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
	
