open Effppl.Infer
open Effppl.Print 

(*
	moving-average model for time series analysis
	A moving average model uses previous errors as predictors for future outcomes. For a moving average model of order Q,  
	MA(Q), there is an overall mean parameter μ and regression coefficients θq for previous error terms. With ϵt
  	being the noise at time t, the model for outcome yt
  	 yt = μ + th1 ϵt-1 + ... + thQ ϵt-Q + ϵt

  	with the noise term ϵt  for outcome  yt modeled as normal noise

*)
let ma obs_points ay () =
	
	let* mu = normal 0. 3. in 
	let* th1 = normal 0. 3. in 
	let* th2 = normal 0. 3. in 
	
	let* ep1 = (mk ay.(0)) -. mu in 
	let* ep2 = (mk ay.(1)) -. mu -. (th1 *. ep1) in 
	
	for i = 2 to (obs_points-1) do 
		ignore (
			let* t1 = ep1 in 
			let* t2 = ep2 in 
			let* ep1 = t2 in 
			let* ep2 = (mk ay.(i)) -. (mu) -. (th1*.ep1) -. (th2*.t1)  in 
			(* Printf.printf "%f\n" (get ep2); *)
			observe (ep2) (Effppl.Primitive.logpdf Effppl.Primitive.(normal 0. 2.));
			(mk 1.)
		)
	done ;
	(mk 1.)
;; 

(*
	We generate the data with beta = 1.1
	and alpha = 2
*)
let epochs = 10000 in
let ls =[0.33569351029137096 ;0.14576864453033417 ;0.5582696098767312 ;0.2763700158229467 ;0.38121362084867233 ;0.1730928091294832 ;0.5978015329258275 ;-0.013285101438061553 ;0.12043137794591877 ;0.30089822532584354 ;0.7136787371735624 ;0.3061880863885969 ;0.5481478392225184 ;0.3419853595660427 ;0.31163540672031254 ;0.6535702049894094 ;0.4622255774542885 ;-0.17276537887816795 ;0.5012348466923106 ;0.4978422136062441 ;-0.3629278205532641 ;0.09614667874874475 ;0.32409392451302893 ;0.14557254373332326 ;-0.19115446202026515 ;0.24275823209323744 ;-0.1502150172026271 ;0.03418669652527867 ;-0.2066920086347102 ;0.327369481530889 ;-0.0027832817888945316 ;0.421053647656494 ;0.5033730127044225 ;0.03151250925002713 ;0.23187695895791993 ;0.648786214041563 ;0.26622655392865674 ;0.6668691594821078 ;0.6346169137199793 ;0.7340801083268693 ;0.4363155789722333 ;0.25880371084954074 ;0.025719784675716895 ;0.5379364868199659 ;0.26509131670647473 ;0.11475750500533502 ;0.18695986471345336 ;0.0952093380455461 ;0.4258046665071741 ;0.7235568784889788 ;0.11715081920112286 ;0.5923232640142078 ]in
let ax = Array.of_list ls in 
let obs = Array.length ax in 

let fils = (hmc (ma obs ax) 4 0.125 epochs) in

let mcl = List.map (fun ls -> (List.nth ls 0, List.nth ls 1, List.nth ls 2)) fils in 
let sm =  List.map (fun (ax, _, _) -> ax) mcl in  
let sc =  List.map (fun (_, ay, _) -> ay) mcl in  
let sz =  List.map (fun (_, _, az) -> az) mcl in  

print_endline "===============================\nPrinting statistics for mu\n===============================";
print_statistics sm;
print_endline "===============================\nPrinting statistics for theta 1\n===============================";
print_statistics sc;
print_endline "===============================\nPrinting statistics for theta 2\n===============================";
print_statistics sz;

(* let mns = Owl_stats.mean sma in  *)
(* let mnc = Owl_stats.mean sca in *)
(* print_normal_list sm; *)
(* Printf.printf "%f %f \n" mns mnc;  *)

(*
	The beta converges fast, but the alpha requires a lot of data, because of its lower relevance.
*)