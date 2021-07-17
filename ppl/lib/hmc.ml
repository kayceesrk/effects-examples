exception Unknown

open Primitive

(* type 'a support =
  | DiscreteFinite of 'a list
  | DiscreteInfinite
  | ContinuousFinite of ('a * 'a) list
  | ContinuousInfinite
  | ContinuousPositive of 'a
  | Merged of 'a support * 'a support *)

module type S = sig
  
	type t
	
	val mk : float -> t
	
	val (+.) :  t -> t -> t
	val (-.) :  t -> t -> t
	val ( *. ) : t ->  t ->  t
	val ( /. ) : t ->  t ->  t
	val ( let* ) :  t -> (t -> t ) ->  t

	val grad  : ( unit ->  t) -> (t list * float list * float Primitive.t list)

	val normal' : t -> t -> t
	val normal : float -> float -> t

	val cauchy' : t -> t -> t	
	val cauchy : float -> float -> t

	val beta' : t -> t -> t
	val beta : float -> float -> t

	val gamma' : t -> t -> t
	val gamma : float -> float -> t

	val poly': t -> t -> t -> t
	val poly: float -> float -> int -> t

	val exp' : t -> t
	val exp : float -> t

	val chi2' : t -> t
	val chi2 : float -> t

	val uniform : float -> float -> t

	val get : t -> float
	val get_der : ( unit ->  t) -> float list -> (float * t list)
	val get_val : ( unit ->  t) -> float list -> (float)
	val cond: bool -> t -> t -> t
	val hmc : ( unit ->  t)  -> int -> float -> int -> float list list

	val get_samples: ( unit ->  t)  -> int -> float -> int -> float list
	val observe: t -> (float -> float) -> unit

	val listadd': float list -> float list -> float -> int -> float list
	val print_list : t list -> unit
	
end 

module Infer : S =

struct

	type t = { v : float;  mutable d : float ; m : float}

	let mk v = 
		{v; d = 0.0; m=1.0}
	
	let get t' = 
		match t' with 
			{v= v' ; d = _ ;  m = _} -> v'

	effect Add : t * t -> t
	effect Sub : t * t -> t
	effect Mult : t * t -> t
	effect Div : t * t ->  t
	effect Obs : t * (float -> float) ->  t
	effect Norm : t * t ->  t
	effect Cauc : t * t ->  t
	effect Beta : t * t ->  t
	effect Gamma : t * t ->  t
	effect Leet : t * (t -> t) -> t
	
	let print_list ls = 
		List.iter 
  		(
  			fun {v=v'; d=d'; m=_} -> 
	  			Caml.Printf.printf "%f %f \n" v' (d');
  		) ls

	
	let rec find_list v ls = 
		match ls with 
		| [] -> 
			raise Unknown
		| {v = v1; d= d1; m=_}::tl -> 
			if(v=v1) then d1 else find_list v tl

  	let modif_der ls vc dc = 
  		List.map 
  		(
  			fun {v=v'; d=d'; m=m'} -> 
	  			if(v'=vc) then  {v=v'; d=dc; m=m'}
	  			else {v=v'; d=d'; m=m'}
  		)  ls

  	let rec run_grad f ls sls pls=
		match f () with
		| r -> 
			(* print_endline "in r"; *)
			r.d <- 1.0; 
			ls := modif_der !ls r.v r.d;
			(r)
		
		| effect (Obs(_,_)) k ->  
			let x = {v = 0.0; d = 0.; m=1.} in
			(* print_endline "in obs";
			Printf.printf "Value %f\n" tu.v;
			Printf.printf "probab %f\n" (f tu.v); *)
			ignore (continue k x);
			(x)

		| effect (Add(a,b)) k ->
			(* print_endline "in add"; *)
			let x = {v = a.v +. b.v; d = 0.; m=1.} in
			ignore (continue k x);
			a.d <- a.d +. x.d;
			b.d <- b.d +. x.d; 
			ls := modif_der !ls a.v a.d;
			ls := modif_der !ls b.v b.d;
			(x)
		
		| effect (Sub(a,b)) k ->
			let x = {v = a.v -. b.v; d = 0.; m=1.} in
			ignore (continue k x);
			a.d <- a.d +. x.d;
			b.d <- b.d -. x.d; 
			ls := modif_der !ls a.v a.d;
			ls := modif_der !ls b.v b.d;
			(x)
		
		| effect (Mult(a,b)) k ->
			(* print_endline "in mult"; *)
			let x = {v = a.v *. b.v; d = 0.;  m=1.} in
			ignore (continue k x);
			a.d <- a.d +. (b.v *. x.d);
			b.d <- b.d +. (a.v *. x.d);
			ls := modif_der !ls a.v a.d;
			ls := modif_der !ls b.v b.d;
			(x)
		
		| effect (Div(a,b)) k ->
			let x = {v = a.v /. b.v; d = 0.;  m=1.} in
			ignore (continue k x);
			a.d <- a.d +. (x.d /. b.v);
			b.d <- b.d +. (a.v *. x.d /. (b.v *. b.v));
			ls := modif_der !ls a.v a.d;
			ls := modif_der !ls b.v b.d;
			(x)
		
		| effect (Leet(m,f')) _ ->
			(* print_endline "in let"; *)
			let x = {v = m.v; d = 0.0; m=m.m} in 
			ls := !ls@[x];
			(* List.iter (fun {v=v'; d=_; m=_} -> Printf.printf "%f " v';) !ls; print_endline ""; *)
			let (x1) = (run_grad (fun () -> f' m) ls sls pls) in
			(x1)

		| effect (Norm(mu,si)) k ->
			let m = get(mu) in
			let s = get(si) in
			let p = Primitive.(normal m s) in 
			let v1 = Primitive.sample p in
			sls := !sls@[v1];
			pls := !pls@[p];
			let v2 = Primitive.logder p v1 in
			let x = {v=v1; d=0.0 ; m= v2 } in
			ignore (continue k x);
			(*print_endline "in norm";
			Printf.printf "Finding %f\n" v1;
			List.iter (fun {v=v'; d=_; m=_} -> Printf.printf "%f " v';) !ls; print_endline ""; *)
			let dn = find_list v1 !ls  in 
			ls := modif_der !ls v1 (dn (* *. v2 *));
			ls := modif_der !ls m (mu.d +. x.d *. v2);
			let vf = (((v1 -. m) *. (v1 -. m)) -. (s *. s))/. (s *. s *. s) in
			ls := modif_der !ls s (si.d +. x.d *. vf);
			(x)


		| effect (Cauc(mu,si)) k ->
			let m = get(mu) in
			let s = get(si) in
			let p = Primitive.(cauchy m s) in 
			let v1 = Primitive.sample p in
			sls := !sls@[v1];
			pls := !pls@[p];
			let v2 = Primitive.logder p v1 in
			let x = {v=v1; d=0.0 ; m= v2 } in
			ignore (continue k x);
			let dn = find_list v1 !ls  in 
			ls := modif_der !ls v1 (dn *. v2);
			ls := modif_der !ls m (mu.d +. x.d *. v2);
			(x)
		
		| effect (Beta(a1,b1)) k ->
			let a = get(a1) in
			let b = get(b1) in
			let p = Primitive.(beta a b) in 
			let v1 = Primitive.sample p in
			sls := !sls@[v1];
			pls := !pls@[p];
			let v2 = Primitive.logder p v1 in
			let x = {v=v1; d=0.0 ; m= v2 } in
			ignore (continue k x);
			let dn = find_list v1 !ls  in 
			ls := modif_der !ls v1 (dn *. v2);
			ls := modif_der !ls a (a1.d +. x.d *. Float.log(v1));
			ls := modif_der !ls b (b1.d +. x.d *. Float.log(1.0 -. v1));
			(x)

		| effect (Gamma(k1,t1)) c ->
			let k = get(k1) in
			let t = get(t1) in
			let p = Primitive.(gamma k t) in 
			let v1 = Primitive.sample p in
			sls := !sls@[v1];
			pls := !pls@[p];
			let v2 = Primitive.logder p v1 in
			let x = {v=v1; d=0.0 ; m= v2 } in
			ignore (continue c x);
			let dn = find_list v1 !ls  in 
			ls := modif_der !ls v1 (dn *. v2);
			let td = (v1 /. (t *. t)) -. (k /. t) in 
			let kd = (Float.log (v1 /. t)) -. (Owl_maths.psi k)  in 
			ls := modif_der !ls k (k1.d +. x.d *. kd);
			ls := modif_der !ls t (t1.d +. x.d *. td);
			(x)

  	let grad f =
  		let rls = ref [] in 
  		let sls = ref [] in 
  		let pls = ref [] in 
		(* print_endline "-2";		 *)
		let _ = run_grad f rls sls pls in 
		(!rls,!sls,!pls)

	let rec get_val' f ls' = 
		match f () with 
		| r -> 
			r.d <- 1.0; 
			r

		| effect (Add(a,b)) k ->
			let x = {v = a.v +. b.v; d = 0.; m=1.} in
			(continue k x);
		
		| effect (Sub(a,b)) k ->
			let x = {v = a.v -. b.v; d = 0.; m=1.} in
			(continue k x);
			
		
		| effect (Mult(a,b)) k ->
			let x = {v = a.v *. b.v; d = 0.;  m=1.} in
			(continue k x);
			
		| effect (Obs(_,_)) k ->
			let x = {v = 0.0; d = 0.;  m=1.} in
			(continue k x);


		| effect (Div(a,b)) k ->
			let x = {v = a.v /. b.v; d = 0.;  m=1.} in
			(continue k x);
			
		
		| effect (Leet(m,f')) _ ->
			(* let x = {v = m.v; d = 0.0; m=m.m} in  *)
			let x1 = (get_val' (fun () -> f' m) ls')  in
			x1


		| effect (Norm(mu,si)) k -> begin
			match !ls' with 
			| [] -> raise Unknown
			| v1::tl -> 
				let m = get(mu) in
				let s = get(si) in
				let p = Primitive.(normal m s) in 
				let v2 = Primitive.logder p v1 in
				let x = {v=v1; d=0.0 ; m= v2 } in
				ls' := tl;
				(continue k x);
					
		end

		| effect (Cauc(mu,si)) k -> begin
			match !ls' with 
			| [] -> raise Unknown
			| v1::tl -> 
				let m = get(mu) in
				let s = get(si) in
				let p = Primitive.(cauchy m s) in 
				let v2 = Primitive.logder p v1 in
				let x = {v=v1; d=0.0 ; m= v2 } in
				ls' := tl;
				(continue k x);
					
		end

		| effect (Beta(a1,b1)) k -> begin
			match !ls' with 
			| [] -> raise Unknown
			| v1::tl ->
				let a = get(a1) in
				let b = get(b1) in
				let p = Primitive.(beta a b) in 
				let v2 = Primitive.logder p v1 in
				let x = {v=v1; d=0.0 ; m= v2 } in
				ls' := tl;
				(continue k x);
				
		end

		| effect (Gamma (k1,t1)) c -> begin
			match !ls' with 
			| [] -> raise Unknown
			| v1::tl ->
				let k = get(k1) in
				let t = get(t1) in
				let p = Primitive.(gamma k t) in  
				let v2 = Primitive.logder p v1 in
				let x = {v=v1; d=0.0 ; m= v2 } in
				ls' := tl;
				(continue c x);
				
		end

	let get_val f sls =
		let rsls = ref sls in 
		let x1 = ( get_val' f rsls) in
		(x1.v)

	let rec get_der' f ls' ls = 
		match f () with 
		| r -> 
			r.d <- 1.0; 
			ls := modif_der !ls r.v r.d;
			(r,ls)
		
		| effect(Obs(_,_)) k ->
			let x = {v = 0.0; d = 0.; m=1.} in
			let (rv,_) =  (continue k x) in
			(rv, ls)

		| effect (Add(a,b)) k ->
			let x = {v = a.v +. b.v; d = 0.; m=1.} in
			let (rv,_) =  (continue k x) in
			a.d <- a.d +. x.d;
			b.d <- b.d +. x.d; 
			ls := modif_der !ls a.v a.d;
			ls := modif_der !ls b.v b.d;
			(rv, ls)
		
		| effect (Sub(a,b)) k ->
			let x = {v = a.v -. b.v; d = 0.; m=1.} in
			let (rv,_) =  (continue k x) in
			a.d <- a.d +. x.d;
			b.d <- b.d -. x.d; 
			ls := modif_der !ls a.v a.d;
			ls := modif_der !ls b.v b.d;
			(rv, ls)
		
		| effect (Mult(a,b)) k ->
			let x = {v = a.v *. b.v; d = 0.;  m=1.} in
			let (rv,_) =  (continue k x) in
			a.d <- a.d +. (b.v *. x.d);
			b.d <- b.d +. (a.v *. x.d);
			ls := modif_der !ls a.v a.d;
			ls := modif_der !ls b.v b.d;
			(rv, ls)
		
		| effect (Div(a,b)) k ->
			let x = {v = a.v /. b.v; d = 0.;  m=1.} in
			let (rv,_) =  (continue k x) in
			a.d <- a.d +. (x.d /. b.v);
			b.d <- b.d +. (a.v *. x.d /. (b.v *. b.v));
			ls := modif_der !ls a.v a.d;
			ls := modif_der !ls b.v b.d;
			(rv, ls)
		
		| effect (Leet(m,f')) _ ->
			let x = {v = m.v; d = 0.0; m=m.m} in 
			ls := !ls@[x];
			let (x1,_) = (get_der' (fun () -> f' m) ls' ls)  in
			(x1,ls)


		| effect (Norm(mu,si)) k -> begin
			match !ls' with 
			| [] -> raise Unknown
			| v1::tl -> 
				let m = get(mu) in
				let s = get(si) in
				let p = Primitive.(normal m s) in 
				let v2 = Primitive.logder p v1 in
				let x = {v=v1; d=0.0 ; m= v2 } in
				ls' := tl;
				let (rv,_) =  (continue k x) in
				
				let dn = find_list v1 !ls  in 
				ls := modif_der !ls v1 (dn *. v2);
				mu.d <- mu.d +. x.d *. v2;
				ls := modif_der !ls m (mu.d);
				let vf = (((v1 -. m) *. (v1 -. m)) -. (s *. s))/. (s *. s *. s) in
				ls := modif_der !ls s (si.d +. x.d *. vf);
				(rv, ls)
		end

		| effect (Cauc(mu,si)) k -> begin
			match !ls' with 
			| [] -> raise Unknown
			| v1::tl -> 
				let m = get(mu) in
				let s = get(si) in
				let p = Primitive.(cauchy m s) in 
				let v2 = Primitive.logder p v1 in
				let x = {v=v1; d=0.0 ; m= v2 } in
				ls' := tl;
				let (rv,_) =  (continue k x) in
				
				let dn = find_list v1 !ls  in 
				ls := modif_der !ls v1 (dn *. v2);
				mu.d <- mu.d +. x.d *. v2;
				ls := modif_der !ls m (mu.d);
				(rv, ls)
		end

		| effect (Beta(a1,b1)) k -> begin
			match !ls' with 
			| [] -> raise Unknown
			| v1::tl ->
				let a = get(a1) in
				let b = get(b1) in
				(* Printf.printf "Beta %f\n" v1; *)
				let p = Primitive.(beta a b) in 
				let v2 = Primitive.logder p v1 in
				let x = {v=v1; d=0.0 ; m= v2 } in
				ls' := tl;
				let (rv,_) =  (continue k x) in
					
				let dn = find_list v1 !ls  in 
				ls := modif_der !ls v1 (dn *. v2);
				ls := modif_der !ls a (a1.d +. x.d *. Float.log(v1));
				ls := modif_der !ls b (b1.d +. x.d *. Float.log(1.0 -. v1));
				(rv, ls)
		end

		| effect (Gamma(k1,t1)) c -> begin
			match !ls' with 
			| [] -> raise Unknown
			| v1::tl ->
				let k = get(k1) in
				let t = get(t1) in
				(* Printf.printf "Gamma %f\n" v1; *)
				let p = Primitive.(gamma k t) in 
				let v2 = Primitive.logder p v1 in
				let x = {v=v1; d=0.0 ; m= v2 } in
				ls' := tl;
				let (rv,_) =  (continue c x) in
				let dn = find_list v1 !ls  in 
				ls := modif_der !ls v1 (dn *. v2);
				let td = (v1 /. (t *. t)) -. (k /. t) in 
				let kd = (Float.log (v1 /. t)) -. (Owl_maths.psi k)  in 
				ls := modif_der !ls k (k1.d +. x.d *. kd);
				ls := modif_der !ls t (t1.d +. x.d *. td);
				(rv, ls)
		end

	let get_der f sls =
		let rls = ref [] in
		let rsls = ref sls in 
		let (x1, _) = ( get_der' f rsls rls ) in
		(x1.v, !rls)

  	let normal' mu si = 
  		perform (Norm(mu,si))
  	let normal mu si = 
  		normal' (mk mu) (mk si)

	let cauchy' mu si = 
  		perform (Cauc(mu,si))
  	let cauchy mu si = 
  		cauchy' (mk mu) (mk si)

	let beta' a b = 
		perform (Beta(a,b))
 	let beta a b = 
		beta' (mk a) (mk b)

	

 	let gamma' k t = 
		perform (Gamma(k,t))
	let gamma k t = 
		gamma' (mk k) (mk t)

	let poly' st en d =
		perform (Mult 
			(perform (Sub 
				(en,st)), 
			perform(Add(
				st,
				perform(Beta
					(d,
					(mk 1.)))))))
	let poly st en d =
		let d' = Float.of_int d in
		poly' (mk st) (mk en) (mk d')


 	let observe t fu =
 		ignore (perform (Obs(t,fu)))

 	let norm_list n = 
 		List.init n (fun _ -> Primitive.sample (Primitive.normal 0. 1.)) 

 	let rec listadd' l1 l2 mul ind =
 		match l1 with
 		| [] -> []
 		| h1::t1 -> begin
 			match l2 with 
 			| [] -> raise Unknown
 			| h2::t2 -> 
 				if(ind = 0) then
 					(h1 +. (mul *. h2))::(listadd' t1 t2 mul (ind-1))
 				else
 					(h1)::(listadd' t1 t2 mul (ind-1))
 		end

 	let listadd l1 l2 mul= 
 		List.map2 (fun x y -> x +. (y *. mul) ) l1 l2

 	let rec subs l1 l2 l3= 
 		match l1 with 
 		| [] -> l3
 		| hd::tl -> 
 			match l2 with 
 			| [] -> l3
 			| h::t -> 
 				if(h=hd.v) then
 					subs tl t (hd.d::l3)
 				else
 					subs tl l2 l3

 	let rec get_obs_log' f ls' vl = 
		match f () with 
		| r -> 
			r.d <- 1.0; 
			r

		| effect (Add(a,b)) k ->
			let x = {v = a.v +. b.v; d = 0.; m=1.} in
			(continue k x);
		
		| effect (Sub(a,b)) k ->
			let x = {v = a.v -. b.v; d = 0.; m=1.} in
			(continue k x);
		
		| effect (Mult(a,b)) k ->
			let x = {v = a.v *. b.v; d = 0.;  m=1.} in
			(continue k x);
	
			
		| effect (Div(a,b)) k -> begin
			let x = {v = a.v /. b.v; d = 0.;  m=1.} in
			(continue k x);
		end
			
		| effect (Obs(tu,f)) k -> begin
			let x = {v = 0.0; d = 0.;  m=1.} in
			(* Printf.printf "%f %f \n" tu.v (f tu.v) ;  *)
			ignore (vl := !vl -. ((f tu.v)));
			(continue k x);
		end		
		
		| effect (Leet(m,f')) _ ->
			let x1 = (get_obs_log' (fun () -> f' m) ls' vl)  in
			x1

		| effect (Norm(mu,si)) k -> begin
			match !ls' with 
			| [] -> raise Unknown
			| v1::tl -> 
				let m = get(mu) in
				let s = get(si) in
				let p = Primitive.(normal m s) in 
				let v2 = Primitive.logder p v1 in
				let x = {v=v1; d=0.0 ; m= v2 } in
				ls' := tl;
				(continue k x);
					
		end


		| effect (Cauc(mu,si)) k -> begin
			match !ls' with 
			| [] -> raise Unknown
			| v1::tl -> 
				let m = get(mu) in
				let s = get(si) in
				let p = Primitive.(cauchy m s) in 
				let v2 = Primitive.logder p v1 in
				let x = {v=v1; d=0.0 ; m= v2 } in
				ls' := tl;
				(continue k x);
					
		end

		| effect (Beta(a1,b1)) k -> begin
			match !ls' with 
			| [] -> raise Unknown
			| v1::tl ->
				let a = get(a1) in
				let b = get(b1) in
				let p = Primitive.(beta a b) in 
				let v2 = Primitive.logder p v1 in
				let x = {v=v1; d=0.0 ; m= v2 } in
				ls' := tl;
				(continue k x);
				
		end

		| effect (Gamma (k1,t1)) c -> begin
			match !ls' with 
			| [] -> raise Unknown
			| v1::tl ->
				let k = get(k1) in
				let t = get(t1) in
				let p = Primitive.(gamma k t) in  
				let v2 = Primitive.logder p v1 in
				let x = {v=v1; d=0.0 ; m= v2 } in
				ls' := tl;
				(continue c x);
				
		end


 	let get_obs_log f sls =
		let rsls = ref sls in 
		let vl = ref 0.0 in 
		let _ = ( get_obs_log' f rsls vl) in
		(!vl)


 	let nlp q pls f =
 		assert(List.length q = List.length pls);
 		let a1 = List.fold_left2 
 		( 
 			fun acc y p -> 
 			acc -. (Primitive.logpdf p y)
 		) 0. q pls in 
 		let a2 = get_obs_log f q in 
 		a1 +. a2 

 	let nlp1 p = 
 		List.fold_left 
 		(	
 			fun acc y -> 
 				acc -. (Primitive.logpdf (Primitive.normal 0. 1.) y)
 		) 0. p



 	let get_hmc_grad f q = 
 		(* let () = print_normal_list q in  *)
 		let (_, dv) = get_der f q in
		let dv = List.rev ( subs dv q [] ) in 
		List.map (fun f -> f *. (-. 1.0)) (dv) 

	


 	let rec check_single_support s vl =
 		match s with 
 		| ContinuousPositive v -> begin
 			if(vl > v) then 
 				true
 			else
 				false
 		end
 		| ContinuousInfinite -> begin
 			true
 		end
 		| ContinuousFinite ls -> begin
 			match ls with 
 			| [] -> true 
 			| hd :: tl -> 
 				match hd with (v1,v2) ->
 				if(v1 < vl && vl < v2) then begin 
 					check_single_support (ContinuousFinite tl) (vl)
 				end else
 					false
 		end
 		| _ -> raise Unknown


 	let check_in_support q pls = 
 		assert(List.length q = List.length pls);
 		List.fold_left2
 		(
 			fun bl q p ->
 				if (bl = false) then
 					bl
 				else
 					let sp = Primitive.support p in 
 					if (check_single_support sp q) then
 						bl
 					else
 						false
 		) true q pls



	let rec leapfrog (li:int ) (stp:float) (p1:float list) (q1:float list) f pls =
		let b1 = check_in_support q1 pls in
		if b1 then 
		begin
	 		let dVdQ = get_hmc_grad f q1 in 
	 		let len = List.length dVdQ in 
	 		let _ = Random.int(len) in 
	 		
	 		if(li = 0) then 
	 		begin
	 			Some (p1, q1)
	 		end
	 		else begin
	 			let p1 = listadd p1 dVdQ (stp/.2.0) (* t1 *) in 
	 			let q1 = listadd q1 p1 1.0 (* t1 *) in 
				let b2 = check_in_support q1 pls in
	 			if (b2) then 
	 			begin
		 			let dVdQ1 = get_hmc_grad f q1 in 
		 			let p1 = listadd p1 dVdQ1 (stp/.2.0) (* t1 *) in 
		 			leapfrog (li -1) stp p1 q1 f pls			
		 		end 
		 		else 
		 		begin
		 			None
		 		end
		 	end
		end
		else 
		begin
			None
		end



	let rec hmc' (f: ( unit ->  t) ) (li:int) (stp:float) (ep:int) (samp_list: float list list) (tot_ep:int)  pls =
		if(ep=0) then
			samp_list
		else
			let q0 = List.nth samp_list 0 in
			let q1 = q0 in

			let p0 = norm_list (List.length q1) in
			let p1 = p0 in

			

			let x = leapfrog li stp p1 q1 f pls in
			match x with 
			| None -> 
				hmc' f li stp ep samp_list tot_ep pls
			| Some (p1, q1) -> begin
				let p1 = List.map (fun f -> f *. (-. 1.0)) p1 in 

				if ((check_in_support q0 pls) && (check_in_support q1 pls)) then begin

					let p0p = nlp1 p0 in
					let p1p = nlp1 p1 in
					let q0p = nlp q0 pls f in
					let q1p = nlp q1 pls f in

					let tgt = q0p -. q1p in
					let adj = p1p -. p0p in 
					let acc = tgt +. adj in

					let x' = Primitive.sample (Primitive.continuous_uniform 0. 1.) in
					let x = Float.log x' in
					let () = 
					if((ep mod ((tot_ep)/10)) = 0 ) then 
						Printf.printf "%d epochs done\n" (tot_ep-ep)
					in 
					if (x < acc) then begin
						hmc' f li stp (ep-1) (q1 :: samp_list) tot_ep pls
					end
					else
						hmc' f li stp (ep-1) (q0 :: samp_list) tot_ep pls
				end
				else 
					hmc' f li stp ep samp_list tot_ep pls
			end
				
	let hmc (f: ( unit ->  t) ) (li:int)  (stp:float) (ep:int) : float list list =
		let (_, smp, pls) = grad f in 
		hmc' f li stp (ep-1) [smp] ep pls


	let get_samples (f: ( unit ->  t) ) (li:int ) (stp:float) (ep:int) =
		List.map (fun ll->  get_val f ll ) (hmc f li stp (ep))

	let (+.) a b = 
		perform (Add(a,b))
  	let (-.) a b = 
  		perform (Sub(a,b))
  	let ( *. ) a b = 
  		perform (Mult(a,b))
  	let ( /. ) a b = 
  		perform (Div(a,b))

  	let exp' l =
		perform (Gamma(mk 1.0, (mk 1.0) /. l )) 
	let exp l =
		exp' (mk l)

	let chi2' k =
		perform (Gamma( k /. (mk 2.0) , mk 2.0) )
	let chi2 k =
		chi2' (mk k)

	let uniform st en = 
		if(st = 0.0 && en = 1.0) then
			(beta 1. 1.)
		else
			(mk st) +. ((mk (Float.sub en st)) *. (beta 1. 1.))

  	let (let*) m f = 
  		perform (Leet(m,f))

  	let cond b y n =
  		if b then y else n
end;;
