(*open Base*)

exception NotImplemented
exception NotContinous

type 'a support =
  | DiscreteFinite of 'a list
  | DiscreteInfinite
  | ContinuousFinite of ('a * 'a) list (* set of endpoints *)
  | ContinuousInfinite
  | ContinuousPositive of 'a
  | Merged of 'a support * 'a support

(* type 'a support = Discrete of 'a list | Continuous *)

let merge_supports = function
  | DiscreteFinite xs, DiscreteFinite xs' -> DiscreteFinite (xs @ xs')
  | ContinuousFinite xs, ContinuousFinite xs' -> ContinuousFinite (xs @ xs')
  | x, x' when Base.Poly.(x = x') -> x
  | x, x' -> Merged (x, x')

module type PRIM_DIST = sig
  type t

  val sample : unit -> t

  (* val sample_n : int -> t  *)

  val pdf : t -> float
  
  val logpdf : t -> float

  val cdf : t -> float

  val ppf : t -> float

  val der : t -> float

  val params : t list

  val support : t support
  
end

type 'a t = (module PRIM_DIST with type t = 'a)

let binomial n p = 
( 
	module struct
	    type t = int

	    let sample () = Owl_stats.binomial_rvs ~n ~p
      let pdf =
Owl_stats.binomial_pdf ~n ~p
	    let logpdf =
			Owl_stats.binomial_logpdf ~n ~p
	    let cdf =
				Owl_stats.binomial_cdf ~n ~p
	    let ppf _ =
					raise NotImplemented
	    let support = DiscreteFinite (List.init (n+1) (fun x->x))
      let der _ =
					raise NotContinous
      let params =
					raise NotContinous

  	end : PRIM_DIST with type t = int 
)

let categorical (type a) xs =
let xs_arr = Base.Array.of_list_map ~f:fst xs in
let ps = Base.Array.of_list_map ~f:snd xs in
( 
	module struct
		type t = a

		let sample () =
		  xs_arr.(Owl_stats.categorical_rvs ps)

		let pdf x =
		  let rec lookup l = function
		    | (a, p) :: xs -> if Stdlib.(a = x) then p else lookup l xs
		    | [] -> 0.
		  in
		  lookup x xs

		let cdf _ =
				raise NotImplemented

    let ppf _ =
				raise NotImplemented
		
    let logpdf _ =
				raise NotImplemented

    let der _ =
				raise NotContinous

    let params =
				raise NotContinous

		let support = 
        DiscreteFinite (Base.List.map ~f:fst xs)
		
    end : PRIM_DIST
	with type t = a 
)

let normal mu sigma =
( 
	module struct
    type t = float

    let sample () =
				 let x = Owl_stats.gaussian_rvs ~mu ~sigma in 
    (* print_float x; *) x
    let pdf =
				 Owl_stats.gaussian_pdf ~mu ~sigma
    let logpdf =
				 Owl_stats.gaussian_logpdf ~mu ~sigma
    let cdf =
				 Owl_stats.gaussian_cdf ~mu ~sigma
    let ppf =
				 Owl_stats.gaussian_ppf ~mu ~sigma
    let support =
				 ContinuousInfinite
    let der =
				 ( fun x->
      let s2p = 2.50662827463 in
      let x' = x -. mu in
      let coeff1 =  Caml.Float.neg (x' /. (s2p *. sigma *. sigma *. sigma)) in
      let coeff2 =  Caml.Float.neg ((x' *. x') /. (2.0 *. sigma *. sigma)) in
      (coeff1 *. (Float.exp coeff2))
    )
    let params =
				 mu :: [sigma]

  end : PRIM_DIST with type t = float 
)

let discrete_uniform a b =
(
   	module struct
    type t = int 

    let sample () =
				 a + Random.int (b-a+1)
    let pdf x = 
      	if  ( (x>=a) && (x<=b)) then 1. /. (float_of_int (b-a+1)) 
      	else 0. 

    let cdf x = 
        (min 1. 
          (max 0. 
            ( float_of_int (1+x)) /. (float_of_int @@ (b-a+1))))

    let ppf _ =
				
				 raise NotImplemented
    let logpdf _ =
				 raise NotImplemented

    let support =
				 DiscreteFinite (List.init (b-a+1) (fun x-> x +a))

    let der _ =
				 raise NotContinous

    let params =
				 raise NotContinous

  end : PRIM_DIST
    with type t = int 
)
open Owl.Maths


let beta a b =
( 
	module struct
	    
	    type t = float

	    let sample () =
				 Owl_stats.beta_rvs ~a ~b
      let pdf =
				 Owl_stats.beta_pdf ~a ~b
	    let logpdf =
				 Owl_stats.beta_logpdf ~a ~b
	    let cdf =
				 Owl_stats.beta_cdf ~a ~b
	    let ppf =
				 Owl_stats.beta_ppf ~a ~b
	    let support =
				 ContinuousFinite [ (0., 1.) ]
      let der =
				 fun x -> (
        let be alp bet =
          gamma (alp +. bet) /. (gamma alp *. gamma bet)
        in 
        let lhv = (a-.1.) *. ((Float.pow x (a-.2.)) *. (Float.pow (1.-.x) (b-.1.))) in
        let rhv = (b-.1.) *. ((Float.pow x (a-.1.)) *. (Float.pow (1.-.x) (b-.2.))) in
        (lhv-. rhv) /. (be a b)
      )
  
      let params =
				 a :: [b]


  	end : PRIM_DIST with type t = float 
)

let gamma shape scale =
( 
	module struct
	    
	    type t = float
	    let sample () =
				 Owl_stats.gamma_rvs ~shape ~scale
      let pdf =
				 Owl_stats.gamma_pdf ~shape ~scale
	    let logpdf =
				 Owl_stats.gamma_logpdf ~shape ~scale
	    let cdf =
				 Owl_stats.gamma_cdf ~shape ~scale
	    let ppf _ =
				 raise NotImplemented
	    let support =
				 ContinuousPositive 0.
      let der =
				 fun x -> (
        let vl =  Owl_stats.gamma_pdf ~shape:shape ~scale:scale x in
        vl*. (((shape -. 1.) /. x) -. (1. /. scale))
      )
      let params =
				 shape :: [scale]


    end : PRIM_DIST with type t = float 
)

let continuous_uniform a b =
( 	
	module struct
    
	    type t = float

	    let sample () =
				 Owl_stats.uniform_rvs ~a ~b
      let pdf =
				 Owl_stats.uniform_pdf ~a ~b
	    let logpdf =
				 Owl_stats.uniform_logpdf ~a ~b
	    let cdf =
				 Owl_stats.uniform_cdf ~a ~b
	    let ppf =
				 Owl_stats.uniform_ppf ~a ~b
	    let support =
				 ContinuousFinite [ (a, b) ]
      let der _ = 0.
      let params = a :: [b]


	end : PRIM_DIST with type t = float 
)

let chi2 df =
( 
	module struct
	    
	    type t = float
	    let sample () =
				 Owl_stats.chi2_rvs ~df
      let pdf =
				 Owl_stats.chi2_pdf ~df
	    let logpdf =
				 Owl_stats.chi2_logpdf ~df
	    let cdf =
				 Owl_stats.chi2_cdf ~df
	    let ppf =
				 Owl_stats.chi2_ppf ~df
	    let support =
				 ContinuousInfinite
      let der =
				 fun x->  (( (((df /. 2.) -. 1.) /. x) -. 0.5) *. (Owl_stats.chi2_pdf ~df:df  x))

      let params =
				 [df]

    end : PRIM_DIST with type t = float 
)

let f dfnum dfden =
( 
	module struct
	    
	    type t = float
	    let sample () =
				 Owl_stats.f_rvs ~dfnum ~dfden
      let pdf =
				 Owl_stats.f_pdf ~dfnum ~dfden
	    let logpdf =
				 Owl_stats.f_logpdf ~dfnum ~dfden
	    let cdf =
				 Owl_stats.f_cdf ~dfnum ~dfden
	    let ppf =
				 Owl_stats.f_ppf ~dfnum ~dfden
	    let support =
				 ContinuousInfinite
      let der _ =
				 raise NotImplemented
      let params =
				 dfnum :: [dfden]


    end : PRIM_DIST with type t = float 
)



let exponential lambda =
( 
	module struct
	    
	    type t = float
	    let sample () =
				 Owl_stats.exponential_rvs ~lambda
      let pdf =
				 Owl_stats.exponential_pdf ~lambda 
	    let logpdf =
				 Owl_stats.exponential_logpdf ~lambda 
	    let cdf =
				 Owl_stats.exponential_cdf ~lambda
	    let ppf =
				 Owl_stats.exponential_ppf ~lambda 
	    let support =
				 ContinuousInfinite
      let der =
				 fun x -> (
        lambda *. (Owl_stats.exponential_pdf ~lambda:lambda x)
      )
      let params =
				 [lambda]


    end : PRIM_DIST with type t = float 
)

let cauchy loc scale =
(
	module struct
	    
	    type t = float
	    let sample () =
				 Owl_stats.cauchy_rvs ~loc ~scale
      let pdf =
				 Owl_stats.cauchy_pdf  ~loc ~scale
	    let logpdf =
				 Owl_stats.cauchy_logpdf  ~loc ~scale
	    let cdf =
				 Owl_stats.cauchy_cdf  ~loc ~scale
	    let ppf =
				 Owl_stats.cauchy_ppf  ~loc ~scale
	    let support =
				 ContinuousInfinite
      let der =
				 fun x -> (
        let vl =(Owl_stats.cauchy_pdf ~loc:loc ~scale:scale x) in
        let n2p = -6.28318530718 in
        vl*.vl*.n2p*.(x -. loc)*.scale
      )
      let params = [loc;scale]

    end : PRIM_DIST with type t = float 

)



let pdf (type a) d =
  let (module D : PRIM_DIST with type t = a) = d in
  D.pdf

let der (type a) d =
  let (module D : PRIM_DIST with type t = a) = d in
  D.der

let logpdf (type a) d =
  let (module D : PRIM_DIST with type t = a) = d in
  D.logpdf

let logder (type a) d x =
  let (module D : PRIM_DIST with type t = a) = d in
  (D.der x) /. (D.pdf x)

let cdf (type a) d =
  let (module D : PRIM_DIST with type t = a) = d in
  D.cdf

let sample (type a) d =
  let (module D : PRIM_DIST with type t = a) = d in
  D.sample ()

let support (type a) d =
  let (module D : PRIM_DIST with type t = a) = d in
  D.support

let params (type a) d =
  let (module D : PRIM_DIST with type t = a) = d in
  D.params

let ppf (type a) d =
  let (module D : PRIM_DIST with type t = a) = d in
  D.ppf




let create_primitive (type a) ~sample ~pdf ~cdf ~support ~ppf ~der ~params ~logpdf =
  let module D = struct
    type t = a

    let sample = sample

    let pdf = pdf
    
    let logpdf = logpdf

    let cdf = cdf

    let ppf = ppf

    let der = der

    let params = params

    let support = support
  end in
  (module D : PRIM_DIST with type t = a)


let poisson l =
  ( module struct
    type t = int

    let sample () =
      let l' = ref (exp (-.l)) in
      let k = ref 0 in
      let p = ref 1. in
      while Base.Float.(!p > !l') do
        k := !k + 1;
        let u = Owl_stats.uniform_rvs ~a:0. ~b:1. in
        p := !p *. u
      done;
      !k - 1

    let pdf k = 
      ((Float.pow l (float_of_int k)) /. (fact k *. (Float.exp l)))

    let cdf k = gammainc (float_of_int (k + 1)) l /. fact k

    let ppf _ = raise NotImplemented
    let logpdf _ = raise NotImplemented
    let der _ = raise NotContinous
    let params = raise NotContinous

    let support = DiscreteInfinite
  end : PRIM_DIST
    with type t = int )

let geometric p =
(
   	module struct
    type t = int 

    let sample () = let x = Random.float 1. in (Int.of_float @@ (Float.log x /. (Float.log (1. -. p))))
    
    let pdf x =  if (x>0) then (p *. (Float.pow (1. -. p) (Float.of_int @@ (x - 1)) ))
	else 0.

    let cdf x = if (x>0) then let cmp = (1. -. p)  in (1. -. (Float.pow cmp (Float.of_int x))) else 0.
    
    let ppf _ = raise NotImplemented

    let support = DiscreteInfinite
    let der _ = raise NotContinous
    let params = raise NotContinous
    let logpdf _ = raise NotImplemented


  end : PRIM_DIST
    with type t = int 
)