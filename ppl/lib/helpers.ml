module type S = sig
  
	val get_mean : float list -> float

	val get_median :  float list -> float

	val get_min_max : float list -> (float * float)

	val get_var : float list -> float

	val get_std : float list -> float

	val get_mean' : float array -> float

	val get_median' :  float array -> float

	val get_min_max' : float array -> (float * float)

	val get_var' : float array -> float

	val get_std' : float array -> float

end 

module Basics : S =

struct

	let get_mean' =
		Owl_stats.mean 

	let get_median' =
		Owl_stats.median

	let get_min_max' =
		Owl_stats.minmax

	let get_var' ar = 
		Owl_stats.var ar

	let get_std' ar = 
		Owl_stats.std ar

	let get_mean ls =
		get_mean' (Array.of_list ls)

	let get_median ls =
		get_median' (Array.of_list ls)

	let get_min_max ls =
		get_min_max' (Array.of_list ls)

	let get_var ls = 
		get_var' (Array.of_list ls)

	let get_std ls = 
		get_std' (Array.of_list ls)
end;;
