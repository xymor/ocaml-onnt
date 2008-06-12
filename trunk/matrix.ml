open Printf

type 'a matrix = 'a array array

exception Not_a_matrix
exception Not_same_dimension
exception Empty_matrix
exception Wrong_dimension

external to_array : 'a matrix -> 'a array array = "%identity"

let (|>) x f = f x

module Matrix = struct
	let of_array = function
		| [|[||]|] -> [|[||]|]
		| m ->
			let col_dim = Array.length m in
			let row_dim = Array.length m.(0) in
			for i=1 to col_dim - 1 do
				if row_dim <> (Array.length m.(i)) then raise Not_a_matrix
			done;
			m
	
	let copy a =
		Array.map (Array.copy) a
	
	let col_dim m =
		Array.length m
		
	let row_dim m =
		Array.length m.(0)
	
	let unsafe_set m i j x =
		Array.unsafe_set (Array.get m i) j x
	
	let get m i j =
		m.(i).(j)

	let unsafe_get m i j =
		Array.unsafe_get (Array.unsafe_get m i) j
	
	let create col_dim row_dim x =
		let res = Array.create col_dim [||] in
		for i=0 to col_dim - 1 do
			Array.unsafe_set res i (Array.create row_dim x)
		done;
		res
	
	let init col_dim row_dim f =
		let res = Array.create col_dim [||] in
		for i=0 to col_dim - 1 do
			Array.unsafe_set res i (Array.create row_dim (f i 0));
			for j=1 to row_dim - 1 do
				Array.unsafe_set (Array.unsafe_get res i) j (f i j)
			done;
		done;
		res
	
	let iter f m =
		let row_dim = Array.length m.(0) in
		for i=0 to (Array.length m) - 1 do
			for j=0 to row_dim - 1 do
				f (unsafe_get m i j)
			done
		done
	
	let iterij f m =
		let row_dim = Array.length m.(0) in
		for i=0 to (Array.length m) - 1 do
			for j=0 to row_dim - 1 do
				f i j (unsafe_get m i j)
			done
		done
	
	let map f m =
		match m with
			| [|[||]|] -> [|[||]|]
			| m ->
				let col_dim = col_dim m in
				let row_dim = row_dim m in
				let res = Array.create col_dim [||] in
				for i=0 to col_dim - 1 do
					let line = Array.unsafe_get m i in
					let first = f (Array.unsafe_get line 0) in
					let new_line = Array.create row_dim first in
					Array.unsafe_set res i new_line;
					for j=1 to row_dim - 1 do
						Array.unsafe_set new_line j (f (Array.unsafe_get line j))
					done
				done;
				res
	
	let mapij (f : int -> int -> 'a -> 'b) = function
		| [|[||]|] -> [|[||]|]
		| m ->
			let col_dim = col_dim m in
			let row_dim = row_dim m in
			let res = Array.create col_dim [||] in
			for i=0 to col_dim - 1 do
				let line = Array.unsafe_get m i in
				let first = f i 0 (Array.unsafe_get line 0) in
				let new_line = Array.create row_dim first in
				Array.unsafe_set res i new_line;
				for j=1 to row_dim - 1 do
					Array.unsafe_set new_line j (f i j (Array.unsafe_get line j))
				done
			done;
			res
	
	let modif f = function
		| [|[||]|] -> ()
		| m ->
			let col_dim = col_dim m in
			let row_dim = row_dim m in
			for i=0 to col_dim - 1 do
				let line = Array.unsafe_get m i in
				for j=0 to row_dim - 1 do
					Array.unsafe_set line j (f (Array.unsafe_get line j))
				done;
			done
	
	let modifij (f : int -> int -> 'a -> 'a) = function
		| [|[||]|] -> ()
		| m ->
			let col_dim = col_dim m in
			let row_dim = row_dim m in
			for i=0 to col_dim - 1 do
				let line = Array.unsafe_get m i in
				for j=0 to row_dim - 1 do
					Array.unsafe_set line j (f i j (Array.unsafe_get line j))
				done;
			done
	
	
	let map2 (f : 'a -> 'b -> 'c) m1 m2 =
		let col_dim1 = col_dim m1 in
		let row_dim1 = row_dim m1 in
		if (col_dim1 <> col_dim m2) || (row_dim1 <> row_dim m2) then
			raise Not_same_dimension
		else
			match m1 with
				| [|[||]|] -> [|[||]|]
				| _ ->
					let res = Array.create col_dim1 [||] in
					for i=0 to col_dim1 - 1 do
						let line1 = Array.unsafe_get m1 i in
						let line2 = Array.unsafe_get m2 i in
						let first = f (Array.unsafe_get line1 0) (Array.unsafe_get line2 0) in
						let new_line = Array.create row_dim1 first in
						Array.unsafe_set res i new_line;
						for j=1 to row_dim1 - 1 do
							Array.unsafe_set new_line j (f (Array.unsafe_get line1 j) (Array.unsafe_get line2 j))
						done
					done;
					res
	
	let fold_left (f : 'b -> 'a -> 'b) x = function
		| [|[||]|] -> raise Empty_matrix
		| m ->
			let res = ref x in
			let col_dim = col_dim m in
			let row_dim = row_dim m in
			for i=0 to col_dim - 1 do
				let line = Array.unsafe_get m i in
				for j=0 to row_dim - 1 do
					res := f !res (Array.unsafe_get line j)
				done
			done;
			!res
	
	let fold_right (f : 'a -> 'b -> 'b) m x = match m with
		| [|[||]|] -> raise Empty_matrix
		| m ->
			let res = ref x in
			let col_dim = col_dim m in
			let row_dim = row_dim m in
			for i = col_dim - 1 downto 0 do
				let line = Array.unsafe_get m i in
				for j = row_dim - 1 downto 0 do
					res := f (Array.unsafe_get line j) !res
				done
			done;
			!res
	
	let identity n =
		let m = create n n 0. in
		for i=0 to n-1 do
			m.(i).(i) <- 1.
		done;
		m
	
	let transpose m =
		init (col_dim m) (row_dim m) (fun i j -> unsafe_get m j i)
	
	let random_int col_dim row_dim range =
		Random.self_init ();
		let int () =
			if Random.bool () then Random.int range
			else - (Random.int range) in
		let res = Array.create col_dim [||] in
		for i=0 to col_dim - 1 do
			let new_line = Array.create row_dim (int()) in
			Array.unsafe_set res i new_line;
			for j=1 to row_dim - 1 do
				Array.unsafe_set new_line j (int())
			done
		done;
		res
				
	let print_int a =
		let print_line line = Array.iter (printf "%d ") line; printf "\n" in
		Array.iter print_line a
		
	let println_int a =
		print_int a;
		printf "\n"
end

module FloatMatrix = struct
	let of_array = function
	| [|[||]|] -> [|[||]|]
	| m ->
		let col_dim = Array.length m in
		let row_dim = Array.length m.(0) in
		for i=1 to col_dim - 1 do
			if row_dim <> (Array.length m.(i)) then raise Not_a_matrix
		done;
		m
	
	let copy a =
		Array.map (Array.copy) a
	
	let col_dim m =
		Array.length m
		
	let row_dim m =
		Array.length m.(0)
	
	let unsafe_set m i j x =
		Array.unsafe_set (Array.get m i) j x
	
	let unsafe_get m i j =
		Array.unsafe_get (Array.unsafe_get m i) j
	
	let create col_dim row_dim x =
		let res = Array.create col_dim [||] in
		for i=0 to col_dim - 1 do
			Array.unsafe_set res i (Array.create row_dim x)
		done;
		res
	
	let init col_dim row_dim f =
		let res = Array.create col_dim [||] in
		for i=0 to col_dim - 1 do
			Array.unsafe_set res i (Array.create row_dim (f i 0));
			for j=1 to row_dim - 1 do
				Array.unsafe_set (Array.unsafe_get res i) j (f i j)
			done;
		done;
		res
	
	let iter f m =
		let row_dim = Array.length m.(0) in
		for i=0 to (Array.length m) - 1 do
			for j=0 to row_dim - 1 do
				f (unsafe_get m i j)
			done
		done
	
	let iterij f m =
		let row_dim = Array.length m.(0) in
		for i=0 to (Array.length m) - 1 do
			for j=0 to row_dim - 1 do
				f i j (unsafe_get m i j)
			done
		done
	
	let map f m =
		match m with
			| [|[||]|] -> [|[||]|]
			| m ->
				let col_dim = col_dim m in
				let row_dim = row_dim m in
				let res = Array.create col_dim [||] in
				for i=0 to col_dim - 1 do
					let line = Array.unsafe_get m i in
					let first = f (Array.unsafe_get line 0) in
					let new_line = Array.create row_dim first in
					Array.unsafe_set res i new_line;
					for j=1 to row_dim - 1 do
						Array.unsafe_set new_line j (f (Array.unsafe_get line j))
					done
				done;
				res
	
	let mapij (f : int -> int -> 'a -> 'b) = function
		| [|[||]|] -> [|[||]|]
		| m ->
			let col_dim = col_dim m in
			let row_dim = row_dim m in
			let res = Array.create col_dim [||] in
			for i=0 to col_dim - 1 do
				let line = Array.unsafe_get m i in
				let first = f i 0 (Array.unsafe_get line 0) in
				let new_line = Array.create row_dim first in
				Array.unsafe_set res i new_line;
				for j=1 to row_dim - 1 do
					Array.unsafe_set new_line j (f i j (Array.unsafe_get line j))
				done
			done;
			res
	
	let modif f = function
		| [|[||]|] -> ()
		| m ->
			let col_dim = col_dim m in
			let row_dim = row_dim m in
			for i=0 to col_dim - 1 do
				let line = Array.unsafe_get m i in
				for j=0 to row_dim - 1 do
					Array.unsafe_set line j (f (Array.unsafe_get line j))
				done;
			done
	
	let modifij (f : int -> int -> 'a -> 'a) = function
		| [|[||]|] -> ()
		| m ->
			let col_dim = col_dim m in
			let row_dim = row_dim m in
			for i=0 to col_dim - 1 do
				let line = Array.unsafe_get m i in
				for j=0 to row_dim - 1 do
					Array.unsafe_set line j (f i j (Array.unsafe_get line j))
				done;
			done
	
	
	let map2 (f : 'a -> 'b -> 'c) m1 m2 =
		let col_dim1 = col_dim m1 in
		let row_dim1 = row_dim m1 in
		if (col_dim1 <> col_dim m2) || (row_dim1 <> row_dim m2) then
			raise Not_same_dimension
		else
			match m1 with
				| [|[||]|] -> [|[||]|]
				| _ ->
					let res = Array.create col_dim1 [||] in
					for i=0 to col_dim1 - 1 do
						let line1 = Array.unsafe_get m1 i in
						let line2 = Array.unsafe_get m2 i in
						let first = f (Array.unsafe_get line1 0) (Array.unsafe_get line2 0) in
						let new_line = Array.create row_dim1 first in
						Array.unsafe_set res i new_line;
						for j=1 to row_dim1 - 1 do
							Array.unsafe_set new_line j (f (Array.unsafe_get line1 j) (Array.unsafe_get line2 j))
						done
					done;
					res
	
	let fold_left (f : 'b -> 'a -> 'b) x = function
		| [|[||]|] -> raise Empty_matrix
		| m ->
			let res = ref x in
			let col_dim = col_dim m in
			let row_dim = row_dim m in
			for i=0 to col_dim - 1 do
				let line = Array.unsafe_get m i in
				for j=0 to row_dim - 1 do
					res := f !res (Array.unsafe_get line j)
				done
			done;
			!res
	
	let fold_right (f : 'a -> 'b -> 'b) m x = match m with
		| [|[||]|] -> raise Empty_matrix
		| m ->
			let res = ref x in
			let col_dim = col_dim m in
			let row_dim = row_dim m in
			for i = col_dim - 1 downto 0 do
				let line = Array.unsafe_get m i in
				for j = row_dim - 1 downto 0 do
					res := f (Array.unsafe_get line j) !res
				done
			done;
			!res
	
	let identity n =
		let m = create n n 0. in
		for i=0 to n-1 do
			m.(i).(i) <- 1.
		done;
		m
	
	let transpose m =
		init (col_dim m) (row_dim m) (fun i j -> unsafe_get m j i)
	
	let random col_dim row_dim range =
		Random.self_init ();
		let float () =
			if Random.bool () then Random.float range
			else -. (Random.float range) in
		let res = Array.create col_dim [||] in
		for i=0 to col_dim - 1 do
			let new_line = Array.create row_dim (float()) in
			Array.unsafe_set res i new_line;
			for j=1 to row_dim - 1 do
				Array.unsafe_set new_line j (float())
			done
		done;
		res
	
	let print a =
		let print_line line = Array.iter (printf "%2.2f ") line; printf "\n" in
		Array.iter print_line a
		
	let println a =
		print a;
		printf "\n"
			
	let mul m1 m2 =
		if (row_dim m1) <> (col_dim m2) then
			(raise Wrong_dimension);
		let product i j =
			let res = ref 0.0 in
			for k=0 to (row_dim m1) - 1 do
				res := !res +. (unsafe_get m1 i k) *. (unsafe_get m2 k j)
			done;
			!res
		in init (col_dim m1) (row_dim m2) product;;
	
	let null_of_same_dim m =
		create (col_dim m) (row_dim m) 0.
	
	let add m1 m2 =
		map2 (+.) m1 m2
			
	let sub m1 m2 =
		map2 (-.) m1 m2
	
	let modif_add m2 ~on:m1 =
		modifij (fun i j x -> x +. unsafe_get m2 i j) m1
	
	let modif_sub m2 ~on:m1 =
		modifij (fun i j x -> x -. unsafe_get m2 i j) m1
	
	let scal_mul s m =
		map (( *. ) s) m
		
	let mul_vect m v =
		let vec_dim = Array.length v in
		let col_dim = col_dim m in
		let row_dim = row_dim m in
		if row_dim <> vec_dim then raise Wrong_dimension
		else (
			let res = Array.create col_dim 0. in
			for i=0 to col_dim - 1 do
				let sum = ref 0. in
				for j=0 to row_dim - 1 do
					sum := !sum +. (unsafe_get m i j) *. (Array.unsafe_get v j)
				done;
				Array.unsafe_set res i !sum
			done;
			res
		)
	
	let to_string a =
		let str_of_line line = line 
			|> Array.map (sprintf "%3.2f ")
			|> Array.to_list
			|> String.concat "" in
		(String.concat "\n" (Array.to_list (Array.map str_of_line a))) ^ "\n"
end
