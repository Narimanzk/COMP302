module TreeCont : TREE = 
struct

  type 'a tree = Node of 'a * ('a tree) list 

  exception NotImplemented

  let rec find_cont p t cont = match t with
	| Node (x, children) -> 
		if (p x) then Some x
		else find_children_cont p children cont

	and find_children_cont p tree_list cont = match tree_list with
		| [] -> cont ()
		| t::ts -> find_cont p t (fun () -> find_children_cont p ts cont)

  let find p t = find_cont p t (fun () -> None)

end
