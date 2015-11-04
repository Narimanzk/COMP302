module List_Library : TAILREC_LIST_LIBRARY = 
  struct
    (* We reimplement some of OCaml's list library functions tail-recursively. As a
     result, these functions will be able to run on large lists and we will not
     get a stack overflow.
     
     *)
    
    exception NotImplemented
		
    (* partition list *)
    (* partition : ('a -> bool) -> 'a list -> 'a list * 'a list
     
     partition p l returns a pair of lists (l1, l2), where l1 is the list of all the elements of l that satisfy the predicate p, and l2 is the list of all the elements of l that donot satisfy p. The order of the elements in the input list is preserved.
     
     *)
    let partition p list = 
      let rec part p l c = match l with
        | [] -> c ([], [])
        | h::t -> if (p h) then part p t (fun (l1, l2) -> c ((h::l1), l2) )
          else part p t (fun (l1, l2) -> c (l1, (h::l2)) )
      in part p list (fun (l1, l2) -> (l1, l2) )
				 
  end
