

(* unify: typ * typ -> unit 

   unify(T1, T2) = () 
   
   succeeds if T1 and T2 are unifiable, fails otherwise.

   Side Effect: Type variables in T1 and T2 will have been
    updated, s.t. if T1 and T2 are unifiable AFTER unification
    they will denote the "same" type.
*)

open Type
module M = Minml

exception Error of string
exception Unimplemented

let freshVar () = TVar (ref None)

let rec occurs s t = match s, t with
  | _, Int -> false
  | _, Bool -> false
  | _, Arrow (t1, t2) ->
      (occurs s t1) || (occurs s t2)
  | _, Product ts ->
      List.exists (occurs s) ts
  | _, TVar r ->
   match !r with
    | None -> (s == r)
    | Some t' -> (s == r) || (occurs s t')

let fail str = raise (Error str)

(* Question 4. *)
let rec unify s t = match s, t with
  | Bool, Bool -> ()
  | Int, Int -> ()
  | Arrow(s1, s2), Arrow(t1, t2) -> if (unify s1 t1 = ()) && (unify s2 t2 = ()) then () else fail "Unify Arrow Error"
  | Product(list1), Product (list2) -> List.iter2 (fun s1 t1 -> if (unify s1 t1) = () then () else fail "Unify Product Error") list1 list2
  | TVar (s1), TVar (t1) -> (match !s1, !t1 with
    | None, None -> if !s1 = !t1 then () else fail "Tvar unmatched" 
    | Some tp1, Some tp2 -> if !s1 = !t1 then () else fail "Tvar umatched"
    | None, Some tp2 -> if (occurs s1 t) then fail "Unify Tvar occurs in t" else s1 := Some (t)
    | Some tp1, None -> if (occurs t1 s) then fail "Unify Tvar uccurs in s" else t1 := Some (s)
  )

  | TVar (s1), _ -> (match !s1 with 
    | None -> if (occurs s1 t) then fail "Unify s1 occurs in t" else s1 := Some(t)
    | Some tp -> unify tp t
  )
  | _, TVar (t1) -> (match !t1 with 
    | None -> if (occurs t1 s) then fail "Unify s1 uccurs in s" else t1 := Some(s)
    | Some tp -> unify tp s
  )
  | _ -> fail "Dis is the end of Unify"

let unifiable (t, t') = 
  try
    let _ = unify t t' in true
  with Error s -> false
