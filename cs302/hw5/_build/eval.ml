(* Evaluation of MinML Expression via big step semantics *)

let verbose = ref 0
let bigstep_depth = ref 0
let rec indent i = match i with
  | 0 -> ""
  | n -> " " ^ indent (n - 1)

open Minml

exception Stuck of string
exception Unimplemented

let genCounter = 
  let counter = ref 0 in
  ((fun x -> 
    let _ = counter := !counter+1 in
    string_of_int (!counter) ^ x),
  fun () ->
    counter := 0)

let (freshVar, resetCtr) = genCounter

let member x l = List.exists (fun y -> y = x) l

let rec union p = match p with
  | ([], l) -> l
  | (x::t, l) -> 
    if member x l then
      union (t, l)
    else
      x :: union (t, l)

let unionList sets = List.fold_right (fun s1 s2 -> union (s1, s2)) sets []

let rec delete d = match d with
  | (vlist, []) -> []
  | (vlist, h :: t) -> 
    if member h vlist then delete (vlist, t)
    else h :: delete (vlist, t)

let boundVars d = match d with
  | Val (_, name) -> [name]
  | Valtuple (_, names) -> names
  | ByName(_, name) -> [name]

let rec varsDecs d = match d with
  | [] -> ([],[])
  | dec1::decs ->
    let (free, bound) = varsDecs decs in
    (union(freeVarsDec dec1, delete(boundVars dec1, free)), bound)

and freeVarsDec d = match d with
  | Val (exp, name) -> freeVars exp
  | Valtuple (exp, names) -> freeVars exp
  | ByName (exp, name) -> freeVars exp

(* freeVars(e) = list of names occurring free in e 
 *
 *  Invariant: every name occurs at most once. *)
and freeVars e = match e with
  | Var y -> [y]
  | Int n -> []
  | Bool b -> []
  | If(e, e1, e2) ->
    union (freeVars e, union (freeVars e1, freeVars e2))
  | Primop (po, args) ->
    List.fold_right (fun e1 e2 -> union (freeVars e1, e2)) args []
  | Tuple exps ->
    unionList (List.map freeVars exps)
  | Fn (x, _, e) ->
    delete ([x], freeVars e)
  | Rec (x, _, e) ->
      delete ([x], freeVars e)
  | Let (decs, e2) ->
      let (free, bound) = varsDecs decs in
      union (free, delete (bound, freeVars e2))
  | Apply (e1, e2) ->
      union(freeVars e1, freeVars e2)
  | Anno (e, _) ->
      freeVars e

let freeVariables e = freeVars e

(* ---------------------------------------------------------- *)
(* Question 1 *)

let rec allVars e acc = 
  match e with
  | Let(declist , exp) -> (match declist with
      | Valtuple(exp2, namelst)::_ -> allVars exp acc@namelst
      | Val(exp2, name)::_ -> allVars exp acc@[name]
      | ByName(exp2, name)::_ -> allVars exp acc@[name]
      | [] -> allVars exp acc)
  | Rec(name, tp, exp) -> allVars exp (acc@[name])
  | Fn (name, tp, exp) -> allVars exp (acc@[name])
  | If(exp1, exp2, exp3) -> allVars exp1 acc@(allVars exp2 []@(allVars exp3 [])) 
  (*| Primop(_, expList) -> [] primitives, no need to keep track of variables*)
  | Tuple(explist) -> let templist = List.map (fun exp -> allVars exp []) explist in 
      List.flatten templist
  | Apply(exp1, exp2) ->  allVars exp1 acc@(allVars exp2 [])
  | Anno(exp, _) -> allVars exp acc
(* | Var (name) -> 
  | Int (_)-> []
  | Bool (_)-> []*)
  | _ -> acc (*acc - freevars*)

let rec select l1 l2 l3 acc= match l1 with
  | [] -> acc
  | x::t -> if member x l3 then acc@[x]
    else (if member x l2 then select t l2 (l3@[x]) acc else select t l2 l3 acc@[x])

let unusedVariables e = let allv = allVars e [] in
  select allv (freeVariables e) [] []

(* ---------------------------------------------------------- *)
(* Substitution (corrected description)
   subst : (exp * name) -> exp -> exp

   subst (e',x) e = [e'/x]e

   subst replaces every occurrence of the variable x
   in the expression e with e'.
*)

let rec substArg s a = match a with
  | []     -> []
  |a::args -> (subst s a) :: (substArg s args)

and subst s exp =
  let (e', x) = s in
  let result =
    match exp with
    | Var y ->
        if x = y then e'
        else Var y

    | Int n -> Int n
    | Bool b -> Bool b
    | Primop(po, args) -> Primop(po, substArg s args)
    | If(e, e1, e2) ->
        If(subst s e, subst s e1, subst s e2)

    | Tuple es ->
        Tuple (List.map (subst s) es)

    | Anno (e, t) ->
        Anno (subst s e, t)

    | Let([], e2) -> Let([], subst s e2)

    | Let(dec1::decs, e2) ->
        let rest = Let(decs, e2) in
        (match dec1 with
        | Val(exp, name) ->
            let (name, rest) =
              if member name (freeVars e') then
                rename (name, rest)
              else
                (name, rest)
            in
            let exp = subst s exp in
            if name = x then
              Let(Val(exp, name) :: decs, e2)
            else
              (match subst s rest with
              | Let(decs, e2) -> Let(Val(exp, name) :: decs, e2)
              | _ -> assert false)
              

        | ByName(exp, name) ->
            let (name, rest) =
              if member name (freeVars e') then
                rename (name, rest)
              else
                (name, rest)
            in
            let exp = subst s exp in
            if name = x then
              Let(ByName(exp, name) :: decs, e2)
            else
              (match subst s rest with
              | Let(decs, e2) -> Let(ByName(exp, name) :: decs, e2)
              | _ -> assert false)

        | Valtuple(exp, names) ->
            let (names', rest) = renameListAsNeeded names e' rest in
            let exp = subst s exp in
            if member x names then
              Let(Valtuple(exp, names) :: decs, e2)
            else
              (match subst s rest with
              | Let(decs, e2) -> Let(Valtuple(exp, names') :: decs, e2)
              | _ -> assert false))

    | Apply (e1, e2) -> Apply (subst s e1, subst s e2)

    | Fn (y, t, e) ->
        if y = x then
          Fn (y, t, e)
        else
          if member y (freeVars e') then
            let (y,e1) = rename (y,e) in
            Fn (y, t, subst s e1)
          else
            Fn(y, t, subst s e)

    | Rec (y, t, e) ->
        if y = x then
          Rec (y, t, e)
        else
          if member y (freeVars e') then
            let (y, e1) = rename (y,e) in
            Rec (y, t, subst s e1)
          else
            Rec (y, t, subst s e)
  in
  if !verbose >= 2 then
    print_endline ("subst: " ^ Print.expToString e' ^ " for " ^ x ^ " in "
           ^ Print.expToString exp ^ "\n =    " ^ Print.expToString result ^ "\n")
  else ();
  result

and substList l e = match l with
| [] -> e
| (x,e')::pairs ->
    subst (x,e') (substList pairs e)

and rename (x, e) =
  let x' = freshVar x in
  (x', subst (Var x', x) e)

and rename2 (x, y, e1, e2) =
  let x' = freshVar x in
  let y' = freshVar y in
  let subst2 e = subst (Var x', x) (subst (Var y', y) e) in
  (x', y', subst2 e1, subst2 e2)

and renameAll e = match e with
  | ([], e) -> ([], e)
  | (x::xs, e) ->
      let (x', e) = rename (x, e) in
      let (xs', e) = renameAll (xs, e) in
      (x' :: xs', e)

and renameListAsNeeded names e' exp =
      if List.exists (fun name -> member name (freeVars e')) names then
        renameAll(names, exp)
      else
        (names, exp)

(*------------------------------------------------------------------
 * Q2: Evaluation
 *-------------------------------------------------------------------*)

let rec evalList (exps : exp list) =
  List.map eval exps

and evalValtuple (e1, xs, decs, e2) =
  match eval e1 with
  | Tuple es ->
    if List.length es = List.length xs then
      eval (substList (List.combine es xs) (Let(decs, e2)))
    else
      raise (Stuck "Tuple binding failure (length mismatch)")

  | _ -> raise (Stuck "Tuple binding failure")

(* Question 2. *)

(* eval : exp -> exp *)
and eval exp =
  let _ =
    if !verbose >= 1 then
      print_endline (indent (!bigstep_depth) ^ "eval { " ^ Print.expToString exp ^ " }\n")
    else ()
  in
  let _ = bigstep_depth := !bigstep_depth + 1 in
  let result = match exp with

  (* Values evaluate to themselves... *)
(*Fn changed*)
  | Fn (x, t, e) -> exp
  | Int _ -> exp
  | Bool _ -> exp

  | Var x -> raise (Stuck ("Free variable (" ^ x ^ ") during evaluation"))
(*Rec changed, maybe remove eval*)
  | Rec (f, _, e) -> eval(subst (exp,f) e)

  (* primitive operations +, -, *, <, = *)
  | Primop(po, args) ->
      let argvalues = evalList args in
      (match evalOp(po, argvalues) with
      | None -> raise (Stuck "Bad arguments to primitive operation")
      | Some v -> v)

  | Tuple es -> Tuple (evalList es)

  | Let(d, e2) -> (match d with 
	| [] -> eval e2
	| d1::d2 -> match d1 with
		| Val (e,n) -> eval (subst (eval e,n) (Let (d2,e2)))
		| Valtuple (e,n) -> evalValtuple (e,n,d2,e2)
		| ByName (e,n) -> eval (subst (e,n) (Let (d2,e2))))

  | Anno (e, _) -> eval e     (* types are ignored in evaluation *)

  | If(e, e1, e2) ->
       (match eval e with
       | Bool true -> eval e1
       | Bool false -> eval e2
       | _ -> raise (Stuck "Left term of application is not an Fn"))
(*changed apply*)
  | Apply (e1, e2) -> match (eval e1) with
	| Fn (n,t,e) -> eval (subst (eval e2, n) e) 
	| _ -> raise (Stuck "Case not seen")
  in
    bigstep_depth := !bigstep_depth - 1;
    if !verbose >= 1 then
      print_endline (indent (!bigstep_depth)
          ^ "result of eval { " ^ Print.expToString exp ^ " } = "
          ^ Print.expToString result ^ "\n")
    else ();
    result



