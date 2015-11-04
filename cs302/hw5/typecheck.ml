open Type
module M = Minml
open Unify
  
type context = Ctx of (M.name * tp) list

let empty = Ctx []

exception Unimplemented

exception NotFound
let rec assoc x y = match y with
  | [] -> raise NotFound
  | (y, r)::rest -> if x = y then r else assoc x rest

let rec lookup ctx x = let Ctx list = ctx in assoc x list
let extend ctx (x, v) = let Ctx list = ctx in Ctx ((x,v)::list)

let rec extend_list ctx l = match l with
  | [] -> ctx
  | (x,y) :: pairs -> extend_list (extend ctx (x, y)) pairs

exception TypeError of string

let fail message = raise (TypeError message)

let primopType p = match p with
  | M.Equals   -> Arrow(Product[Int; Int], Bool)
  | M.LessThan -> Arrow(Product[Int; Int], Bool)
  | M.Plus     -> Arrow(Product[Int; Int], Int)
  | M.Minus    -> Arrow(Product[Int; Int], Int)
  | M.Times    -> Arrow(Product[Int; Int], Int)
  | M.Negate   -> Arrow(Int, Int)

(* Question 3. *)
(*helper function to zip 2 lists*)
let ziplists l1 l2 = List.map2 (fun a b -> (a,b)) l1 l2

(* infer : context -> M.exp -> tp  *)
let rec infer ctx exp = match exp with 
  | M.Int(_) -> Int
  | M.Bool(_) -> Bool
  | M.If(e1, e2, e3) -> if (infer ctx e1) = Bool then (if (infer ctx e2)=(infer ctx e3) then (infer ctx e2) else fail "Error infer")
    else fail "Error e1 not a Bool"
  | M.Primop(op, explist) -> ( match (primopType op), explist with
    | Arrow(Int, Int), h::[] -> if infer ctx h = Int then Int else fail "Not an Int"
    | Arrow(Int, Int), _ -> fail "Error explist"
    | Arrow(_, Bool), h::t::[] -> if (infer ctx h = Int) && (infer ctx t = Int) && (List.length explist =2)then Bool else fail "Primop not an Int"
    | Arrow(_, Int), h::t::[] -> if (infer ctx h = Int) && (infer ctx t = Int) && (List.length explist =2)then Int else fail "Primop not an Int"
    | _, _ -> fail "Primop Error"
  )
  | M.Tuple(explist) -> Product(List.map (infer ctx) explist)
  | M.Fn(name, tp, e) -> (match tp with 
    | None -> fail "Function type None Error"
    | Some alpha -> Arrow(alpha, (infer (extend ctx (name, alpha)) e))

  )
  | M.Rec(name, tp, e) -> (match tp with
    | Some alpha -> if ((infer (extend ctx (name, alpha)) e) = alpha) then alpha else fail "Error Recursion type"
    | None -> fail "Error Rec None"
  )
  | M.Let(declist, e) -> infer (buildctx ctx declist) e
  | M.Apply(e1,e2) -> (match (infer ctx e1) with
    | Arrow(a, b) -> if (infer ctx e2)=a then b else fail "Apply wrong Type" 
    | _ -> fail "Apply Error"
  )
  | M.Var(name) -> lookup ctx name
  | M.Anno(e, tp) -> if (infer ctx e) = tp then tp else fail "Error Annot type"

and buildctx ctx declist = match declist with
  | [] -> ctx
  | dec::decs -> (match dec with 
    |M.Val(e, name) -> buildctx (extend ctx (name, (infer ctx e))) decs
    |M.ByName(e, name) -> buildctx (extend ctx (name, (infer ctx e))) decs
    |M.Valtuple(e, namelist) -> (match (infer ctx e) with
      |Product(tlist) -> buildctx (extend_list ctx (ziplists namelist tlist)) decs
      | _ -> fail "Valtuple BuildContext Error"
    )
  )
(*    |M.Valtuple(e, namelist) -> let Product tlist = (infer ctx e) in buildctx (extend_list ctx (List.map2 (fun a b -> (a,b)) namelist tlist)) decs
*)
