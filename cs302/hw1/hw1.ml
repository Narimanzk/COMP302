(* HOMEWORK 1 : COMP 302 Fall 2014 
   
   PLEASE NOTE:  

   * All code files must be submitted electronically
     BEFORE class on 18 Sep, 2014

  *  The submitted file name must be hw1.ml 

  *  Your program must type-check and run usig 
     OCaml of at least OCaml 4.0

  * Remove all "raise NotImplemented" with your solutions
*)

exception NotImplemented
exception Domain

(* ------------------------------------------------------------*)
(* QUESTION 2 : WARM-UP                                        *)
(* ------------------------------------------------------------*)
(* Q2 Computing the square root                                *)

(* Instead of making another function called 'next', I just wrote/inserted
the equation provided in the assingment paper*)

let square_root a =  
  	if a = 1.0 then 1.0
	else let rec findroot x acc =
		if abs_float(x-.(((a/.x)+.x)/.2.0))<acc then x
		else findroot (((a/.x)+.x)/.2.0) acc
  	in
  	findroot 1.0 epsilon_float


(* ------------------------------------------------------------*)
(* QUESTION 3 : House of Cards                                 *)
(* ------------------------------------------------------------*)

type suit = Clubs | Spades | Hearts | Diamonds

type rank =  Six | Seven | Eight | Nine | Ten | 
             Jack | Queen | King | Ace

type card = rank * suit

type hand = Empty | Hand of card * hand

(* dom : suit -> suit -> bool

   dom(s1,s2) = true iff suit s1 beats or is equal to suit s2
                relative to the ordering S > H > D > C         
   Invariants: none
   Effects: none
*)

let dom s1 s2 = match s1, s2 with
  | Spades, _        -> true
  | Hearts, Diamonds -> true
  | Hearts, Clubs    -> true
  | Diamonds, Clubs  -> true
  | s1, s2           -> s1 = s2

let dom_rank r1 r2 = match r1 with
  | Ace     -> true
  | King    -> not(r2 = Ace)
  | Queen   -> not((r2=Ace)||(r2=King))
  | Jack    -> not((r2=Ace)||(r2=King)||(r2=Queen))
  | Ten     -> r2=Six||r2=Seven||r2=Eight||r2=Nine||r2=Ten
  | Nine    -> r2=Six||r2=Seven||r2=Eight||r2=Nine
  | Eight   -> r2=Six||r2=Seven||r2=Eight
  | Seven   -> r2=Seven||r2=Eight
  | Six     -> r2=Six

(* the change to 'greater' was provided by TA David Thibodeau via discussion board*)
let greater (r1, s1) (r2, s2) = 
 if s1 = s2 then dom_rank r1 r2 else dom s1 s2

let rec insert c h = match h with
  | Empty -> Hand(c, h)
  | Hand(c',h') -> if (greater c' c) then Hand(c', insert c h') else Hand(c, h)

(* the assignment says function sort, but in the given hw1.ml file, the prof(or TA) wrote as ins_sort
I will stick ins_sort, which is already written here*)

let rec ins_sort h = match h with
  | Empty -> h
  | Hand(c1, h1) -> insert c1 (ins_sort h1)


(* --------------------------------------------------------------------*)
(* QUESTION 4 Sparse representation of binary numbers                  *)
(* ------------------------------------------------------------------- *)

type nat = int list (* increasing list of weights, each a power of two *)

(* For example: 

5  = [1,4]
13 = [1,4,8]

*)

(* ------------------------------------------------------------------- *)
(* Q4.1 : Incrementing a binary number (10 points)                     *)
(* ------------------------------------------------------------------- *)

(* the exponent use of ** is only for floats. So, I need a helper function called power for int version*)

let rec power n exp = match exp with
 | 0 -> 1
 | exp' -> n*(power n (exp-1))

let inc (ws:nat) =
  let rec increments (ws:nat) count = match ws with
	| [] -> [power 2 count]
	| l1::l2 -> if (l1=power 2 count) then increments l2 (count+1)
		    else (power 2 count)::ws
  in increments (ws:nat) 0

(* ------------------------------------------------------------------- *)
(* Q4.2 : Decrementing a sparse binary number  (10 points)             *)
(* ------------------------------------------------------------------- *)

(* the function makeInt makes the int version of the given sparse representation*)
(* the function sparse makes a sparse representation*)

let makeInt x =
  let rec making x acc = match x with
	| [] -> acc
	| l1::l2 -> making l2 (l1+acc)
  in making x 0

let sparse number = 
 let rec sparsing number (t:nat) = match number with
	| 0 -> t
	| number' -> sparsing (number-1) (inc t)
 in sparsing number []

let dec ws:nat = match ws with
	| [] -> ws
	| l1::l2 -> sparse ((makeInt ws)-1)

(* ------------------------------------------------------------------- *)
(* Q4.3 : Adding sparse binary numbers  (10 points)                    *)
(* ------------------------------------------------------------------- *)

(* It seems I already partially solved this problem in 4.2 with sparse function
In 4.3, I will give it a name sparse' in case different TA corrects each subquestion of Q4
*)

let sparse' number = 
 let rec sparsing' number (t:nat) = match number with
	| 0 -> t
	| number' -> sparsing' (number-1) (inc t)
 in sparsing' number []

let add (m:nat) (n:nat) =
 let rec addition g sum = match g with
	| [] -> sum
	| g1::g2 -> addition g2 (g1+sum)
 in sparse' (addition (m@n) 0)

(* ------------------------------------------------------------------- *)
(* Q4.4 : Converting to integer - tail recursively  (10 points)        *)
(* ------------------------------------------------------------------- *)
let sbinToInt (n:nat) =
  let rec localfun n accumulator = match n with
	| [] -> accumulator
	| l1::l2 -> localfun l2 (l1+accumulator)
  in localfun n 0
    
(* --------------------------------------------------------------------*)
(* QUESTION 5 Negation Normal Form                                     *)
(* ------------------------------------------------------------------- *)

type prop = 
  | Atom of string
  | Neg of prop
  | Conj of prop * prop
  | Disj of prop * prop
  | Impl of prop * prop

let rec nnf p = match p with
  | Atom(s) -> p
  | Neg(Neg(p1)) -> p1
  | Neg(Atom(s)) -> Neg(Atom(s))
  | Neg(Conj(p1,p2)) -> Disj(nnf (Neg(p1)),nnf (Neg(p2)))
  | Neg(Disj(p1,p2)) -> Conj(nnf (Neg(p1)),nnf (Neg(p2)))
  | Neg(Impl(p1,p2)) -> Neg(nnf (Disj(Neg p1, p2)))
  | Conj(p1,p2) -> Conj(nnf p1, nnf p2)
  | Disj(p1,p2) -> Disj(nnf p1, nnf p2)
  | Impl(p1,p2) -> Disj(nnf (Neg(p1)), nnf p2)

let f1 = Neg (Conj (Atom "p", Disj (Atom "q", Atom "r")))
let f2 = Neg (Conj (Neg (Atom "p"), Disj (Atom "q", Atom "r")))
