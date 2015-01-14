(************************************************************)
(*                           ENVIRONMENT                    *)
(************************************************************)

(* env: function from identifiers (ide) to dval *)

type ide = string;;
type loc = int;;

type generic = A | B | C | D | E | F | G | H | I | L | M | N | O | P | Q | R | S | T | U | V | Z;;

type typ =
  | Int
  | Bool 
  | Char 
  | List of typ
  | Fun of typ list * typ
  | Gen of generic
;;

type env = Env of (ide -> typ)

exception UnboundIde of ide;;


(* interface with the environment *)

let bind (Env rho) x d = Env (fun y -> if y=x then d else rho y);;

type mval = int;;

(************************************************************)
(*                           EXPRESSIONS                    *)
(************************************************************)

type exp =
    Eint of int
  | Ebool of bool
  | Echar of char
  | Empty
  | Cons of exp * exp
  | Den of ide
  | Prod of exp * exp
  | Sum of exp * exp
  | Diff of exp * exp
  | Mod of exp * exp
  | Div of exp * exp
  | Lessint of exp * exp
  | Eqint of exp * exp
  | Iszero of exp
  | Lesschar of exp * exp
  | Eqchar of exp * exp
  | Or of exp * exp
  | And of exp * exp
  | Not of exp
  | Ifthenelse of exp * exp * exp 
  | Let of (ide * exp) list * exp (*([(ide,exp)],exp) *)
  | Fun of ide list * exp
  | Apply of exp * exp list
;;

exception TypeMismatch of string;;

let rec sem e (r, s) =

 match e with
  Eint n->Mint(n)
 |Ebool b -> Mbool(b)
 |Echar c ->Mchar(c)
 |Sum (a,b)->  ( match sem a (r,s) , sem b (r,s) with
     (Mint (a) , Mint(b) )-> Mint (a+b)
   | _ -> raise (TypeMismatch "TypeMismatch"))
 
 |Diff (a,b)->  ( match sem a (r,s) , sem b (r,s) with
     (Mint (a) , Mint(b) )-> Mint (a-b)
   | _ -> raise (TypeMismatch "TypeMismatch"))
 
 |Prod (a,b)->  ( match sem a (r,s) , sem b (r,s) with
    (Mint (a) , Mint(b) )-> Mint (a*b)
   | _ -> raise (TypeMismatch "TypeMismatch"))

 |Div (a,b)->  ( match sem a (r,s) , sem b (r,s) with
     (Mint (a) , Mint(b) )-> Mint (a/b)
   | _ -> raise (TypeMismatch "TypeMismatch"))

 |Mod (a,b)->  ( match sem a (r,s) , sem b (r,s) with
     (Mint (a) , Mint(b) )-> Mint (a mod b)
   | _ -> raise (TypeMismatch "TypeMismatch"))

 |Eqint (e1,e2) -> if sem e1 (r,s) = sem e2 (r,s) then Mbool(true) else Mbool(false)
 
 |Lessint (e1,e2) ->  if sem e1 (r,s) < sem e2 (r,s) then Mbool(true) else Mbool(false)
 
 |Lesschar (e1,e2) ->  if sem e1 (r,s) < sem e2 (r,s) then Mbool(true) else Mbool(false)
 
 |Eqchar (e1,e2) ->  if sem e1 (r,s) = sem e2 (r,s) then Mbool(true) else Mbool(false)

 |Iszero e1 -> let w= sem e1 (r,s) in if w=Mint(0) then Mbool(true) else Mbool(false)

 | Not(x) -> (match sem x (r, s) with 
       Mbool(true) -> Mbool(false) 
  	 | Mbool(false) -> Mbool(true)
     | _ -> raise (TypeMismatch "Errore di tipo"))

 | And (a,b) -> (match sem a (r,s) , sem b (r,s) with 
	   (Mbool (x1) , Mbool (x2)) -> Mbool(x1 && x2)
	  | _ -> raise (TypeMismatch "Errore di tipo"))

 | Or (a,b) -> (match sem a (r,s) , sem b(r,s) with
  	 (Mbool (x1),Mbool (x2)) -> Mbool(x1 || x2)
  	| _ -> raise (TypeMismatch "Errore di tipo"))

 |Empty -> Mempty

 |Ifthenelse (a,b,c) -> let w= sem a (r,s) in 
     let x = sem b (r,s) in 
     let y = sem c (r,s) in if w=Mbool(false) || w=Mbool(true) 
     then if w=Mbool(true) then x else y else raise (TypeMismatch "Errore di tipo")		   
 | Den x -> (match applyenv r x with 
 | DVar (l,t) -> applystore s l
 | DConstI c -> Mint(c)
 | DConstB c -> Mbool(c)
 | DConstC c -> Mchar(c)
 | _ -> raise (UnboundIde x)
 )

 | Cons ( x1 , x2 ) -> Mpair (sem x1 (r,s) , sem x2 (r,s))

 | Let(a,b)-> (match a with
     (i,e)::[]->sem b (r,s)
    |(i,e)::hd->sem (Let(hd,b)) (r,s)
    |_ -> raise (TypeMismatch "Errore di tipo"))
    

 | Fun(z,e) ->(match z with
   |i::[] ->sem e (r,s)
   |i::hd -> sem (Fun(hd,e)) (r,s)
   |_-> raise (TypeMismatch "Errore di tipo"))

 |Apply(ex,ex2) ->(match ex2 with
   |[]-> sem ex (r,s)
   |ex2::hd -> sem(Apply(ex,hd))(r,s)
   | _ -> raise (TypeMismatch "Errore di tipo"))

;;
