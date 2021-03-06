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

let istrue b = if b!=0 then true else false;;

let rec sem e (r, s) =
  match e with
    Eint n-> n
  | Ebool b -> if b then 1 else 0
  | Echar c ->int_of_char c
  | Empty -> (-1) 
  | Sum (a,b) -> let x = sem a (r,s) in let y = sem b (r,s) in x+y
  | Prod (a,b)-> let x = sem a (r,s) in let y = sem b (r,s) in x*y
  | Diff (a,b)-> let x = sem a (r,s) in let y = sem b (r,s) in x-y
  | Div (a,b) -> let x = sem a (r,s) in let y = sem b (r,s) in if (y!=0) then x/y else failwith "Divisione per Zero!"
  | Mod (a,b) ->  let x = sem a (r,s) in let y = sem b (r,s) in x mod y
  | Eqint (e1,e2) ->  let x = sem e1 (r,s) in let y = sem e2 (r,s) in if x=y then 1 else 0
  | Lessint (e1,e2) -> let x = sem e1 (r,s) in let y = sem e2 (r,s) in if x<y then 1 else 0
  | Lesschar (c1,c2) -> let x = sem c1 (r,s) in let y = sem c2 (r,s) in if x<y then 1 else 0
  | Eqchar (e1,e2) -> let x = sem e1 (r,s) in let y = sem e2 (r,s) in if x=y then 1 else 0
  | Not e' -> let x = sem e' (r,s) in if x = 0 then 1 else 0
  | And (e1,e2) -> let x =sem e1 (r,s) in let y =sem e2 (r,s) in if x = 0 || y = 0 then 1 else 0
  | Or (e1,e2) -> let x =sem e1 (r,s) in let y =sem e2 (r,s) in if x = 0 && y = 0 then 1 else 0
  | Iszero e1 -> let x =sem e1 (r,s) in if x=0 then 1 else 0
  | Ifthenelse(b,e1,e2)-> let x = sem e1 (r,s) in let y=sem e2 (r,s) in if istrue (sem b (r,s)) then x else y
  | Den i -> int_of_string i
;;
