type generic = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | Z;;
type typ = Int | Bool | Char | List of typ | Fun of typ list * typ | Gen of generic;;
type ide = string;;

type exp = 
    Eint of int
  | Ebool of bool
  | Echar of char
  | Empty
  | Den of ide
  | Cons of exp * exp
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
  | Let of (ide * exp) list * exp
  | Fun of ide list * exp
  | Apply of exp * exp list;;

let rec type_inf e = match e with
   Eint n -> Int
  |Ebool b -> Bool
  |Echar c -> Char
  |Empty -> Gen(Z)
  |Den x -> Gen(A)
  |Cons (e1,e2) -> let t1 = type_inf e1 in
                   let t2 = type_inf e2 in 
		   if t1=t2 then (List t1)
		   else if (List t1)=t2 then (List t1)
		        else if t1=(List t2) then (List t2)
                             else if t1=Gen(Z) && t2!=Gen(Z) then (List t2)
                                  else if t1!=Gen(Z) && t2=Gen(Z) then (List t1)
                                       else if t1=List(Gen(Z)) && t2!=List(Gen(Z)) then (List t2)
                                            else if t1!=List(Gen(Z)) && t2=List(Gen(Z)) then (List t1)
                                                 else failwith "Invalid list" 
  |Prod (e1,e2)
  |Sum (e1,e2)
  |Diff (e1,e2)
  |Mod (e1,e2)
  |Div (e1,e2) -> let t1 = type_inf e1 in
                  let t2 = type_inf e2 in
		  if t1=t2 && t1=Int then Int 
		  else failwith "Invalid types"
  |Lessint (e1,e2)
  |Eqint (e1,e2) -> let t1 = type_inf e1 in
                    let t2 = type_inf e2 in
		    if t1=t2 && t1=Int then Bool 
		    else failwith "Invalid types"
  |Lesschar (e1,e2)
  |Eqchar (e1,e2) -> let t1 = type_inf e1 in
                     let t2 = type_inf e2 in
		     if t1=t2 && t1=Char then Bool 
		     else failwith "Invalid types"
  |Iszero e -> let t1 = type_inf e in 
	       if t1=Int then Bool 
	       else failwith "Invalid types"
  |And (e1,e2)
  |Or (e1,e2) -> let t1 = type_inf e1 in
                 let t2 = type_inf e2 in 
		 if t1=t2 && t1=Bool then Bool 
		 else failwith "Invalid types"
  |Not e -> let t1 = type_inf e in 
	    if t1=Bool then Bool 
	    else failwith "Invalid type"
  |Ifthenelse (e0,e1,e2) -> let t0 = type_inf e0 in
                            let t1 = type_inf e1 in
                            let t2 = type_inf e2 in
			    if t0=Bool && t1=t2 then t1 
			    else failwith "Invalid types"
  |Let (x,e1) -> let t1 = type_inf e1 in t1
  |Fun (idel,e1) -> let t1 = type_inf e1 in 
		    let param = List.map (fun a -> type_inf (Den(a))) idel in Fun(param,t1)
  |Apply (e1,el) -> let t1 = type_inf e1 in t1;;

type_inf (Cons(Eint 5,Cons(Empty,Empty)));;
