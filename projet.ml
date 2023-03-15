open Str;;


(*  Def  *)
type op = Var of char | Const of int | Op of char;;

type tree = Leaf of op | BiOp of (op * tree * tree) | MonoOp of (op * tree);;



(*  Utils  *)
let isStringFun f = String.fold_left f true;; 

let isInt = isStringFun (fun acc v -> v >= '0'  && v <= '9' && acc);;

let isMinusChar = isStringFun (fun acc v -> v >= 'a'  && v <= 'z' && acc);;

let isOp (s : string) : bool = List.mem s ["*";"-";"+";"/";"~"];;

let getOp (s : string) : op =
  if (List.mem s ["*";"-";"+";"/";"~"])
  then Op(s.[0])
  else if (isInt s)
  then Const(int_of_string s)
  else if (isMinusChar s && (String.length s) == 1)
  then Var(s.[0])
  else failwith("Pas une operation")
;;


let printOp (o : op) =
  match o with
  |Var(c) -> "Var " ^ Printf.sprintf "%c" c
  |Const(i) -> string_of_int i
  |Op(c) -> Printf.sprintf "%c" c
;;


let rec printTree (t : tree) =
  match t with
  |Leaf(o) -> printOp o
  |MonoOp(o, t) -> (printOp o) ^ (printTree t)
  |BiOp(o, t1, t2) -> "(" ^ (printTree t1) ^ (printOp o) ^ (printTree t2) ^ ")"
;;


(*  Question 1  *)
let parse (l : string list) : tree =
  parseTwo [] l;;
 


let rec parse_aux (arity : string list) (l : string list) : tree =
  match l with
  |[] -> failwith("Liste vide")
  |h::[] ->(
    match getOp h with
    |Op('~') -> MonoOp(Op('~'), Leaf(getOp(List.hd arity)))
    |Op(c) -> let left = (if getOp(List.hd arity)
                BiOp(Op(c), Leaf(getOp(List.hd arity)),  Leaf(getOp(List.hd (List.tl arity))))
    |_ -> Leaf(getOp h)
  )
  |h::t ->
    match getOp h with
    |Op('~') -> MonoOp(Op('~'), parse_aux (List.tl arity) ((List.hd arity)::t))
    |Op(c) -> BiOp(Op(c), Leaf(getOp(List.hd arity)), parse_aux (List.tl arity) ((List.hd arity)::t))
    |_ -> parse_aux (h::arity) t
;;

let tr = BiOp(Op('*'), Leaf(Var('n')), MonoOp(Op('~'), Leaf(Const(112))));;
let test = printTree (parse_aux ["13";"n"] ["~";"*"]);;
let tr2 = parse_aux [] ["n"; "13";"~";"*"];;
