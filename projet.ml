(* DEVIGNE Mathis, RICHARD Louis, DUSEHU Theo *)


(*  Def  *)
(* Un operateur peut etre : une variable, une constance ou une operation *)
type op = Var of char | Const of int | Op of char;; 

(* Un tree peut etre une feuille avec une valeur ou un arbre a un ou deux fils si il y a une operation appliquée.
L'operateur d'une feuille contiendra une variable ou une constante, et ceux de BiOp et MonoOp une operation dans un arbre correct *)
type tree = Leaf of op | BiOp of (op * tree * tree) | MonoOp of (op * tree);;



(*  Utils  *)

(* Fonction d'analyse de string pour les tests *)
let isStringFun f = String.fold_left f true;; 

let isInt = isStringFun (fun acc v -> v >= '0'  && v <= '9' && acc);;

let isMinusChar = isStringFun (fun acc v -> v >= 'a'  && v <= 'z' && acc);;


(* Fonction renvoyant l'operateur en fonction du string reçu *)
let getOp (s : string) : op =
  if (List.mem s ["*";"-";"+";"/";"~"])
  then Op(s.[0])
  else if (isInt s)
  then Const(int_of_string s)
  else if (isMinusChar s && (String.length s) == 1)
  then Var(s.[0])
  else failwith("Pas une operation")
;;

(* Fonction renvoyant la fonction du caractere representant une operation binaire passé en paramêtre *)
let getFunBiOp (c:char) : (int->int->int) =
  match c with
  |'*' -> ( * )
  |'-' -> ( - )
  |'+' -> ( + )
  |'/' -> ( / )
  |_ -> failwith("Not an biop")
;;



(*  Question 1  *)

(* Fonction qui parse une liste de string pour en faire un arbre. Les arites sont les arbres deja creer sur lesquels on fera les operations. *)
let rec parse ?(arity : tree list = []) (l : string list) : tree =
  match l with
  |[] -> failwith("Liste vide")
  (* On redefini notre liste d'arites en fonction de l'operateur :
     - une operation : on prend les arites en fonctions de l'operation pour creer un arbre qu'on rajoutera a la liste
     - une variable ou un constante : on rajoute une feuille contenant l'operateur *)
  |h::t -> let newArity = (
      try
        match getOp h with
        |Op('~') -> MonoOp(Op('~'), List.hd arity)::(List.tl arity)
        |Op(c) ->  BiOp(Op(c), List.hd (List.tl arity), List.hd arity)::(List.tl(List.tl arity))
        |_ -> Leaf(getOp h)::arity
      with e -> failwith("Pas assez d'arites, liste donnee incoherente")
    ) in
    (* Si c'est le dernier element de la liste à analyser, on renvoie la tete des arites, sinon on rappelle la fonction sur le reste de la liste *)
    if (t = []) then List.hd newArity
    else parse ~arity:newArity t
;;


(*  Question 2  *)


let rec simplify (t:tree) : tree =
  match t with
  |Leaf(o) -> t (* Feuille : pas de simplification *)
  (* Pour les operations, on simplifie d'abord leurs fils puis on analyse les possibilités *)
  |MonoOp(o, son) -> ( 
      let simpSon = simplify son in
      match simpSon with
      |Leaf(Const(i)) -> Leaf(Const(-i)) (* negation de constante *)
      |_ -> MonoOp(o, simpSon)
    )
  |BiOp(Op(c), g, d) -> (
      let (simpG, simpD) = (simplify g, simplify d) in
      match (c, simpG, simpD) with
      |('-', Leaf(Var(c)), Leaf(Const(0))) (* x-0 *)
      |('+', Leaf(Var(c)), Leaf(Const(0))) (* x+0 *)
      |('+', Leaf(Const(0)), Leaf(Var(c)))
      |('*', Leaf(Var(c)), Leaf(Const(1))) (* x*1 *)
      |('*', Leaf(Const(1)), Leaf(Var(c)))
      |('/', Leaf(Var(c)), Leaf(Const(1))) (* x/1 *) -> Leaf(Var(c))  (* =x *)

      |('*', Leaf(Var(c)), Leaf(Const(-1))) (* x*-1 *)
      |('*', Leaf(Const(-1)), Leaf(Var(c)))
      |('-', Leaf(Const(0)), Leaf(Var(c)))  (* 0-x *) -> MonoOp(Op('~'), Leaf(Var(c))) (* =-x *)

      |('/', Leaf(Const(0)), _) (* x/_ *)
      |('*', _, Leaf(Const(0))) (* _*0 *)
      |('*', Leaf(Const(0)), _) -> Leaf(Const(0)) (* =0 *)
      |(_, Leaf(Const(ig)), Leaf(Const(id))) -> Leaf(Const((getFunBiOp c) ig id)) (* Calcul constantes *)

      |('/', _, Leaf(Const(0))) -> failwith("Division par zero !!!")
      |_ -> if(simpG = simpD && c = '-') then Leaf(Const(0)) (* _-_ = 0 *)
            else if(simpG = simpD && c = '/') then Leaf(Const(1)) (* _/_ = 1 *)
            else BiOp(Op(c), simpG, simpD)
    )
  |_ -> failwith("Arbre non simplifiable")
;;


(*  Question 3  *)

let printOp (o : op) =
  match o with
  |Const(i) -> string_of_int i
  |Var(c)
  |Op(c) -> Printf.sprintf "%c" c
;;

(* fonction print avec les parentheses pour les autres cas le nécessitant. Les parenthèses sont en fonction de l'operateur courant et de celuis d'avant. Ces fonctions sont fusionable mais la solution que j'ai trouvé été bien trop compliqué *)
let rec print_aux (t : tree) =
  match t with
  |Leaf(o) -> printOp o
  |MonoOp(o, t) -> (printOp o) ^ (print_aux t)
  |BiOp(Op('*'), t1, t2) -> (print_aux t1) ^ "*"  ^ (print_aux t2)
  |BiOp(Op('/'), t1, t2) -> (print_aux t1) ^ "/"  ^ (print_aux t2)
  |BiOp(Op('+'), t1, t2) -> "(" ^ (print_aux t1) ^ "+"  ^ (print_aux t2) ^ ")"
  |BiOp(o, t1, t2) -> "(" ^ (print_aux t1) ^ (printOp o) ^ (print_aux t2) ^ ")"
;;

(* fonction print sans les parentheses, cas du debut d'un print ou d'après un plus*)
let rec print (t : tree) =
  match t with
  |Leaf(o) -> printOp o
  |MonoOp(o, t) -> (printOp o) ^ (print_aux t)
  |BiOp(Op('+'), t1, t2) -> (print_aux t1) ^ "+"  ^ (print_aux t2)
  |BiOp(o, t1, t2) -> (print_aux t1) ^ (printOp o) ^ (print_aux t2)
;;
