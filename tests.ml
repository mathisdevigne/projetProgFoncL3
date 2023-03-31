#use "projet.ml";;

(* ======================== *)
(* Variables pour les tests *)
(* ======================== *)

let l = ["a";"b";"*";"c";"*";"e";"f";"+";"*"];;
let lerror = ["13";"2";"5";"+";"*";"1";"0";"/";"-";"+"];;

let tr = BiOp (Op '*', BiOp (Op '*', BiOp (Op '*', Leaf (Var 'a'), Leaf (Var 'b')), Leaf (Var 'c')), BiOp (Op '+', Leaf (Var 'e'), Leaf (Var 'f')));;

(* ============================ *)
(* Fonctions de tests Unitaires *)
(* ============================ *)

(* Les fonctions de tests prennent en paramètres les données pour appeler la fct ainsi que la valeur de retour desirée *)

(*Test getOp *)

let testGetOp (s : string) (o : op) : bool =
  o = getOp(s)
;;

(*Test l'erreur de getOp *)

let testGetOpError (s : string) (error : string) : bool =
  match getOp(s) with
  | exception(Failure error) -> true
  | _ -> false
;;

(*===========*)

(* Test l'erreur de getFunBiOp *)

let testGetFunBiOpError (c : char) (error : string) : bool =
  match getFunBiOp(c) with
  |  exception(Failure error) -> true
  | _ -> false
;;

(* =============== *)

(* Test parse *)

let testParse (l : string list) (t : tree) : bool =
  parse(l) = t
;;

(* Test l'erreur de parse *)

let testParseError (l : string list) (error : string) : bool =
  match parse(l) with
  | exception(Failure error) -> true
  | _ -> false
;;

(* ========== *)

(* Test simplify *)

let testSimplify (t : tree) (tres : tree) : bool =
  simplify(t) = tres
;;

(* ========== *)

(* =================== *)
(* Affichage des tests *)
(* =================== *)

(* Affiche si le test est une réussite ou un échec *)
let printTest (name : string) (b : bool) : unit =
  if b then
    print_endline(name ^ " is a success")
  else
    print_endline(name ^ " is unsuccessful")
;;

(* =================== *)
(* Lancement des tests *)
(* =================== *)

let launch() : unit =
  (
    (* Test getOp *)

    print_endline("------------");
    print_endline("Test getOp :");
    print_endline("------------");
    printTest ("getOp Op Test") (testGetOp "+" (Op '+')); (* Test le cas pour l'Opération de getOp*)
    printTest ("getOp Var Test") (testGetOp "e" (Var 'e')); (* Test le cas pour une variable de getOp *)
    printTest ("getOp Const Test") (testGetOp "3" (Const 3)); (* Test le cas pour une constante de getOp *)
    printTest ("getOpError") (testGetOpError "R" "Pas une operation"); (* Test l'erreur pas une opération de getOp *)
    (* ========== *)

    print_endline("");

    (* Test getFunBiOp *)

    print_endline("-----------------");
    print_endline("Test getFunBiOp :");
    print_endline("-----------------");
    printTest ("getFunBiOpError") (testGetFunBiOpError 'd' "Not an biop"); (* Test pour l'erreur *)
    (* =============== *)

    print_endline("");

    (* Test parse *)

    print_endline("------------");
    print_endline("Test parse :");
    print_endline("------------");
    printTest ("parse") (testParse l tr); (* Test la fonction parse avec un liste et l'arbre attendu *)
    printTest ("parseError empty list") (testParseError [] "Liste vide"); (* Test l'erreur liste vide de la fonction parse *)
    printTest ("parseError not enought arities") (testParseError lerror "Pas assez d'arites, liste donnee incoherente"); (* Test l'erreur pas assez d'arites de la fonction parse *)

    (* ========== *)

    print_endline("");

    (* Test simplify *)

    print_endline("---------------");
    print_endline("Test simplify :");
    print_endline("---------------");
    printTest ("simplify MonoOp") (testSimplify (parse ["x"; "2";"5";"*";"+";"~"]) (MonoOp (Op '~', BiOp (Op '+', Leaf (Var 'x'), Leaf (Const 10))))); (* Test du MonoOp de simplify *)
    printTest ("simplify Leaf") (testSimplify (parse ["13"; "2";"5";"*";"+"]) (Leaf (Const 23))); (* Test du Leaf de simplify *)

    (* ============= *)
  )
;;
