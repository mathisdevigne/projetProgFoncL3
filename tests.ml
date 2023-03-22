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

(* Les fonctions de tests prennent en paramètres les données pour appeler la fct ainsi que la valeur de retour desirer *)

(*Test getOp *)

let testGetOp (s : string) (o : op) : bool =
  o = getOp(s)
;;

let testGetOpError (s : string) (error : string) : bool =
  match getOp(s) with
  | exception(Failure error) -> true
  | _ -> false
;;

(*===========*)

(* Test getFunBiOp *)

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
    printTest ("getOp Op Test") (testGetOp "+" (Op '+')); (* Test pour l'Opération*)
    printTest ("getOp Var Test") (testGetOp "e" (Var 'e')); (* Test pour une variable *)
    printTest ("getOp Const Test") (testGetOp "3" (Const 3)); (* Test pour une constante *)
    printTest ("getOpError") (testGetOpError "R" "Pas une operation"); (* Test pour l'erreur *)
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
    printTest ("parse") (testParse l tr);
    printTest ("parseError empty list") (testParseError [] "Liste vide");
    printTest ("parseError not enought arities") (testParseError lerror "Pas assez d'arites, liste donnee incoherente");

    (* ========== *)

    print_endline("");

    (* Test simplify *)

    print_endline("---------------");
    print_endline("Test simplify :");
    print_endline("---------------");
    printTest ("simplify") (testSimplify (parse ["x"; "x";"/"]) (Leaf (Const 1)) );
    printTest ("simplify") (testSimplify (parse ["x"; "4"; "x";"/"]) (BiOp (Op '/', Leaf (Const 4), Leaf (Var 'x'))) );
    (* Il faut un test pour le MonoOp *)

    (* ============= *)
  )
;;
