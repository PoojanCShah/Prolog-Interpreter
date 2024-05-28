open List 
open Set 
open String 
open Hashtbl 
open Parser

module StringSet = Set.Make(String) 





type sub = 
  | Sub of (string * exp) list


let rec belongs x xs = match xs with
  | [] -> false
  | h :: t -> if (x=h) then true else (belongs x t) 

let rec vars e = match e with
  | Const c -> StringSet.empty
  | Var x -> StringSet.add x StringSet.empty
  | Term(f, ts) -> List.fold_left (StringSet.union) (StringSet.empty) (List.map vars ts)
  | CUT -> StringSet.empty
  | FAIL -> StringSet.empty
  | PList l -> List.fold_left (StringSet.union) (StringSet.empty) (List.map vars l)
  | _ -> StringSet.empty
  

let rec build_hashtable l = match l with
  | [] -> Hashtbl.create 0
  | (x, t) :: tail -> let h = build_hashtable tail in
      Hashtbl.add h x t;
      h

let rec subst sub e = match sub with 
  | Sub s -> 
    let sigma = build_hashtable s in
    match e with
    | Var x -> if (Hashtbl.mem sigma x) then Hashtbl.find sigma x else Var x
    | Const c -> Const c
    | Term (f, ts) -> Term(f, (List.map (subst sub) ts))
    | CUT -> CUT
    | FAIL -> FAIL
    | PList l -> PList (List.map (subst sub) l)
    | Num n -> Num n

let id_subst x = x 

let rec compose_subst l = match l with
  | [] -> id_subst
  | h :: t -> let ans t' = h ((compose_subst t) t') in ans 

exception NOT_UNIFIABLE 

let rec mgu t u  = match t,u with
  | Var x, Var y -> if (x = y) then id_subst else subst (Sub [( x, Var y)])
  | Var x , Const c -> subst (Sub [( x,  Const c)])
  | Const c, Var x -> subst (Sub [( x,  Const c)])
  | Term(f,ts) , Term(f',ts') -> if (f = f') then 
        let rec compose_mgu ts us = match ts,us with
          | [],[] -> id_subst
          | h :: t , h' :: t' ->let sigma' = compose_mgu t t' in  compose_subst ([ mgu (sigma' h) (sigma' h')  ; sigma'])
          | _ -> raise NOT_UNIFIABLE
        in compose_mgu ts ts'
      else
        raise NOT_UNIFIABLE
  | Var x, Term(f,ts) ->  if (StringSet.mem x (vars (Term(f,ts)))) then raise NOT_UNIFIABLE
      else subst (Sub [(x, Term(f,ts))]) 
  | Term(f,ts), Var x ->  if (StringSet.mem x (vars (Term(f,ts)))) then raise NOT_UNIFIABLE
      else subst (Sub [(x, Term(f,ts))]) 
  | Const c, Const c' -> if (c=c') then id_subst else raise NOT_UNIFIABLE
  | CUT , CUT -> id_subst
  | FAIL, FAIL -> id_subst
  | Var x , PList l ->if (StringSet.mem x (vars (PList l))) then raise NOT_UNIFIABLE else subst (Sub [(x, PList l)])
  | PList l, Var x -> if (StringSet.mem x (vars (PList l))) then raise NOT_UNIFIABLE else subst (Sub [(x, PList l)])
  | PList l, PList l' -> if (List.length l = List.length l') then 
      let rec compose_mgu ts us = match ts,us with
        | [],[] -> id_subst
        | h :: t , h' :: t' ->let sigma' = compose_mgu t t' in  compose_subst ([ mgu (sigma' h) (sigma' h')  ; sigma'])
        | _ -> raise NOT_UNIFIABLE
      in compose_mgu l l'
    else raise NOT_UNIFIABLE 
  | Num n, Num n' -> if (n = n') then id_subst else raise NOT_UNIFIABLE
  | Num n , Var x -> subst (Sub [(x, Num n)])
  | Var x, Num n -> subst (Sub [(x, Num n)])
  | _, _ -> raise NOT_UNIFIABLE



let rec print_exp = function
  | Var x -> print_string x
  | Const c -> print_string c


  | Term("List" , ts) -> let printlist = 
    match ts with
    | [] -> print_string "[]"
    | h :: t -> print_string "["; print_exp h; print_string "|" ; List.iter print_exp t; print_string "]"
  in printlist
  



  | Term(f, ts) -> print_string f; print_string "("; List.iter (fun x -> print_exp x; print_string ", ") ts; print_string ")"
  | CUT -> print_string "!"
  | FAIL -> print_string "fail"
  | PList l -> print_string "["; List.iter (fun x -> print_exp x; print_string ", ") l; print_string "]"
  | Num n -> print_int n

let rec print_db db = match db with
  | [] -> ()
  | Clause(h, b) :: tail -> print_exp h; print_string " :- "; List.iter (fun x -> print_exp x; print_string ", ") b; print_string " "; print_db tail

let rec print_goal g = match g with
  | [] -> ()
  | h :: t -> print_exp h; print_string ", "; print_goal t


  let goal_vars g = List.fold_left (StringSet.union) StringSet.empty (List.map vars g)

  let rec rename_vars e = 
    match e with 
    | Var x -> Var ("'" ^ x )
    | Const c -> Const c
    | Term(f, ts) -> Term(f, (List.map rename_vars ts))
    | CUT -> CUT
    | FAIL -> FAIL
    | PList l -> PList (List.map rename_vars l)
    | Num n -> Num n
  
  let rename_vars_goal g = List.map rename_vars g
  let rename_vars_clause (Clause(h,b)) = Clause(rename_vars h, List.map rename_vars b)
  
  let rec rename_vars_db db = match db with
    | [] -> []
    | h :: t -> (rename_vars_clause h) :: (rename_vars_db t)
    
let rec eval_arith t = 
  (* print_exp t; *)
  match t with
  | Num n -> n
  | Term("Plus", [t1; t2]) -> (eval_arith t1) + (eval_arith t2)
  | Term("Minus", [t1; t2]) -> (eval_arith t1) - (eval_arith t2)
  | Term("Times", [t1; t2]) -> (eval_arith t1) * (eval_arith t2)
  | _ -> raise NOT_UNIFIABLE


let rec solve db g sigma = 
 (* print_string "\n goal  ";  print_goal g; print_string "\n"; *)
  match g with 
  | [] -> sigma
  | g1 :: gs ->
    let rec iterate_db db' = match db' with
      | [] -> raise NOT_UNIFIABLE
      | Clause(h, b) :: tail -> 
        match g1 with  
          | CUT -> sigma 
          | FAIL -> raise NOT_UNIFIABLE
          | Term("Is" , [Var x; e]) -> 
            let n = eval_arith e in
            let theta = subst (Sub [(x, Num n)]) in
            let new_sigma = compose_subst [theta ; sigma] in
            let new_goal = (List.map ( theta) (gs)) in
            solve (rename_vars_db db) new_goal new_sigma

          | Term("Greater", [t1;t2]) -> 
            let n1 = eval_arith t1 in
            let n2 = eval_arith t2 in
            if (n1 > n2) then 
              let new_goal = (List.map ( sigma) (gs)) in
              solve (rename_vars_db db) new_goal sigma
            else iterate_db tail

          | Term("Lesser", [t1;t2]) ->
            let n1 = eval_arith t1 in
            let n2 = eval_arith t2 in
            if (n1 < n2) then 
              let new_goal = (List.map ( sigma) (gs)) in
              solve (rename_vars_db db) new_goal sigma
            else iterate_db tail

          | Term("Equal", [t1;t2]) ->
            let n1 = eval_arith t1 in
            let n2 = eval_arith t2 in
            if (n1 = n2) then 
              let new_goal = (List.map ( sigma) (gs)) in
              solve (rename_vars_db db) new_goal sigma
            else iterate_db tail


          | Term("NotEqual", [t1;t2]) ->
            let n1 = eval_arith t1 in
            let n2 = eval_arith t2 in
            if (n1 <> n2) then 
              let new_goal = (List.map ( sigma) (gs)) in
              solve (rename_vars_db db) new_goal sigma
            else iterate_db tail


          | Term("DeepEqual", [t1 ; t2]) ->
            let theta = mgu t1 t2 in
            let new_goal = (List.map ( theta) (gs)) in
            let new_sigma = compose_subst [theta ; sigma] in
            solve (rename_vars_db db) new_goal new_sigma

          | Term("DeepNotEqual", [t1 ; t2]) ->
            if (t1 = t2) then raise NOT_UNIFIABLE
            else 
              let new_goal = (List.map ( sigma) (gs)) in
              solve (rename_vars_db db) new_goal sigma


            
          | _ -> 
        try 
          let theta = mgu g1 h in
          let new_goal = (List.map ( theta) (b@gs)) in
          let new_sigma = compose_subst [theta ; sigma] in
          solve (rename_vars_db db) (new_goal ) new_sigma
          
        with NOT_UNIFIABLE -> iterate_db tail
      in
      iterate_db db

          (* let new_db = List.map (fun (Clause(h,b)) -> Clause( theta h, List.map (theta) b)) db in *)





(*
if the variable is 'x then it is actually x   
*)

let actual x = String.sub x 1 (String.length x - 1)  

let rec print_ans sigma vars = 
  match vars with
  | [] -> ()
  | x :: xs -> print_string (actual x); print_string " = "; print_exp ( sigma (Var (x))); print_string "\n"; print_ans sigma xs


let interpret db g = 
  let g' = rename_vars_goal g in
  let vars = StringSet.elements (goal_vars g') in
  try 
  let sigma = solve db g' id_subst in
  print_endline "true";
  print_ans sigma vars
  with NOT_UNIFIABLE -> print_string "fail\n"


(*

mem(X,[]) :- fail.
mem(X,[X|_]).
mem(X,[H|T]) :- mem(X,T).

*)
