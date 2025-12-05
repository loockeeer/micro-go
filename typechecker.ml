open Mgoast

exception Error of Mgoast.location * string

let error loc s = raise (Error (loc, s))

let rec levenshtein s1 s2 =
  match s1, s2 with
  | [], _ -> List.length s2
  | _, [] -> List.length s1
  | c1 :: t1, c2 :: t2 when c1 = c2 -> levenshtein t1 t2
  | _ :: t1, _ :: t2 ->
    1 + min (min (levenshtein s1 t2) (levenshtein t1 s2)) (levenshtein t1 t2)
;;

let find_closest_opt f l =
  let f = String.to_seq f |> List.of_seq in
  let distances =
    List.map (fun x -> x, levenshtein f (List.of_seq (String.to_seq x))) l
  in
  match List.fast_sort (fun (_, d1) (_, d2) -> d1 - d2) distances with
  | [] -> None
  | (_, d) :: _ when d > 3 -> None
  | (v, _) :: _ -> Some v
;;

let type_error ?hint:hint_opt loc ty_actual ty_expected =
  error
    loc
    (Printf.sprintf
       "expected %s, got %s%s"
       (typ_to_string ty_expected)
       (typ_to_string ty_actual)
       (match hint_opt with
        | Some h -> "Hint : " ^ h
        | None -> ""))
;;

let undefined_any objects_list loc object_name =
  match find_closest_opt object_name objects_list with
  | None -> error loc (Printf.sprintf "undefined: %s" object_name)
  | Some v -> error loc (Printf.sprintf "undefined: %s. Did you mean %s ?" object_name v)
;;

let type_not_found loc ty = error loc (Printf.sprintf "undefined type %s" ty)

(* TODO : ajout did you mean *)
let member_not_found loc mem sname fields =
  match find_closest_opt mem fields with
  | None -> error loc (Printf.sprintf "undefined member %s in struct %s" mem sname)
  | Some v ->
    error
      loc
      (Printf.sprintf "undefined member %s in struct %s. Did you mean %s ?" mem sname v)
;;

let rec combine_fold f acc l1 l2 =
  match l1, l2 with
  | [], [] -> acc
  | [], _ | _, [] -> failwith "sizes do not match"
  | x1 :: t1, x2 :: t2 -> combine_fold f (f acc x1 x2) t1 t2
;;

module Env = Map.Make (String)

let env_keys e = Env.fold (fun k _ acc -> k :: acc) e []

(* 3 environnements pour stocker
     les variables avec leur type,
     les fonctions avec leur signature
     les structures avec leurs champs
*)

type tenv = typ Env.t
type fenv = ident * (ident * typ) list * seq * typ list Env.t 
type senv = (ident * typ * int) list (* l'entier à la fin = le décalage pour
ce champ *)

let dummy = "_"

let add_env l tenv =
  List.fold_left (fun env (x, t) -> if x = dummy then env else Env.add x t env) tenv l
;;

let prog (_, ld) =
  (* collecte les noms des fonctions et des structures sans les vérifier *)
  let fenv, senv =
    List.fold_left
      (fun (fenv, senv) d ->
         match d with
         | Struct s -> 
                 (* Si c'est une structure, on l'ajoute avec le décalage de chaque champ *)
                 fenv, Env.add s.sname.id (List.mapi (fun i (id, typ) -> id, typ, 4*i) s.fields) senv
         | Fun f -> Env.add f.fname.id (f.fname, f.params, f.body, f.return) fenv, senv)
      (Env.empty, Env.empty)
      ld
  in
  let function_names, struct_names = env_keys fenv, env_keys senv in
  let undefined_function, undefined_struct =
    undefined_any function_names, undefined_any struct_names
  in
  let check_typ t : bool =
    match t with
    | TInt | TBool | TString -> true
    | TStruct s -> Env.mem s senv
  in
  let type_struct_field s f =
    match Env.find_opt s senv with
    | None -> failwith "runtime error" (* N'arrive pas *)
    | Some members ->
      (match List.find_opt (fun ({ id; _ }, _, _) -> id = f.id) members with
       | None -> member_not_found f.loc f.id s (List.map (fun ({id;_}, _, _) -> id) members)
       | Some (_, typ, _) -> typ)
  in
  let rec check_fields lf =
    match lf with
    | [] -> ()
    | (ident, t, _) :: tail ->
      if check_typ t then () else type_not_found ident.loc ident.id;
      check_fields tail
  in
  let rec check_expr e typ tenv : unit =
    if e.edesc = Nil
    then ()
    else (
      let typ_e = type_expr e tenv in
      if typ_e <> typ then type_error e.eloc typ_e typ)
  and check_exprs exprs types tenv =
    match exprs, types with
    | [], [] -> ()
    | [], _ | _, [] -> failwith "incorrect number of parameters"
    | e1 :: t1, typ1 :: t2 ->
      check_expr e1 typ1 tenv;
      check_exprs t1 t2 tenv
  and check_call (f, params) ret tenv =
    match Env.find_opt f.id fenv with
    | None -> undefined_function f.loc f.id
    | Some (_, ptypes, _, rtypes) ->
      if rtypes <> ret
      then failwith "non-matching types"
      else (
        match params with
        | [ { edesc = Call (f2, p2); _ } ] ->
          check_call (f2, p2) (List.map snd ptypes) tenv
        | _ -> check_exprs params (List.map snd ptypes) tenv)
  and type_call (f, params) tenv =
    (* Attention, renvoie une liste de types ! *)
    match Env.find_opt f.id fenv with
    | None -> undefined_function f.loc f.id
    | Some (_, ptypes, _, rtypes) ->
      (match params with
       | [ { edesc = Call (f2, p2); _ } ] ->
         check_call (f2, p2) (List.map snd ptypes) tenv
       | _ -> check_exprs params (List.map snd ptypes) tenv);
      rtypes
  and type_expr e tenv : typ =
    match e.edesc with
    | Int _ -> TInt
    | Bool _ -> TBool
    | String _ -> TString
    | Unop (Opp, e) ->
      check_expr e TInt tenv;
      TInt
    | Unop (Not, e) ->
      check_expr e TBool tenv;
      TInt
    | Var i ->
      (match Env.find_opt i.id tenv with
       | Some t -> t
       | None -> undefined_any (env_keys tenv) e.eloc i.id)
    | Dot (e, i) ->
      (match type_expr e tenv with
       | TStruct s -> type_struct_field s i
       | _ as t ->
         type_error
           ~hint:"non-struct expressions cannot be subscripted"
           e.eloc
           t
           (TStruct "any_struct"))
    | Binop ((Eq | Neq), e1, e2) ->
      (match e1.edesc, e2.edesc with
       | Nil, Nil -> error e.eloc "nil cannot be compared to nil"
       | Nil, _ -> type_expr e2 tenv
       | _, Nil -> type_expr e1 tenv
       | _, _ ->
         let t = type_expr e2 tenv in
         check_expr e1 t tenv;
         t)
    | Binop ((And | Or), e1, e2) ->
      check_expr e1 TBool tenv;
      check_expr e2 TBool tenv;
      TBool
    | Binop ((Le | Ge | Gt | Lt), e1, e2) ->
      check_expr e1 TInt tenv;
      check_expr e2 TInt tenv;
      TBool
    | Binop (_, e1, e2) ->
      check_expr e1 TInt tenv;
      check_expr e2 TInt tenv;
      TInt
    | Call (f, p) ->
      (match type_call (f, p) tenv with
       | [] -> error e.eloc (Printf.sprintf "call to %s is no value; used as value" f.id)
       | [ r ] -> r
       | _ ->
         error e.eloc (Printf.sprintf "call to %s is many values; used as value" f.id))
    | New s -> if Env.mem s senv then TStruct s else undefined_struct e.eloc s
    | Print _ -> error e.eloc "print calls cannot be used as values"
    | Nil -> error e.eloc "nil cannot be used as a value"
  in
  let get_rhs_typelist exprl tenv =
    let rec aux exprl acc =
      match exprl with
      | [] -> acc
      | { edesc = Call (f, p); eloc } :: tail ->
        (match type_call (f, p) tenv, acc with
         | [], _ ->
           error eloc (Printf.sprintf "call to %s is no value; used as value" f.id)
         | [ t ], _ -> aux tail ((eloc, t) :: acc)
         | types, [] ->
           (match tail with
            | [] -> List.map (fun t -> eloc, t) types
            | _ ->
              error
                eloc
                (Printf.sprintf "multiple value call to %s in single-value content" f.id))
         | _, _ ->
           error
             eloc
             (Printf.sprintf "multiple value call to %s in single-value content" f.id))
      | x :: tail -> aux tail ((x.eloc, type_expr x tenv) :: acc)
    in
    List.rev (aux exprl [])
  in
  let check_lvalue expr =
    match expr.edesc with
    | Var _ | Dot _ -> ()
    | _ -> failwith "not lvalue !"
  in
  let rec check_instr i ret tenv =
    match i.idesc with
    | Set (lvalues, rvalues) ->
      (* TODO : à refaire proprement *)
      (* D'abord, on fait la distinction rvalues = Call / rvalues = Liste de valeurs *)
      (match rvalues with
       | [ { edesc = Call (f, p); _ } ] ->
         (* Dans ce cas, on peut vérifier la validité de l'appel et récupérer le type de retour avec *)
         let rec aux left right lhsL rhsL =
           match left, right with
           | [], [] -> ()
           | [], _ ->
             let rhsL = rhsL + List.length right in
             error
               i.iloc
               (Printf.sprintf
                  "argument mismatch: expected %d values but call to %s is %d"
                  lhsL
                  f.id
                  rhsL)
           | _, [] ->
             let lhsL = lhsL + List.length left in
             error
               i.iloc
               (Printf.sprintf
                  "argument mismatch: expected %d values but call to %s is %d"
                  lhsL
                  f.id
                  rhsL)
           | expr :: t1, typ :: t2 ->
             check_expr expr typ tenv;
             aux t1 t2 (lhsL + 1) (rhsL + 1)
         in
         aux lvalues (type_call (f, p) tenv) 0 0
       | _ ->
         (* Dans ce cas, on vérifie qu'on a autant d'arguments à gauche qu'à droite et qu'on a que des single-rvalues *)
         let rec aux left right lhsL rhsL =
           match left, right with
           | [], [] -> ()
           | [], _ ->
             let rhsL = rhsL + List.length right in
             error
               i.iloc
               (Printf.sprintf "argument mismatch: expected %d values got %d" lhsL rhsL)
           | _, [] ->
             let lhsL = lhsL + List.length left in
             error
               i.iloc
               (Printf.sprintf "argument mismatch: expected %d values got %d" lhsL rhsL)
           | e1 :: t1, e2 :: t2 ->
             check_expr e1 (type_expr e2 tenv) tenv;
             aux t1 t2 (lhsL + 1) (rhsL + 1)
           (* ici on doit vérifier qu'on a bien le bon typage à gauche et à droite *)
         in
         aux lvalues rvalues 0 0);
      tenv
    | Vars (identifiers, typ, expressions) ->
      let rhs = get_rhs_typelist expressions tenv in
      (match typ with
       | Some t ->
         (match List.find_opt (fun (_, current_typ) -> t <> current_typ) rhs with
          | None -> () (* ok *)
          | Some (loc, bad_type) -> type_error loc bad_type t)
       | None -> () (* dans ce cas, rhs donne le type de chaque variable, aucun pb *));
      (match rhs with
       | [] ->
         List.fold_left
           (fun tenv { id; _ } -> Env.add id (Option.get typ) tenv)
           tenv
           identifiers
       | _ ->
         let lhsL = List.length identifiers in
         let rhsL = List.length rhs in
         if lhsL <> rhsL
         then
           error
             i.iloc
             (Printf.sprintf
                "argument mismatch: %d variables but got %d values"
                lhsL
                rhsL);
         combine_fold
           (fun tenv { id; _ } (_, typ) -> Env.add id typ tenv)
           tenv
           identifiers
           rhs)
    | For (expr, seq) ->
      check_expr expr TBool tenv;
      check_seq seq ret tenv |> ignore;
      tenv
    | Inc expr ->
      check_lvalue expr;
      check_expr expr TInt tenv;
      tenv
    | Dec expr ->
      check_lvalue expr;
      check_expr expr TInt tenv;
      tenv
    | If (expr, then_s, else_s) ->
      check_expr expr TBool tenv;
      check_seq then_s ret tenv |> ignore;
      check_seq else_s ret tenv |> ignore;
      tenv
    | Block s -> check_seq s ret tenv
    | Expr { edesc = Print _; _ } -> tenv
    | Expr e ->
      type_expr e tenv |> ignore;
      tenv
    | Return exprs ->
      (* Soit on a exprs = un appel qui renvoie bon type, bon nombre
             * Soit on a exprs = une liste d'expressions à vérifier *)
      (match exprs with
       | [ { edesc = Call (f, p); _ } ] -> check_call (f, p) ret tenv
       | _ -> check_exprs exprs ret tenv);
      tenv
  and check_seq s ret tenv =
    List.fold_left (fun tenv i -> check_instr i ret tenv) tenv s
  in
  let rec check_returns seq =
    match seq with
    | [] -> false
    | [ { idesc = Return _; _ } ] -> true
    | { idesc = Block []; _ } :: tail -> check_returns tail
    | { idesc = Block (h :: t); iloc } :: tail ->
      check_returns (h :: { idesc = Block t; iloc } :: tail)
    | { idesc = If (_, t, e); _ } :: tail ->
      check_returns tail || (check_returns t && check_returns e)
    | _ :: tail -> check_returns tail
  in
  let check_function fname params seq ret =
    let env = add_env (List.map (fun (i, t) -> i.id, t) params) Env.empty in
    check_seq seq ret env |> ignore;
    if ret <> [] && not (check_returns seq)
    then
      error
        fname.loc
        (Printf.sprintf "returning function %s missing a return statement" fname.id)
    else ()
    (* On ne soucie pas de l'environnement
    à la fin de l'exécution d'une fonctione *)
  in
  Env.iter (fun _ lf -> check_fields lf) senv;
  Env.iter (fun _ (decl, params, seq, ret) -> check_function decl params seq ret) fenv;
  ld
;;
