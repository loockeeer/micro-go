open Mgoast
open Mips

let env = Hashtbl.create 10;;

let new_label =
  let cpt = ref (-1) in
  fun () -> incr cpt; Printf.sprintf "_label_%i" !cpt

(* le résultat de l'expression est dans le registre $t0,
   la pile est utilisée pour les valeurs intermédiaires *)
let rec tr_expr env (e: Ir.expr) : asm * asm = 
    match e with
  | Int(n)  -> li t0 (Int64.to_int n), Nop   (* on supposera que les constantes entières
                                           sont représentables sur 32 bits *)
  | Bool b -> li t0 (if b then 1 else 0), Nop
  | String(s) -> 
          let lbl = new_label () in 
          let data = label lbl @@ asciiz s in
          S (Printf.sprintf "li t0,%s" lbl), data
  | Var(id) -> let shift = Hashtbl.find env id in
    lw t0 shift fp, Nop
  | New(size) ->
          li v0 9
          @@ li a0 size
          @@ syscall
          @@ move t0 v0, Nop
  | Unop(op, expr) ->
          let op = match op with
          | Opp -> opp
          | Not -> not_
          in
          tr_expr env expr
          $@@ op t0 t0
  | Binop(bop, e1, e2) ->
    let op = match bop with
      | Add -> add
      | Sub -> sub
      | Mul -> mul
      | Div -> div
      | Rem -> rem
      | Lt  -> slt
      | And -> and_
      | Or -> or_
      | Le -> sle
      | Gt -> sgt
      | Ge -> sge
      | Eq -> seq
      | Neq -> sne
    in
       tr_expr env e2
    $@@ vpush t0
    $@@$ tr_expr env e1
    $@@ vpop t1
    $@@ op t0 t0 t1
  | Call (s, exprs) ->
            addi sp sp (-(List.length exprs))
            @@$ ((List.fold_left (fun (shift, acc) e -> (4 + shift, 
                 acc
              $@@$ tr_expr env e
              $@@ sw t0 shift fp
            )) (4, (Nop, Nop)) exprs) |> snd)
          $@@ jal s
  | Nil -> li t0 0, Nop
  | Dummy -> S "li t0,dummy", Nop
  | DerefShift(expr, shift) ->
          tr_expr env expr
          $@@ lw t0 shift t0
  | Print _ -> failwith ""

let rec tr_seq env = function
  | []   -> Nop, Nop
  | [i]  -> tr_instr env i
  | i::s -> tr_instr env i $@@$ tr_seq env s

and tr_instr env (instr: Ir.instr) : asm * asm = match instr with
  | If(c, s1, s2) ->
    let then_label = new_label()
    and end_label = new_label()
    in
         tr_expr env c
    $@@  bnez t0 then_label
    $@@$ tr_seq env s2
    $@@  b end_label
    $@@  label then_label
    $@@$ tr_seq env s1
    $@@  label end_label

  | For(c, s) ->
    let test_label = new_label()
    and code_label = new_label()
    in
    (    b    test_label
    @@   label code_label)
    @@$  tr_seq env s
    $@@  label test_label
    $@@$ tr_expr env c
    $@@  bnez t0 code_label
  | Inc e | Dec e ->
        let action = match instr with Inc _ -> 1 | _ -> (-1) in
        (match e with
        | Var i ->
            let shift = Hashtbl.find env i in
                lw t0 shift fp
            @@  addi t0 t0 action
            @@  sw t0 shift fp, Nop
        | DerefShift(e, shift) ->
                tr_expr env e
            $@@ lw t1 shift t0
            $@@ addi t1 t1 action
            $@@ sw t1 shift t0
        | _ -> failwith "runtime error")
  | Block s -> tr_seq env s
  | SetRefShift(e1, shift, e2) ->
          tr_expr env e2
          $@@ move t1 t0
          $@@$ tr_expr env e1
          $@@ sw t1 shift t0
  | SetVariable (s, e) ->
          let shift = Hashtbl.find env s in
          tr_expr env e
          $@@ sw t0 shift fp
   | Return e ->
           (match e with 
            | None -> Nop, Nop
            | Some e -> tr_expr env e)
            $@@ lw ra (-4) fp (* on restaure ra *)
            $@@ lw fp 0 fp (* puis fp *)
               (* redimensionnement de la pile : 2 * 4 pour ra et fp, et 4 *
                  le nombre de variables dans ce contexte *)
            $@@ addi sp sp (8 + 4 * (Hashtbl.length env)) 
            $@@ jr ra (* jump vers l'appelante *)
    | Expr e ->
            tr_expr env e

let tr_fun (df: Ir.func_def) =
    Hashtbl.reset env;
    List.iteri (fun k p -> Hashtbl.add env p (4*(k+1))) df.params;
    List.iteri (fun k l -> Hashtbl.add env l ((-4)*(k+2))) df.locals;
       label df.fname
    @@$ tr_seq env df.body
    $@@ (if df.fname = "main" then 
        li v0 10
        @@ syscall else Nop)

let rec tr_ldecl = function
    df::p -> tr_fun df $@@$ tr_ldecl p
  | [] -> Nop, Nop

let tr_prog p = 
    let text, data = tr_ldecl p in
{ 
    text;
    data; 
}
