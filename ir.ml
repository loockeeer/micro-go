open Mgoast

type expr =
  | Int of int64
  | Bool of bool
  | String of string
  | Unop of unop * expr
  | Binop of binop * expr * expr
  | Var of string
  | DerefShift of expr * int
  | New of int
  | Call of string * expr list
  | Print of expr list
  | Nil
  | Dummy (* variable spéciale *)

and instr =
  | If of expr * seq * seq
  | For of expr * seq
  | Inc of expr
  | Dec of expr
  | Block of seq
  | SetVariable of string * expr
  | SetRefShift of expr * int * expr
  | Return of expr option
  | Expr of expr

and seq = instr list

type func_def =
  { fname : string
  ; params : string list
  ; locals : string list
  ; body : seq
  }

type program = func_def list
