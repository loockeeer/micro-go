{
  open Lexing
  open Mgoparser

  exception Error of string

  let keyword_or_ident =
  let h = 
    Hashtbl.create 20
  in
  List.iter (fun (s, k) -> Hashtbl.add h s k)
    [ "package",    PACKAGE;
      "import",     IMPORT;
      "type",       TYPE;      
      "struct",     STRUCT;
      "else",       ELSE;
      "if",         IF;
      "for",        FOR;
      "false",      FALSE;
      "true",       TRUE;
      "return",     RETURN;
      "func",       FUNC;
      "var",        VAR;
      "nil",        NIL;

      "int",        TINT;
      "bool",       TBOOL;
      "string",     TSTRING;

      "fmt",        FMT;
      "Print",      PRINT;
      "new",        NEW;
    ] ;
  fun s ->
    try  Hashtbl.find h s
    with Not_found -> IDENT(s);;

  let candidate_for_semi = ref false;;
  let ms () = candidate_for_semi := true;;
  let nms () = candidate_for_semi := false;;
}

let digit = ['0'-'9']
let hexa = ['0'-'9' 'a'-'f' 'A'-'F']
let number = digit+ | (('0' ('x' | 'X')) hexa+)
let alpha = ['a'-'z' 'A'-'Z' '_']
let char = ([' '-'~'] # ['\\' '"']) | ("\\\\" | "\\\"" | "\\n" | "\\t")
let ident = alpha (alpha | digit)*
let fmt = "fmt" 

rule token =
  parse
  | ['\n']                        { 
      new_line lexbuf;
      if !candidate_for_semi 
      then (nms(); SEMI) 
      else (nms();token lexbuf) 
}
  | [' ' '\t' '\r']+              { token lexbuf }

  | '"' fmt '"'                   { ms(); STRING("fmt") }
  | '"' char* as chaine '"'       { ms(); STRING(chaine) }
  | number as n  { try ms(); INT(Int64.of_string n) 
                   with _ -> raise (Error "literal constant too large") }
  | ident as id  { let kw = keyword_or_ident id in 
  (match kw with
  | RETURN | TRUE | FALSE | NIL | IDENT _ -> ms()
  | _ -> nms());
  kw }

  | ";"  { nms(); SEMI }
  | "("  { nms(); LPAR }
  | ")"  { ms (); RPAR }
  | "{"  { nms(); BEGIN }
  | "}"  { ms (); END }
  | "*"  { nms(); STAR }
  | "++" { ms (); INCR }
  | "--" { ms (); DECR }
  | "==" { nms(); EQ }
  | "!=" { nms(); NEQ }
  | "<"  { nms(); LT }
  | "<=" { nms(); LE }
  | ">"  { nms(); GT }
  | ">=" { nms(); GE }
  | "+"  { nms(); PLUS }
  | "-"  { nms(); MINUS }
  | "%"  { nms(); PERCENT }
  | "&&" { nms(); AMPAMP }
  | "||" { nms(); PIPEPIPE }
  | "."  { nms(); DOT }
  | "!"  { nms(); BANG }
  | ","  { nms(); COMMA }
  | ":=" { nms(); DEFSET }
  | "="  { nms(); SET }
  | "//" (_ # '\n')* "\n" { nms(); new_line lexbuf; token lexbuf }
  | "/*" { nms();comment lexbuf; token lexbuf }
  | "/"  { nms(); SLASH }
  | eof  { nms(); EOF }
  | _    { raise (Error ("unknown character : " ^ lexeme lexbuf)) }

and comment = parse
  | '\n' { new_line lexbuf; comment lexbuf }
  | "*/" { () }
  | _    { comment lexbuf }
  | eof  { raise (Error "unterminated comment") }
