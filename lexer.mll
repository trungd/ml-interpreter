{
let reservedWords = [
  (* Keywords *)
  ("else", Parser.ELSE);
  ("false", Parser.FALSE);
  ("if", Parser.IF);
  ("then", Parser.THEN);
  ("true", Parser.TRUE);
  ("in", Parser.IN);
  ("let", Parser.LET);
  ("fun", Parser.FUN);
  ("rec", Parser.REC);
  ("and", Parser.ANDKW);
  ("match", Parser.MATCH);
  ("with", Parser.WITH);
  ("dfun", Parser.DFUN)
]
}

rule main = parse
  (* ignore spacing and newline characters *)
  [' ' '\009' '\012' '\n']+     { main lexbuf }

| "-"? ['0'-'9']+
    { Parser.INTV (int_of_string (Lexing.lexeme lexbuf)) }

| "(" { Parser.LPAREN }
| ")" { Parser.RPAREN }
| "[]" { Parser.EMPTYLIST }
| "[" { Parser.LSQBRACKET }
| "]" { Parser.RSQBRACKET }
| ";;" { Parser.SEMISEMI }
| "+" { Parser.PLUS }
| "-" { Parser.MINUS }
| "*" { Parser.MULT }
| "<" { Parser.LT }
| ">" { Parser.GT }
| "=" { Parser.EQ }
| "||" { Parser.OR }
| "&&" { Parser.AND }
| "->" { Parser.RARROW }
| "|" { Parser.SEP }
| "::" { Parser.TWOCOLONS }
| ";" { Parser.SEMI }

| ['a'-'z'] ['a'-'z' '0'-'9' '_' '\'']*
    { let id = Lexing.lexeme lexbuf in
      try 
        List.assoc id reservedWords
      with
      _ -> Parser.ID id
     }
| '(' '*' [^'*']* '*' ')' { main lexbuf }
| eof { exit 0 }

