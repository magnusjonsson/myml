structure Parser = struct
  type 'a parser = Lexer.stream -> ('a * Lexer.stream) option
  fun dropWhile pred =
      let
          fun done stream = SOME ((), stream)
          fun loop stream =
              case Lexer.input1 stream of
		  (token,stream') => if pred token then
					 loop stream'
				     else
					 done stream
      in
          loop
      end
  fun matchToken t1 stream =
      case Lexer.input1 stream of
	  (t, stream) => if t = t1 then SOME (t,stream) else NONE
(*
  fun takeWhile1 pred : Lexer.lexeme list parser =
      let
          fun done taken stream = if List.null taken then NONE else SOME (List.rev taken, stream)
          fun loop taken stream =
              case Lexer.input1 stream of
		  (t, stream') => if pred t then
				      loop (t::taken) stream'
				  else
				      done taken stream
      in
          loop []
      end
*)
  val nop : unit parser = fn stream => SOME ((),stream)
  fun result (f : 'x -> 'y) (p : 'x parser) : 'y parser =
      fn stream =>
         case p stream of
             NONE => NONE
           | SOME (x, stream) => SOME (f x, stream)
  fun const (y : 'y) (p : 'x parser) : 'y parser =
      result (fn _ => y) p
  fun guard (pred : 'x -> bool) (p : 'x parser) : 'x parser =
      fn stream =>
         case p stream of
             NONE => NONE
           | SOME (x, stream) => if pred x then SOME (x, stream) else NONE
  fun seq  (p1 : 'x parser , p2  : 'y parser) : ('x * 'y) parser =
      fn stream =>
         case p1 stream of
             NONE => NONE
           | SOME (x, stream) =>
             case p2 stream of
                 NONE => NONE
               | SOME (y, stream) =>
                 SOME ((x, y), stream)
  fun seq' (p1 : 'x parser, mkp2 : 'x -> 'y parser) : 'y parser =
      fn stream =>
         case p1 stream of
             NONE => NONE
           | SOME (x,stream) => mkp2 x stream
  fun alt (ps : 'x parser list) : 'x parser =
      fn stream =>
         case ps of
             [] => NONE
           | p::pr => case p stream of
                          NONE => alt pr stream
                        | result => result
  fun first (p1,p2) : 'a parser = result #1 (seq (p1,p2))
  fun second (p1,p2) : 'a parser = result #2 (seq (p1,p2))
  fun parseInt stream =
      case Lexer.input1 stream of
	  (Lexer.Int value, stream') => SOME (AST.Int value, stream')
	| _ => NONE
  fun parseId stream =
      case Lexer.input1 stream of
	  (Lexer.Id name, stream') => SOME (AST.Id name, stream')
	| _ => NONE
  fun parseTerm () : AST.expr parser = alt [parseInt, parseId, parseParenExpr ()]
  and parseExprTail (x : AST.expr) : AST.expr parser =
      alt [seq' (matchToken Lexer.Dot,
                 (fn _ => seq' (result (fn y => AST.Apply (x,y))
                                       (parseTerm ()),
                                parseExprTail))),
           const x nop]
  and parseExpr () : AST.expr parser =
      fn stream => (seq' (parseTerm (), parseExprTail)) stream
  and parseParenExpr () : AST.expr parser =
      second (matchToken Lexer.LParen, first (parseExpr (), matchToken Lexer.RParen))
  val parse = parseExpr ()
  fun parseFile fileName =
      let
          val stream = TextIO.openIn fileName
      in
          parse (Lexer.make (TextIO.getInstream stream))
          before TextIO.closeIn stream
      end
end
