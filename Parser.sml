structure Parser = struct
  structure Stream = TextIO.StreamIO
  type 'a parser = Stream.instream -> ('a * Stream.instream) option 
  fun dropWhile pred =
      let
          fun done stream = SOME ((), stream)
          fun loop stream =
              case Stream.input1 stream of
                  NONE => done stream
                | SOME (t, stream') => if pred t then
                                          loop stream'
                                      else
                                          done stream
      in
          loop
      end
  val dropWhite = dropWhile Char.isSpace
  fun matchChar c1 stream =
      case Stream.input1 stream of
          NONE => NONE
        | SOME (c, stream) => if c = c1 then SOME (c,stream) else NONE
  fun takeWhile1 pred : Char.char list parser =
      let
          fun done taken stream = if List.null taken then NONE else SOME (List.rev taken, stream)
          fun loop taken stream =
              case Stream.input1 stream of
                  NONE => done taken stream
                | SOME (t, stream') => if pred t then
                                           loop (t::taken) stream'
                                       else
                                           done taken stream
      in
          loop []
      end
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
  fun isIdentifierCharacter c = Char.isAlphaNum c
  val identifier = result String.implode (takeWhile1 isIdentifierCharacter)
  fun theIdentifier (name:string) = guard (fn n => n = name) identifier
  val takeDigits1 = takeWhile1 Char.isDigit
  fun first (p1,p2) : 'a parser = result #1 (seq (p1,p2))
  fun second (p1,p2) : 'a parser = result #2 (seq (p1,p2))
  val parseInt =
      result (AST.Int o valOf o Int.fromString o String.implode)
             (alt [result (fn (a,b) => #"~" :: b) (seq (matchChar #"-", takeDigits1)),
                   takeDigits1])
  val parseBool = alt [const (AST.Bool true) (theIdentifier "true"),
                       const (AST.Bool false) (theIdentifier "false")]
  val parseId = result AST.Id identifier
  fun parseTerm () : AST.expr parser = alt [parseInt, parseBool, parseId, parseParenExpr ()]
  and parseExprTail (x : AST.expr) : AST.expr parser =
      alt [seq' (matchChar #".",
                 (fn _ => seq' (result (fn y => AST.Apply (x,y))
                                       (parseTerm ()),
                                parseExprTail))),
           const x nop]
  and parseExpr () : AST.expr parser =
      fn stream => (seq' (parseTerm (), parseExprTail)) stream
  and parseParenExpr () : AST.expr parser =
      second (matchChar #"(", first (parseExpr (), matchChar #")"))
  val parse = parseExpr ()
  fun parseFile fileName =
      let
          val stream = TextIO.openIn fileName
      in
          parse (TextIO.getInstream stream)
          before TextIO.closeIn stream
      end
end
