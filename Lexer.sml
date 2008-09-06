structure Lexer = struct
  datatype lexeme = Id of string
		  | Int of int
		  | LParen
		  | Comma
		  | Dot
		  | RParen
		  | Colon
		  | Unexpected of char
		  | Eof
  datatype state = Cons of lexeme * stream
		 | Nil
		 | Unparsed of TextIO.StreamIO.instream
  withtype stream = state ref

  fun make stream = ref (Unparsed stream)

  fun takeWhile (pred,stream) =
      let
	  fun loop (taken,stream) =
	      case TextIO.StreamIO.input1 stream of
		  NONE => (List.rev taken, stream)
		| SOME (char, rest) =>
		  if pred char then
		      loop (char::taken, rest)
		  else
		      (List.rev taken, stream)
      in
	  loop ([],stream)
      end
  fun readId (firstChar,stream) =
      case takeWhile (Char.isAlphaNum, stream) of
	  (tail,stream) => (Id (String.implode (firstChar::tail)), stream)
  fun readNumber (firstChar,stream) =
      let
	  fun digitToInt digit = Char.ord digit - Char.ord #"0"
	  fun digitsToInt digits =
	      List.foldl (fn (digit,accum) => digitToInt digit + 10 * accum)
			 0 digits
      in
	  case takeWhile (Char.isDigit, stream) of
	      (tail,stream) => (Int (digitsToInt (firstChar::tail)), stream)
      end
  fun input1 lexer =
      case !lexer of
	  Cons result => result
	| Nil => (Eof,lexer)
	| Unparsed stream =>
	  (case TextIO.StreamIO.input1 stream of
	       NONE => (lexer := Nil; (Eof,lexer))
	     | SOME (char,stream) =>
	       let
		   fun yield (lexeme,stream) =
		       let
			   val result = (lexeme, ref (Unparsed stream))
		       in
			   (lexer := Cons result; result)
		       end
	       in
		   if Char.isSpace char then
		       (lexer := Unparsed stream;  input1 lexer) else
		   if Char.isAlpha char then
		       yield (readId (char,stream)) else
		   if Char.isDigit char then
		       yield (readNumber (char,stream))
		   else
		       (case char of
			    #"(" => yield (LParen,stream)
			  | #")" => yield (RParen,stream)
			  | #":" => yield (Colon,stream)
			  | #"." => yield (Dot,stream)
			  | #"," => yield (Comma,stream)
			  | _ => yield (Unexpected char,stream)
		       )
	       end
	  )
end
