(* TODO: compiler should output code into a file and then call gcc. *)

structure Compiler = struct
  type cname = string
  type ctype = string
  type name = string

  datatype value = Int of cname
                 | Bool of cname
                 | Proc of value -> value
                 | Record of cname * (name * value) list
                 | Void

  exception Apply of value * value
  exception BuiltinExn
  exception RecordRef of value * string

  val localOutput : string list ref = ref []
  fun push location value = location := value :: !location
  fun globalPrint string = TextIO.print string
  fun localPrint string = push localOutput string
  fun flushLocal () =
      (List.app globalPrint (List.rev (! localOutput));
       localOutput := [])

  val gensymCounter = ref 0
  fun gensym prefix =
      let val c = !gensymCounter in
          gensymCounter := 1+c;
          prefix^Int.toString c
      end
  fun mangle name = "f"^name

  fun makeValue (ctype,cexpr) =
      let
          val name = gensym "v"
      in
          localPrint (String.concat [ctype," ",name," = ",cexpr,";\n"]);
          name
      end
  val structs = ref [] : ((string * string) list * string) list ref
  fun findStruct fields =
      case List.find (fn (fields2,name) => fields = fields2) (!structs) of
	  NONE => NONE
	| SOME (fields2,name) => SOME name
  fun newStruct fields =
      let
          val structName = gensym "s"
      in
          globalPrint
          (String.concat
           (List.concat
            [["struct ",structName," { "],
             List.map (fn (fieldName,fieldType) =>
                          String.concat [fieldType," ",mangle fieldName,"; "])
                      fields,
             ["};\n"]]));
          "struct "^structName
      end
  fun makeStruct fields =
      case findStruct fields of
	  NONE =>
	  let
	      val name = newStruct fields
	  in
	      structs := (fields,name) :: !structs;
	      name
	  end
	| SOME name => name

  fun cname value =
      case value of
          Bool n => n
        | Int n => n
        | Record (n,_) => n
  fun ctype value =
      case value of
          Bool _ => "int"
        | Int _ => "int"
        | Record (_,fields) => makeStruct (map (fn (name,value) => (name,ctype value)) fields)

  fun makeValueLike (v : value, cexpr : string) =
      case v of
          Bool _ => Bool (makeValue ("int",cexpr))
        | Int _ => Int (makeValue ("int",cexpr))

  fun recordRef (value : value,name : name) : value =
      case value of
          Record (recordName,fields) => (case List.find (fn (n,v) => n = name) fields of
                                             SOME (n,v) => makeValueLike (v, String.concat
                                                                                 [recordName,".",mangle n])
                                           | NONE => raise RecordRef (value,name))
        | _ => raise RecordRef (value,name)
  fun makeRecord fields =
      let
          val structName = makeStruct (List.map (fn (name,value) => (name,ctype value)) fields)
          val recordName = makeValue (structName, String.concat
                                                  (List.concat [["{ "],
                                                                List.map (fn (name,value) => cname value^", ") fields,
                                                                ["}"]]))
      in
          Record (recordName,fields)
      end

  exception MatchRecord of value * string list
  fun matchRecord (value,fieldNames) =
      case value of
          Record (recordName,fields) =>
          if List.length fields = List.length fieldNames then
              List.map (fn f => recordRef (value,f)) fieldNames
          else
              raise MatchRecord (value,fieldNames)
        | _ =>
          raise MatchRecord (value,fieldNames)

  exception MatchPair
  fun matchPair value =
      case matchRecord (value,["0","1"]) of
          [a,b] => (a,b)
	| _ => raise MatchPair

  val initialEnv : value AbstractInterpreter.Env.map =
      List.foldl
          AbstractInterpreter.Env.insert'
          AbstractInterpreter.Env.empty
          [
           ("intAdd",Proc (fn args => case matchPair args of
                                          (Int a,Int b) => Int (makeValue ("int",String.concat [a," + ",b]))
                                        | _ => raise BuiltinExn)),
           ("intMul",Proc (fn args => case matchPair args of
                                          (Int a,Int b) => Int (makeValue ("int",String.concat [a," * ",b]))
                                        | _ => raise BuiltinExn)),
           ("intEqual",Proc (fn args => case matchPair args of
                                            (Int a,Int b) => Bool (makeValue ("int",String.concat [a," == ",b]))
                                          | _ => raise BuiltinExn)),
           ("intLess",Proc (fn args => case matchPair args of
                                           (Int a,Int b) => Bool (makeValue ("int",String.concat [a," < ",b]))
                                         | _ => raise BuiltinExn)),
           ("dup",Proc (fn x => makeRecord [("0",x),("1",x)])),
           ("intPrint",Proc (fn x => case x of
                                         Int name =>
                                         (localPrint (String.concat
                                                          ["printf(\"%i\",",name,");\n"]);
                                          Void)
                                       | _ => raise BuiltinExn)),
           ("newLine",Proc (fn x => case x of 
                                        Void => (localPrint "printf(\"\\n\");\n"; Void)
                                      | _ => raise BuiltinExn))
          ]

  fun apply (value,function) =
      case function of
          Proc proc => proc value
        | _ => raise Apply (function,value)

  fun makeProc app =
      Proc app

  val target : value AbstractInterpreter.target = {
      makeInt = (fn i => Int (makeValue ("int",Int.toString i))),
      makeBool = (fn b => Int (makeValue ("int",if b then "1" else "0"))),
      makeRecord = makeRecord,
      makeProc = makeProc,
      apply = apply,
      recordRef = recordRef
  }
  val eval = AbstractInterpreter.eval target
  val run = AbstractInterpreter.run target

  fun compile (prog : AST.expr) =
      eval initialEnv prog
      before (globalPrint "int main() {\n";
              flushLocal ();
              globalPrint "return 0;\n";
              globalPrint "}\n";
	      structs := []
             )
end
