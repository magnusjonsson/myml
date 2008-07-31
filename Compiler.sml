structure Compiler = struct
  type cname = string
  type name = string

  datatype value = Int of cname
                 | Bool of cname
                 | Proc of value -> value
                 | Record of cname * (name * value) list
                 | Void

  exception Apply of value * value
  exception BuiltinExn
  exception RecordRef of value * string

  val gensymCounter = ref 0
  fun gensym () =
      let val c = !gensymCounter in
          gensymCounter := 1+c;
          "gensym"^Int.toString c
      end
  fun mangle name = "x"^name

  fun makeValue (ctype,cexpr) =
      let
          val name = gensym()
      in
          TextIO.print (String.concat [ctype," ",name,"=",cexpr,";\n"]);
          name
      end

  fun makeStruct fields =
      let
          val structName = gensym()
      in
          TextIO.print
          (String.concat
           (List.concat
            [["struct ",structName," { "],
             List.map (fn (fieldName,fieldType) =>
                          String.concat [fieldType," ",mangle fieldName,"; "])
                      fields,
             ["}\n"]]));
          "struct "^structName
      end

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
          val recordName = gensym ()
      in
          TextIO.print
          (String.concat
               (List.concat [[structName," ",recordName," = { "],
                             List.map (fn (name,value) => cname value^", ") fields,
                             ["};\n"]]));
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

  fun matchPair value =
      case matchRecord (value,["0","1"]) of
          [a,b] => (a,b)

  val initialEnv : value AbstractInterpreter.Env.map =
      List.foldl
          AbstractInterpreter.Env.insert'
          AbstractInterpreter.Env.empty
          [
           ("intAdd",Proc (fn args => case matchPair args of
                                          (Int a,Int b) => Int (makeValue ("int",String.concat [a,"+",b]))
                                        | _ => raise BuiltinExn)),
           ("intMul",Proc (fn args => case matchPair args of
                                          (Int a,Int b) => Int (makeValue ("int",String.concat [a,"*",b]))
                                        | _ => raise BuiltinExn)),
           ("intEqual",Proc (fn args => case matchPair args of
                                            (Int a,Int b) => Bool (makeValue ("int",String.concat [a,"==",b]))
                                          | _ => raise BuiltinExn)),
           ("intLess",Proc (fn args => case matchPair args of
                                           (Int a,Int b) => Bool (makeValue ("int",String.concat [a,"<",b]))
                                         | _ => raise BuiltinExn)),
           ("dup",Proc (fn x => makeRecord [("0",x),("1",x)])),
           ("intPrint",Proc (fn x => case x of
                                         Int name =>
                                         (TextIO.print (String.concat
                                                            ["printf(\"%i\",",name,");\n"]);
                                          Void)
                                       | _ => raise BuiltinExn)),
           ("newLine",Proc (fn x => case x of 
                                        Void => (TextIO.print "printf(\"\\n\");"; Void)
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
end
