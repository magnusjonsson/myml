structure Interpreter = struct
  type name = string

  datatype value = Int of int
                 | Bool of bool
                 | Record of (string * value) list
                 | Proc of value -> value
                 | Void
		 | TypeError of string

  exception Apply of value * value
  exception BuiltinExn of string
  exception RecordRef of value * string

  fun recordRef (value,name) =
      case value of
          Record fields => (case List.find (fn (n,v) => n = name) fields of
                                SOME (n,v) => v
                              | NONE => raise RecordRef (value,name))
        | _ => raise RecordRef (value,name)

  exception MatchRecord of value * string list
  fun matchRecord (value,fieldNames) =
      case value of
          Record fields =>
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
                                          (Int a,Int b) => Int (a+b)
                                        | _ => raise BuiltinExn "intAdd")),
           ("intMul",Proc (fn args => case matchPair args of
                                          (Int a,Int b) => Int (a*b)
                                        | _ => raise BuiltinExn "intMul")),
           ("intEqual",Proc (fn args => case matchPair args of
                                            (Int a,Int b) => Bool (a=b)
                                          | _ => raise BuiltinExn "intEqual")),
           ("intLess",Proc (fn args => case matchPair args of
                                           (Int a,Int b) => Bool (a<b)
                                         | _ => raise BuiltinExn "intLess")),
           ("dup",Proc (fn x => Record [("0",x),("1",x)])),
           ("intPrint",Proc (fn x => case x of
                                         Int i => (TextIO.print (Int.toString i); Void)
				       | _ => raise BuiltinExn "intPrint")),
           ("newLine",Proc (fn Void => (TextIO.print "\n"; Void)
			     | _ => raise BuiltinExn "newLine"))
          ]

  fun apply (value,function) =
      case function of
          Proc proc => proc value
        | _ => raise Apply (function,value)

  val target : value AbstractInterpreter.target = {
      makeInt = Int,
      makeBool = Bool,
      makeRecord = Record,
      makeProc = Proc,
      apply = apply,
      recordRef = recordRef
  }
  val eval = AbstractInterpreter.eval target
  val run = AbstractInterpreter.run target

  fun interpret prog =
      eval initialEnv prog
      handle BuiltinExn what => TypeError what
end
