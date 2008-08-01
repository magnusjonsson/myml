structure TypeChecker = struct
  type name = string

  datatype value = Int
                 | Bool
                 | Record of (string * value) list
                 | Proc of value -> value
                 | Void

  exception Apply of value * value
  exception BuiltinExn
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

  fun matchPair value =
      case matchRecord (value,["0","1"]) of
          [a,b] => (a,b)

  val initialEnv : value AbstractInterpreter.Env.map =
      List.foldl
          AbstractInterpreter.Env.insert'
          AbstractInterpreter.Env.empty
          [
           ("intAdd",Proc (fn args => case matchPair args of
                                          (Int,Int) => Int
                                        | _ => raise BuiltinExn)),
           ("intMul",Proc (fn args => case matchPair args of
                                          (Int,Int) => Int
                                        | _ => raise BuiltinExn)),
           ("intEqual",Proc (fn args => case matchPair args of
                                            (Int,Int) => Bool
                                          | _ => raise BuiltinExn)),
           ("intLess",Proc (fn args => case matchPair args of
                                           (Int,Int) => Bool
                                         | _ => raise BuiltinExn)),
           ("dup",Proc (fn x => Record [("0",x),("1",x)])),
           ("intPrint",Proc (fn x => case x of
                                         Int => Void
                                       | _ => raise BuiltinExn)),
           ("newLine",Proc (fn x => case x of
                                        Void => Void
                                      | _ => raise BuiltinExn))
          ]

  fun apply (value,function) =
      case function of
          Proc proc => proc value
        | _ => raise Apply (function,value)

  val target : value AbstractInterpreter.target = {
      makeInt = (fn _ => Int),
      makeBool = (fn _ => Bool),
      makeRecord = Record,
      makeProc = Proc,
      apply = apply,
      recordRef = recordRef
  }
  val eval = AbstractInterpreter.eval target
  val run = AbstractInterpreter.run target

  fun check prog = eval initialEnv prog
end
