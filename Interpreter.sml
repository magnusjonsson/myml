structure Interpreter = struct
  type expr = AST.expr
  type binding = AST.binding
  type name = string
  structure Env = StringMap
  datatype value = Int of int
                 | Bool of bool
                 | Proc of env * name * expr
                 | Record of (string * value) list
                 | Builtin of value -> value
  withtype env = value Env.map


  exception Undefined of name
  exception Apply of value * value
  exception Builtin

  val emptyEnv = Env.empty

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

  fun matchPair value = matchRecord (value,["0","1"])

  val initialEnv =
      List.foldl
          Env.insert'
          emptyEnv
          [("int+",fn args => case matchPair args of
                                  [Int a,Int b] => Int (a+b)
                                | _ => raise Builtin)
          ,("int=",fn args => case matchPair args of
                                  [Int a,Int b] => Bool (a=b)
                                | _ => raise Builtin)
          ,("int<",fn args => case matchPair args of
                                  [Int a,Int b] => Bool (a<b)
                                | _ => raise Builtin)
          ]

  fun eval (env : env) (expr : expr) : value =
      case expr of
          AST.Int i => Int i
        | AST.Bool b => Bool b
        | AST.Id name =>
          (case Env.find (env,name) : value option of
               SOME value => value
             | NONE => raise Undefined name)
        | AST.Lambda (name,body) =>
          Proc (env,name,body)
        | AST.Apply (e1,e2) =>
          let
              val value = eval env e1
              val function = eval env e2
          in
              (case function of
                   Proc (env, name, body) =>
                   eval (Env.insert (env, name, value)) body
                 | _ => raise Apply (function,value))
          end 
        | AST.Record fields =>
          Record (List.map (fn (name,expr) => (name,eval env expr)) fields)
        | AST.RecordRef (expr,name) =>
          recordRef (eval env expr, name)
  fun run (binding : binding) (env : env) : env =
      case binding of
          AST.Define {name,expr} =>
          Env.insert (env, name, eval env expr)
          

end
