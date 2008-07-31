structure AbstractInterpreter = struct
  type expr = AST.expr
  type binding = AST.binding
  type name = string
  structure Env = StringMap
  type 'value target = { apply : 'value * 'value -> 'value,
                         makeInt : int -> 'value,
                         makeBool : bool -> 'value,
                         makeProc : ('value -> 'value) -> 'value,
                         makeRecord : (name * 'value) list -> 'value,
                         recordRef : 'value * name -> 'value
                       }
  exception Undefined of name

  fun eval (target : 'value target) (env : 'value Env.map) (expr : expr) : 'value =
      case expr of
          AST.Int i => #makeInt target i
        | AST.Bool b => #makeBool target b
        | AST.Id name =>
          (case Env.find (env,name) : 'value option of
               SOME value => value
             | NONE => raise Undefined name)
        | AST.Lambda (name,body) =>
          #makeProc target (fn value => eval target (Env.insert (env,name,value)) body)
        | AST.Apply (e1,e2) =>
          let
              val value = eval target env e1
              val function = eval target env e2
          in
              #apply target (value,function)
          end
        | AST.Record fields =>
          #makeRecord target (List.map (fn (name,expr) => (name,eval target env expr)) fields)
        | AST.RecordRef (expr,name) =>
          #recordRef target (eval target env expr, name)
  fun run (target : 'value target) (binding : binding) (env : 'value Env.map) : 'value Env.map =
      case binding of
          AST.Define {name,expr} =>
          Env.insert (env, name, eval target env expr)
          

end
