structure ML = struct
  type expr = AST.expr
  type toplevel_binding = string * expr
  exception UniquifyInternalError
  fun uniquify (env,expr) =
      case expr of
          AST.Int a => expr
        | AST.Var var => AST.Var (StringMap.lookup (env,var))
        | AST.Lambda (var,body) =>
          let val var' = Gensym.gensym "a"
          in AST.Lambda (var',uniquify (StringMap.insert (env,var,var'),body))
          end
        | AST.Apply (expr1,expr2) =>
          AST.Apply (uniquify (env,expr1), uniquify (env,expr2))
        | AST.Closure (procName, variables) =>
          AST.Closure (procName, map (fn v => StringMap.lookup(env,v)) variables)
        | AST.ClosureVar v => expr
  exception UnexpectedClosureVar of string
  fun free expr : StringSet.set =
      case expr of
          AST.Int a => StringSet.empty
        | AST.Var v => StringSet.singleton v
        | AST.Lambda (var,body) =>
          StringSet.difference (free body, StringSet.singleton var)
        | AST.Apply (expr1,expr2) =>
          StringSet.union (free expr1, free expr2)
        | AST.Closure (procName, closedVars) =>
          foldl StringSet.add' (StringSet.empty) closedVars
        | AST.ClosureVar v => raise UnexpectedClosureVar v
  fun mapVars f expr =
      case expr of
          AST.Int a => expr
        | AST.Var v => expr
        | AST.Lambda (var,body) => AST.Lambda (var,mapVars f body)
        | AST.Apply (expr1,expr2) => AST.Apply (mapVars f expr1, mapVars f expr2)
        | AST.Closure (procName, closedVars) => expr
        | AST.ClosureVar v => expr
  fun unlambda expr : expr * expr StringMap.map =
      case expr of
          AST.Int a => (AST.Int a, StringMap.empty)
        | AST.Var v => (AST.Var v, StringMap.empty)
        | AST.Lambda (var,body) =>
          let val (body,bindings) = unlambda body
              val name = Gensym.gensym "anonymous_proc_"
              val variables = free expr
              val body = mapVars (fn (var) =>
                                     if StringSet.member (variables,var) then
                                         AST.ClosureVar var
                                     else
                                         AST.Var var)
                                 body
          in (AST.Closure (name,StringSet.listItems variables),
              StringMap.insert(bindings,name,body))
          end
        | AST.Apply (expr1,expr2) =>
          let val (expr1,bindings1) = unlambda expr1
              val (expr2,bindings2) = unlambda expr2
          in (AST.Apply (expr1,expr2),
              StringMap.unionWith (fn (a,b) => a) (bindings1,bindings2))
          end
        | AST.Closure c => (AST.Closure c, StringMap.empty)
        | AST.ClosureVar v => raise UnexpectedClosureVar v
  fun unlambdaProgram program =
      StringMap.foldli (fn (name,expr,accumBindings) =>
                           case expr of
                               AST.Lambda (arg,body) =>
                               let val (body,bindings) = unlambda(body)
                               in StringMap.insert
                                      (StringMap.unionWith
                                           (fn (a,b) => a)
                                           (bindings,accumBindings),
                                       name,
                                       AST.Lambda(arg,body))
                               end
                             | _ => StringMap.insert (accumBindings,name,expr)
                       )
                       StringMap.empty
                       program
end
