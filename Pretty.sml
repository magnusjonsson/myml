structure Pretty = struct
fun pretty expr =
    case expr of
        AST.Int i => Int.toString i
      | AST.Id id => id
      | AST.Apply (x,f) => "(" ^ pretty x ^ "." ^ pretty f ^ ")"
end
