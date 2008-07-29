structure AST = struct
  datatype expr =
           Int of int
         | Bool of bool
         | Id of string
         | Lambda of string * expr
         | Apply of expr * expr
         | Record of (string * expr) list
         | RecordRef of expr * string
  datatype binding = Define of { name : string,
                                 expr : expr }
  datatype module = Module of  { name : string,
                                 exports : string list,
                                 imports : string list,
                                 bindings : binding list
                               }
end
