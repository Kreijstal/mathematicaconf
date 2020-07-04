(* Paclet Info File *)

(* created 2017/01/13*)

Paclet[
    Name -> "ResourceSystemClient",
    Version -> "1.3.3",
    MathematicaVersion -> "11.0+",
    Loading -> Automatic,
    Extensions -> 
        {
            {"Kernel", Symbols -> 
                {"System`ResourceObject", "System`ResourceAcquire", "System`ResourceRemove", "System`ResourceSearch", "System`ResourceSubmit", "System`ResourceSubmissionObject"}
            , Root -> "Kernel", Context -> 
                {"ResourceSystemClient`"}
            }, 
            {"FrontEnd", Prepend -> True}
        }
]


