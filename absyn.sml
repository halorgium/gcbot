structure Absyn =
struct
    type symbol = string
    type pos = int

    datatype oper = Plus
                  | Minus
                  | Times
                  | Divide
                  | Eq
                  | NEq
                  | LtEq
                  | GtEq
                  | Lt
                  | Gt
                  | BAnd
                  | BOr
                  | Cons
                  | Append

    datatype exp = 
   Int_e of int
 | Real_e of real
 | Bool_e of bool
 | Var_e of symbol * symbol option
 | String_e of string
 | If_e of exp * exp * exp
 | FnGuarded_e of symbol * exp * exp list
 | Fn_e of exp * exp
 | MutableAssign_e of symbol * exp
 | Seq_e of exp * exp
 | Apply_e of exp * exp
 | List_e of exp list
 | Unit_e
 | Tuple_e of exp list
 | Infix_e of exp * oper * exp
 | BNot_e of exp
 | GuardBool_e of exp
 | GuardAs_e of symbol * exp

    datatype stm = Assign_s of symbol * exp
                 | Clause_s of symbol * exp
                 | Expr_s of exp * pos

    fun op2s Plus = "+"
      | op2s Minus = "-"
      | op2s Times = "*"
      | op2s Divide = "/"
      | op2s Eq = "=="
      | op2s NEq = "!="
      | op2s LtEq = "<="
      | op2s GtEq = ">="
      | op2s Lt = "<"
      | op2s Gt = ">"
      | op2s BAnd = "and"
      | op2s BOr = "or"
      | op2s Cons = "::"
      | op2s Append = "@"

   fun ppe (Int_e i) = Int.toString i
     | ppe (Real_e r) = Real.toString r
     | ppe (Bool_e r) = Bool.toString r
     | ppe (Var_e(v,_)) = v
     | ppe (String_e r) = "\"" ^ r ^ "\""
     | ppe (If_e(a,b,c)) = "if (" ^ ppe a ^ ") then (" ^ ppe b ^ ") else (" ^ ppe c ^ ")"
     | ppe (Fn_e(n,e)) = "fn " ^ ppe n ^ " = (" ^ ppe e ^ ")"
     | ppe (MutableAssign_e(s,e)) = "(" ^ s ^ " := (" ^ ppe e ^ "))"
     | ppe (Seq_e(e1,e2)) = "(" ^ ppe e1 ^ "; " ^ ppe e2 ^ ")"
     | ppe (Apply_e(e1, e2)) = "(" ^ ppe e1 ^ ") "^ ppe e2
     | ppe (List_e(l)) = "[" ^ (String.concatWith "," (map ppe l)) ^ "]"
     | ppe (Unit_e) = "()"
     | ppe (Tuple_e(l)) = "(" ^ (String.concatWith "," (map ppe l)) ^ ")"
     | ppe (Infix_e(e1,opr,e2)) = "((" ^ ppe e1 ^ " " ^ op2s opr ^ " (" ^ ppe e2 ^ "))"
     | ppe _ = "???"

end

