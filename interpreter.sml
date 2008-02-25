structure Interpreter =
struct
  structure A = Absyn

  datatype stateValue = String of string
                      | Int of int
                      | Real of real
                      | Bool of bool
                      | Exp of A.exp
                      | List of stateValue list
                      | Tuple of stateValue list (* Should probably carry around arity too.  Oh well. *)
                      | Fn of A.exp * A.exp * state
                      | ForwardRef of stateValue ref
                      | BuiltIn of state -> stateValue -> stateValue
		      | Print of string
                      | Raw of string
  		      | Unit
                      | Nil
  withtype state = (string * stateValue) list (* TODO: do something more sensible than a list here *)

  fun printable (String s) = "\"" ^ s ^ "\""
    | printable (Print s) = s
    | printable (Int i) = Int.toString i
    | printable (Real r) = Real.toString r
    | printable (Bool b) = Bool.toString b
    | printable (Exp e) = A.ppe e
    | printable (List l) = "[" ^ (String.concatWith "," (map printable l)) ^ "]"
    | printable (Tuple t) = "(" ^ (String.concatWith "," (map printable t)) ^ ")"
    | printable (Fn (n,e,_)) = "fn " ^ A.ppe n ^ " = " ^ A.ppe e
    | printable (ForwardRef s) = "FORWARDREF: " ^ (printable (!s))
    | printable (BuiltIn s) = "_builtin_"
    | printable (Raw s) = s
    | printable (Unit) = "()"
    | printable (Nil) = "Nil"

  val globalState : state ref = ref []

  val stateReplay : string ref = ref ""

  fun getVar [] name = raise (Fail ("Identifier " ^ name ^ " not found!"))
    | getVar ((n,v)::t) name = if name = n then v else getVar t name

  fun bindVar state name value = (name,value) :: state

  fun bindGlobalVar name value = globalState := (bindVar (!globalState) name value)

  (* Some basic builtins *)

  fun builtin_say st (String s) = (Print s)
    | builtin_say st _ = Print "Argument is of wrong type."

  fun builtin_onjoin st _ = Nil

  fun builtin_undo st _ = (globalState := tl (!globalState); Print "Done.")

  fun builtin_time st _ = (String (Time.fmt 0 (Time.now ())))
 
  fun builtin_raw st (String s) = Raw s
    | builtin_raw st _ = (Print "Argument is of wrong type.")

  fun builtin_hd st (List l) = ((hd l) handle (Empty) => (Print "hd applied to empty list"))
    | builtin_hd st _ = Print "Argument is of wrong type." 

  fun builtin_tl st (List l) = ((List (tl l)) handle (Empty) => (Print "tl applied to empty list"))
    | builtin_tl st _ = Print "Argument is of wrong type." 

  fun builtin_serialize_s () = 
    let 
      fun serialize (n, ForwardRef _) = ""
        | serialize (n, BuiltIn _) = ""
        | serialize (n, x) = n ^ " = " ^ printable x
    in
	String.concatWith "\n" (List.rev (map serialize (!globalState)))
    end

  fun builtin_serialize st _ =
      let
	val f = TextIO.openOut ("state.gcb")
	val _ = TextIO.output(f, !stateReplay)
	val _ = TextIO.flushOut f
	val _ = TextIO.closeOut f
      in
	(Print "State saved")
      end handle Io => raise (Fail "Error while serializing")

  fun builtin_explode st (String s) = List (map (fn x => (String (Char.toString x))) (String.explode s))
    | builtin_explode st _ = raise (Fail "Invalid non-string argument to explode")

  fun builtin_env st _ = List (map (fn (s,sv) => 
	(Tuple [String s,sv])) (List.filter (fn (_,ForwardRef _) => false | _ => true)  (!globalState)))

  fun initialize_builtins () =
  let
    val _ = bindGlobalVar "say" (BuiltIn builtin_say)
    val _ = bindGlobalVar "_on_join" (BuiltIn builtin_onjoin)
    val _ = bindGlobalVar "undo" (BuiltIn builtin_undo)
    val _ = bindGlobalVar "time" (BuiltIn builtin_time)
    val _ = bindGlobalVar "raw" (BuiltIn builtin_raw)
    val _ = bindGlobalVar "hd" (BuiltIn builtin_hd)
    val _ = bindGlobalVar "tl" (BuiltIn builtin_tl)
    val _ = bindGlobalVar "nil" (List [])
    val _ = bindGlobalVar "explode" (BuiltIn builtin_explode)
    val _ = bindGlobalVar "_serialize" (BuiltIn builtin_serialize)
    val _ = bindGlobalVar "env" (BuiltIn builtin_env)
  in
    ()
  end

  fun interp_list (A.Cons) x (List l) = List (x :: l)
    | interp_list (A.Append) (List l1) (List l2) = List(l1 @ l2)
    | interp_list _ _ _ = Print "Arguments are of wrong type."

  fun interp_plus (Int lhs) (Int rhs) = Int (lhs + rhs)
    | interp_plus (Int lhs) (Real rhs) = Int (lhs + (Real.trunc rhs))
    | interp_plus (Int lhs) (String rhs) = (Int (lhs + (valOf (Int.fromString rhs))) handle (Option) => 
        raise (Fail "Invalid string to int conversion"))
    | interp_plus (Real lhs) (Real rhs) = Real (lhs + rhs)
    | interp_plus (Real lhs) (Int rhs) = Real (lhs + (Real.fromInt rhs))
    | interp_plus (Real lhs) (String rhs) = (Real (lhs + (valOf (Real.fromString rhs))) handle (Option) => 
        raise (Fail "Invalid string to real conversion"))
    | interp_plus (String lhs) (String rhs) = String (lhs ^ rhs)
    | interp_plus (String lhs) (Int rhs) = String (lhs ^ (Int.toString rhs))
    | interp_plus (String lhs) (Real rhs) = String (lhs ^ (Real.toString rhs))
    | interp_plus _ _ = raise (Fail "Plus is not overloaded for operand types.")

  fun interp_numerical (A.Minus) (Int lhs) (Int rhs) = Int (lhs - rhs)
    | interp_numerical (A.Minus) (Real lhs) (Real rhs) = Real (lhs - rhs)
    | interp_numerical (A.Minus) (Int lhs) (Real rhs) = Int (lhs - (Real.trunc rhs))
    | interp_numerical (A.Minus) (Real lhs) (Int rhs) = Real (lhs + (Real.fromInt rhs))
    | interp_numerical (A.Times) (Int lhs) (Int rhs) = Int (lhs * rhs)
    | interp_numerical (A.Times) (Real lhs) (Real rhs) = Real (lhs * rhs)
    | interp_numerical (A.Times) (Int lhs) (Real rhs) = Int (lhs * (Real.trunc rhs))
    | interp_numerical (A.Times) (Real lhs) (Int rhs) = Real (lhs * (Real.fromInt rhs))
    | interp_numerical (A.Divide) (Int lhs) (Int rhs) = Int (lhs div rhs)
    | interp_numerical (A.Divide) (Real lhs) (Real rhs) = Real (lhs / rhs)
    | interp_numerical (A.Divide) (Int lhs) (Real rhs) = Int (lhs div (Real.trunc rhs))
    | interp_numerical (A.Divide) (Real lhs) (Int rhs) = Real (lhs / (Real.fromInt rhs))
    | interp_numerical _ _ _ = raise (Fail "Operator is not overloaded for operand types")

  fun interp_boolean (A.Eq) (Int lhs) (Int rhs) = Bool (lhs = rhs)
    | interp_boolean (A.Eq) (Real lhs) (Real rhs) = Bool (Real.== (lhs, rhs))
    | interp_boolean (A.Eq) (String lhs) (String rhs) = Bool (lhs = rhs)
    | interp_boolean (A.Eq) (List lhs) (List rhs) = interp_listeq lhs rhs
    | interp_boolean (A.Lt) (Int lhs) (Int rhs) = Bool (lhs < rhs)
    | interp_boolean (A.Gt) (Int lhs) (Int rhs) = Bool (lhs > rhs)
    | interp_boolean (A.BAnd) (Bool lhs) (Bool rhs) = Bool (lhs andalso rhs)
    | interp_boolean (A.BOr) (Bool lhs) (Bool rhs) = Bool (lhs orelse rhs)
    | interp_boolean _ _ _ = Bool (false)
  and interp_listeq [] [] = Bool(true)
    | interp_listeq [] (h::t) = Bool(false)
    | interp_listeq (h::t) [] = Bool(false)
    | interp_listeq (h1::t1) (h2::t2) = Bool(((fn (Bool x) => x)(interp_boolean (A.Eq) h1 h2)) andalso 
						((fn (Bool x) => x)(interp_listeq t1 t2)))

  fun interpret_expr (A.Int_e l) st = Int l
    | interpret_expr (A.Real_e l) st = Real l
    | interpret_expr (A.Bool_e l) st = Bool l
    | interpret_expr (A.Unit_e) st = Unit
    | interpret_expr (A.Var_e (n,_)) st = getVar st n
    | interpret_expr (A.String_e s) st = String s
    | interpret_expr (A.If_e(b,tc,fc)) st = 
      let
          val cond = case (interpret_expr b st) of (Bool x) => x 
			| _ => raise (Fail "If requires a boolean condition.")
      in
          if cond then (interpret_expr tc st) else (interpret_expr fc st)
      end
    | interpret_expr (A.Fn_e(i,e)) st = Fn(i,e,st)
    | interpret_expr (A.MutableAssign_e(i,e)) st = (bindGlobalVar i (interpret_expr e st); Nil)
    | interpret_expr (A.Seq_e(e1,e2)) st = (interpret_expr e1 st; interpret_expr e2 st)
    | interpret_expr (A.Apply_e(exp1,exp2)) st =
      let
          val f = interpret_expr exp1 st
          val parm = interpret_expr exp2 st
      in 
	  case f of (Fn(pn,e,st')) =>
		let
		    fun bindTuple [] [] m = m
                      | bindTuple [] (h::t) m = raise (Fail "Tuple arity mismatch")
                      | bindTuple (h::t) [] m = raise (Fail "Tuple arity mismatch")
		      | bindTuple (A.Var_e(h1,_)::t1) (h2::t2) m = bindTuple t1 t2 (bindVar m h1 h2)
                      | bindTuple (_::t1) (_::t2) m = bindTuple t1 t2 m

		    val st'' = case pn of 
                       (A.Var_e(vn,_)) => (bindVar st' vn parm)
                     | (A.Tuple_e(l)) => ((bindTuple l ((fn Tuple(p) => p)parm) st') handle Match => 
                              raise (Fail "Non-tuple passed to function expecting tuple"))
		     | _ => raise (Fail "Invalid parameter")
		in
			interpret_expr e st''
		end
             | (BuiltIn bi) => (bi st parm)
	     | (Exp e) => interpret_expr e st
	     | (ForwardRef fr) => (case (!fr) of (Fn(pn,e,st')) => 
                let
		    fun bindTuple [] [] m = m
                      | bindTuple [] (h::t) m = raise (Fail "Tuple arity mismatch")
                      | bindTuple (h::t) [] m = raise (Fail "Tuple arity mismatch")
		      | bindTuple (A.Var_e(h1,_)::t1) (h2::t2) m = bindTuple t1 t2 (bindVar m h1 h2)
                      | bindTuple (_::t1) (_::t2) m = bindTuple t1 t2 m

		    val st'' = case pn of 
                       (A.Var_e(vn,_)) => (bindVar st' vn parm)
                     | (A.Tuple_e(l)) => ((bindTuple l ((fn Tuple(p) => p)parm) st') handle Match => 
                              raise (Fail "Non-tuple passed to function expecting tuple"))
		     | _ => raise (Fail "Invalid parameter")
		in
			interpret_expr e st''
		end
				      | fr' => raise (Fail ("Unresolved forward reference in " ^ printable fr')))
             | s => raise (Fail ("Non-function applied to argument: " ^ (printable s)))
      end
    | interpret_expr (A.List_e e) st = List (map (fn x => interpret_expr x st) e)
    | interpret_expr (A.Tuple_e e) st = Tuple (map (fn x => interpret_expr x st) e)
    | interpret_expr (A.Infix_e(e1,opr,e2)) st =
      let
          val lhs = interpret_expr e1 st
          val rhs = interpret_expr e2 st
      in
          case opr of 
               A.Plus => interp_plus lhs rhs
             | A.Minus => interp_numerical (A.Minus) lhs rhs
             | A.Times => interp_numerical (A.Times) lhs rhs
             | A.Divide => interp_numerical (A.Divide) lhs rhs
             | A.Eq => interp_boolean (A.Eq) lhs rhs
             | A.Lt => interp_boolean (A.Lt) lhs rhs
             | A.Gt => interp_boolean (A.Gt) lhs rhs
             | A.BAnd => interp_boolean (A.BAnd) lhs rhs
             | A.BOr => interp_boolean (A.BOr) lhs rhs
             | A.Cons => interp_list (A.Cons) lhs rhs
             | A.Append => interp_list (A.Append) lhs rhs
             | _ => raise (Fail "Operator not implemented yet")
      end
          
  val _ = initialize_builtins ()

  fun interpret (A.Assign_s(i,e),src) = 
     let
       val fr = ref Nil
       val _ = bindGlobalVar i (ForwardRef fr)
       val _ = fr := (interpret_expr e (!globalState))
       val _ = bindGlobalVar i (!fr)
       val _ = stateReplay := (!stateReplay) ^ "\n" ^ src
     in
       Nil
     end
    | interpret (A.Clause_s(i,e),src) = raise (Fail "Clause assignment not implemented")
    | interpret (A.Expr_s(e,_),src) = (stateReplay := (!stateReplay) ^ "\n" ^ src; interpret_expr e (!globalState))
	
end
