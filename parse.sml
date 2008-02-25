structure Parse : sig val parse : string -> Absyn.stm end =
struct
  structure GcLrVals = GcLrValsFun(structure Token = LrParser.Token)
  structure Lex = GcLexFun(structure Tokens = GcLrVals.Tokens)
  structure GcP = Join(structure ParserData = GcLrVals.ParserData
			structure Lex=Lex
			structure LrParser = LrParser)
  val i : string ref = ref ""

  fun parse inp = let
	  val _ = i := inp
	  fun get _ = let 
		val x = !i
		val _ = i := ""
	   in x end
	  fun parseerror(s,p1,p2) = print ("Parse Error: " ^ s ^ "\n")
	  val lexer = LrParser.Stream.streamify (Lex.makeLexer get)
	  val (absyn,_) = GcP.parse(30,lexer,parseerror,())
       in 
	    absyn
      end

end


