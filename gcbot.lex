type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (svalue,pos) token

fun eof() = Tokens.EOF(0,0)

fun someOrFail (SOME x) = x
  | someOrFail NONE = raise (Fail "Invalid Constant!")

val tliteral = ref ""
val tlstart = ref 0

%%
%header (functor GcLexFun(structure Tokens : Gc_TOKENS));

%s GCBOT STRINGLIT;

digits=[0-9]+;
real=([0-9]+"."[0-9]*)|([0-9]*"."[0-9]+);
ident=[a-zA-Z_][a-zA-Z0-9_]*;
stringlit="\""([^\"]*)"\"";
ws=[\ \t];
eol=[\n\r];
%%
<INITIAL>{ws}*    => (YYBEGIN GCBOT; continue());

<GCBOT>{digits}    => (Tokens.LINT(someOrFail(Int.fromString yytext),yypos,yypos+size yytext));
<GCBOT>{real}      => (Tokens.LREAL(someOrFail(Real.fromString yytext),yypos,yypos+size yytext));
<GCBOT>"true"      => (Tokens.TRUE(yypos,yypos+4));
<GCBOT>"false"	  => (Tokens.FALSE(yypos,yypos+5));
<GCBOT>"\""        => (YYBEGIN STRINGLIT; tlstart := yypos; tliteral := ""; continue());

<GCBOT>"if"        => (Tokens.IF(yypos,yypos+2));
<GCBOT>"then"      => (Tokens.THEN(yypos,yypos+4));
<GCBOT>"else"      => (Tokens.ELSE(yypos,yypos+4));
<GCBOT>"fn"  	   => (Tokens.FN(yypos,yypos+2));
<GCBOT>"where"     => (Tokens.WHERE(yypos,yypos+5));
<GCBOT>"as"  	   => (Tokens.AS(yypos,yypos+2));

<GCBOT>"and"	   => (Tokens.BAND(yypos,yypos+3));
<GCBOT>"or"	   => (Tokens.BOR(yypos,yypos+2));
<GCBOT>"not"	   => (Tokens.BNOT(yypos,yypos+3));

<GCBOT>{ident}     => (Tokens.IDENT(yytext,yypos,yypos + size yytext));

<GCBOT>"::"					=> (Tokens.CONS(yypos,yypos+2));
<GCBOT>"@"					=> (Tokens.APPEND(yypos,yypos+1));

<GCBOT>"+"					=> (Tokens.PLUS(yypos,yypos+1));
<GCBOT>"-"					=> (Tokens.MINUS(yypos,yypos+1));
<GCBOT>"*"					=> (Tokens.TIMES(yypos,yypos+1));
<GCBOT>"/"					=> (Tokens.DIV(yypos,yypos+1));
<GCBOT>"=="				=> (Tokens.EQ(yypos,yypos+2));
<GCBOT>"!="				=> (Tokens.NEQ(yypos,yypos+2));
<GCBOT>"<="				=> (Tokens.LTEQ(yypos,yypos+2));
<GCBOT>">="				=> (Tokens.GTEQ(yypos,yypos+2));
<GCBOT>"<"					=> (Tokens.LT(yypos,yypos+1));
<GCBOT>">"					=> (Tokens.GT(yypos,yypos+1));
<GCBOT>"="					=> (Tokens.ASSIGN(yypos,yypos+1));
<GCBOT>":="					=> (Tokens.MUTASSIGN(yypos,yypos+2));
<GCBOT>"|="					=> (Tokens.CLAUSEASSIGN(yypos,yypos+2));

<GCBOT>"("					=> (Tokens.LPAR(yypos,yypos+1));
<GCBOT>")"					=> (Tokens.RPAR(yypos,yypos+1));
<GCBOT>"{"					=> (Tokens.LBR(yypos,yypos+1));
<GCBOT>"}"					=> (Tokens.RBR(yypos,yypos+1));
<GCBOT>"["					=> (Tokens.LSQ(yypos,yypos+1));
<GCBOT>"]"					=> (Tokens.RSQ(yypos,yypos+1));
<GCBOT>","					=> (Tokens.COMMA(yypos,yypos+1));
<GCBOT>";"					=> (Tokens.SEMI(yypos,yypos+1));

<STRINGLIT>"\""   => (YYBEGIN GCBOT; Tokens.LSTR(!tliteral,!tlstart,!tlstart + size (!tliteral)));
<STRINGLIT>{eol}  => (Tokens.ERROR(!tlstart,yypos));
<STRINGLIT>"\\\"" => (tliteral := !tliteral ^ "\""; continue());
<STRINGLIT>.      => (tliteral := !tliteral ^ yytext; continue());

<GCBOT>{eol}					=> (Tokens.EOF(0,0));
<GCBOT>{ws}*					=> (continue());
<GCBOT>.					=> (Tokens.ERROR(yypos,yypos+size yytext));

