type pos = ErrorMsg.pos
type svalue = Tokens.svalue
type ('svalue,'pos) token = ('svalue,'pos) Tokens.token
type lexresult  = (svalue,ErrorMsg.pos) token

val newLine = ErrorMsg.newLine
val numNested = ref 0

fun make_pos (yypos,yytext) : ErrorMsg.pos2
    = (yypos, yypos + String.size(yytext) - 1)

fun make_proj_args (yypos, yytext) : int * ErrorMsg.pos * ErrorMsg.pos =
    let val (pos1, pos2) = make_pos (yypos, yytext) 
        val value        = valOf(Int.fromString(String.substring (yytext, 1, pos2 - pos1))) in
    (value, pos1, pos2)
end

fun make_id_args (yypos, yytext) : string * ErrorMsg.pos * ErrorMsg.pos =
    let val (pos1, pos2) = make_pos (yypos, yytext)
        val str          = yytext in
    (str, pos1, pos2)
end

fun make_int_args (yypos, yytext) : int * ErrorMsg.pos * ErrorMsg.pos =
    let val (pos1, pos2) = make_pos(yypos,yytext)
        val value        = valOf(Int.fromString(yytext)) in
    (value, pos1, pos2)
end

fun is_num_valid (yytext) : bool =
    if String.size yytext > 10 then false
    else
        let fun padding (acc, len) =
            if len >= 10 then acc
            else padding ("0" ^ acc, len + 1) in
        String.compare (padding (yytext, String.size yytext), "1073741824") = LESS
end

(* Handling EOF.  Note that this function reports the wrong file-position for
   end-of-file.  Because of a design infelicity of ML-Lex, it's possible but
   not easy to get access to the correct file position.  There is a way to 
   do it using the %arg feature of ML-Lex, but you don't need to bother 
   with it for this exercise. 
*)
fun eof () = 
     Tokens.EOF(0,0)

%%

%s COMMENT;
%header (functor FunLexFun(structure Tokens: Fun_TOKENS));

alpha = [A-Za-z];
digit = [0-9];

%%
<INITIAL>"->"       => (Tokens.ARROW(make_pos(yypos, yytext)));
<INITIAL>"fun"      => (Tokens.FUN(make_pos(yypos, yytext)));
<INITIAL>"in"       => (Tokens.IN(make_pos(yypos, yytext)));
<INITIAL>"let"      => (Tokens.LET(make_pos(yypos, yytext)));
<INITIAL>"else"     => (Tokens.ELSE(make_pos(yypos, yytext)));
<INITIAL>"then"     => (Tokens.THEN(make_pos(yypos, yytext)));
<INITIAL>"if"       => (Tokens.IF(make_pos(yypos, yytext)));
<INITIAL>":="       => (Tokens.ASSIGN(make_pos(yypos, yytext)));
<INITIAL>"!"        => (Tokens.BANG(make_pos(yypos, yytext)));
<INITIAL>"ref"      => (Tokens.REF(make_pos(yypos, yytext)));
<INITIAL>"do"       => (Tokens.DO(make_pos(yypos, yytext)));
<INITIAL>"while"    => (Tokens.WHILE(make_pos(yypos, yytext)));
<INITIAL>"||"       => (Tokens.OR(make_pos(yypos, yytext)));
<INITIAL>"not"      => (Tokens.NOT(make_pos(yypos, yytext)));
<INITIAL>"&"        => (Tokens.AND(make_pos(yypos, yytext)));
<INITIAL>">"        => (Tokens.GT(make_pos(yypos, yytext)));
<INITIAL>"="        => (Tokens.EQ(make_pos(yypos, yytext)));
<INITIAL>"<"        => (Tokens.LT(make_pos(yypos, yytext)));
<INITIAL>"#"(0 | [1-9]{digit}*)
                    => (Tokens.PROJ(make_proj_args(yypos, yytext)));
<INITIAL>"#"0{digit}+
                    => (ErrorMsg.error(make_pos(yypos, yytext), "Invalid PROJ format"); continue());
<INITIAL>"*"        => (Tokens.TIMES(make_pos(yypos, yytext)));
<INITIAL>"-"        => (Tokens.MINUS(make_pos(yypos, yytext)));
<INITIAL>"+"        => (Tokens.PLUS(make_pos(yypos, yytext)));
<INITIAL>")"        => (Tokens.RPAREN(make_pos(yypos, yytext)));
<INITIAL>"("        => (Tokens.LPAREN(make_pos(yypos, yytext)));
<INITIAL>":"        => (Tokens.COLON(make_pos(yypos, yytext)));
<INITIAL>";"        => (Tokens.SEMICOLON(make_pos(yypos, yytext)));
<INITIAL>","        => (Tokens.COMMA(make_pos(yypos, yytext)));
<INITIAL>{alpha}({alpha} | {digit} | "_")* 
                    => (Tokens.ID(make_id_args(yypos, yytext)));
<INITIAL>{digit}+
                    => (if is_num_valid(yytext) then (Tokens.INT(make_int_args(yypos, yytext)))
                        else (ErrorMsg.error(make_pos(yypos, yytext), "Invalid number range"); continue()));
<INITIAL>{digit}+({alpha} | "_")+{digit}*
                    => (ErrorMsg.error(make_pos(yypos, yytext), "Invalid ID format"); continue());

<INITIAL, COMMENT>(" " | \t | \r)
                    => (continue());
<INITIAL, COMMENT>\n        
                    => (newLine yypos; continue ());

<INITIAL, COMMENT>"/*" 
                    => ((numNested := !numNested + 1); YYBEGIN COMMENT; continue());
<COMMENT>"*/"       => ((numNested := !numNested - 1); 
                        if (!numNested <> 0) then (continue())
                        else (YYBEGIN INITIAL; continue()));
<COMMENT>.          => (continue());

<INITIAL>"*/"       => (ErrorMsg.error(make_pos(yypos, yytext), "Invalid comment closing"); continue());
<INITIAL>.          => (ErrorMsg.error(make_pos(yypos, yytext), "Invalid token"); continue());
