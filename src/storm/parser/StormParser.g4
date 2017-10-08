parser grammar StormParser;

options {
    superClass=Helper;
    tokenVocab=StormLexer;
}

@header {
  import storm.ast.Node;
  import scala.collection.Seq;
}


parse returns [Seq<Node> result]:
      (exprs+=expr (';' exprs+=expr)*)? ';'? EOF            { $result = seq($exprs); }
    ;

expr returns [Node result]:
      logic                                                 { $result = $logic.result; }
    | 'if' cond=expr 'then' then_expr=expr 'else' else_expr=expr    { $result = if_($cond.result, $then_expr.result, $else_expr.result); }
    ;

logic returns [Node result]:
      comparison                                            { $result = $comparison.result; }
    | exprs+=comparison ('and' exprs+=comparison)+          { $result = and($exprs); }
    | exprs+=comparison ('or' exprs+=comparison)+           { $result = or($exprs); }
    ;

comparison returns [Node result]:
      arithmetic                                            { $result = $arithmetic.result; }
    | exprs+=arithmetic (ops+=('==' | '!=' | '<=' | '>=' | '<' | '>') exprs+=arithmetic)+       { $result = cmp($ops, $exprs); }
    ;

arithmetic returns [Node result]:
      atomic        { $result = $atomic.result; }
    | left=arithmetic op=('*' | '/') right=arithmetic       { $result = op($left.result, $op, $right.result); }
    | left=arithmetic op=('+' | '-') right=arithmetic       { $result = op($left.result, $op, $right.result); }
    | op=('-' | '+') arithmetic                             { $result = op($op, $arithmetic.result); }
    ;

atomic returns [Node result]:
      INTEGER               { $result = integer($INTEGER); }
    | IDENT                 { $result = ident($IDENT); }
    | 'true'                { $result = bool(true); }
    | 'false'               { $result = bool(false); }
    | '(' expr ')'          { $result = $expr.result; }
    | (parts+=STRING_PART exprs+=expr)* parts+=STRING_END       { $result = str($parts, $exprs); }
    | record=atomic '.' IDENT                                   { $result = field($record.result, $IDENT); }
    | fn=atomic '(' (args+=expr (',' args+=expr )* ','?)? ')'   { $result = call($fn.result, $args); }
    | '{' (fields+=IDENT '=' values+=expr (',' fields+=IDENT '=' values+=expr)* ','?)? '}'           { $result = record($fields, $values); }
    ;
