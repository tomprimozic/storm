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
      statements EOF                                        { $result = $statements.result; }
    ;

statements returns [Seq<Node> result]:
      (stmts+=statement (';' stmts+=statement)*)? ';'?      { $result = seq($stmts); }
    ;

statement returns [Node result]:
      expr                                                  { $result = $expr.result; }
    | pattern '=' expr                                      { $result = assign($pattern.result, $expr.result); }
    | decl=('let' | 'var') pattern '=' expr                 { $result = declare($decl, $pattern.result, $expr.result); }
    | ('fun' | 'function') logic '{' statements '}'         { $result = function($logic.result, $statements.result); }
    | 'print' expr                                          { $result = print($expr.result); }
    | 'while' expr '{' statements '}'                       { $result = while_($expr.result, $statements.result); }
    | block_if                                              { $result = $block_if.result; }
    | 'return'                                              { $result = return_(); }
    | 'return' expr                                         { $result = return_($expr.result); }
    | 'break'                                               { $result = break_(); }
    | 'continue'                                            { $result = continue_(); }
    ;

block_if returns [Node result]:
      'if' expr '{' statements '}'                          { $result = block_if($expr.result, $statements.result); }
    | 'if' expr '{' then_block=statements '}' 'else' '{' else_block=statements '}'              { $result = block_if($expr.result, $then_block.result, $else_block.result); }
    | 'if' expr '{' then_block=statements '}' 'else' block_if       { $result = block_if($expr.result, $then_block.result, $block_if.result); }
    ;


pattern returns [Node result]:
      atomic                { $result = $atomic.result; }
    | op=('^' | '*' | '/' | '+' | '-' | '==' | '!=' | '<' | '>' | '<=' | '>=')                      { $result = ident($op); }
    ;

expr returns [Node result]:
      logic                                                 { $result = $logic.result; }
    | 'if' cond=expr 'then' then_expr=expr 'else' else_expr=expr    { $result = if_($cond.result, $then_expr.result, $else_expr.result); }
    | IDENT '->' expr                                       { $result = arrow($IDENT, $expr.result); }
    | '(' (params+=IDENT (',' params+=IDENT)* ','?)? ')' '->' expr  { $result = arrow($params, $expr.result); }
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
      <assoc=right>left=arithmetic op='^' right=arithmetic  { $result = op($left.result, $op, $right.result); }
    | left=arithmetic op=('*' | '/') right=arithmetic       { $result = op($left.result, $op, $right.result); }
    | op=('+' | '-') '(' args+=expr (',' args+=expr )+ ','? ')'     { $result = call($op, $args); }
    | op=('^' | '*' | '/' | '==' | '!=' | '<' | '>' | '<=' | '>=') '(' (args+=expr (',' args+=expr )* ','?)? ')'       { $result = call($op, $args); }
    | op=('-' | '+') arithmetic                             { $result = op($op, $arithmetic.result); }
    | atomic                                                { $result = $atomic.result; }
    | op=('^' | '*' | '/' | '+' | '-' | '==' | '!=' | '<' | '>' | '<=' | '>=')                      { $result = ident($op); }
    | left=arithmetic op=('+' | '-') right=arithmetic       { $result = op($left.result, $op, $right.result); }
    ;

atomic returns [Node result]:
      INTEGER               { $result = integer($INTEGER); }
    | IDENT                 { $result = ident($IDENT); }
    | 'true'                { $result = bool(true); }
    | 'false'               { $result = bool(false); }
    | '(' stmts+=statement (';' stmts+=statement)* ';'? ')'     { $result = sequence($stmts); }
    | (parts+=STRING_PART exprs+=expr)* parts+=STRING_END       { $result = str($parts, $exprs); }
    | value=atomic '.' IDENT                                    { $result = field($value.result, $IDENT); }
    | fn=atomic '(' (args+=expr (',' args+=expr )* ','?)? ')'   { $result = call($fn.result, $args); }
    | '{' (fields+=IDENT '=' values+=expr (',' fields+=IDENT '=' values+=expr)* ','?)? '}'          { $result = record($fields, $values); }
    ;
