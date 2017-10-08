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
      exprs+=expr (';' exprs+=expr)* ';'? EOF               { $result = seq($exprs); }
    ;

expr returns [Node result]:
      arithmetic                                                    { $result = $arithmetic.result; }
    | 'if' cond=expr 'then' then_expr=expr 'else' else_expr=expr    { $result = if_($cond.result, $then_expr.result, $else_expr.result); }
    ;

arithmetic returns [Node result]:
      atomic        { $result = $atomic.result; }
    | left=arithmetic op=('*' | '/') right=arithmetic       { $result = op($left.result, $op, $right.result); }
    | left=arithmetic op=('+' | '-') right=arithmetic       { $result = op($left.result, $op, $right.result); }
    | op=('-' | '+') arithmetic                             { $result = op($op, $arithmetic.result); }
    ;

atomic returns [Node result]:
      INTEGER       { $result = integer($INTEGER); }
    | 'true'        { $result = bool(true); }
    | 'false'       { $result = bool(false); }
    | '(' expr ')'  { $result = $expr.result; }
    ;
