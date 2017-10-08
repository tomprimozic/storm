lexer grammar StormLexer;


WHITESPACE:         [ \t\r\n]+ -> skip ;

PLUS:       '+' ;
MINUS:      '-' ;
STAR:       '*' ;
SLASH:      '/' ;
SEMICOLON:  ';' ;
LPAREN:     '(' ;
RPAREN:     ')' ;

IF:         'if' ;
THEN:       'then' ;
ELSE:       'else' ;
TRUE:       'true' ;
FALSE:      'false' ;

INTEGER:                    [0-9]+ ;
