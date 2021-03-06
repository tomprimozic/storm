lexer grammar StormLexer;

@header {
  import java.util.Stack;
}

@members {
  private int currentParenCount = 0;
  private Stack<Integer> parenCounts = new Stack<Integer>();
}

tokens { STRING_PART, STRING_END, STRING_EOF }

WHITESPACE:         [ \t\r\n]+ -> skip ;

PLUS:       '+' ;
MINUS:      '-' ;
STAR:       '*' ;
SLASH:      '/' ;
CARET:      '^' ;
SEMICOLON:  ';' ;
LBRACE:     '{' ;
RBRACE:     '}' ;
LBRACKET:   '[' ;
RBRACKET:   ']' ;
ASSIGN:     '=' ;
COMMA:      ',' ;
DOTDOTDOT:  '...' ;
DOTDOT:     '..' ;
DOT:        '.' ;
EQ:         '==' ;
NE:         '!=' ;
GE:         '>=' ;
LE:         '<=' ;
GT:         '>' ;
LT:         '<' ;
ARROW:      '->' ;
ASSIGN_PLUS:    '+=' ;
ASSIGN_MINUS:   '-=' ;
ASSIGN_STAR:    '*=' ;
ASSIGN_SLASH:   '/=' ;

IF:         'if' ;
THEN:       'then' ;
ELSE:       'else' ;
TRUE:       'true' ;
FALSE:      'false' ;
AND:        'and' ;
OR:         'or' ;
LET:        'let' ;
VAR:        'var' ;
PRINT:      'print' ;
WHILE:      'while' ;
FOR:        'for' ;
IN:         'in' ;
RETURN:     'return' ;
BREAK:      'break' ;
CONTINUE:   'continue' ;
FUNCTION:   'function' ;
FUN:        'fun' ;

IDENT:                      [_a-zA-Z] ([-_a-zA-Z0-9]* [_a-zA-Z0-9])? ;

INTEGER:                    [0-9]+ ;

STRING_DOUBLE_START:        '"' -> more, pushMode(STRING_DOUBLE_MODE) ;
STRING_SINGLE_START:        '\'' -> more, pushMode(STRING_SINGLE_MODE) ;
STRING_CONTINUE:            { currentParenCount == 0 }? ')' { currentParenCount = parenCounts.pop(); } -> more, popMode ;

LPAREN:                     '(' { currentParenCount++; };
RPAREN:                     ')' { currentParenCount--; };

mode STRING_DOUBLE_MODE;

STRING_DOUBLE_CHARACTER:    ~[\\"] -> more ;
STRING_DOUBLE_ESCAPE:       '\\' [trn"'\\] -> more ;
STRING_DOUBLE_END:          '"' -> type(STRING_END), popMode ;
STRING_DOUBLE_PART:         '\\(' { parenCounts.push(currentParenCount); currentParenCount = 0; } -> type(STRING_PART), pushMode(DEFAULT_MODE) ;
STRING_DOUBLE_EOF:          EOF -> type(STRING_EOF);


mode STRING_SINGLE_MODE;

STRING_SINGLE_CHARACTER:    ~[\\'] -> more ;
STRING_SINGLE_ESCAPE:       '\\' [trn"'\\] -> more ;
STRING_SINGLE_END:          '\'' -> type(STRING_END), popMode ;
STRING_SINGLE_PART:         '\\(' { parenCounts.push(currentParenCount); currentParenCount = 0; } -> type(STRING_PART), pushMode(DEFAULT_MODE) ;
STRING_SINGLE_EOF:          EOF -> type(STRING_EOF);
