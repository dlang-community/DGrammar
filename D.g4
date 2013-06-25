grammar D;

Assign: '=';
At: '@';
BitAnd: '&';
BitAndEqual: '&=';
BitOr: '|';
BitOrEqual: '|=';
CatEqual: '~=';
Colon: ':';
Comma: ',';
Decrement: '--';
Div: '/';
DivEqual: '/=';
Dollar: '$';
Dot: '.';
Equal: '==';
GoesTo: '=>';
Greater: '>';
GreaterEqual: '>=';
Hash: '#';
Increment: '++';
LBrace: '{';
LBracket: '[';
Less: '<';
LessEqual: '<=';
LessEqualGreater: '<>=';
LessOrGreater: '<>';
LogicAnd: '&&';
LogicOr: '||';
LParen: '(';
Minus: '-';
MinusEqual: '-=';
Mod: '%';
ModEqual: '%=';
MulEqual: '*=';
Not: '!';
NotEqual: '!=';
NotGreater: '!>';
NotGreaterEqual: '!>=';
NotLess: '!<';
NotLessEqual: '!<=';
NotLessEqualGreater: '!<>';
Plus: '+';
PlusEqual: '+=';
Pow: '^^';
PowEqual: '^^=';
RBrace: '}';
RBracket: ']';
RParen: ')';
Semicolon: ';';
ShiftLeft: '<<';
ShiftLeftEqual: '<<=';
ShiftRight: '>>';
ShiftRightEqual: '>>=';
Slice: '..';
Star: '*';
Ternary: '?';
Tilde: '~';
Unordered: '!<>=';
UnsignedShiftRight: '>>>';
UnsignedShiftRightEqual: '>>>=';
Vararg: '...';
Xor: '^';
XorEqual: '^=';
Bool: 'bool';
Byte: 'byte';
Cdouble: 'cdouble';
Cent: 'cent';
Cfloat: 'cfloat';
Char: 'char';
Creal: 'creal';
Dchar: 'dchar';
Double: 'double';
Float: 'float';
Function: 'function';
Idouble: 'idouble';
Ifloat: 'ifloat';
Int: 'int';
Ireal: 'ireal';
Long: 'long';
Real: 'real';
Short: 'short';
Ubyte: 'ubyte';
Ucent: 'ucent';
Uint: 'uint';
Ulong: 'ulong';
Ushort: 'ushort';
Void: 'void';
Wchar: 'wchar';
Align: 'align';
Deprecated: 'deprecated';
Extern: 'extern';
Pragma: 'pragma';
Export: 'export';
Package: 'package';
Private: 'private';
Protected: 'protected';
Public: 'public';
Abstract: 'abstract';
Auto: 'auto';
Const: 'const';
Final: 'final';
Gshared: '__gshared';
Immutable: 'immutable';
Inout: 'inout';
Scope: 'scope';
Shared: 'shared';
Static: 'static';
Synchronized: 'synchronized';
Alias: 'alias';
Asm: 'asm';
Assert: 'assert';
Body: 'body';
Break: 'break';
Case: 'case';
Cast: 'cast';
Catch: 'catch';
Class: 'class';
Continue: 'continue';
Debug: 'debug';
Default: 'default';
Delegate: 'delegate';
Delete: 'delete';
Do: 'do';
Else: 'else';
Enum: 'enum';
False: 'false';
Finally: 'finally';
Foreach: 'foreach';
Foreach_reverse: 'foreach_reverse';
For: 'for';
Goto: 'goto';
If: 'if';
Import: 'import';
In: 'in';
Interface: 'interface';
Invariant: 'invariant';
Is: 'is';
Lazy: 'lazy';
Macro: 'macro';
Mixin: 'mixin';
Module: 'module';
New: 'new';
Nothrow: 'nothrow';
Null: 'null';
Out: 'out';
Override: 'override';
Pure: 'pure';
Ref: 'ref';
Return: 'return';
Struct: 'struct';
Super: 'super';
Switch: 'switch';
Template: 'template';
This: 'this';
Throw: 'throw';
True: 'true';
Try: 'try';
Typedef: 'typedef';
Typeid: 'typeid';
Typeof: 'typeof';
Union: 'union';
Unittest: 'unittest';
Version: 'version';
Volatile: 'volatile';
While: 'while';
With: 'with';

SpecialDate: '__DATE__';
SpecialEof: '__EOF__';
SpecialTime: '__TIME__';
Specialimestamp: '__TIMESTAMP__';
SpecialVendor: '__VENDOR__';
SpecialVersion: '__VERSION__';
SpecialFile: '__FILE__';
SpecialLine: '__LINE__';
SpecialModule: '__MODULE__';
SpecialFunction: '__FUNCTION__';
SpecialPrettyFunction: '__PRETTY_FUNCTION__';
Traits: '__traits';
Parameters: '__parameters';
Vector: '__vector';

Whitespace: [\u0020\u0009\u000b\u000c\u000a\u000d]+ -> skip;
fragment EndOfLine : '\u000d' | '\u000a' | '\u000d' '\u000a' | '\u2028' | '\u2029';

fragment Character: [\u0001-\uffff];
fragment WysiwygCharacter: Character | Whitespace;
fragment HexDigit: [a-fA-F0-9];
fragment OctalDigit: [0-7];
fragment BinDigit: [01];
fragment DecimalDigit: [0-9];

fragment BlockComment: '/*' .*? '*/';
fragment LineComment: '//' (~[\u000D\u000A\u2028\u2029])* (EndOfLine | EOF);
fragment NestingBlockComment: '/+' (NestingBlockComment | .)*? '+/';
Comment : (BlockComment | LineComment | NestingBlockComment) -> skip;

Identifier : ([a-zA-Z_])([a-zA-Z0-9_])*;

fragment WysiwygString : 'r"' .*? '"' StringPostfix?;
fragment AlternativeWysiwygString : '`' .*? '`' StringPostfix?;
fragment EscapeSequence : '\\\''
    | '\\"'
    | '\\\\'
    | '\\?'
    | '\\0'
    | '\\a'
    | '\\b'
    | '\\f'
    | '\\n'
    | '\\r'
    | '\\t'
    | '\\v'
    | '\\x' HexDigit HexDigit
    | '\\' OctalDigit OctalDigit? OctalDigit?
    | '\\u' HexDigit HexDigit HexDigit HexDigit
    | '\\U' HexDigit HexDigit HexDigit HexDigit HexDigit HexDigit HexDigit HexDigit
    | '\\&' Identifier ';';
fragment HexStringChar : [0-9a-fA-F] | Whitespace | EndOfLine;
fragment StringPostfix : [dwc];
fragment DoubleQuotedCharacter : EscapeSequence | ~('"' | '\\' );
fragment DoubleQuotedString : '"' DoubleQuotedCharacter* '"'  StringPostfix?;
fragment HexString: 'x"' HexStringChar* '"' StringPostfix?;
// fragment DelimitedString: 'q"' Delimiter WysiwygCharacter+ MatchingDelimiter '"';
// fragment TokenString: 'q{' Token* '}';
fragment StringFragment : WysiwygString | AlternativeWysiwygString | DoubleQuotedString | HexString /*| DelimitedString | TokenString*/;
StringLiteral : StringFragment (Whitespace? StringFragment)*;

CharacterLiteral: '\'' ( EscapeSequence | ~[\\'] )*? '\'';

IntegerLiteral: Integer IntegerSuffix?;
fragment Integer: BinaryInteger | DecimalInteger | HexadecimalInteger;
fragment IntegerSuffix: ('L' | 'u' | 'U' | 'Lu' | 'LU' | 'uL' | 'UL');
fragment DecimalInteger: DecimalDigit (DecimalDigit | '_')*;
fragment BinaryInteger: ('0b' | '0B') BinDigit (BinDigit | '_')*;
fragment HexadecimalInteger: ('0x' | '0X') HexDigit (HexDigit | '_')*;

FloatLiteral: (FloatOption (FloatSuffix | RealSuffix)?) | (Integer (FloatSuffix | RealSuffix)? ImaginarySuffix);
fragment FloatOption: DecimalFloat | HexFloat;
fragment DecimalFloat
    : DecimalInteger '.' (DecimalDigit (DecimalDigit | '_')* DecimalExponent?)?  // BUG: can't lex a[0..1] properly
    | '.' DecimalInteger DecimalExponent?
    | DecimalInteger DecimalExponent
    ;
fragment DecimalExponent: ('e' | 'E' | 'e+' | 'E+' | 'e-' | 'E-') DecimalInteger;
fragment FloatSuffix: 'F' | 'f';
fragment RealSuffix: 'L';
fragment ImaginarySuffix: 'i';
fragment HexFloat: ('0x' | '0X') ((HexDigit (HexDigit | '_')* '.' HexDigit (HexDigit | '_')*) | ('.' HexDigit (HexDigit | '_')*) | (HexDigit (HexDigit | '_')*)) HexExponent;
fragment HexExponent: ('p' | 'P' | 'p+' | 'P+' | 'p-' | 'P-') DecimalDigit (DecimalDigit | '_')*;

SpecialTokenSequence: '#line' Space+ IntegerLiteral Space* ('"' .*? '"' Space*)? (EndOfLine | EOF) -> skip;
fragment Space: [\u0020\u0009\u000B\u000C];

module: moduleDeclaration? declaration*
    ;

moduleDeclaration: 'module' identifierChain ';'
    ;

declaration: aliasDeclaration
    | aliasThisDeclaration
    | attributedDeclaration
    | classDeclaration
    | conditionalDeclaration
    | constructor
    | destructor
    | enumDeclaration
    | functionDeclaration
    | importDeclaration
    | interfaceDeclaration
    | mixinDeclaration
    | pragmaDeclaration
    | sharedStaticConstructor
    | sharedStaticDestructor
    | staticAssertDeclaration
    | staticConstructor
    | staticDestructor
    | structDeclaration
    | templateDeclaration
    | unionDeclaration
    | unittest
    | variableDeclaration
    ;

importDeclaration: 'import' (singleImport (',' singleImport)* | importBindings) ';'
    ;

importList: singleImport (',' importList)?
    | importBindings
    ;

singleImport: (Identifier '=')? identifierChain
    ;

importBindings: singleImport ':' importBind (',' importBind)*
    ;

importBind: Identifier ('=' Identifier)?
    ;

aliasThisDeclaration: 'alias' Identifier 'this' ';'
    ;

structDeclaration: 'struct' Identifier (templateParameters constraint? structBody | (structBody | ';'))
    ;

templateParameters: '(' templateParameterList? ')'
    ;

constraint: 'if' '(' expression ')'
    ;

structBody: '{' structBodyItem* '}'
    ;

structBodyItem: declaration | postBlit | invariant
    ;

postBlit: 'this' '(' 'this' ')' functionBody
    ;

classDeclaration: 'class' Identifier (templateParameters constraint?)? (':' baseClassList)? classBody
    ;

baseClassList: baseClass (',' baseClass)*
	;

baseClass:
	 typeofExpression ('.' identifierOrTemplateChain)?
	| identifierOrTemplateChain
	;

classBody: '{' declarationOrInvariant* '}'
    ;

declarationOrInvariant : declaration
    | invariant
    ;

invariant: 'invariant' '(' ')' blockStatement
    ;

constructor: 'this' parameters functionBody
    ;

destructor: '~' 'this' '(' ')' functionBody
    ;

interfaceDeclaration: 'interface' Identifier (templateParameters constraint?)? (':' identifierList)? structBody
    ;

unionDeclaration: 'union' Identifier ((templateParameters constraint? structBody)? | (structBody | ';'))
    ;

enumDeclaration: 'enum' Identifier (':' type )? untypedEnumBody
    | 'enum' typedEnumBody
    ;

typedEnumBody: ';'
    | '{' typedEnumMember (',' typedEnumMember?)* '}'
    ;

typedEnumMember: Identifier
    | Identifier type? '=' assignExpression
    ;

untypedEnumBody: ';'
    | '{' untypedEnumMember (',' untypedEnumMember?)* '}'
    ;

untypedEnumMember: Identifier ('=' assignExpression)?
    ;

statement: statementNoCaseNoDefault
    | caseStatement
    | caseRangeStatement
    | defaultStatement
    ;

statementNoCaseNoDefault: labeledStatement
    | blockStatement
    | assignStatement
    | ifStatement
    | whileStatement
    | doStatement
    | forStatement
    | foreachStatement
    | switchStatement
    | finalSwitchStatement
    | continueStatement
    | breakStatement
    | returnStatement
    | gotoStatement
    | withStatement
    | synchronizedStatement
    | tryStatement
    | throwStatement
    | scopeGuardStatement
    | asmStatement
    | foreachRangeStatement
    | conditionalStatement
    | staticAssertStatement
    | assertStatement
    | templateMixinStatement
    | versionSpecification
    | debugSpecification
    | functionCallStatement
    | deleteStatement
    ;

labeledStatement: Identifier ':' statement
    ;

returnStatement: 'return' expression? ';'
    ;

switchStatement: 'switch' '(' expression ')' switchBody
    ;

switchBody: '{' (statement)+ '}'
    ;

finalSwitchStatement: 'final' switchStatement
    ;

caseStatement: 'case' argumentList ':' declarationsAndStatements
    ;

caseRangeStatement: 'case' assignExpression ':' '...' 'case' assignExpression ':' declarationsAndStatements
    ;

defaultStatement: 'default' ':' declarationsAndStatements
    ;

statementNoCaseNoDefault: ';'
    | statementNoCaseNoDefault
    ;

continueStatement: 'continue' Identifier? ';'
    ;

breakStatement: 'break' Identifier? ';'
    ;

gotoStatement: 'goto' (Identifier | 'default' | 'case' expression?) ';'
    ;

withStatement: 'with' '(' (expression | symbol | templateInstance) ')' statementNoCaseNoDefault
    ;

synchronizedStatement: 'synchronized' ('(' expression ')')? statementNoCaseNoDefault
    ;

tryStatement: 'try' statementNoCaseNoDefault (catches | catches finally_ | finally_)
    ;

catches: catch_* lastCatch?
    ;

lastCatch: 'catch' statementNoCaseNoDefault
    ;

catch_: 'catch' '(' type Identifier? ')' statementNoCaseNoDefault
    ;

finally_: 'finally' statementNoCaseNoDefault
    ;

throwStatement: 'throw' expression ';'
    ;

scopeGuardStatement: 'scope' '(' Identifier ')' statementNoCaseNoDefault
    ;

asmStatement: 'asm' '{' asmInstruction+ '}'
    ;

asmInstruction: Identifier
    | 'align' IntegerLiteral
    | 'align' Identifier
    | Identifier ':' asmInstruction
    | Identifier asmExp
    | Identifier operands
    ;

operands: asmExp+
    ;

register: Identifier
    | Identifier '(' IntegerLiteral ')'
    ;

asmExp: asmLogOrExp ('?' asmExp ':' asmExp)?
    ;

asmLogOrExp: asmLogAndExp ('||' asmLogAndExp)?
    ;

asmLogAndExp: asmOrExp ('&&' asmOrExp)?
    ;

asmOrExp: asmXorExp ('|' asmXorExp)?
    ;

asmXorExp: asmAndExp ('^' asmAndExp)?
    ;

asmAndExp: asmEqualExp ('&' asmEqualExp)?
    ;

asmEqualExp: asmRelExp (('==' | '!=') asmRelExp)?
    ;

asmRelExp: asmShiftExp (('<' | '<=' | '>' | '>=') asmShiftExp)?
    ;

asmShiftExp: asmAddExp (('<<' | '>>' | '>>>') asmAddExp)?
    ;

asmAddExp: asmMulExp (('+' | '-') asmMulExp)?
    ;

asmMulExp: asmBrExp (('*' | '/' | '%') asmBrExp)?
    ;

asmBrExp: asmUnaExp
    | asmBrExp '[' asmExp ']'
    ;

asmUnaExp: asmTypePrefix asmExp
    | Identifier asmExp
    | '+' asmUnaExp
    | '-' asmUnaExp
    | '!' asmUnaExp
    | '~' asmUnaExp
    | asmPrimaryExp
    ;

asmPrimaryExp: IntegerLiteral
    | FloatLiteral
    | '$'
    | register
    | identifierChain
    ;

asmTypePrefix: Identifier Identifier
    | 'byte' Identifier
    | 'short' Identifier
    | 'int' Identifier
    | 'float' Identifier
    | 'double' Identifier
    | 'real' Identifier
    ;

pragmaDeclaration: pragmaExpression ';'
    ;

pragmaExpression: 'pragma' '(' Identifier (',' argumentList)? ')'
    ;

foreachRangeStatement: 'foreach' '(' foreachType ';' expression '..' expression ')' statementNoCaseNoDefault
    ;

conditionalStatement: compileCondition statementNoCaseNoDefault ('else' statementNoCaseNoDefault)?
    ;

compileCondition: versionCondition
    | debugCondition
    | staticIfCondition
    ;

versionCondition: 'version' '(' (IntegerLiteral | Identifier | 'unittest' | 'assert') ')'
    ;

versionSpecification: 'version' '=' (Identifier | IntegerLiteral) ';'
    ;

castExpression: 'cast' '(' (type | castQualifier)? ')' unaryExpression
    ;

castQualifier: 'const'
    | 'const' 'shared'
    | 'immutable'
    | 'inout'
    | 'inout' 'shared'
    | 'shared'
    | 'shared' 'const'
    | 'shared' 'inout'
    ;


debugCondition: 'debug' ('(' (IntegerLiteral | Identifier) ')')?
    ;

debugSpecification: 'debug' '=' (Identifier | IntegerLiteral) ';'
    ;

staticIfCondition: 'static' 'if' '(' assignExpression ')'
    ;

staticAssertStatement: 'static' assertStatement
    ;

assertStatement: assertExpression ';'
    ;

templateMixinStatement: 'mixin' mixinTemplateName templateArguments? Identifier? ';'
    ;

mixinTemplateName: (typeofExpression? '.')? identifierOrTemplateChain
    ;

functionCallStatement: functionCallExpression ';'
    ;

deleteStatement: deleteExpression ';'
    ;

assignStatement: unaryExpression assignOperator assignExpression (',' unaryExpression assignOperator assignExpression)* ';'
    | preIncDecExpression ';'
    | postIncDecExpression ';'
    ;

assignOperator: '='
    | '>>>='
    | '>>='
    | '<<='
    | '+='
    | '-='
    | '*='
    | '%='
    | '&='
    | '/='
    | '|='
    | '^^='
    | '^='
    | '~='
    ;

ifStatement: 'if' '(' expression ')' statementNoCaseNoDefault ('else' statementNoCaseNoDefault)?
    ;

forStatement: 'for' '(' (declaration | statement) expression? ';' expression? ')' statementNoCaseNoDefault
    ;

initialize: ';'
    | statementNoCaseNoDefault
    ;

foreachStatement: ('foreach' | 'foreach_reverse') '(' foreachTypeList ';' expression ')' statementNoCaseNoDefault
    ;

foreachTypeList: foreachType (',' foreachType)*
    ;

foreachType: 'ref'? type? Identifier
    ;

expression: assignExpression (',' expression)?
    ;

identifierOrTemplateInstance: Identifier
    | templateInstance
    ;

templateInstance: Identifier templateArguments
    ;

typeofExpression: 'typeof' '(' (expression | 'return') ')'
    ;

typeidExpression: 'typeid' '(' (type | expression) ')'
    ;

isExpression: 'is' '(' (assignExpression | (type Identifier? ((':' | '==') typeSpecialization (',' templateParameterList)?)?)) ')'
    ;

templateParameterList: templateParameter (',' templateParameter?)*
    ;

templateParameter: templateTypeParameter
    | templateValueParameter
    | templateAliasParameter
    | templateTupleParameter
    | templateThisParameter
    ;

templateTypeParameter: Identifier (':' type)? ('=' type)?
    ;

templateValueParameter: type Identifier (':' expression)? templateValueParameterDefault?
    ;

templateValueParameterDefault:  '=' ('__FILE__' | '__MODULE__' | '__LINE__' | '__FUNCTION__' | '__PRETTY_FUNCTION__' | assignExpression)
    ;

templateAliasParameter: 'alias' type? Identifier (':' (type | expression))? ('=' (type | expression))?
    ;

templateTupleParameter: Identifier '...'
    ;

templateThisParameter: 'this' templateTypeParameter
    ;

typeSpecialization: type
    | 'struct'
    | 'union'
    | 'class'
    | 'interface'
    | 'enum'
    | 'function'
    | 'delegate'
    | 'super'
    | 'const'
    | 'immutable'
    | 'inout'
    | 'shared'
    | 'return'
    | '__parameters'
    ;

templateArguments: '!' ('(' templateArgumentList? ')' | templateSingleArgument)
    ;

templateArgumentList: templateArgument (',' templateArgument?)*
    ;

templateArgument: type
    | assignExpression
    | symbol
    ;

symbol: '.'? identifierOrTemplateChain
    ;

templateSingleArgument: Identifier
    | basicType
    | CharacterLiteral
    | StringLiteral
    | IntegerLiteral
    | FloatLiteral
    | 'true'
    | 'false'
    | 'null'
    | 'this'
    | '__DATE__'
    | '__TIME__'
    | '__TIMESTAMP__'
    | '__VENDOR__'
    | '__VERSION__'
    | '__FILE__'
    | '__LINE__'
    | '__MODULE__'
    | '__FUNCTION__'
    | '__PRETTY_FUNCTION__'
    ;

functionCallExpression: unaryExpression templateArguments? arguments
    ;

arguments: '(' argumentList? ')'
    ;

argumentList: assignExpression (',' assignExpression?)*
    ;

newExpression: 'new' type ('[' assignExpression ']' | arguments)?
    | newAnonClassExpression
    ;

newAnonClassExpression: 'new' arguments? 'class' arguments? Identifier identifierList? classBody
    ;

deleteExpression: 'delete' unaryExpression
    ;

assignExpression: ternaryExpression (assignOperator assignExpression)?
    ;

ternaryExpression: orOrExpression ('?' expression ':' ternaryExpression)?
    ;

orOrExpression: andAndExpression
	| orOrExpression '||' andAndExpression
    ;

andAndExpression: orExpression
	| andAndExpression '&&' orExpression
    ;

orExpression: xorExpression
	| orExpression '|' xorExpression
    ;

xorExpression: andExpression
    | xorExpression '^' andExpression
    ;

andExpression: cmpExpression
    | andExpression '&' cmpExpression
    ;

cmpExpression: shiftExpression
    | equalExpression
    | identityExpression
    | relExpression
    | inExpression
    ;

equalExpression: shiftExpression ('==' | '!=') shiftExpression;

identityExpression: shiftExpression ('is' | '!' 'is') shiftExpression;

relExpression: shiftExpression
    | relExpression ('<' | '<=' | '>' | '>=' | '!<>=' | '!<>' | '<>' | '<>=' | '!>' | '!>=' | '!<' | '!<=') shiftExpression
    ;

inExpression: shiftExpression ('in' | '!' 'in') shiftExpression;

shiftExpression: addExpression
    | shiftExpression ('<<' | '>>' | '>>>') addExpression
    ;

addExpression:  mulExpression
    | addExpression ('+' | '-' | '~') mulExpression
    ;

mulExpression: unaryExpression
	| mulExpression ('*' | '/' | '%') unaryExpression
    ;

powExpression: unaryExpression
	| powExpression '^^' unaryExpression
    ;

unaryExpression: primaryExpression
    | '&' unaryExpression
    | '!' unaryExpression
    | '*' unaryExpression
    | '+' unaryExpression
    | '-' unaryExpression
    | '~' unaryExpression
    | newExpression
    | deleteExpression
    | castExpression
    | functionCallExpression
	| preIncDecExpression
    | postIncDecExpression
    | sliceExpression
    | indexExpression
    | unaryExpression '.' identifierOrTemplateInstance
    | assertExpression
    ;

sliceExpression: unaryExpression '[' (assignExpression '..' assignExpression)? ']'
	;

indexExpression: unaryExpression '[' argumentList ']'
	;

assertExpression: 'assert' '(' assignExpression (',' assignExpression)? ')'
    ;

postIncDecExpression: unaryExpression ('++' | '--')
    ;

preIncDecExpression: ('++' | '--') unaryExpression
    ;

primaryExpression:
    | symbol
    | type '.' Identifier
    | typeofExpression
    | typeidExpression
    | '$'
    | 'this'
    | 'super'
    | 'null'
    | 'true'
    | 'false'
    | '__DATE__'
    | '__TIME__'
    | '__TIMESTAMP__'
    | '__VENDOR__'
    | '__VERSION__'
    | '__FILE__'
    | '__LINE__'
    | '__MODULE__'
    | '__FUNCTION__'
    | '__PRETTY_FUNCTION__'
    | IntegerLiteral
    | FloatLiteral
    | StringLiteral
    | CharacterLiteral
    | arrayLiteral
    | assocArrayLiteral
    | '(' expression ')'
    | isExpression
    | lambdaExpression
    | functionLiteralExpression
    | traitsExpression
    | mixinExpression
    | importExpression
    ;

whileStatement: 'while' '(' expression ')' statementNoCaseNoDefault
    ;

doStatement: 'do' statementNoCaseNoDefault 'while' '(' expression ')' ';'
    ;

blockStatement: '{' declarationsAndStatements? '}'
    ;

declarationsAndStatements: (declaration | statementNoCaseNoDefault)+
    ;

functionDeclaration: memberFunctionAttribute* (type | 'auto' 'ref'? | 'ref' 'auto'?) Identifier (templateParameters parameters memberFunctionAttribute* constraint? functionBody | parameters memberFunctionAttribute* (functionBody | ';'))
    ;

type: typeConstructors? type2
    ;

type2: type3 typeSuffix?
    | type2 typeSuffix
    ;

type3: basicType
    | symbol
    | typeofExpression ('.' identifierOrTemplateChain)?
    | typeConstructor '(' type ')'
    ;

identifierOrTemplateChain : identifierOrTemplateInstance ('.' identifierOrTemplateInstance)*
    ;

typeSuffix: '*'
    | '[' (type | assignExpression)? ']'
    | ('delegate' | 'function') parameters memberFunctionAttribute*
    ;

basicType: 'bool'
    | 'byte'
    | 'ubyte'
    | 'short'
    | 'ushort'
    | 'int'
    | 'uint'
    | 'long'
    | 'ulong'
    | 'char'
    | 'wchar'
    | 'dchar'
    | 'float'
    | 'double'
    | 'real'
    | 'ifloat'
    | 'idouble'
    | 'ireal'
    | 'cfloat'
    | 'cdouble'
    | 'creal'
    | 'void'
    ;

typeConstructors: typeConstructor+
    ;

typeConstructor: 'const'
    | 'immutable'
    | 'inout'
    | 'shared'
    ;

parameters: '(' ((parameter (',' parameter)*)? (',' '...')? | '...') ')'
    ;

parameter: parameterAttribute* type (Identifier? '...' | (Identifier ('=' assignExpression)?))?
    ;

parameterAttribute: 'auto'
    | 'final'
    | 'in'
    | 'lazy'
    | 'out'
    | 'ref'
    | 'scope'
    | typeConstructor
    ;

functionAttribute: 'nothrow'
    | 'pure'
    | atAttribute
    ;

memberFunctionAttribute: 'const'
    | 'immutable'
    | 'inout'
    | 'shared'
    | functionAttribute
    ;

functionBody: blockStatement
    | (inStatement | outStatement | outStatement inStatement | inStatement outStatement)? bodyStatement
    ;

inStatement: 'in' blockStatement
    ;

outStatement: 'out' ('(' Identifier ')')? blockStatement
    ;

bodyStatement: 'body' blockStatement
    ;

aliasDeclaration: 'alias' (aliasInitializer (',' aliasInitializer)* | type declarator) ';'
    ;

aliasInitializer: Identifier '=' type
    ;

variableDeclaration: storageClass? type declarator (',' declarator)* ';'
    | autoDeclaration
    ;

autoDeclaration: storageClass Identifier '=' initializer (',' Identifier '=' initializer)* ';'
    ;

storageClass : 'abstract'
    | 'auto'
    | typeConstructor
    | 'deprecated'
    | 'enum'
    | 'extern'
    | 'final'
    | 'nothrow'
    | 'override'
    | 'pure'
    | '__gshared'
    | atAttribute
    | 'scope'
    | 'static'
    | 'synchronized'
    ;

declarator: Identifier declaratorSuffix? ('=' initializer)?
    ;

declaratorSuffix: '[' (type | assignExpression)? ']'
    ;

mixinDeclaration: mixinExpression ';'
    ;

identifierList: Identifier (',' Identifier)*
    ;

identifierChain: Identifier ('.' Identifier)*
    ;

attributedDeclaration: attribute (':' | declaration | '{' declaration* '}')
    ;

attribute:
      linkageAttribute
    | alignAttribute
    | pragmaExpression
    | deprecated
    | atAttribute
    | 'private'
    | 'package'
    | 'protected'
    | 'public'
    | 'export'
    | 'extern'
    | 'final'
    | 'synchronized'
    | 'override'
    | 'abstract'
    | 'const'
    | 'auto'
    | 'scope'
    | '__gshared'
    | 'shared'
    | 'immutable'
    | 'inout'
    | 'static'
    | 'pure'
    | 'nothrow'
    ;

linkageAttribute: 'extern' '(' Identifier '++'? ')'
    ;

atAttribute: '@' (Identifier | '(' argumentList ')' | functionCallExpression)
    ;

alignAttribute: 'align' ('(' IntegerLiteral ')')?
    ;

deprecated: 'deprecated' ('(' assignExpression ')')?
    ;

traitsExpression: '__traits' '(' Identifier (',' traitsArgument)+ ')'
    ;

traitsArgument: assignExpression
    | type
    ;

mixinExpression: 'mixin' '(' assignExpression ')'
    ;

importExpression: 'import' '(' assignExpression ')'
    ;

unittest: 'unittest' blockStatement
    ;

staticAssertDeclaration: staticAssertStatement
    ;

templateDeclaration: 'template' Identifier templateParameters constraint? '{' declaration+ '}'
    ;

staticConstructor: 'static' 'this' '(' ')' functionBody
    ;

staticDestructor: 'static' '~' 'this' '(' ')' functionBody
    ;

sharedStaticDestructor: 'shared' 'static' 'this' '(' ')' functionBody
    ;

sharedStaticConstructor: 'shared' 'static' '~' 'this' '(' ')' functionBody
    ;

conditionalDeclaration: compileCondition (declaration | '{' declaration* '}') ('else' (declaration | '{' declaration* '}'))?
    ;

arrayInitializer:
	  '[' ']'
	| '[' arrayMemberInitialization (',' arrayMemberInitialization)* ']'
    ;

arrayMemberInitialization: (assignExpression ':')? nonVoidInitializer
    ;

initializer: 'void'
    | nonVoidInitializer
    ;

nonVoidInitializer: assignExpression
    | arrayInitializer
    | structInitializer
    ;

structInitializer: '{' structMemberInitializers? '}'
    ;

structMemberInitializers: structMemberInitializer (',' structMemberInitializer?)*
    ;

structMemberInitializer: (Identifier ':')? nonVoidInitializer
    ;

lambdaExpression: (Identifier | parameters functionAttribute* ) '=>' assignExpression
    ;

functionLiteralExpression: (('function' | 'delegate') type?)? (parameters functionAttribute*)? functionBody
    ;

arrayLiteral: '[' argumentList ']'
    ;

assocArrayLiteral: '[' keyValuePairs ']'
    ;

keyValuePairs: keyValuePair (',' keyValuePair)*
    ;

keyValuePair: assignExpression ':' assignExpression
    ;
