import ply.lex as lex
import ply.yacc as yacc

reserved = {
		'int' : 'INT',
		'float' : 'FLOAT',
		'if' : 'IF',
		'else': "ELSE"
}

tokens = ['INUM','FNUM','PLUS','MINUS','TIMES','DIVIDE','LPAREN','RPAREN','LBRACE','RBRACE','EQUALS','ID','SEMICOL','GREATER','LESSER','GE','LE','EE','NE','AND','OR'] + list(reserved.values())
# Regular expression rules for simple tokens
t_PLUS    = r'\+'
t_MINUS   = r'\-'
t_TIMES   = r'\*'
t_DIVIDE  = r'/'
t_LPAREN  = r'\('
t_RPAREN  = r'\)'
t_LBRACE  = r'{'
t_RBRACE  = r'}'
t_EQUALS  = r'='
t_SEMICOL = r';'
t_GREATER = r'>'
t_LESSER  = r'<'
t_GE      = r'>='
t_LE      = r'<='
t_EE            = r'=='
t_NE            = r'!='
t_AND           = r'\&\&'
t_OR            = r'\|\|'

# A regular expression rule with some action code

def t_ID(t):
		r'[a-zA-Z][a-zA-Z_0-9]*'
		t.type = reserved.get(t.value,'ID')    # Check for reserved words
		return t

def t_FNUM(t):
		r'\d+\.\d+'
		try:
				t.value = float(t.value)    
		except ValueError:
				print("float value too large %d", t.value)
				t.value = 0
		return t

def t_INUM(t):
		r'\d+'
		try:
				t.value = int(t.value)    
		except ValueError:
				print("Integer value too large %d", t.value)
				t.value = 0
		return t

# Define a rule so we can track line numbers
def t_newline(t):
		r'\n+'
		t.lexer.lineno += len(t.value)

# A string containing ignored characters (spaces and tabs)
t_ignore  = ' \t'

# Error handling rule
def t_error(t):
		print("Illegal character '%s'" % t.value[0])
		t.lexer.skip(1)

# Build the lexer
lexer = lex.lex()

precedence = (
		('left','PLUS','MINUS'),
		('left','TIMES','DIVIDE'),
		('right','UMINUS'),
		)

# dictionary of names
names = { }

def p_start(t):
	'''   start : IF LPAREN boolean_expression RPAREN LBRACE start RBRACE
										| IF LPAREN boolean_expression RPAREN LBRACE start RBRACE ELSE LBRACE start RBRACE
						| statement
						 '''

def p_statement_assign(t):
		'''statement : INT ID EQUALS expression SEMICOL
								 | FLOAT ID EQUALS expression SEMICOL
								 | reassignment_stmt
								 '''
		if(t[1] == 'int' and isinstance(t[4],int)):
				names[t[2]] = t[4]
		elif(t[1] == 'float' and isinstance(t[4],float)):
				names[t[2]] = t[4]
		

def p_reassignment_stmt(t):
		'''
		reassignment_stmt : ID EQUALS expression SEMICOL    
		'''
		if(t[1]) in names:
				names[t[1]] = t[3]
		else:
				raise NameError("This variable " + t[1] + " not defined")

def p_statement_expr(t):
		'statement : expression'
		print(t[1])

def p_expression_binop(t):
		'''expression : expression PLUS expression
									| expression MINUS expression
									| expression TIMES expression
									| expression DIVIDE expression
									 '''
		if t[2] == '+'  : t[0] = t[1] + t[3]
		elif t[2] == '-': t[0] = t[1] - t[3]
		elif t[2] == '*': t[0] = t[1] * t[3]
		elif t[2] == '/': t[0] = t[1] / t[3]
			
def p_expression_boolean(t):
		'''
		boolean_expression : boolean_expression OR boolean_expression
												 | boolean_expression AND boolean_expression
																 | expression relational expression
																 | expression'''

def p_relational_operator(t):
		'''
		relational :    GREATER
							 |    LESSER
							 |    GE
							 |    LE
							 |    NE
							 |    EE           '''
		

def p_expression_uminus(t):
		'expression : MINUS expression %prec UMINUS'
		t[0] = -t[2]

def p_expression_group(t):
		'expression : LPAREN expression RPAREN'
		t[0] = t[2]

def p_expression_number(t):
		'''expression : INUM
									| FNUM'''
		t[0] = t[1]

def p_expression_name(t):
		'expression : ID'
		try:
				t[0] = names[t[1]]
		except LookupError:
				print("Undefined name '%s'" % t[1])
				t[0] = 0

def p_error(t):
		print("Syntax error at '%s'" % t.value)

parser = yacc.yacc()

while True:
		try:
				s = input('input> ')  # Use raw_input on Python 2
		except EOFError:
				break
		parser.parse(s)

