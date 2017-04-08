import ply.lex as lex
import ply.yacc as yacc

reserved = {
	'int' : 'INT',
	'float' : 'FLOAT',
	'if' : 'IF',
	'else': "ELSE",
	'main': "MAIN"
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

precedence = (
	('left','PLUS','MINUS'),
	('left','TIMES','DIVIDE'),
	)

#names dict
names = {}

# Temporary variable
temp_num = 0
label_num = 0

def p_init(t):
	'init : INT MAIN LPAREN RPAREN LBRACE start RBRACE'
	print("Parse success")

def p_start(t):
	'''   start : IF LPAREN boolean_expression RPAREN LBRACE print_goto start RBRACE print_label start
						| IF LPAREN boolean_expression RPAREN LBRACE print_goto start goto_label RBRACE ELSE LBRACE print_label start RBRACE start
						| statement start
						| empty
	'''
	#print([i for i in t])

def p_else(t):
	'''
	else_stmt : 
	'''



def p_print_goto(t):
	'''
		print_goto : empty
	'''
	global label_num
	#print([i for i in t])
	print("iffalse", t[-3], "goto", "L"+str(label_num)) 

def p_print_goto_label(t):
	'''
		goto_label : empty
	'''
	global label_num
	print("goto","L",str(label_num+1))

def p_print_label(t):
	'''
		print_label  : empty
	'''
	global label_num
	print("L",str(label_num),":", sep="")
	label_num = label_num + 1


def p_empty(t):
	'empty :'
	pass
	

def p_statement_assign(t):
	'''statement : INT ID eqex SEMICOL
				   | FLOAT ID eqex SEMICOL
				   | reassignment_stmt SEMICOL
				 '''

	#print(t[2], "=", t[3])
	if len(t) == 5:
		names[t[2]] = t[3]
		if not names[t[2]]:
			names[t[2]] = t[2]
		if t[3] is not None:
			print(t[2],"=",t[3])

	global temp_num
	#print([i for i in t])

		
	


def p_statement_eqex(t):
	'''eqex : EQUALS expression
			 | empty
	'''	
	#print(t[1])
	if t[1] == '=':
		t[0] = t[2]

def p_reassignment_stmt(t):
	'''
	reassignment_stmt : ID eqex    
	'''

	global temp_num
	names[t[1]] = t[2]
	print(t[1],"=",t[2])


def p_statement_expr(t):
	'statement : expression'
	t[0] = t[1]	

def p_expression_binop(t):
	'''expression : expression PLUS expression
					| expression MINUS expression
					| expression TIMES expression
					| expression DIVIDE expression
								 '''
	global temp_num
	t[0] = "t_" + str(temp_num)
	print(t[0],"=",t[1], t[2], t[3], sep = '')
	temp_num = temp_num + 1

def p_expression_boolean(t):
	'''
	boolean_expression : c logop c                                                 
								 | c '''
	global temp_num
	if len(t) > 2:
		t[0] = "t_" + str(temp_num)
		print(t[0],"=",t[1], t[2], t[3], sep = '')
		temp_num = temp_num + 1

def p_c(t):
	'''
	c : expression relational expression
		| expression '''
	global temp_num
	if len(t) > 2:
		t[0] = "t_" + str(temp_num)
		print(t[0],"=",t[1], t[2], t[3], sep = '')
		temp_num = temp_num + 1


def p_logop(t):
	'''
			logop : AND
					| OR
	'''
	t[0] = t[1]
def p_relational_operator(t):
	'''
	relational :    GREATER
				 |    LESSER
				 |    GE
				 |    LE
				 |    NE
				 |    EE           '''
	t[0] = t[1]

def p_expression_group(t):
	'expression : LPAREN expression RPAREN'
	t[0] = t[2]

def p_expression_number(t):
	'''expression : INUM
	 | FNUM'''
	t[0] = str(t[1])

def p_expression_name(t):
	'expression : ID'
	try:
					t[0] = names[t[1]]
	except LookupError:
					print("Undefined name '%s'" % t[1])
					t[0] = 0


def p_error(t):
	print("Syntax error at '%s'" % t.value)



data = '''
		int main()
		{
			int a;
			a = a * 1 + 2;
			int b = 2 / a;
			if(a > b || b < a)
			{
				a = (b * b) + a;
			}
			else
			{
				b = 100 + a;
			}
		}
		'''


lexer = lex.lex()
lexer.input(data)
symtab = {}
while True:
	tok = lexer.token()
	if not tok: 
		break      # No more input
	if not symtab.__contains__(tok.value):
		symtab[tok.value] = []
	symtab[tok.value] = symtab[tok.value] + [(tok.type, tok.lineno, tok.lexpos)]
	#print(tok.type, tok.value, tok.lineno, tok.lexpos)

#for keys,values in symtab.items():
#	print(keys, values)

print()


parser = yacc.yacc()
parser.parse(data)


print(names)

