
import ply.lex as lex

# Token list
tokens = (
    'ID',
    'INT_CONST',
    'REAL_CONST',
    'CHAR_CONST',
    'BOOL_CONST',
    'STRING_CONST',
    'LONG_CONST',
    'PLUS',
    'MINUS',
    'MULTIPLY',
    'DIVIDE',
    'MOD',
    'AND',
    'OR',
    'XOR',
    'LOGICAL_OR',
    'LOGICAL_AND',
    'ASSIGNMENT',
    'EQUALS',
    'NOT_EQUALS',
    'GREATER_THAN',
    'LESS_THAN',
    'GREATER_THAN_EQUALS',
    'LESS_THAN_EQUALS',
    'LPAREN',
    'RPAREN',
    'LBRACKET',
    'RBRACKET',
    'LBRACE',
    'RBRACE',
    'COMMA',
    'SEMICOLON',
    'COLON',
    'INCREMENT',
    'DECREMENT',
)

# Symbol table
symbol_table_dict = {}

# Regular expression rules
t_PLUS = r'\+'
t_MINUS = r'-'
t_MULTIPLY = r'\*'
t_DIVIDE = r'/'
t_MOD = r'%'
t_AND = r'&'
t_OR = r'\|'
t_XOR = r'\^'
t_LOGICAL_OR = r'\|\|'
t_LOGICAL_AND = r'&&'
t_ASSIGNMENT = r'='
t_EQUALS = r'=='
t_NOT_EQUALS = r'!='
t_GREATER_THAN_EQUALS = r'>='
t_LESS_THAN_EQUALS = r'<='
t_GREATER_THAN = r'>'
t_LESS_THAN = r'<'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LBRACKET = r'\['
t_RBRACKET = r'\]'
t_LBRACE = r'\{'
t_RBRACE = r'\}'
t_COMMA = r','
t_SEMICOLON = r';'
t_COLON = r':'
# t_DOT = r'\.'
t_INCREMENT = r'\+\+'
t_DECREMENT = r'--'

# Ignored characters
t_ignore = ' \t\n'

# Regular expression for identifiers (ID)
def t_ID(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    t.type = 'ID'
    # Store identifier, type, and value in the symbol table
    if t.value in [
        'bool', 'break', 'case', 'char', 'const', 'continue', 'default', 'double', 'else', 'false', 'function',
        'float', 'for', 'if', 'input', 'int', 'long', 'output', 'return', 'sizeof', 'string', 'switch', 'true'
    ]:
        t.type = 'keyword'
        # print("Illegal identifier '%s' (keyword)" % t.value)
        symbol_table_dict[t.value] = (t.type, t.value)
        t.lexer.skip(1)
        return

    symbol_table_dict[t.value] = (t.type, t.value)
    return t

# Regular expression for integer constants (INT_CONST)
def t_INT_CONST(t):
    r'\d+'
    t.value = int(t.value)
    symbol_table_dict[t.type] = (t.type, t.value)
    return t

# Regular expression for real constants (REAL_CONST)
# Regular expression for real constants (REAL_CONST)
def t_REAL_CONST(t):
    r'\d+\.\d*([eE][+-]?\d+)?|\.\d+([eE][+-]?\d+)?'
    t.value = float(t.value)
    symbol_table_dict[t.type] = (t.type, t.value)
    return t

# Regular expression for character constants (CHAR_CONST)
def t_CHAR_CONST(t):
    r'\'([^\\\n]|(\\.))*?\''
    symbol_table_dict[t.type] = (t.type, t.value)
    return t

# Regular expression for boolean constants (BOOL_CONST)
def t_BOOL_CONST(t):
    r'true|false'
    t.value = True if t.value == 'true' else False
    symbol_table_dict[t.type] = (t.type, t.value)
    return t

# Regular expression for string constants (STRING_CONST)
def t_STRING_CONST(t):
    r'\"([^\\\n]|(\\.))*?\"'
    symbol_table_dict[t.type] = (t.type, t.value)
    return t

# Regular expression for long constants (LONG_CONST)
# Regular expression for long constants (LONG_CONST)
def t_LONG_CONST(t):
    r'[+-]?\d+[lL]?'
    t.value = int(t.value[:-1]) if t.value[-1] in 'lL' else int(t.value)
    symbol_table_dict[t.type] = (t.type, t.value)
    return t
# Error handling rule
def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

def t_COMMENT_SINGLE(t):
    r'@@.*'
    pass  # Ignore single-line comments

# Regular expression for multi-line comments (/@ ... @/)
def t_COMMENT_MULTI(t):
    r'/@([^@]|\r|\n|@[^/])*@/'
    pass  # Ignore multi-line comments
# Build the lexer
lexer = lex.lex()

# Test the lexer
data = '''
int x = 10 + 3;
char c = 'a';
bool flag = true;
string str = "Hello, World!";
long l = +1234567890123456789L;
if x>y {
    cout << "x";
}
return 0;
}

'''
# def input(data):
#     with open(data , 'r') as f:
#         input_txt = f.read()
#     return lexer.input(input_txt)
    
# input('data.txt')
lexer.input(data)
# while True:
#     tok = lexer.token()
#     if not tok:
#         break 
#     print(tok)
def cout():
    while True:
        tok = lexer.token()
        if not tok:
            break
        print(tok)
    print("Symbol Table:")
    for identifier, (typ, value) in symbol_table_dict.items():
        print(f"{identifier}: {typ} = {value}")

cout()
def cout(tree, level=0):
    if isinstance(tree, tuple):
        print("  " * level + f"{tree[0]}")
        for child in tree[1:]:
            print_parse_tree(child, level + 1)
    else:
        print("  " * level + f"{tree}")

print("\nParse Tree:")
cout(symbol_table_dict)