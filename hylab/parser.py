""" 
Created by: Yilun Guan

This script contains utility classes for parsing python expression
into the form of s-expression as a string that can be evaluated in 
hy.

Example:
>>> from parser import PyParser
>>> ps = PyParser()
>>> ps.parse("-10*y+x**2+2/5*np.sin(2*theta)")
'(+ (+ (* -10 y) (** x 2)) (* (/ 2 5) (np.sin (* 2 theta))))'

This makes it easy to work as a macro in hy. 

This script is based on the pyparsing official example:
http://www.ccp4.ac.uk/dist/checkout/pyparsing-2.0.1/examples/fourFn.py

The operator translations are defined in various look up tables that
can be easily extended to include more functions.

Dependencies: [pyparsing]

TODO: more documentation
TODO: more optimizations
"""


from pyparsing import Literal,CaselessLiteral,Word,Group,Optional, \
    ZeroOrMore,Forward,nums,alphas,Regex,ParseException


OPERATORS_LOOKUP = {
    "+"  : "+",
    "-"  : "-",
    "*"  : "*",
    "/"  : "/",
    "**" : "**",
    "^"  : "**"
}

FUNCTIONS_LOOKUP = { 
    "np.sin" : "np.sin",
    "np.cos" : "np.cos",
    "np.tan" : "np.tan",
    "np.abs" : "np.abs",
    "np.exp" : "np.exp",
    "sin"    : "np.sin",
    "cos"    : "np.cos",
    "tan"    : "np.tan",
    "exp"    : "np.exp",
    "round"  : "round",
    # more to be added ...
}

CONSTANTS_LOOKUP = {
    "pi" : "np.pi",
    # more to be added ...    
}


def s_expr_(s):
    """A function to generate s-expression based on an operator stack"""
    op = s.pop()
    if op == 'unary -':
        return "-%s" % s_expr_(s)
    
    if op in OPERATORS_LOOKUP:
        op2 = s_expr_(s)
        op1 = s_expr_(s)
        return "(%s %s %s)" % (OPERATORS_LOOKUP[op], op1, op2)
    
    elif op in CONSTANTS_LOOKUP:
        return CONSTANTS_LOOKUP[op]
    
    elif op in FUNCTIONS_LOOKUP:
        return "(%s %s)" % (FUNCTIONS_LOOKUP[op], s_expr_(s))
    
    # if everything else fails, treat it as a variable
    else:  
        return str(op)


class ExpressionStack:
    """A stack to manage operators precedence"""

    def __init__(self):
        self._stack = []
        
    def push_first(self, strg, loc, toks):
        self._stack.append( toks[0] )
    
    def push_u_minus(self, strg, loc, toks):
        for t in toks:
            if t == '-': 
                self._stack.append( 'unary -' )
            else:
                break
                
    def get_stack(self):
        return self._stack.copy()
    
    def initialize(self):
        self._stack = []


class PyParser:
    """ A parser wrapper based on:
    http://www.ccp4.ac.uk/dist/checkout/pyparsing-2.0.1/examples/fourFn.py

    Operators:

    expop   :: '^' | '**'
    multop  :: '*' | '/'
    addop   :: '+' | '-'
    integer :: ['+' | '-'] '0'..'9'+
    atom    :: PI | E | real | fn '(' expr ')' | '(' expr ')'
    factor  :: atom [ expop factor ]*
    term    :: factor [ multop factor ]*
    expr    :: term [ addop term ]*
    """
    def __init__(self):
        self._bnf = None
        self._stack = ExpressionStack()
        
    def generate_parser(self):
        """This method returns a pyparsing Parser"""
        self._stack.initialize()
        
        if not self._bnf:
            # float numbers
            fnumber = Regex(r"[+-]?\d+(:?\.\d*)?(:?[eE][+-]?\d+)?")

            # function calls
            ident = Word(alphas, alphas+nums+"._$")  

            # operators
            plus  = Literal( "+" )
            minus = Literal( "-" )
            mult  = Literal( "*" )
            div   = Literal( "/" )
            lpar  = Literal( "(" ).suppress()
            rpar  = Literal( ")" ).suppress()
            addop  = plus | minus
            multop = mult | div
            expop = Literal( "**" ) | Literal("^")

            # define parsing process
            expr = Forward()
            atom = ((0,None)*minus + ( fnumber | (ident + lpar + expr + rpar) | ident ).setParseAction( self._stack.push_first ) | 
                    Group( lpar + expr + rpar )).setParseAction(self._stack.push_u_minus) 

            factor = Forward()
            factor << atom + ZeroOrMore( ( expop + factor ).setParseAction( self._stack.push_first ) )
            term = factor + ZeroOrMore( ( multop + factor ).setParseAction( self._stack.push_first ) )
            expr << term + ZeroOrMore( ( addop + term ).setParseAction( self._stack.push_first ) )

            self._bnf = expr

        return self._bnf

    def get_stack(self):
        return self._stack.get_stack()

    def get_s_expr(self):
        return s_expr_(self.get_stack())
        
    def parse(self, s):
        self.generate_parser().parseString(s, parseAll=True)
        return self.get_s_expr()
        
