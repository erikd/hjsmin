const GLOBAL = this;

var tokens = [

    "END",

    // Operators and punctuators.  Some pair-wise order matters, e.g. (+, -)
    // and (UNARY_PLUS, UNARY_MINUS).
    "\n", ";"];

var opTypeNames = {
    '\n':   "NEWLINE",
    ';':    "SEMICOLON",
    ',':    "COMMA"
};

var assignOps = ['|', '^', '&', '<<', '>>', '>>>', '+', '-', '*', '/', '%'];

function eval(s) {
	a=-1;
}

var global = {
    // Value properties.
    NaN: NaN, Infinity: Infinity, undefined: undefined,


   

    // Exte
    snarf: snarf, evaluate: evaluate,
    load: function load(s) {
        if (typeof s != "string")
            return s;
 
        evaluate(snarf(s), s, 1);
    },
    print: print, version: null
};

function eval(s) {
;
}
