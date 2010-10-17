const GLOBAL=this;var tokens=["END","\n",";"],opTypeNames={'\n':"NEWLINE",';':"SEMICOLON",',':"COMMA"},assignOps=['|','^','&','<<','>>','>>>','+','-','*','/','%']
function eval(s){a=-1};var global={NaN:NaN,Infinity:Infinity,undefined:undefined,snarf:snarf,evaluate:evaluate,load:function load(s){if(typeof s!="string")return s;evaluate(snarf(s),s,1)},print:print,version:null}
function eval(s){}
