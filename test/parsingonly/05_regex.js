x = 1/y;
var newlines = spaces.match(/\n/g);
var newlines = spaces.match(/\n/g);
opRegExpSrc += i.replace(/[?|^&(){}\[\]+\-*\/\.]/g, "\\$&");

for (i = 0;;){
    var t=1;
    ;
}

for (var i = 0, j = tokens.length; i < j; i++) {
    if (i > 0)
        consts += ", ";
    var t = tokens[i];

    if (/^[a-z]/.test(t)) {
        consts += t.toUpperCase();
        keywords[t] = i;
    } else {
        consts += (/^\W/.test(t) ? opTypeNames[t] : t);
    }
    consts += " = " + x;
    tokens[t] = j;
}