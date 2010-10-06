
t = 1 + b

t ? '1' : 'b';

if (getValue)
   execute;
else {
   execute;
}


for (i = 0; i <= 5; i++)
{
	x += i;
}

for (var i = 0; i < n; i++) {
  this[i] = 1 + v;
}

var sumOfDivisors = new ExprArray(n+1,1);


for (var i = 0; i < j; i++) {
    if (i > 0)
        consts += ", ";
    var t = tokens[i];
}

for (var i = 0, j = tokens.length; i < j; i++) {
    if (i > 0)
        consts += ", ";
    var t = tokens[i];
}


for (i = 0; ; i++)
{
	x += i;
}

for( x in bob)
{

}

if (typeof s != "string")
            return;

 while (--n >= 0)
            s += t;

var a, f, i, j, r, s, t, u, v;

(x.stmtStack.length > 1)
? STATEMENT_FORM
: DECLARED_FORM;

 n2 = new Node(t);
                if (tt == DEFAULT)
                    n.defaultIndex = n.cases.length;
                else
                    n2.caseLabel = Expression(t, x, COLON);
                break;

 n = new Node(t);
        t.mustMatch(LEFT_PAREN);
        n.discriminant = Expression(t, x);
        t.mustMatch(RIGHT_PAREN);
        n.cases = [];
        n.defaultIndex = -1;
        x.stmtStack.push(n);
        t.mustMatch(LEFT_CURLY);
        while ((tt = t.get()) != RIGHT_CURLY) {
            switch (tt) {
              case DEFAULT:
                if (n.defaultIndex >= 0)
                    throw t.newSyntaxError("More than one switch default");
                // FALL THROUGH
              case CASE:
                n2 = new Node(t);
                if (tt == DEFAULT)
                    n.defaultIndex = n.cases.length;
                else
                    n2.caseLabel = Expression(t, x, COLON);
                break;
              default:
                throw t.newSyntaxError("Invalid switch case");
            }
            t.mustMatch(COLON);
            n2.statements = new Node(t, BLOCK);
            while ((tt=t.peek()) != CASE && tt != DEFAULT && tt != RIGHT_CURLY)
                n2.statements.push(Statement(t, x));
            n.cases.push(n2);
        }
        x.stmtStack.pop();
        return n;
   


 do {
            try {
                execute(n.body, x);
            } catch (e if e == BREAK && x.target == n) {
                break;
            } catch (e if e == CONTINUE && x.target == n) {
                continue;
            }
        } while (getValue(execute(n.condition, x)));
        break;