try {
	print("hello");
}

try {}
finally {}

try {}
catch(bob)
{
}

try 
{
}
catch(bob if 1==0)
{
}



try
   {
   adddlert("Welcome guest!");
   }
catch(err)
   {
   txt="There was an error on this page.\n\n";
   txt+="Error description: " + err.description + "\n\n";
   txt+="Click OK to continue.\n\n";
   alert(txt);
   }


 try {
            execute(parse(s), x2);
        } catch (e if e == THROW) {
            x.result = x2.result;
            throw e;
        } finally {
            ExecutionContext.current = x;
        }

 try {
                execute(n.body, x);
            } catch (e if e == BREAK && x.target == n) {
                break;
            } catch (e if e == CONTINUE && x.target == n) {
                continue;
            }
            n.update && getValue(execute(n.update, x));