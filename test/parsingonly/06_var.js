// Why does this fail !!
// This fails as well parseProgram "for(i=0;;)\n{\n;\nvar t=1;}"
for (i = 0;;){
    ;
var t=1;

}
