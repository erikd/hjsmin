function bob(){}
function bob(a,b){}
function bob(a,b){a=1}
function ExprArray(n,v){for(var i=0;i<n;i++)this[i]=""+v}
function Hello(){ExprArray(1,1)};Reference.prototype.toString=function(){return this.node.getSource()}
function getValue(v){if(v instanceof Reference){if(!v.base)throw new ReferenceError(v.propertyName+" is not defined",v.node.filename,v.node.lineno);return v.base[v.propertyName]};return v}
