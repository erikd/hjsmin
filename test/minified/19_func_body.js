function HSFun(name,arity,body){this._n=name;this._r=(arity==0);this._x=arity;this._d=this;this._y=0;this._u=null;this._b=body};f=new HSFun("myfunc",1,function(){return 99});f._b.apply()==99
