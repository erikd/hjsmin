function bob() {
	return 1;
}

function fred(a1,a2) {
	return a1 + a2;
}

function MyClass() { }

MyClass.prototype.mary =  function (a1,a2) {
	this.prop1 = a1 + a2;
}

o = new MyClass();

o.mary.apply(o,[1,2]);

bob.apply() == 1 && o.prop1 == 3 && fred.apply(null,[1,2]) == 3