// From http://developer.mozilla.org/en/docs/Core_JavaScript_1.5_Reference:Global_Objects:Function:apply


function product(name, value){
   this.name = name;
   if(value > 1000)
      this.value = 999;
   else
      this.value = value;
}

function prod_dept(name, value, dept){
   this.dept = dept;
   product.apply(this, arguments);
}

prod_dept.prototype = new product();

// since 5 is less than 1000 value is set
cheese = new prod_dept("feta", 5, "food");

// since 5000 is above 1000, value will be 999
car = new prod_dept("honda", 5000, "auto");

cheese.value == 5 && car.value == 999;