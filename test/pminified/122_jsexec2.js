function execute(n,x){switch(n.type){case IN:v=getValue(execute(n[0],x)) in getValue(execute(n[1],x))};return v}

