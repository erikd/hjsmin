function bob(n){try{throw n}catch(e){if(e<=50){return true}else throw (e)}}
function fred(n){try{return bob(n)}catch(e){return false}};bob(11)&&!fred(1234)
