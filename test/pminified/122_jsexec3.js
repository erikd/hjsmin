function isObject(v){var t=typeof v;return(t=="object")?v!==null:t=="function"}
function toObject(v,r,rn){switch(typeof v){case"boolean":return new global.Boolean(v);case"number":return new global.Number(v);case"string":return new global.String(v);case"function":return v;case"object":if(v!==null)return v};var message=r+" (type "+(typeof v)+") has no properties";throw rn?new TypeError(message,rn.filename,rn.lineno):new TypeError(message)}


