function arrayIndexEx(index,a){
	var r = a[index];
	if(typeof r === 'undefined'){
		throw "array length == " + a.lentgh + ", index == " + index;
	}
}

var arrayEmpty = []

var arrayIndex = function(i,a){
	var r = a[index];
	if(typeof r === 'undefined'){
		return {just: r};
	}else{
		return oc.std.maybes.none;
	}
}

var arrayLength = function(a){
	return a.length;
}

var arrayEq = function(eq, a1,a2) {
		if(a1.length != a2.length){
			return false;
		}
		for (var i = 0, l=this.length; i < l; i++) {
			if(!eq(a1[i],a2[i])){
				return false;
			}
		}
		return true;
}

var arrayMap = function(f, a){ return a.map(f); }

var arrayPure = function(x){ return [x]; }

var arrayAppend = function(a1, a2){ return a1.concat(a2); }

var arrayFoldl = function(f, s, a){ return a.reduce(f,s); }
