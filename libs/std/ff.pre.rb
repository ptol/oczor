oc = {}
oc[:instances] = {}

oc[:cloneObject] = -> (obj) do
	result = {};
	obj.each {|key, value|  
	  result[key] = value
	}
	result;
end

class UniqObject 
end
