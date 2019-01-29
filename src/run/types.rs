use std::result;

enum Value {
	Int(i64),
	Char(char),
	Float(f64),
	Array(Vec<Box<Value>>),
}

struct Type {
	value: Value,
	constraints: Vec<Box<Constraint>>,
	attributes: Vec<Box<Type>>,
}

impl Type {
	fn assign(t: Type) {
		
	}
}

struct TypeError {
	what: String,
}

type Result<Type> = std::result::Result<Type, TypeError>; // have to fully spec name to avoid circular reference

trait Constraint {
	fn check(&self, value: &Type) -> Result<Type>; // for bounds[l,h], l and h are specified in Self
}