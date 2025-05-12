use strum::EnumDiscriminants;

#[derive(Debug, Clone, PartialEq, Eq, EnumDiscriminants)]
pub enum Type {
    Function {
        parameters: Vec<Type>,
        return_type: Box<Type>,
    },
    String,
    Int,
    Bool,
    List(Option<Box<Type>>),
    Nullable(Option<Box<Type>>),
}

impl Type {
    pub fn is_compatible(self, second: Type) -> bool {
        // let first_discriminant: TypeDiscriminants = first.into();
        // let second_discriminant: TypeDiscriminants = second.into();

        if self == second {
            return true;
        }

        match (self, second) {
            (Type::Nullable(first), Type::Nullable(second)) => {
                if first == None || second == None {
                    return true;
                } else {
                    first.unwrap().is_compatible(*second.unwrap())
                }
            }
            (Type::List(first), Type::List(second)) => {
                if first == None || second == None {
                    return true;
                } else {
                    first.unwrap().is_compatible(*second.unwrap())
                }
            }
            _ => {
                return false;
            }
        }
    }
}
