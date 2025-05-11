#[derive(Debug, Clone, PartialEq, Eq)]
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
