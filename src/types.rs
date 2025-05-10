#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Function,
    String,
    Int,
    Bool,
    List(Box<Type>),
    // Option(Box<Type>),
}
