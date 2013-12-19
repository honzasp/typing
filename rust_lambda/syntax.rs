pub struct Term(Span, TermE);

pub enum TermE {
  TmVar(uint),
  TmApp(~Term, ~Term),
  TmAbs(~str, ~Type, ~Term),
  TmLet(~str, ~Term, ~Term),
  TmTrue(),
  TmFalse(),
  TmIf(~Term, ~Term, ~Term),
  TmNat(uint),
  TmSucc(~Term),
  TmPred(~Term),
  TmIszero(~Term),
  TmGlobal(uint),
}

#[deriving(Clone)]
pub enum Value<'t> {
  ValBool(bool),
  ValNat(uint),
  ValLambda(~[Value<'t>], &'t Term),
}

#[deriving(Eq, Clone)]
pub enum Type {
  TyNat(),
  TyBool(),
  TyArr(~Type, ~Type),
  TyGlobal(uint),
}

pub type Line = uint;
pub type Column = uint;

#[deriving(Eq, Clone, ToStr)]
pub struct Span {
  from: (Line, Column),
  to: (Line, Column),
}

impl<'t> Value<'t> {
  pub fn unwrap_lambda(self) -> (~[Value<'t>], &'t Term) {
    match self {
      ValLambda(cls, term) => (cls, term),
      _ => fail!("unwrap_lambda did not get a lambda value"),
    }
  }

  pub fn unwrap_bool(self) -> bool {
    match self {
      ValBool(b) => b,
      _ => fail!("unwrap_bool did not get a bool value"),
    }
  }

  pub fn unwrap_nat(self) -> uint {
    match self {
      ValNat(n) => n,
      _ => fail!("unwrap_nat did not get a nat value"),
    }
  }
}
