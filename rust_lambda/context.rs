use syntax::*;

pub struct GlobalCtx<'t> {
  priv type_abbrs: ~[TypeAbbr],
  priv values: ~[Value<'t>],
}

pub struct TypeAbbr {
  name: ~str,
  ty: Type,
}

impl<'t> GlobalCtx<'t> {
  pub fn new() -> GlobalCtx<'t> {
    GlobalCtx { type_abbrs: ~[], values: ~[] }
  }

  pub fn type_index(&self, idx: uint) -> Type {
    self.type_abbrs[idx].ty.clone()
  }

  pub fn value_index(&self, idx: uint) -> Value<'t> {
    self.values[idx].clone()
  }
}
