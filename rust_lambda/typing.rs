use context::{GlobalCtx};
use list::{List};
use syntax::*;

#[deriving(ToStr)]
pub struct TypeErr {
  span: Span,
  msg: ~str,
}

pub fn typecheck(ctx: &GlobalCtx, term: &Term) -> Result<Type, TypeErr> {
  type_of(ctx, &List::new_empty(), term)
}

fn type_of(ctx: &GlobalCtx, stack: &List<Type>, term: &Term) 
  -> Result<Type, TypeErr> 
{
  let &Term(t_span, ref t) = term;
  match *t {
    TmVar(k) =>
      Ok((*stack.index(k)).clone()),
    TmApp(ref t1, ref t2) => {
      type_of(ctx, stack, *t1).and_then(|ty1| {
      type_of(ctx, stack, *t2).and_then(|ty2| {
        match ty1 {
          TyArr(ref ty11, ref ty12) if &**ty11 == &ty2 => Ok((**ty12).clone()),
          TyArr(_, _) => type_err(t_span, "Domain type mismatch"),
          _ => type_err(t_span, "Expected arrow type"),
        }
      })})
    },
    TmAbs(_, ref ty1, ref t2) => {
      type_of(ctx, &stack.cons((**ty1).clone()), *t2).and_then(|ty2| {
        Ok(TyArr(ty1.clone(), ~ty2))
      })
    },
    TmLet(_, ref t1, ref t2) => {
      type_of(ctx, stack, *t1).and_then(|ty1| {
        type_of(ctx, &stack.cons(ty1), *t2)
      })
    },
    TmTrue() =>
      Ok(TyBool),
    TmFalse() =>
      Ok(TyBool),
    TmIf(ref t1, ref t2, ref t3) => {
      type_of(ctx, stack, *t1).and_then(|ty1| {
        if ty1 != TyBool {
          type_err(t_span, "Condition guard must be a Bool")
        } else {
          type_of(ctx, stack, *t2).and_then(|ty2| {
          type_of(ctx, stack, *t3).and_then(|ty3| {
            if ty2 == ty3 {
              Ok(ty2.clone())
            } else {
              type_err(t_span, "Condition arms must have the same type")
            }
          })})
        }
      })
    },
    TmNat(_) =>
      Ok(TyNat),
    TmSucc(ref t1) => 
      expect_type(ctx, stack, &**t1, TyNat,
        | | type_err(t_span, "succ must be applied to Nat"))
        .and(Ok(TyNat)),
    TmPred(ref t1) => 
      expect_type(ctx, stack, &**t1, TyNat, 
        | | type_err(t_span, "pred must be applied to Nat"))
        .and(Ok(TyNat)),
    TmIszero(ref t1) => 
      expect_type(ctx, stack, &**t1, TyNat, 
        | | type_err(t_span, "iszero must be applied to Nat"))
        .and(Ok(TyBool)),
    TmGlobal(k) =>
      Ok(ctx.type_index(k)),
  }
}

fn expect_type(
  ctx: &GlobalCtx, stack: &List<Type>, term: &Term,
  expected_ty: Type, unexpected: | | -> Result<(), TypeErr>
) -> Result<(), TypeErr>
{
  type_of(ctx, stack, term).and_then(|ty| {
    if ty == expected_ty {
      Ok(())
    } else {
      unexpected()
    }
  })
}

fn type_err<A>(span: Span, msg: &str) -> Result<A, TypeErr> {
  Err(TypeErr { span: span, msg: msg.to_owned() })
}
