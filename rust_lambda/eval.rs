use std::vec;

use context::{GlobalCtx};
use syntax::*;

pub fn evaluate<'t>(ctx: &GlobalCtx<'t>, term: &'t Term) -> Value<'t> {
  eval(ctx, &mut ~[], term)
}

fn eval<'t>(ctx: &GlobalCtx<'t>, stack: &mut ~[Value<'t>], term: &'t Term) -> Value<'t> {
  match term.t {
    TmVar(k) => {
      let v1 = &stack[stack.len() - k - 1];
      match v1 {
        &ValFix(ref cls, ref body) => 
          eval(ctx, &mut vec::append_one(cls.clone(), v1.clone()), *body),
        _other => (*v1).clone(),
      }
    },
    TmApp(ref t1, ref t2) => {
      let v1 = eval(ctx, stack, *t1);
      let v2 = eval(ctx, stack, *t2);
      let (cls, t11) = v1.unwrap_lambda();
      eval(ctx, &mut vec::append_one(cls, v2), t11)
    },
    TmAbs(_, _, ref t1) =>
      ValLambda(stack.clone(), *t1),
    TmLet(_, ref t1, ref t2) => {
      let v1 = eval(ctx, stack, *t1);
      stack.push(v1);
      let v2 = eval(ctx, stack, *t2);
      stack.pop();
      v2
    },
    TmTrue() =>
      ValBool(true),
    TmFalse() =>
      ValBool(false),
    TmIf(ref t1, ref t2, ref t3) => {
      if eval(ctx, stack, *t1).unwrap_bool() {
        eval(ctx, stack, *t2)
      } else {
        eval(ctx, stack, *t3)
      }
    },
    TmNat(n) =>
      ValNat(n),
    TmSucc(ref t1) => {
      let n = eval(ctx, stack, *t1).unwrap_nat();
      ValNat(n + 1)
    },
    TmPred(ref t1) => {
      let n = eval(ctx, stack, *t1).unwrap_nat();
      ValNat(if n == 0 { 0 } else { n - 1 })
    },
    TmIszero(ref t1) => {
      let n = eval(ctx, stack, *t1).unwrap_nat();
      ValBool(n == 0)
    },
    TmFix(ref t1) => {
      let (cls, t11) = eval(ctx, stack, *t1).unwrap_lambda();
      eval(ctx, &mut vec::append_one(cls.clone(), ValFix(cls, t11.clone())), t11)
    },
    TmGlobal(k) =>
      ctx.value_index(k).clone(),
  }
}
