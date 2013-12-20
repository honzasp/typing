#[feature(globs)];

use context::*;
use eval::*;
use syntax::*;
use typing::*;

mod context;
mod eval;
mod lexer;
mod list;
mod syntax;
mod typing;

fn main() {
  fn s(t: TermE) -> Term {
    Term { span: Span { from: (0,0), to: (0,0) }, t: t }
  }

  let body = ~s(TmIf(~s(TmIszero(~s(TmVar(1)))),
      ~s(TmVar(0)),
      ~s(TmSucc(~s(TmApp(~s(TmApp(~s(TmVar(2)), ~s(TmPred(~s(TmVar(1)))))), ~s(TmVar(1))))))));
  let add_def = ~s(TmFix(~s(TmAbs(~"add", ~TyArr(~TyNat, ~TyArr(~TyNat, ~TyNat)),
      ~s(TmAbs(~"a", ~TyNat, ~s(TmAbs(~"b", ~TyNat, body))))))));
  let prog = ~s(TmLet(~"add", add_def, ~s(TmApp(~s(TmApp(~s(TmVar(0)), ~s(TmNat(3)))), ~s(TmNat(5))))));

  let ctx = &GlobalCtx::new();

  match typecheck(ctx, prog) {
    Ok(ty) => {
      let val = evaluate(ctx, prog);
      println!("{:?} : {:?}", val, ty);
    },
    Err(tyerr) => {
      println!("type err: {:?}", tyerr);
    }
  }
}
