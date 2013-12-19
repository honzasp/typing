#[feature(globs)];

use context::*;
use eval::*;
use syntax::*;
use typing::*;

mod context;
mod eval;
mod list;
mod syntax;
mod typing;

fn main() {
  fn s(t: TermE) -> Term {
    Term(Span { from: (0,0), to: (0,0) }, t)
  }

  let prog = 
    ~s(TmLet(~"and", 
      ~s(TmAbs(~"x", ~TyBool, ~s(TmAbs(~"y", ~TyBool, 
        ~s(TmIf(~s(TmVar(1)), ~s(TmVar(0)), ~s(TmFalse))))))),
      ~s(TmApp(~s(TmApp(~s(TmVar(0)), ~s(TmTrue))), ~s(TmFalse)))));
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
