use std::{iter, from_str};

use syntax::{Span};

#[deriving(Clone,Eq)]
pub struct Token {
  span: Span,
  tok: TokenE,
}

#[deriving(Clone,Eq)]
pub enum TokenE {
  TkKeyword(Keyword),
  TkSymbol(Symbol),
  TkIdentifier(~str),
  TkInteger(uint),
}

#[deriving(Clone,Eq)]
pub enum Keyword {
  KwSucc, KwPred, KwIszero,
  KwTrue, KwFalse, KwUnit,
  KwLet, KwIn,
  KwIf, KwThen, KwElse
}

#[deriving(Clone,Eq)]
pub enum Symbol {
  SymEqual, SymColon, SymLambda,
  SymParenL, SymParenR,
  SymBraceL, SymBraceR,
  SymArrow
}

#[deriving(Clone,Eq,ToStr)]
pub struct LexErr {
  span: Span,
  msg: ~str,
}

#[deriving(Clone)]
pub struct Lexer<'a> {
  priv input: &'a str,
  priv line: uint,
  priv column: uint,
}

impl<'a> iter::Iterator<Result<Token, LexErr>> for Lexer<'a> {
  fn next(&mut self) -> Option<Result<Token, LexErr>> {
    self.consume_while(|ch| ch.is_whitespace());

    if !self.input.is_empty() {
      let start_pos = (self.line, self.column);
      let opt_tok = self.read_token();
      let end_pos = (self.line, self.column);
      let span = Span { from: start_pos, to: end_pos };

      match opt_tok {
        Some(tok) => Some(Ok(Token { span: span, tok: tok })),
        None => Some(Err(LexErr { span: span, msg: ~"Bad characters" })),
      }
    } else {
      None
    }
  }
}

impl<'a> Lexer<'a> {
  pub fn new(input: &'a str) -> Lexer<'a> {
    Lexer { input: input, line: 1, column: 1 }
  }

  fn shift(&mut self) {
    let hd = self.input.char_at(0);
    self.consumed(hd);
    self.input = self.input.slice_from(1);
  }

  fn consumed(&mut self, ch: char) {
    if ch == '\n' {
      self.line = self.line + 1;
      self.column = 1;
    } else if ch == '\t' {
      self.column = (self.column-1) / 8 * 8 + 9;
    } else {
      self.column = self.column + 1;
    }
  }

  fn consume_while(&mut self, f: |char| -> bool) -> &'a str {
    let mut offset_iter = self.input.char_indices();
    loop {
      match offset_iter.next() {
        None => {
          let matched = self.input;
          self.input = self.input.slice(0, 0);
          return matched;
        },
        Some((offset, ch)) => {
          if !f(ch) {
            let matched = self.input.slice_to(offset);
            self.input = self.input.slice_from(offset);
            return matched;
          } else {
            self.consumed(ch);
          }
        }
      }
    }
  }

  fn consume_prefix(&mut self, prefix: &str) -> bool {
    if self.input.starts_with(prefix) {
      for ch in prefix.chars() {
        self.consumed(ch);
      }
      self.input = self.input.slice_from(prefix.len());
      true
    } else {
      false
    }
  }

  fn read_token(&mut self) -> Option<TokenE> {
    assert!(!self.input.is_empty());
    let ch = self.input.char_at(0);

    if ch.is_alphabetic() {
      let id = self.consume_while(|c| c.is_alphabetic());
      assert!(!id.is_empty());

      Some(match id {
        "succ" => TkKeyword(KwSucc),
        "pred" => TkKeyword(KwPred),
        "iszero" => TkKeyword(KwIszero),
        "true" => TkKeyword(KwTrue),
        "false" => TkKeyword(KwFalse),
        "unit" => TkKeyword(KwUnit),
        "let" => TkKeyword(KwLet),
        "in" => TkKeyword(KwIn),
        "if" => TkKeyword(KwIf),
        "then" => TkKeyword(KwThen),
        "else" => TkKeyword(KwElse),
        "\u03bb" => TkSymbol(SymLambda),
        id => TkIdentifier(id.to_owned()),
      })
    } else if ch.is_digit_radix(10) {
      let num = self.consume_while(|ch| ch.is_digit_radix(10));
      Some(TkInteger(from_str::from_str(num).unwrap()))
    } else if self.consume_prefix("->") {
      Some(TkSymbol(SymArrow))
    } else {
      let sym = match ch {
        '=' => SymEqual,
        ':' => SymColon,
        '\\' => SymLambda,
        '(' => SymParenL,
        ')' => SymParenR,
        '{' => SymBraceL,
        '}' => SymBraceR,
        _ => return None,
      };

      self.shift();
      Some(TkSymbol(sym))
    }
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use syntax::{Span};

  fn lex(txt: &str) -> ~[TokenE] {
    Lexer::new(txt).map(|res| res.unwrap().tok).to_owned_vec()
  }

  #[test]
  fn test_keywords() {
    assert_eq!(lex("succ pred iszero"), ~[
        TkKeyword(KwSucc),
        TkKeyword(KwPred),
        TkKeyword(KwIszero),
      ]);

    assert_eq!(lex("true false unit let in"), ~[
        TkKeyword(KwTrue),
        TkKeyword(KwFalse),
        TkKeyword(KwUnit),
        TkKeyword(KwLet),
        TkKeyword(KwIn),
      ]);

    assert_eq!(lex("if then else"), ~[
        TkKeyword(KwIf),
        TkKeyword(KwThen),
        TkKeyword(KwElse),
      ]);
  }

  #[test]
  fn test_symbols() {
    assert_eq!(lex("=:\\λ(){}->"), ~[
        TkSymbol(SymEqual),
        TkSymbol(SymColon),
        TkSymbol(SymLambda),
        TkSymbol(SymLambda),
        TkSymbol(SymParenL),
        TkSymbol(SymParenR),
        TkSymbol(SymBraceL),
        TkSymbol(SymBraceR),
        TkSymbol(SymArrow),
      ]);
  }

  #[test]
  fn test_identifiers() {
    assert_eq!(lex("plus minus times divide"), ~[
        TkIdentifier(~"plus"),
        TkIdentifier(~"minus"),
        TkIdentifier(~"times"),
        TkIdentifier(~"divide"),
      ]);
  }

  #[test]
  fn test_integers() {
    assert_eq!(lex("0 1 20 452 114344"), ~[
        TkInteger(0),
        TkInteger(1),
        TkInteger(20),
        TkInteger(452),
        TkInteger(114344),
      ]);
  }

  #[test]
  fn test_whitespace() {
    assert_eq!(lex("   in\t \r\n10\r0   "), ~[
        TkKeyword(KwIn),
        TkInteger(10),
        TkInteger(0),
      ]);
  }

  #[test]
  fn test_positions() {
    fn tok(from: (uint, uint), to: (uint, uint), t: TokenE) -> Option<Result<Token, LexErr>> {
      Some(Ok(Token { span: Span { from: from, to: to }, tok: t }))
    }

    let txt = "  let\n   \tx\r\n    100 \n   ->  \n  +-*/";
    let mut lexer = Lexer::new(txt);
    assert_eq!(lexer.next(), tok((1,3), (1,6), TkKeyword(KwLet)));
    assert_eq!(lexer.next(), tok((2,9), (2,10), TkIdentifier(~"x")));
    assert_eq!(lexer.next(), tok((3,5), (3,8), TkInteger(100)));
    assert_eq!(lexer.next(), tok((4,4), (4,6), TkSymbol(SymArrow)));

    let err = lexer.next().unwrap().unwrap_err();
    assert_eq!(err.span, Span { from: (5,3), to: (5,3) });
  }
}
