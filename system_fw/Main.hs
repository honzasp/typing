module Main(main) where
import Syntax
import Typing

main :: IO ()
main = do
  let identity = TmTAbs "a" KiStar . TmAbs "x" (TyVar 0) $ TmVar 0
  print $ typeOf topCtxEmpty (TmTApp identity TyBool)
