module G_Groups where

groupCheck :: String -> Bool
groupCheck "" = True
groupCheck s = let symbols = [('(',')'),('[',']'),('{','}')]
                   (open, close) = span (`notElem` map snd symbols) s
               in (null close == null open)
                  && (last open, head close) `elem` symbols
                  && groupCheck (init open ++ tail close)
