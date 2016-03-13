{-# LANGUAGE EmptyDataDecls #-}
module FindWords where
--import System.Random
--import Control.Monad
import Prelude
import FFI

randomIO :: Fay Int
randomIO = ffi "Math.floor( Math.random() * 10000 )"

hiragana0 = [
  "あ","い","う","え","お",
  "か","き","く","け","こ",
  "さ","し","す","せ","そ",
  "た","ち","つ","て","と",
  "な","に","ぬ","ね","の",
  "は","ひ","ふ","へ","ほ",
  "ま","み","む","め","も",
  "や","ゆ","よ",
  "ら","り","る","れ","ろ",
  "わ","を","ん",
  "が","ぎ","ぐ","げ","ご",
  "ざ","じ","ず","ぜ","ぞ",
  "だ","ぢ","づ","で","ど",
  "ば","び","ぶ","べ","ぼ",
  "ぱ","ぴ","ぷ","ぺ","ぽ"
  ]

hiragana1 = [
  "きゃ","きゅ","きょ",
  "しゃ","しゅ","しょ",
  "ちゃ","ちゅ","ちょ",
  "にゃ","にゅ","にょ",
  "ひゃ","ひゅ","ひょ",
  "ぴゃ","ぴゅ","ぴょ",
  "みゃ","みゅ","みょ",
  "りゃ","りゅ","りょ",
  "ぎゃ","ぎゅ","ぎょ",
  "じゃ","じゅ","じょ",
  "ぢゃ","ぢゅ","ぢょ",
  "びゃ","びゅ","びょ"
  ]
           
hiragana = hiragana0 ++ hiragana1

{-
hwords = [
  "いぬ",
  "ねこ",
  "ぞう",
  "いす",
  "つくえ",
  "がっこう",
  "からす",
  "えんぴつ",
  "ごはん"
  ]
-}
hlen = length hiragana
h0len = length hiragana0
--wlen = length hwords


getWord ws = do
  i <- randomIO
  let wlen = length ws
  return (ws !! (i `mod` wlen))
   
getHiragana = do
  i <- randomIO
  return (hiragana !! (i `mod` hlen))

getHiragana0 = do
  i <- randomIO
  return (hiragana0 !! (i `mod` h0len))



genString ws f i len len2 str | length str > len2 = return str
                              | length str > len  = do
                                  w <- getHiragana0
                                  genString ws f i len len2 (str ++ w)
                              | length str >= i && f == 0 = do
                                  w <- getWord ws
                                  genString ws 1 i len len2 (str ++ w)
                              | otherwise = do
                                  w <- getHiragana
                                  genString ws f i len len2 (str ++ w)

genMatrix :: [String] -> Int -> Int -> [String] -> Fay [String] 
genMatrix ws i len str | i > len = return str
                       | otherwise = do
                           r <-randomIO
                           let ll = 21
                               l = ll -4
                               r' = r `mod` l
                           s <- genString ws 0 r' l ll ""
                           genMatrix ws (i+1) len (str++[s])

{-
main = do
  s <- genMatrix 0 10 []
  putStr $ foldr (++) "" $ map (\t -> t++"\n") s 
-}

data Event
data Element

alert :: String -> Fay ()
alert = ffi "alert(%1)"

setBodyHtml :: String -> Fay ()
setBodyHtml = ffi "document.getElementById('lists').innerHTML = %1"

getInputs :: Fay [String]
getInputs = ffi "document.getElementById('InputTextarea').value.split(/\\r\\n|\\r|\\n/)"

addWindowEvent :: String -> (Event -> Fay ()) -> Fay ()
addWindowEvent = ffi "window.addEventListener(%1, %2)"

addClickEvent  :: (Event -> Fay ()) -> Fay ()
addClickEvent = ffi "document.getElementById('genProb').addEventListener('click', %1)"

setToggle  :: Fay ()
setToggle = ffi "setupHighlight()"


withSpan'' :: Char -> String
withSpan'' c = "<span class='char-item'>"++ [c] ++ "</span>"

withSpan' :: String -> String
withSpan' s = concat $ map withSpan'' s

withSpan :: [String] -> [String]
withSpan = map withSpan'

greet :: Event -> Fay()
greet event = do
  ws <- getInputs
  s <- genMatrix ws 0 10 []
--  let str = foldr (++) "" $ map (\t -> "<h3>"++t++"</h3></li>") $ withSpan s
  let str = foldr (++) "" $ map (\t -> "<div><h2>"++t++"</h2></div>") $ withSpan s
  setBodyHtml $ str
  setToggle

ready :: Fay () -> Fay ()
ready = ffi "window['jQuery'](%1)"
                 
main :: Fay ()
main = ready $ do
  addClickEvent greet
  addWindowEvent "load" greet
