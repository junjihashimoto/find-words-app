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
  "だ","ぢ","づ","で","ど"]

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

hlen = length hiragana
h0len = length hiragana0
wlen = length hwords


getWord = do
  i <- randomIO
  return (hwords !! (i `mod` wlen))
   
getHiragana = do
  i <- randomIO
  return (hiragana !! (i `mod` hlen))

getHiragana0 = do
  i <- randomIO
  return (hiragana0 !! (i `mod` h0len))



genString f i len len2 str | length str > len2 = return str
                           | length str > len  = do
                               w <- getHiragana0
                               genString f i len len2 (str ++ w)
                           | length str >= i && f == 0 = do
                               w <- getWord
                               genString 1 i len len2 (str ++ w)
                           | otherwise = do
                               w <- getHiragana
                               genString f i len len2 (str ++ w)

genMatrix :: Int -> Int -> [String] -> Fay [String] 
genMatrix i len str | i > len = return str
                    | otherwise = do
                        r <-randomIO
                        let ll = 15
                            l = ll -4
                            r' = r `mod` l
                        s <- genString 0 r' l ll ""
                        genMatrix (i+1) len (str++[s])

{-
main = do
  s <- genMatrix 0 10 []
  putStr $ foldr (++) "" $ map (\t -> t++"\n") s 
-}

data Event

alert :: String -> Fay ()
alert = ffi "alert(%1)"

setBodyHtml :: String -> Fay ()
setBodyHtml = ffi "document.body.innerHTML = %1"

addWindowEvent :: String -> (Event -> Fay ()) -> Fay ()
addWindowEvent = ffi "window.addEventListener(%1, %2)"

greet :: Event -> Fay()
greet event = do
  s <- genMatrix 0 10 []
  setBodyHtml $ foldr (++) "" $ map (\t -> t++"<br/><br/>") s 

main :: Fay ()
main = do
  addWindowEvent "load" greet
