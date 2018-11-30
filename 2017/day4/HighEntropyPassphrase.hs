{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}

module HighEntropyPassphrase where

import Data.List
import Text.RawString.QQ

dice :: String -> [[String]]
dice = map words . lines

isUniquePassphrase :: [String] -> Bool
isUniquePassphrase xs = xs == (nub xs)

isUniquePassphrase' :: [String] -> Bool
isUniquePassphrase' xs = sorted == nub sorted
  where sorted = map sort xs

countUnique :: ([String] -> Bool) -> String -> Int
countUnique f pass = foldl' (\acc x -> case x of { True -> acc + 1; False -> acc}) 0 counted
  where counted = (map f $ dice pass)

small :: String
small = [r|pphsv ojtou brvhsj cer ntfhlra udeh ccgtyzc zoyzmh jum lugbnk
vxjnf fzqitnj uyfck blnl impo kxoow nngd worcm bdesehw
caibh nfuk kfnu llfdbz uxjty yxjut jcea
qiho qif eupwww avyglnj nxzotsu hio lws|]