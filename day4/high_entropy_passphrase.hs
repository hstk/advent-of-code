import Data.List(nub, foldl', mapAccumL)
import Input(passphrases)

dice :: String -> [[String]]
dice = map words . lines

isUniquePassphrase :: [String] -> Bool
isUniquePassphrase xs = xs == (nub xs)

countUnique :: String -> Int
countUnique pass = foldl' (\acc x -> case x of { True -> acc + 1; False -> acc}) 0 counted
   where counted = (map isUniquePassphrase $ dice pass)

small = "pphsv ojtou brvhsj cer ntfhlra udeh ccgtyzc zoyzmh jum lugbnk \n\
\ vxjnf fzqitnj uyfck blnl impo kxoow nngd worcm bdesehw \n\
\ caibh nfuk kfnu llfdbz uxjty yxjut jcea \n\
\ qiho qif eupwww avyglnj nxzotsu hio lws"