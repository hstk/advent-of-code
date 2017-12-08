import Data.List(nub)
import Input(passphrases)

diceData :: String -> [[String]]
diceData = map words . lines

isUniquePassphrase :: [String] -> Bool
isUniquePassphrase xs = xs == (nub xs)

small = "pphsv ojtou brvhsj cer ntfhlra udeh ccgtyzc zoyzmh jum lugbnk \n\
\ vxjnf fzqitnj uyfck blnl impo kxoow nngd worcm bdesehw \n\
\ caibh nfuk kfnu llfdbz uxjty yxjut jcea \n\
\ qiho qif eupwww avyglnj nxzotsu hio lws"