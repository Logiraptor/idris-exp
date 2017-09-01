module Main

import Data.Vect

mutual
    score_strike : Nat -> Vect n Nat -> Nat
    score_strike n [] = 10
    score_strike n (x :: []) = 10 + x + (score n (x :: []))
    score_strike n (x :: y :: xs) = 10 + x + y + (score n (x :: y :: xs))

    score_spare : Nat -> Vect n Nat -> Nat
    score_spare n [] = 10
    score_spare n (x :: xs) = 10 + x + (score n (x :: xs))

    score : Nat -> Vect n Nat -> Nat
    score _ [] = 0
    score Z _ = 0
    score (S n) ((S (S (S (S (S (S (S (S (S (S Z)))))))))) :: xs) = score_strike n xs
    score (S n) (x :: y :: xs) = if x + y == 10 then score_spare n xs else x + y + score n xs
    score (S n) (x :: xs) = x + score n xs


gutterGame : score 10 (replicate 20 0) = 0
gutterGame = Refl

onePinGames : score 10 (replicate 20 1) = 20
onePinGames = Refl

strike : score 10 ([10, 3, 5] ++ replicate 16 0) = 26
strike = Refl

spare : score 10 ([5, 5, 3] ++ replicate 17 0) = 16
spare = Refl

perfectGame : score 10 (replicate 12 10) = 300
perfectGame = Refl


-- gutterGamesWithNRolls : (n : Nat) -> score 10 (replicate n 0) = 0
-- gutterGamesWithNRolls Z = Refl
-- gutterGamesWithNRolls (S k) = inductiveStep {n=k} {f=10} (gutterGamesWithNRolls k) where
--     inductiveStep : {n : Nat} -> {f: Nat} -> score f (replicate n 0) = 0 -> score f (replicate (S n) 0) = 0
--     inductiveStep {n = Z} prf = ?inductiveStep_rhs_3
--     inductiveStep {n = (S j)} prf = ?inductiveStep_rhs_2



main : IO ()
main = putStrLn "Woop"
