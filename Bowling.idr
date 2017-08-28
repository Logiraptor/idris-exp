module Main


import Data.Fin

Roll : Type
Roll = Fin 11

-- A - add initial clause
-- S - split ident into case clause

-- We mark message as Lazy because it's not always needed
-- Passing tests will not evaluate their messages
assert : Bool -> Lazy String -> IO ()
assert condition message =
    if condition then
        putStrLn "Success"
    else
        putStrLn ("FAIL - " ++ message)


assert_equal : (Show a, Eq a) => a -> a -> Lazy String -> IO ()
assert_equal expected actual message =
    if expected == actual then
        putStr "."
    else
        putStrLn ("expected: " ++ (show expected) ++ ", but got: " ++ (show actual) ++ " - " ++ message)


repeat : a -> Nat -> List a
repeat value n = repeat' value n [] where
    repeat' : a -> Nat -> List a -> List a
    repeat' x Z xs = xs
    repeat' x (S n) xs = (repeat' x n (x::xs))

roll_many : Roll -> Nat -> List Roll
roll_many = repeat {a=Roll}


score : List Roll -> Nat
score xs = score' xs 0 1 where

    next_frame : List Roll -> Nat
    next_frame [] = 0
    next_frame (x :: xs) = finToNat x

    next_two_frames : List Roll -> Nat
    next_two_frames [] = 0
    next_two_frames (x :: []) = finToNat x
    next_two_frames (x :: (y :: xs)) = (finToNat x) + (finToNat y)

    spare_bonus : List Roll -> Nat
    spare_bonus xs = next_frame xs

    strike_bonus : List Roll -> Nat
    strike_bonus xs = next_two_frames xs

    score_frame : List Roll -> (Nat, List Roll)
    score_frame [] = (0, [])
    score_frame (x :: []) = (finToNat x, [])
    score_frame ((FS (FS (FS (FS (FS (FS (FS (FS (FS (FS FZ)))))))))) :: xs) =
        ((strike_bonus xs) + 10, xs)
    score_frame (x :: (y :: xs)) = case (finToNat x) + (finToNat y) of
        (S (S (S (S (S (S (S (S (S (S Z)))))))))) => ((spare_bonus xs) + 10, xs)
        sum => (sum, xs)

    score': List Roll -> Nat -> Nat -> Nat
    score' [] x frame = x
    score' xs x (S (S (S (S (S (S (S (S (S (S (S Z))))))))))) = x
    score' xs x frame =
        let (frame_score, rest) = score_frame xs in
            score' rest (frame_score + x) (frame + 1)


main : IO ()
main = do assert_equal 0   (score (roll_many 0 20))               "Gutter game scores 0"
          assert_equal 20  (score (roll_many 1 20))               "All ones scores 20"
          assert_equal 16  (score ([5,5,3] ++ (roll_many 0 17)))  "One spare"
          assert_equal 24  (score ([10,3,4] ++ (roll_many 0 16))) "One strike"
          assert_equal 300 (score (roll_many 10 12))              "Perfect Game"

