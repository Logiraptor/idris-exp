module Main

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


repeat : Int -> Int -> List Int
repeat value n = repeat' value n [] where
    repeat' : Int -> Int -> List Int -> List Int
    repeat' x 0 xs = xs
    repeat' x n xs = (repeat' x (n-1) (x::xs))


score : List Int -> Int
score xs = score' xs 0 1 where

    next_frame : List Int -> Int
    next_frame [] = 0
    next_frame (x :: xs) = x

    next_two_frames : List Int -> Int
    next_two_frames [] = 0
    next_two_frames (x :: []) = x
    next_two_frames (x :: (y :: xs)) = x + y

    spare_bonus : List Int -> Int
    spare_bonus xs = next_frame xs

    strike_bonus : List Int -> Int
    strike_bonus xs = next_two_frames xs

    score_frame : List Int -> (Int, List Int)
    score_frame [] = (0, [])
    score_frame (x :: []) = (x, [])
    score_frame (10 :: xs) =
        ((strike_bonus xs) + 10, xs)
    score_frame (x :: (y :: xs)) = case x + y of
        10 => ((spare_bonus xs) + 10, xs)
        _ => (x + y, xs)

    score': List Int -> Int -> Int -> Int
    score' [] x frame = x
    score' xs x 11 = x
    score' xs x frame =
        let (frame_score, rest) = score_frame xs in
            score' rest (frame_score + x) (frame + 1)


main : IO ()
main = do assert_equal 0   (score (repeat 0 20))               "Gutter game scores 0"
          assert_equal 20  (score (repeat 1 20))               "All ones scores 20"
          assert_equal 16  (score ([5,5,3] ++ (repeat 0 17)))  "One spare"
          assert_equal 24  (score ([10,3,4] ++ (repeat 0 16))) "One strike"
          assert_equal 300 (score (repeat 10 12))              "Perfect Game"

