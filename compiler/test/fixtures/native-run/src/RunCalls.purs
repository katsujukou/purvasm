module RunCalls where

-- a 2-argument function passed as a first-class value (generic pv_apply inside apply2)
pickFst :: Int -> Int -> Int
pickFst a _ = a

apply2 :: (Int -> Int -> Int) -> Int -> Int -> Int
apply2 f x y = f x y

-- a let-bound lambda capturing an outer local (`k`), calling a top-level fn
withCapture :: Int -> Int
withCapture k =
  let
    f x = pickFst k x
  in
    f 99

answer :: Int
answer = apply2 pickFst (withCapture 42) 7
