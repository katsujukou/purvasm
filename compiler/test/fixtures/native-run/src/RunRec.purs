module RunRec where

-- structural self-recursion (a `Grec`/`SSelf` self-call), terminating at `Nil` — no arithmetic needed.
data L = Nil | Cons Int L

lastOr :: Int -> L -> Int
lastOr d l = case l of
  Nil -> d
  Cons x rest -> lastOr x rest

answer :: Int
answer = lastOr 0 (Cons 1 (Cons 2 (Cons 3 Nil)))
