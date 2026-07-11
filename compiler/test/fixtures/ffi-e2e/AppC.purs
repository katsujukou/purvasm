-- | app-C FFI fixture (ADR-0091 §2): a `foreign import` whose provider is the sibling `AppC.c`,
-- | compiled with `-DPVF_MODULE=AppC` and exporting `PVF_EXPORT(addSeven)`. `tools/ffi-e2e.sh`
-- | builds this to a native binary and asserts the result (`addSeven 35 == 42`).
module AppC where

foreign import addSeven :: Int -> Int

answer :: Int
answer = addSeven 35
