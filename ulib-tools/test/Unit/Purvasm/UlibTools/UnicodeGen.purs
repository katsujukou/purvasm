module Test.Unit.Purvasm.UlibTools.UnicodeGen (spec) where

import Prelude

import Data.Either (Either(..))
import Data.String (Pattern(..), contains)
import Purvasm.UlibTools.UnicodeGen (commitFailureMessage)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "Purvasm.UlibTools.UnicodeGen" do
    describe "commitFailureMessage" do
      it "names the failed stage and cause when the rollback itself succeeds" do
        let
          msg = commitFailureMessage ctxBackupPresent "committing the pin (lock.json)" "boom"
            (Right unit)
        contains (Pattern "failed committing the pin (lock.json): boom") msg `shouldEqual` true
        contains (Pattern "rolled back to its previous state") msg `shouldEqual` true
        -- A clean rollback must not claim (or imply) an inconsistency.
        contains (Pattern "INCONSISTENT") msg `shouldEqual` false

      it "reports an INCONSISTENT pair, not a silent success, when the rollback itself fails" do
        let
          msg = commitFailureMessage ctxBackupPresent "committing the pin (lock.json)" "boom"
            (Left "rollback boom")
        contains (Pattern "boom") msg `shouldEqual` true
        contains (Pattern "ROLLBACK OF out.purs ALSO FAILED: rollback boom") msg `shouldEqual` true
        contains (Pattern "INCONSISTENT") msg `shouldEqual` true
        -- Must not simultaneously read as a clean "rolled back" success message.
        contains (Pattern "rolled back to its previous state") msg `shouldEqual` false

      it "on rollback failure, points at the backup path when it is actually still present" do
        let msg = commitFailureMessage ctxBackupPresent "committing out.purs" "boom" (Left "e")
        contains (Pattern "is preserved at out.purs.bak") msg `shouldEqual` true

      it "on rollback failure with no backup ever taken, warns of unpinned new content instead" do
        let msg = commitFailureMessage ctxNoBackupTaken "committing out.purs" "boom" (Left "e")
        contains (Pattern "newly-generated content with no matching pin") msg `shouldEqual` true
        -- No backup exists, so there is nothing to point at as a recovery source.
        contains (Pattern "is preserved at") msg `shouldEqual` false

      it
        "on rollback failure, does NOT claim the backup is preserved if it is no longer present \
        \(e.g. removed externally, or that removal is the reason the rollback failed) -- the \
        \P2 this test guards: `backupExists` must be a fresh check, not inferred from history"
        do
          let msg = commitFailureMessage ctxBackupTakenButGone "committing out.purs" "boom" (Left "e")
          contains (Pattern "is preserved at") msg `shouldEqual` false
          contains (Pattern "no backup is available to recover from") msg `shouldEqual` true
  where
  -- A backup was taken and is confirmed present right now.
  ctxBackupPresent =
    { out: "out.purs", lockFile: "lock.json", outBak: "out.purs.bak", backupExists: true }
  -- No `--out` existed before the transaction, so no backup was ever taken.
  ctxNoBackupTaken =
    { out: "out.purs", lockFile: "lock.json", outBak: "out.purs.bak", backupExists: false }
  -- A backup WAS taken at the start of the transaction, but a fresh check at failure time finds
  -- it gone (e.g. removed externally). Same `backupExists: false` as `ctxNoBackupTaken` --
  -- `commitFailureMessage` cannot (and must not) distinguish "never had one" from "had one, now
  -- gone"; both must fall back to the same "no backup available" wording, never "is preserved at".
  ctxBackupTakenButGone =
    { out: "out.purs", lockFile: "lock.json", outBak: "out.purs.bak", backupExists: false }
