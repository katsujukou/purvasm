-- | The by-need census report format: one TSV row per object per site class, plus an `all` row
-- | carrying the object's totals. `elided` are the occurrences the compiler proved `NeverByNeed`
-- | (no chain emitted); `emitted` are the rest.
-- |
-- | The reconciliation (`tools/byneed-census.sh`) checks BOTH columns against the object's `.ll`:
-- | `emitted` must equal its `fchk` chain count, and `elided` must equal the chains the same object
-- | has WITHOUT the lattice — the ADR-0107 §2 accounting identity, stated over emission
-- | occurrences rather than proof sites because one proof site can be emitted many times.
module Purvasm.Census.ByNeed.Report
  ( header
  , renderCensus
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Purvasm.Census.ByNeed (Census, SiteClass, elidedSites, emittedSites, siteClasses, siteCount)

-- | The TSV header (a `#` comment line, so `awk`/`sort` pipelines can drop it on the prefix).
header :: String
header = "#object\tclass\telided\temitted"

-- | One object's rows: every site class it exercised, then its `all` total.
renderCensus :: String -> Census -> String
renderCensus object c =
  joinWith "\n" (Array.mapMaybe classRow siteClasses <> [ totalRow ]) <> "\n"
  where
  classRow :: SiteClass -> Maybe String
  classRow cls =
    let
      r = siteCount c cls
    in
      if r.elided + r.emitted == 0 then Nothing
      else Just (row (show cls) r.elided r.emitted)

  totalRow = row "all" (elidedSites c) (emittedSites c)

  row cls elided emitted = joinWith "\t" [ object, cls, show elided, show emitted ]
