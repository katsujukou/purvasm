module Example.RecordMeta.Main where

import Prelude

import Data.Symbol (reflectSymbol)
import Effect (Effect)
import Effect.Console (log, logShow)
import Fmt as Fmt
import Record.Studio (recordKeys, shrink, (//))
import Record.Studio as SingletonRecord

type Person = { name :: String, age :: Int }

alice :: Person
alice = { name: "Alice", age: 15 }

type Profile =
  { epithet :: String
  , speciality :: Array String
  }

aboutAlice :: Profile
aboutAlice =
  { epithet: "Seven-Colored Puppeteer"
  , speciality:
      [ "Puppetry"
      , "Study of Witchcraft"
      ]
  }

main :: Effect Unit
main = do
  logShow alice
  let
    nameAndProfile :: { name :: String, profile :: Profile }
    nameAndProfile = shrink (alice // { profile: aboutAlice })
  logShow nameAndProfile
  logShow (recordKeys nameAndProfile)
  let
    nameOnly :: { name :: String }
    nameOnly = shrink nameAndProfile
    nameKey = reflectSymbol $ SingletonRecord.key nameOnly
    nameValue = SingletonRecord.value nameOnly
  log $ Fmt.fmt @"{nameKey} = {nameValue}" { nameKey, nameValue }