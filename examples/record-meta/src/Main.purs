module Example.RecordMeta.Main where

import Prelude

import Effect (Effect)
import Effect.Console (logShow)
import Record.Studio (recordKeys, shrink, (//))

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
