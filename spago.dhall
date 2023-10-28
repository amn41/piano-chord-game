{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "chord-editor"
, dependencies =
  [ "aff"
  , "arrays"
  , "canvas"
  , "colors"
  , "const"
  , "datetime"
  , "dom-indexed"
  , "drawing"
  , "effect"
  , "either"
  , "enums"
  , "foldable-traversable"
  , "foreign"
  , "halogen"
  , "halogen-components"
  , "integers"
  , "js-fileio"
  , "lists"
  , "maybe"
  , "media-types"
  , "midi"
  , "now"
  , "ordered-collections" 
  , "partial"
  , "prelude"
  , "routing"
  , "routing-duplex"
  , "soundfonts"
  , "stringutils"
  , "strings"
  , "transformers"
  , "tuples"
  , "validation"
  , "web-dom"
  , "web-html"
  , "web-uievents"
  , "web-events"
  , "yoga-json"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}