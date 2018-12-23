module CommonModel exposing (..)

import Parser exposing (DeadEnd)


type alias ExpanderConfig =
    { indentation : String
    , maxFlatExpressionLength : Int
    }


type alias ParserOutput = Result (List DeadEnd) String
