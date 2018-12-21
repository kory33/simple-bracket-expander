module Expander exposing (computeOutput)

import Parser exposing
    ( Parser, Trailing(..), (|.), (|=), run, succeed, problem
    , symbol, oneOf, end, sequence, backtrackable
    , chompWhile, getChompedString, lazy
    )


type alias Symbol = String


type Expression
    = Application Symbol (List Expression)
    | Value Symbol


isWhiteSpace : Char -> Bool
isWhiteSpace c = List.member c <| String.toList " \t\r\n"


isSymbolChar : Char -> Bool
isSymbolChar c = (not <| isWhiteSpace c) && not (List.member c <| String.toList "(,)")


spaces : Parser ()
spaces = chompWhile isWhiteSpace


symbolParser : Parser Symbol
symbolParser =
    let
        chompWhileSymbolChar = 
            getChompedString <|
                succeed ()
                    |. chompWhile isSymbolChar
        matchNonempty match =
            if match /= "" then
                succeed match
            else
                problem "matched on an empty symbol"
    in
        succeed identity
            |. spaces
            |= (chompWhileSymbolChar |> (Parser.andThen matchNonempty))
            |. spaces


expressionParser : Parser Expression
expressionParser =
    oneOf
        [ succeed Application
            |= backtrackable symbolParser
            |. spaces
            |= sequence
                { start = "("
                , separator = ","
                , end = ")"
                , spaces = spaces
                , item = lazy (\_ -> expressionParser)
                , trailing = Forbidden
                }
            |. spaces
            |. end
        , succeed Value |= symbolParser
        ]


parseInput : String -> Result String Expression
parseInput input = run expressionParser input |> Result.mapError Parser.deadEndsToString


expandExpression : Expression -> String
expandExpression expr =
    case expr of
        Application identifier args ->
            let
                argsExpansion = String.join ", " <| List.map expandExpression args
            in
                "Application \"" ++ identifier ++ "\" [ " ++ argsExpansion ++ " ]"
        Value symbol -> "Symbol \"" ++ symbol ++ "\""


computeOutput : String -> Result String String
computeOutput input = parseInput input |> Result.map expandExpression
