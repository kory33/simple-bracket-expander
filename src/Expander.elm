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


spaces : Parser ()
spaces = chompWhile isWhiteSpace


symbolParser : Parser Symbol
symbolParser =
    let
        isSymbolChar : Char -> Bool
        isSymbolChar c = not (List.member c <| String.toList "(,)")

        matchNonempty : String -> Parser String
        matchNonempty match =
            if match /= "" then
                succeed match
            else
                problem "matched on an empty symbol"

        keepWhile : (Char -> Bool) -> Parser String
        keepWhile f = getChompedString <| chompWhile f
    in
        succeed identity
            |. spaces
            |= ((keepWhile isSymbolChar) |> Parser.andThen matchNonempty)
            |. spaces


expressionParser : Parser Expression
expressionParser =
    let
        receiver : Symbol -> Maybe (List Expression) -> Expression
        receiver symbol maybeArgs =
            case maybeArgs of
                Just args -> Application symbol args
                Nothing -> Value symbol
    in
        succeed receiver
            |= symbolParser
            |= oneOf
                [ succeed Just
                    |= sequence
                        { start = "("
                        , separator = ","
                        , end = ")"
                        , spaces = spaces
                        , item = lazy (\_ -> expressionParser)
                        , trailing = Forbidden
                        }
                    |. spaces
                , succeed Nothing
                ]


parseInput : String -> Result String Expression
parseInput input =
    run (expressionParser |. end) input
        |> Result.mapError Parser.deadEndsToString


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
