module Expander exposing (computeOutput)

import Parser exposing
    ( Parser, Trailing(..), (|.), (|=), run, succeed, problem
    , symbol, oneOf, end, sequence, backtrackable
    , chompWhile, getChompedString, lazy
    )
import CommonModel exposing (..)


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
            |= (symbolParser |> Parser.map String.trim)
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


flattenExpression : Expression -> String
flattenExpression expr =
    case expr of
        Application identifier args ->
            identifier ++ "(" ++ (String.join ", " <| List.map flattenExpression args) ++ ")"
        Value symbol -> symbol


indentMultiline : String -> String -> String
indentMultiline indent string =
    indent ++ (String.replace "\n" ("\n" ++ indent) string)


expandExpression : ExpanderConfig -> Expression -> String
expandExpression config expr =
    case expr of
        Application identifier args ->
            let
                flatExpr = flattenExpression expr
            in
                if String.length flatExpr > config.maxFlatExpressionLength then
                    let
                        expandArg = (indentMultiline config.indentation) << (expandExpression config)
                    in
                        identifier
                            ++ "(\n"
                            ++ String.join ",\n" (List.map expandArg args)
                            ++ "\n)"
                else
                    flatExpr
        Value symbol -> symbol


computeOutput : ExpanderConfig -> String -> Result String String
computeOutput config input = parseInput input |> Result.map (expandExpression config)
