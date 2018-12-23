module Format exposing (..)

indentMultiline : String -> String -> String
indentMultiline indent string =
    indent ++ (String.replace "\n" ("\n" ++ indent) string)
