module Hello exposing (..)

import Html exposing (text)

a : number
a = 1
main : Html.Html msg
main =
  text
   (String.join ","
    (List.map (\line -> "+\"" ++ line ++ "\";")
      (String.lines "Hello!")))


-- https://gist.github.com/yang-wei/4f563fbf81ff843e8b1e

type alias  User =
    { name : String
    , age : Int
    }

-- record constructor
-- u = User "a" 1

-- decode

--  is D.Value
-- jv = D.decodeValue (D.field "x" D.int)  The D.Value from outside.

-- decodeString string "\"hello\""         == Ok "hello"
-- decodeString (field "x" int) "{ \"x\": 3 }"            == Ok 3
