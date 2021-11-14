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