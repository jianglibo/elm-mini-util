module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--
-- https://elm-lang.org/docs/syntax
-- https://package.elm-lang.org/packages/elm/core/latest/

import Browser
import Html exposing (Html, div, text, textarea)
import Html.Attributes exposing (class, disabled, property)
import Html.Events exposing (onInput)
import Json.Encode as Encode



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { graphql : String
    , variables : String
    , combined : String
    , length : Int
    }


init : Model
init =
    { graphql = "", variables = "", combined = "", length = 0 }



-- UPDATE


type Msg
    = Graphql String
    | Variable String


type Line
    = FirstLine Escape
    | OtherLine Escape


type Escape
    = Once String
    | Twice String


escapeMe : Escape -> String
escapeMe es =
    case es of
        Once v ->
            v |> String.replace "\"" "\\\""

        Twice v ->
            v |> String.replace "\"" "\\\\\\\""


quotaLine : Line -> String
quotaLine line =
    case line of
        FirstLine v ->
            "+\"\\\"" ++ escapeMe v ++ "\""

        OtherLine v ->
            "+\"" ++ escapeMe v ++ "\""


transform : Model -> Model
transform model =
    let
        newGraphql =
            case .graphql model |> String.trim of
                "" ->
                    ""

                v ->
                    case String.lines v of
                        [] ->
                            ""

                        first :: rest ->
                            let
                                firstLine =
                                    FirstLine (Twice first) |> quotaLine

                                restLines =
                                    rest |> List.map (\x -> OtherLine (Twice x) |> quotaLine)
                            in
                            ((firstLine :: restLines) |> String.join "\n") ++ "\n+\"\\\",\""

        newVariables =
            case .variables model |> String.trim of
                "" ->
                    "+\"" ++ "{}" ++ "\""

                v ->
                    String.lines v
                        |> List.map (\x -> Once x |> OtherLine |> quotaLine)
                        |> String.join "\n"

        combined =
            "String body = \"{\\\"query\\\" :\"\n" ++ newGraphql ++ "\n+\"\\\"variables\\\":\"\n" ++ newVariables ++ "\n+\"}\";"
    in
    { model | combined = combined, length = String.length combined }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Graphql content ->
            transform { model | graphql = content }

        Variable content ->
            transform { model | variables = content }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text "Graphql" ]
        , textarea [ class "box-graphql", onInput Graphql ] [ text (.graphql model) ]
        , div [] [ text "Variables" ]
        , textarea [ class "box-variables", onInput Variable ] [ text (.variables model) ]
        , div [] [ text "Combined" ]
        , textarea
            [ class "box-combined"
            , disabled True
            , property "selectionStart" (Encode.int 0)
            , property "selectionEnd" (Encode.int (.length model))
            ]
            [ text (.combined model) ]
        ]
