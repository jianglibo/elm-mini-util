port module Pusher exposing (..)

import Browser
import Html as H
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Json.Decode as D



--  exposing (Decoder, decodeValue, field, int, string)
-- { "name": "Tom", "age": 42 }


ageDecoder : D.Decoder Int
ageDecoder =
    D.field "age" D.int


channelIdDecoder : D.Decoder String
channelIdDecoder =
    D.field "channel_name" D.string



-- MAIN


main : Program D.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- PORTS


port askForConfirmation : () -> Cmd msg


port confirmations : (Bool -> msg) -> Sub msg



-- MODEL


type alias Model =
    { channel_name : String
    , content : String
    }


init : D.Value -> ( Model, Cmd Msg )
init flags =
    case D.decodeValue channelIdDecoder flags of
        Ok v ->
            ( Model v "", Cmd.none )

        Err _ ->
            ( Model "" "", Cmd.none )



-- case decodeValue flagsDecoder flags of
--    Ok successfulFlags -> 1
--           -- set up your model etc.
--    Err err -> 0
-- set up your model etc.
-- UPDATE


type Msg
    = NoOp
    | AskForConfirmation


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        AskForConfirmation ->
            ( model, askForConfirmation () )



-- VIEW


view : Model -> H.Html Msg
view model =
    H.div []
        [ H.input [] [ H.text model.channel_name ]
        , H.button [ onClick AskForConfirmation ] [ H.text "Push" ]
        , H.p [] []
        , H.textarea [] [ H.text "content" ]
        ]



-- text (String.fromInt model.currentTime)
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    confirmations (\_ -> NoOp)
