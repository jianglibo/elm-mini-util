port module Pusher exposing (..)

import Browser
import Html as H
import Html.Attributes exposing (class, disabled, style, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as D
import Json.Encode as E
import Pusher.ChannelNameUtil as Util exposing (pickoutNumber)
import Pusher.Dto
    exposing
        ( ChannelData
        , ChannelPostData
        , RemoteContentUpldateEvent
        , channelDataDecoder
        , channelPostDataEncoder
        , initValueDecoder
        , remoteContentUpdateEventDecoder
        )
import Pusher.Model exposing (Model, isChannelNameEmpty)
import Time



--  exposing (Decoder, decodeValue, field, int, string)
-- { "name": "Tom", "age": 42 }


channelNameLength : Int
channelNameLength =
    9


channelNameExamplePattern : String
channelNameExamplePattern =
    "345-332-828"



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


port subscribeToPusher : String -> Cmd msg


port remoteContentUpdated : (E.Value -> msg) -> Sub msg


port socketIdAcquired : (String -> msg) -> Sub msg


port askForConfirmation : () -> Cmd msg


port confirmations : (Bool -> msg) -> Sub msg


init : D.Value -> ( Model, Cmd Msg )
init flags =
    case D.decodeValue initValueDecoder flags of
        Ok initValue ->
            ( { cpsyc_url = initValue.cpsyc_url
              , channel_name = initValue.channel_name
              , content = ""
              , last_saved_value = ""
              , error_msg = ""
              , time = Time.millisToPosix 0
              , socket_id = ""
              , channel_name_done = False
              , last_content_update_event = Nothing
              }
              -- Model url "" "ccc" "" (Time.millisToPosix 0)
            , let
                allNumberCode =
                    Util.pickoutNumber initValue.channel_name
              in
              if (allNumberCode |> String.length) == channelNameLength then
                Http.get
                    { url = Util.buildChannelQueryUrl initValue.cpsyc_url allNumberCode
                    , expect = Http.expectJson ReturnChannelData channelDataDecoder
                    }

              else
                Cmd.none
            )

        Err err ->
            ( { cpsyc_url = ""
              , channel_name = ""
              , content = ""
              , last_saved_value = ""
              , error_msg = D.errorToString err
              , time = Time.millisToPosix 0
              , socket_id = ""
              , channel_name_done = False
              , last_content_update_event = Nothing
              }
              --   Model "" "" "" (D.errorToString err) (Time.millisToPosix 0)
            , Cmd.none
            )


type Msg
    = NoOp
    | AskForConfirmation
    | EnteringChannelName String
    | ReturnChannelData (Result Http.Error ChannelData)
    | DoClicked
    | ContentChanged String
    | Tick Time.Posix
    | PostContentReturn (Result Http.Error String)
    | SocketIdAcquired String
    | RemoteContentUpdated (Result D.Error RemoteContentUpldateEvent)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        AskForConfirmation ->
            ( model, askForConfirmation () )

        EnteringChannelName code ->
            let
                allNumberCode =
                    Util.pickoutNumber code
            in
            if (allNumberCode |> String.length) == channelNameLength then
                ( { model | channel_name = code, error_msg = "" }
                , Http.get
                    { url = Util.buildChannelQueryUrl model.cpsyc_url allNumberCode
                    , expect = Http.expectJson ReturnChannelData channelDataDecoder
                    }
                )

            else
                ( { model | channel_name = code }, Cmd.none )

        ReturnChannelData rcn ->
            case rcn of
                Ok cd ->
                    let
                        isCurrentChannelNameEmpty =
                            isChannelNameEmpty model

                        isChannelNameFromServerSuccess =
                            (cd.id |> Util.pickoutNumber) == cd.id
                    in
                    case ( isCurrentChannelNameEmpty, isChannelNameFromServerSuccess ) of
                        ( _, False ) ->
                            ( { model | error_msg = cd.id }, Cmd.none )

                        ( _, True ) ->
                            ( { model
                                | content = cd.content
                                , channel_name = Util.insertDash cd.id 3
                                , channel_name_done = True
                              }
                            , subscribeToPusher cd.id
                            )

                Err err ->
                    ( { model | error_msg = Util.httpErrorToString err }, Cmd.none )

        DoClicked ->
            let
                allNumberCode =
                    Util.pickoutNumber model.channel_name

                -- Debug.log "allNumberCode:" allNumberCode
            in
            if String.isEmpty allNumberCode || (String.length allNumberCode == channelNameLength) then
                ( { model | error_msg = "" }
                , Http.get
                    { url = Util.buildChannelQueryUrl model.cpsyc_url allNumberCode
                    , expect = Http.expectJson ReturnChannelData channelDataDecoder
                    }
                )

            else
                ( { model
                    | error_msg =
                        "Channel name must contains "
                            ++ String.fromInt channelNameLength
                            ++ " digits, like "
                            ++ channelNameExamplePattern
                  }
                , Cmd.none
                )

        ContentChanged content ->
            ( { model | content = content }, Cmd.none )

        Tick newTime ->
            if model.last_saved_value /= model.content then
                ( { model | time = newTime, last_saved_value = model.content }
                , Http.post
                    { url = Util.buildContentPostUrl model.cpsyc_url
                    , body =
                        Http.jsonBody
                            (E.object
                                [ ( "content", E.string model.content )
                                , ( "socket_id", E.string model.socket_id )
                                , ( "id", E.string model.channel_name )
                                , ( "force", E.bool False )
                                ]
                            )
                    , expect = Http.expectString PostContentReturn
                    }
                )

            else
                ( { model | time = newTime }
                , Cmd.none
                )

        PostContentReturn pcr ->
            case pcr of
                Ok s ->
                    if String.isEmpty s then
                        ( { model | error_msg = "" }, Cmd.none )

                    else
                        ( { model | error_msg = s }, Cmd.none )

                Err err ->
                    ( { model | error_msg = Util.httpErrorToString err }, Cmd.none )

        SocketIdAcquired socket_id ->
            ( { model | socket_id = socket_id }
            , Cmd.none
            )

        RemoteContentUpdated contentUpdateResult ->
            case contentUpdateResult of
                Ok v ->
                    ( { model
                        | last_content_update_event = Just v
                        , content =
                            if (v.socket_id == model.socket_id) && not v.force then
                                model.content

                            else
                                v.content
                        , last_saved_value = v.content
                      }
                    , Cmd.none
                    )

                Err err ->
                    ( { model
                        | error_msg = D.errorToString err
                        , last_content_update_event = Nothing
                      }
                    , Cmd.none
                    )



-- VIEW


view : Model -> H.Html Msg
view model =
    let
        ev =
            case model.last_content_update_event of
                Just e ->
                    E.encode 0
                        (E.object
                            [ ( "socket_id", E.string e.socket_id )
                            , ( "content", E.string e.content )
                            ]
                        )

                Nothing ->
                    ""

        channelPostData =
            ChannelPostData (model.channel_name |> pickoutNumber) model.content model.socket_id
    in
    H.div []
        [ H.input
            [ onInput EnteringChannelName
            , disabled model.channel_name_done
            , value model.channel_name
            ]
            [ H.text model.channel_name ]
        , H.button
            [ onClick DoClicked
            , style "margin-left" "5px"
            , disabled model.channel_name_done
            ]
            [ H.text "Do" ]
        , H.span [ style "margin-left" "10px", style "color" "red", style "font-weight" "bold" ] [ H.text model.error_msg ]
        , H.p [] [ H.text "Connect to an existing channel by entering the code. OR, Click the Do to request a new channel." ]

        -- , H.p [] [ H.text ev ]
        -- , H.p [] [ H.text (model.time |> Time.posixToMillis |> String.fromInt) ]
        , H.textarea
            [ onInput ContentChanged
            , value model.content
            , disabled (not model.channel_name_done)
            ]
            [ H.text model.content ]
        , H.div [ class "language-bash", class "highlighter-rouge" ]
            [ H.div [ class "highlight" ]
                [ H.pre [ class "highlight" ]
                    [ H.code []
                        [ H.text ("curl " ++ model.cpsyc_url ++ "/channel/query?channel_name")
                        , H.span [ class "o" ] [ H.text "=" ]
                        , H.text (model.channel_name |> pickoutNumber)
                        ]
                    ]
                ]
            ]
        , H.div [ class "language-bash", class "highlighter-rouge" ]
            [ H.div [ class "highlight" ]
                [ H.pre [ class "highlight" ]
                    [ H.code []
                        [ H.text "curl "
                        , H.span [ class "s1" ] [ H.text ("'" ++ model.cpsyc_url ++ "/channel/post'") ]
                        , H.span [ class "nt" ] [ H.text " -H " ]
                        , H.span [ class "s1" ] [ H.text "'Content-Type: application/json'" ]
                        , H.span [ class "nt" ] [ H.text " --data-raw " ]
                        , H.span [ class "s1" ] [ H.text ("'" ++ E.encode 0 (channelPostDataEncoder channelPostData) ++ "'") ]
                        ]
                    ]
                ]
            ]
        ]



-- text (String.fromInt model.currentTime)
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ confirmations (\_ -> NoOp)
        , socketIdAcquired SocketIdAcquired
        , remoteContentUpdated (\v -> RemoteContentUpdated (D.decodeValue remoteContentUpdateEventDecoder v))
        , Time.every 1000 Tick
        ]
