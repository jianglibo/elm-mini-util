module Pusher.ChannelNameUtil exposing (..)

import Http
import Set exposing (Set)
import Url.Builder as UrlBuilder
import List exposing (length)


buildChannelQueryUrl : String -> String -> String
buildChannelQueryUrl baseUrl code =
    baseUrl
        ++ UrlBuilder.absolute [ "channel", "query" ]
            [ UrlBuilder.string "channel_name" code
            ]

buildContentPostUrl : String ->  String
buildContentPostUrl baseUrl =
    baseUrl ++ UrlBuilder.absolute [ "channel", "post" ] []

allNumber : Set Char
allNumber =
    Set.fromList [ '1', '2', '3', '4', '5', '6', '7', '8', '9', '0' ]


isNumber : Char -> Bool
isNumber c =
    Set.member c allNumber


pickoutNumber : String -> String
pickoutNumber code =
    String.toList code |> List.filter isNumber |> String.fromList

insertDash: String -> Int -> String
insertDash toDash step =
    let
        left = String.left step toDash
        right = String.dropLeft step toDash
        
    in
       if String.isEmpty right then
           left
        else
            left ++ "-" ++ (insertDash right step)


       






httpErrorToString : Http.Error -> String
httpErrorToString err =
    case err of
        Http.BadUrl url ->
            "BadUrl: " ++ url

        Http.Timeout ->
            "Timeout."

        Http.NetworkError ->
            "NetworkError."

        Http.BadStatus i ->
            "BadStatus: " ++ (i |> String.fromInt)

        Http.BadBody b ->
            "BadBody: " ++ b
