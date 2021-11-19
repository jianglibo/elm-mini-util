module Pusher.Model exposing (Model, isChannelNameEmpty, isChannelNameFull, allNumberChannelName)

import Pusher.ChannelNameUtil exposing(pickoutNumber)
import Time
import Pusher.Dto exposing (RemoteContentUpldateEvent)

-- MODEL


type alias Model =
    { cpsyc_url : String
    , channel_name : String
    , content : String
    , error_msg : String
    , time : Time.Posix
    , last_saved_value: String
    , socket_id: String
    , channel_name_done: Bool
    , last_content_update_event: Maybe RemoteContentUpldateEvent
    }

isChannelNameEmpty: Model -> Bool
isChannelNameEmpty model =
    (model.channel_name |> pickoutNumber |> String.length) == 0

isChannelNameFull: String -> Int -> Bool
isChannelNameFull channel_name len =
    (channel_name |> pickoutNumber |> String.length) == len

allNumberChannelName: String -> String
allNumberChannelName channel_name =
    channel_name |> pickoutNumber