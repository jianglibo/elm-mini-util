module Pusher.Dto exposing
    ( ChannelData
    , RemoteContentUpldateEvent
    , channelDataDecoder
    , remoteContentUpdateEventDecoder
    , InitValue
    , initValueDecoder
    )

import Json.Decode exposing (Decoder, field, map2, map3, string)



-- import Json.Encode exposing (Value)

type alias InitValue =
    { cpsyc_url : String
    , channel_name : String
    }


initValueDecoder : Decoder InitValue
initValueDecoder =
    map2 InitValue
        (field "cpsyc_url" string)
        (field "channel_name" string)


type alias ChannelData =
    { id : String
    , content : String
    , created_at : String
    }


channelDataDecoder : Decoder ChannelData
channelDataDecoder =
    map3 ChannelData
        (field "id" string)
        (field "content" string)
        (field "created_at" string)



-- {"event":"pusher_internal:subscription_succeeded","data":"{}","channel":"private-224813686"}
-- subscribeSuceededValue: String
-- subscribeSuceededValue =
--     "pusher_internal:subscription_succeeded"


type alias RemoteContentUpldateEvent =
    { socket_id : String
    , content : String
    }


remoteContentUpdateEventDecoder : Decoder RemoteContentUpldateEvent
remoteContentUpdateEventDecoder =
    map2 RemoteContentUpldateEvent
        (field "socket_id" string)
        (field "content" string)
