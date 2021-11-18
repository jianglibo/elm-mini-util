module Pusher.Dto exposing (channelDataDecoder, cpsycUrlDecoder, ChannelData)

import Json.Decode exposing (Decoder, field, string, map3)

cpsycUrlDecoder : Decoder String
cpsycUrlDecoder =
    field "cpsyc_url" string

type alias ChannelData =
    { id : String
    , content : String
    , created_at: String
    }


channelDataDecoder : Decoder ChannelData
channelDataDecoder =
    map3 ChannelData
        (field "id" string)
        (field "content" string)
        (field "created_at" string)


