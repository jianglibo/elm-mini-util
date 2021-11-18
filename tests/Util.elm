module Util exposing (..)

import Expect exposing (Expectation)
import Pusher.ChannelNameUtil as Util
import Test exposing (..)


suite : Test
suite =
    describe "The String module"
        [ test "dash length than step string" <|
            \_ ->
                let
                    palindrome =
                        Util.insertDash "dash" 3
                in
                Expect.equal palindrome "das-h"
        , test "dash length equal step string" <|
            \_ ->
                let
                    palindrome =
                        Util.insertDash "das" 3
                in
                Expect.equal palindrome "das"
        , test "dash length less than step string" <|
            \_ ->
                let
                    palindrome =
                        Util.insertDash "d" 3
                in
                Expect.equal palindrome "d"

        ]
