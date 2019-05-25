module WebSocketTests exposing (sendTests, setSocketsTests)

import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import WebSocket exposing (..)


setSocketsTests : Test
setSocketsTests =
    describe "setSockets Tests"
        [ test "attempt to open new socket" <|
            \_ ->
                setSockets_ [ ws1 ] initModel
                    |> Tuple.first
                    |> .sockets
                    |> Dict.get ws1
                    |> Expect.equal (Just <| Opening 0)
        ]


sendTests : Test
sendTests =
    describe "send Tests"
        [ test "send when no socket ready adds to queue" <|
            \_ ->
                send_ ws1 "test" initModel
                    |> Tuple.first
                    |> .queues
                    |> Dict.get ws1
                    |> Expect.equal (Just [ "test" ])
        ]


ws1 =
    "ws://test1"
