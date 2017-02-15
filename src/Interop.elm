port module Interop exposing (..)

--ports to send and receive increments
port sendIncr : () -> Cmd msg
port recvIncr : (() -> msg) -> Sub msg

--ports to send and receive decrements
port sendDecr : () -> Cmd msg 
port recvDecr : (() -> msg) -> Sub msg

port sendRadius : Int -> Cmd msg
port recvRadius : (Int -> msg) -> Sub msg

port sendName : String -> Cmd msg

port recvName : (String -> msg) -> Sub msg

port noOp : (() -> msg) -> Sub msg