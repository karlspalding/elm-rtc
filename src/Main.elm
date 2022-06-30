port module Main exposing (..)

import Browser
import Html exposing (Html, div, li, ul, text)
import Html.Attributes exposing (disabled)
import Json.Decode
import Json.Encode

main =
  Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

type RemoteWebRTCSignal
  = RemoteWebRTCSignalOffer Json.Decode.Value
  | RemoteWebRTCSignalAnswer Json.Decode.Value
  | RemoteWebRTCSignalCandidate Json.Decode.Value

type WebRTCPayload
  = WebRTCPayloadData String

-- Websockets

port sendWebSocketMessage : String -> Cmd msg
port handleWebSocketMessage : (String -> msg) -> Sub msg

parseWebSocketMessage : String -> Result Json.Decode.Error RemoteWebRTCSignal
parseWebSocketMessage message =
  let
    candidate : Json.Decode.Decoder String
    candidate =
      Json.Decode.field "candidate" Json.Decode.string

    signal : Json.Decode.Decoder String
    signal =
      Json.Decode.field "type" Json.Decode.string

    isCandidate : Json.Decode.Value -> Bool
    isCandidate value =
      Json.Decode.decodeValue candidate value
      |> Result.map (\_ -> True)
      |> Result.withDefault False

    parseType : Json.Decode.Value -> Result Json.Decode.Error String
    parseType value =
      if isCandidate value then
        Result.Ok "candidate"
      else
        Json.Decode.decodeValue signal value

    pickSignal : Json.Decode.Value -> String -> Result Json.Decode.Error RemoteWebRTCSignal
    pickSignal value kind =
      case kind of
        "answer" ->
          Result.Ok <| RemoteWebRTCSignalAnswer value
        "candidate" ->
          Result.Ok <| RemoteWebRTCSignalCandidate value
        "offer" ->
          Result.Ok <| RemoteWebRTCSignalOffer value
        _ ->
          Json.Decode.Failure ("Unknown message type: " ++ kind) value
          |> Result.Err

    decodeValue : Json.Decode.Value -> Result Json.Decode.Error RemoteWebRTCSignal
    decodeValue value =
      parseType value
      |> Result.andThen (pickSignal value)
  in
    message
    |> Json.Decode.decodeString Json.Decode.value
    |> Result.andThen decodeValue

handleRemoteWebRTCSignal : RemoteWebRTCSignal -> Cmd msg
handleRemoteWebRTCSignal signal =
  case signal of
    RemoteWebRTCSignalOffer offer ->
      handleRemoteWebRTCSignalOffer offer
    RemoteWebRTCSignalAnswer answer ->
      handleRemoteWebRTCSignalAnswer answer
    RemoteWebRTCSignalCandidate candidate ->
      handleRemoteWebRTCSignalCandidate candidate

actOnWebSocketMessage : String -> Cmd msg
actOnWebSocketMessage message =
  parseWebSocketMessage message
  |> Result.map handleRemoteWebRTCSignal
  |> Result.mapError (\e -> Debug.log (Json.Decode.errorToString e) e)
  |> Result.withDefault Cmd.none

-- Offers

port handleLocalWebRTCSignalOffer : (Json.Decode.Value -> msg) -> Sub msg
port handleRemoteWebRTCSignalOffer : Json.Decode.Value -> Cmd msg

--

-- Answers

port handleLocalWebRTCSignalAnswer : (Json.Decode.Value -> msg) -> Sub msg
port handleRemoteWebRTCSignalAnswer : Json.Decode.Value -> Cmd msg

--

-- Candidates

port handleLocalWebRTCSignalCandidate : (Json.Decode.Value -> msg) -> Sub msg
port handleRemoteWebRTCSignalCandidate : Json.Decode.Value -> Cmd msg

--

type alias Model = List String

init : () -> (Model, Cmd Msg)
init _ =
  ( [], Cmd.none )

type Msg
  = WebSocketMessage String
  | LocalWebRTCSignalAnswer Json.Decode.Value
  | LocalWebRTCSignalOffer Json.Decode.Value
  | LocalWebRTCSignalCandidate Json.Decode.Value


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    WebSocketMessage message ->
      (model, actOnWebSocketMessage message)
    LocalWebRTCSignalAnswer answer ->
      showAndSend model answer
    LocalWebRTCSignalOffer offer ->
      showAndSend model offer
    LocalWebRTCSignalCandidate candidate ->
      showAndSend model candidate

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch
    [ handleWebSocketMessage WebSocketMessage
    , handleLocalWebRTCSignalAnswer LocalWebRTCSignalAnswer
    , handleLocalWebRTCSignalOffer LocalWebRTCSignalOffer
    , handleLocalWebRTCSignalCandidate LocalWebRTCSignalCandidate
    ]

view : Model -> Html Msg
view model =
  div []
      [ text "WebRTC"
      , ul []
        (List.map viewLine model)
      ]

viewLine : String -> Html Msg
viewLine line =
  li []
     [ text line ]

showAndSend : Model -> Json.Decode.Value -> (Model, Cmd msg)
showAndSend model value =
  let
    data = Json.Encode.encode 0 value
  in
    (data :: model, sendWebSocketMessage data)
