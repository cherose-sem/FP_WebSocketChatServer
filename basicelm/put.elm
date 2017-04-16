import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode

type Msg
  = SendPut
  | SetModel String
  | PutResponse (Result Http.Error String)

type alias Model =
  { value : String }

view : Model -> Html Msg
view model =
  div []
    [ input [ placeholder "Text to put", onInput SetModel ] []
    , button [ onClick SendPut] [ text "Put it!" ]
    , textarea [] [ text model.value ]
    ]

update : Msg -> Model -> (Model, Cmd Msg)
update message model =
  case message of
    SetModel s -> (Model s, Cmd.none)
    SendPut -> (model, sendPutToServer model.value)
    PutResponse (Ok ok) -> (Model ok, Cmd.none)
    PutResponse (Err error) -> (Model (toString error), Cmd.none)

sendPutToServer : String -> Cmd Msg
sendPutToServer message =
  let
    request = Http.request
      { method = "PUT"
      , headers = []
      , url = "http://localhost:8000"
      , body = Http.emptyBody
      , expect = Http.expectString
      , timeout = Nothing
      , withCredentials = False
      }
  in
    Http.send PutResponse request

main =
  Html.program
    { init = ({value = "nothing "}, Cmd.none)
    , view = view
    , update = update
    , subscriptions = \x -> Sub.none }
