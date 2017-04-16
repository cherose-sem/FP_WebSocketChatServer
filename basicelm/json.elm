import Http exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Json.Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (decode, required, requiredAt)

--
-- Exercise 2
-- In this exercise you receive a JSON object representing a random person
-- from randomuser.me. Read the documentation to get the exact format:
-- https://randomuser.me/documentation#howto
-- Your job is to decode the name, email, username and password from the
-- FIRST user in the JSON result list you get back
--

type Msg
  = FetchPerson
  | NewPerson (Result Http.Error Person)

type alias Person =
  { name: String
  , email: String
  , username : String
  , password : String
  }

type alias Model =
  { person : Result String Person }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    FetchPerson -> (Model (Err ""), fetchRandomPerson)
    NewPerson (Ok person) -> (Model (Ok person), Cmd.none)
    NewPerson (Err error) -> (Model (Err (toString error)), Cmd.none)

personDecoder : Decoder Person
personDecoder =
  let
    toPerson : Decoder Person
    toPerson =
      decode Person
        |> requiredAt ["name", "first"] string
        |> required "email" string
        |> requiredAt ["login", "username"] string
        |> requiredAt ["login", "password"] string
  in
    Decode.at ["results"] (Decode.index 0 toPerson)

fetchRandomPerson : Cmd Msg
fetchRandomPerson =
  Http.send NewPerson
    ( Http.request
      { method = "GET"
      , headers = []
      , url = "https://randomuser.me/api/"
      , body = Http.emptyBody
      , expect = expectJson personDecoder
      , timeout = Nothing
      , withCredentials = False
      }
    )

view : Model -> Html Msg
view model =
  div []
    [ h2 [] [ text "Hello World"]
    , button [ onClick FetchPerson ] [text "Fetch New Person"]
    , hr [] []
    , div [] (displayModel model)
    ]

displayModel : Model -> List (Html Msg)
displayModel model =
  case model.person of
    Ok person ->
      [ h3 [] [text person.username ] ]
      -- Show the rest of the person data
    Err error -> [ h3 [] [text error ]]

main : Program Never Model Msg
main = Html.program
  { init = ((Model (Err "")), Cmd.none)
  , view = view
  , update = update
  , subscriptions = \x -> Sub.none
  }
