module Main2 exposing (..)


import Browser
import Html exposing (Html, button, div, h1, input, li, ol, p, text, ul)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (Decoder)
import Random exposing (Generator)
import Task

main = Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }

type Etat = Failure Http.Error | Loading | Success

type alias Model =
  {
  httpState : Etat
  , jsonState : Etat
  , listeM : List String
  , mot : String
  , number : Int
  , data : List Datas
  , show : Bool
  , found : Bool
  }

type Msg = GotListM (Result Http.Error String)
 | Mot String
 | RandInt Int
 | JSonDef (Result Http.Error (List Datas))
 | Show
 | Change String

type alias Datas =
  {
  word : String
  , meanings : List Meaning
  }

type alias Meaning =
  {
  partOfSpeech : String
  , definitions : List Definition
  }

type alias Definition =
  {
   definition : String
  }

init : () -> ( Model, Cmd Msg )
init _ =
    (Model Loading Loading [] "" 0 [] False False, getList )



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotListM (Err err) ->
            ({model | httpState = Failure err}, Cmd.none )
        GotListM (Ok string) ->
            ({model | listeM = String.words string, httpState = Success}, getRandInt (List.length (String.words string)))
        Mot string ->
            ({model |mot = string}, getJson string)
        RandInt int ->
            case getElemList model.listeM int of
                Nothing -> (model, Cmd.none)
                Just mot -> ({model | number = int}, getMot mot)
        JSonDef (Err err) ->
            ({model | jsonState = Failure err}, Cmd.none)
        JSonDef (Ok datas) ->
            ({model | jsonState = Success, data = datas}, Cmd.none)
        Show ->
            ({model| show = not model.show}, Cmd.none)
        Change str->
            if str == model.mot then
              ({model | found = True},Cmd.none)
            else
              (model,Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

view : Model -> Html Msg
view model =
    div[style "text-align" "left", style "margin-left" "5em"][
      if not model.show then
        div[][
          h1[][text "Guess It !"],
          div[] (textDatas model.data)
        ]
      else
        div[][
          h1[][text model.mot],
          div[] (textDatas model.data)
        ],
      p[][text "Try your luck !"],
      input[onInput Change][],
      if model.found then
        div[][p[][text ("You found the word ! It was " ++ model.mot ++ " !")]]
      else
        div[][text "Nope, try again !"],
      button[onClick Show][text "Show me the Answer !"]
    ]

textDatas: List Datas -> List (Html Msg)
textDatas lData=
    case lData of
        []->[]
        (x::xs)-> [ul[]([text "Définition : "] ++ [li[](textMeanings x.meanings)])] ++ (textDatas xs)
textMeanings: List Meaning -> List (Html Msg)
textMeanings lMeanings =
    case lMeanings of
        []->[]
        (x::xs)-> [li[]([text x.partOfSpeech] ++ [ol[] (textDefinitions x.definitions)])] ++ textMeanings xs

textDefinitions: List Definition -> List (Html Msg)
textDefinitions lDefinitions =
    case lDefinitions of
        []->[]
        (x::xs)-> [li[]([text x.definition])] ++ textDefinitions xs
getList : Cmd Msg
getList =
    Http.get { url = "http://localhost:8000/src/thousand_words_things_explainer.txt", expect = Http.expectString GotListM }

getElemList : List a -> Int -> Maybe a
getElemList list index =
    if index < 0 || index >= List.length list then
        Nothing
    else
        List.head (List.drop index list)
--Returns a Cmd Message that will send a message to the update function
getMot: String -> Cmd Msg
getMot mot = Task.perform Mot (Task.succeed mot)

getRandInt : Int-> Cmd Msg
getRandInt int =
    Random.generate RandInt (Random.int 0 int)

getJson : String -> Cmd Msg
getJson mot =
    Http.get { url = ("https://api.dictionaryapi.dev/api/v2/entries/en/" ++ mot), expect = Http.expectJson JSonDef decodeDatasList }

decodeDatasList : Decoder (List Datas)
decodeDatasList =
    Json.Decode.list decodeDatas

decodeDatas : Decoder Datas
decodeDatas =
    Json.Decode.map2 Datas
        (Json.Decode.field "word" Json.Decode.string)
        (Json.Decode.field "meanings" (Json.Decode.list decodeMeaning))

decodeMeaning : Decoder Meaning
decodeMeaning =
    Json.Decode.map2 Meaning
        (Json.Decode.field "partOfSpeech" Json.Decode.string)
        (Json.Decode.field "definitions" (Json.Decode.list decodeDefinition))

decodeDefinition : Decoder Definition
decodeDefinition =
    Json.Decode.map Definition
        (Json.Decode.field "definition" Json.Decode.string)
