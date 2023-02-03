module Main2 exposing (..)


import Browser
import Html exposing (Html, br, button, div, h1, input, li, ol, p, span, text, ul)
import Html.Attributes as Html exposing (attribute, class, tabindex, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (Decoder)
import Random exposing (Generator)
import Task

main = Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }

type Etat = Failure | Loading | Success

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
  , content : String
  , shouldCheck : Bool
  , count : Int
  }

type Msg = GotListM (Result Http.Error String)
 | Mot String
 | RandInt Int
 | JSonDef (Result Http.Error (List Datas))
 | Change String
 | Retry
 | Try

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
    (Model Loading Loading [] "" 0 [] False False "" False 0, getList )



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotListM (Err err) ->
            ({model | httpState = Failure}, getList2 )
        GotListM (Ok string) ->
            ({model | listeM = String.words string, httpState = Success}, getRandInt (List.length (String.words string)))
        Mot string ->
            ({model | mot = string}, getJson string)
        RandInt int ->
            case getElemList model.listeM int of
                Nothing -> (model, Cmd.none)
                Just mot -> ({model | number = int}, getMot mot)
        JSonDef (Err err) ->
            ({model | jsonState = Failure}, Cmd.none)
        JSonDef (Ok datas) ->
            ({model | jsonState = Success, data = datas}, Cmd.none)
        Change str->
            ({model | content = str, shouldCheck = False},Cmd.none)
        Try ->
            if model.content == model.mot && not model.found then
              ({model | found = True, count = model.count + 1 },Cmd.none)
            else if not (model.content == "") then
              ({model | shouldCheck = True},Cmd.none)
            else
              (model, Cmd.none)
        Retry ->
            if model.found then
              ({model | content="", show = False, found = False, shouldCheck = False, jsonState = Loading},getRandInt (List.length model.listeM))
            else
              ({model | content="", show = False, found = False, shouldCheck = False, jsonState = Loading, count = 0},getRandInt (List.length model.listeM))


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

view : Model -> Html Msg
view model =
    div[class "p-5"][
      if ((model.httpState == Success) && (model.jsonState == Success)) then
        div[][
          div[][
            span[class "d-flex justify-content-center"][h1[Html.class "p-2  border-bottom border-3 border-primary"][text "Guess It !"]],
            ul[class "list-group"][li[class "list-group-item list-group-item-primary"][text "Définition : "], ul[class "list-group-item list-group-item-secondary"](textDatas model.data)]
          ],
          div[class "modal", tabindex -1, Html.id "modalAnswer", Html.attribute "aria-hidden" "true", attribute "data-bs-backdrop" "static", attribute "data-bs-keyboard" "false"][
            div[class "modal-dialog modal-dialog-centered"][
              div[class "modal-content"][
                div[class "modal-header"][
                  h1[class "modal-title"][text "The answer was ",span[class "text-danger"][text model.mot]],
                  button[class "btn-close", attribute "data-bs-dismiss" "modal", onClick Retry][]
                ]
              ]
            ]
          ],
          div[class "d-flex justify-content-center"][p[class ""][text "Try your luck !"]],
          div[class "d-flex justify-content-center"][p[][text "So far, you've found ",span[class "badge rounded-pill text-bg-secondary"][text (String.fromInt model.count)], text " words consecutively"]],
          div[class "d-flex justify-content-center mb-3"][
          div[class "w-50 input-group"][
          input[class "form-control", onInput Change, value model.content][],
          button[class "btn btn-outline-primary", onClick Try][text "Guess !"]
          ]],
          if model.found then
            div[class "d-flex justify-content-center"][p[class "alert alert-success"][text ("You found the word ! It was " ++ model.mot ++ " !")]]
          else
            if model.shouldCheck then
              div[class "d-flex justify-content-center"][p[class "alert alert-danger"][text "Nope, try again !"]]
            else
              div[][],
          br[][],
          div[class "d-flex justify-content-center my-1"][
            button[class "btn btn-warning", onClick Retry][text "Try Again !"]
          ],
          div[class "d-flex justify-content-center my-1"][
            button[class "btn btn-danger", Html.attribute "data-bs-toggle" "modal", Html.attribute "data-bs-target" "#modalAnswer"][text "Show me the Answer !"]
          ]
        ]
    else
      div[][
        span[class "d-flex justify-content-center"][h1[Html.class "p-2  border-bottom border-3 border-primary"][text "Guess It !"]],
        if model.jsonState == Failure then
          div[][
            span[class "d-flex justify-content-center my-5"][span[class "text-danger"][text "Something went wrong while fetching the definitions. Maybe the API is down ?"]],
            span[class "d-flex justify-content-center"][button[class "btn btn-danger", onClick Retry][text "Retry"]]
          ]
        else if model.httpState == Failure then
          div[][
            span[class "d-flex justify-content-center my-5"][span[class "text-danger"][text "Something went wrong while fetching the words"]]
          ]
        else
          div[][
            span[class "d-flex justify-content-center my-5"][span[class "spinner-border text-primary"][]],
            span[class "d-flex justify-content-center"][span[][text "Loading..."]]
          ]
      ]
    ]

textDatas: List Datas -> List(Html Msg)
textDatas lData=
    case lData of
        []->[]
        (x::xs)-> [ul[class "list-group"](textMeanings x.meanings)] ++ textDatas xs
textMeanings: List Meaning -> List(Html Msg)
textMeanings lMeanings =
    case lMeanings of
        []-> []
        (x::xs)-> [li[class "list-group-item list-group-item-dark"][text x.partOfSpeech,ol[class "list-group list-group-flush list-group-numbered"](textDefinitions x.definitions)]] ++ textMeanings xs

textDefinitions: List Definition -> List (Html Msg)
textDefinitions lDefinitions =
    case lDefinitions of
        []->[]
        (x::xs)-> [li[class "list-group-item list-group-item-light"][text x.definition]] ++ textDefinitions xs
getList : Cmd Msg
getList =
    Http.get { url = "src/thousand_words_things_explainer.txt", expect = Http.expectString GotListM }
getList2 : Cmd Msg
getList2 =
    Http.get { url = "thousand_words_things_explainer.txt", expect = Http.expectString GotListM }

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
