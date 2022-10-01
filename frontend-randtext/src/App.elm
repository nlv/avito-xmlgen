module App exposing (main)

import Platform.Cmd as Cmd
import Html
import Element as E
import Element.Input as EI
import Element.Border as EB
import Element.Region as Region
import Element.Background as Background
import Http
import Browser
import Browser.Navigation

import Url.Builder as Url
import Html.Attributes exposing (rows)

type Msg = 
    
    CountInput String
  | TextInput String

  | DisabledSendRequestButtonPressed
  | SendRequestButtonPressed

  | RequestSent (Result Http.Error ())
  
type alias Model = {
    count : Int
  , text  : String
  }

initModel : Model
initModel = {
    count = 0
  , text  = ""
  }

-- sendRequest : Int -> String -> Cmd Msg
-- sendRequest skip src = 
--     Http.get { 
--         url = "api/" ++ Url.toQuery [Url.string "src" src, Url.int "skip" skip]
--       , expect = Http.expectWhatever RequestSent
--       }

update : Msg -> Model -> (Model, Cmd Msg)
update action model =

  case action of

    CountInput count -> ({model | count = String.toInt (if count == "" then "0" else count) |> Maybe.withDefault model.count}, Cmd.none)

    TextInput text -> ({model | text = text}, Cmd.none)

    SendRequestButtonPressed -> 
      -- if model.src /= "" then (model, sendRequest model.skip model.src) else (model, Cmd.none)
      if model.text /= "" then (model, Browser.Navigation.load <| "api/" ++ Url.toQuery [Url.string "text" model.text, Url.int "count" model.count]) else (model, Cmd.none)

    DisabledSendRequestButtonPressed -> (model, Cmd.none)

    RequestSent _ -> (model, Cmd.none)

main : Program () Model Msg
main =  Browser.element { init = \_ -> (initModel, Cmd.none), update = update, view = view, subscriptions = \_ -> Sub.none}

view : Model -> Html.Html Msg
view model = 
    E.layout [] <| E.column [E.padding 10] <| [countInput model.count, textInput model.text, sendRequestButton (model.text /= "")]

lableStyle : List (E.Attribute Msg) 
lableStyle = [E.width <| E.minimum 200 E.fill]

rowStyle : List (E.Attribute Msg) 
rowStyle = [E.padding 10]

countInput : Int -> E.Element Msg
countInput count = 
  E.row 
    rowStyle [
      EI.text 
        []
        {
          onChange = CountInput, 
          text = String.fromInt count, 
          placeholder = Nothing, 
          label = EI.labelLeft [] <| E.el lableStyle <| E.text "Количество вариантов:"
        }
    ]

textInput : String -> E.Element Msg
textInput text = 
  E.row 
    rowStyle [
      EI.multiline
        [E.width <| E.minimum 1100 E.fill]
        -- [E.width <| E.minimum 1100 E.fill, E.htmlAttribute <| rows 10]
        -- [E.htmlAttribute <| rows 10]
        {
          onChange = TextInput, 
          text = text, 
          placeholder = Nothing, 
          label = EI.labelLeft [] <| E.el lableStyle <| E.text "Текст для рандомизации:",
          spellcheck = False
        }
    ]

buttonStyle : List (E.Attribute Msg)
buttonStyle = [EB.solid, EB.width 1, E.padding 10]

sendRequestButton : Bool -> E.Element Msg
sendRequestButton ready =
  E.row 
    rowStyle [
      if ready then 
        EI.button 
          ((Background.color <| E.rgb 0 255 255) :: buttonStyle)
          { onPress = Just SendRequestButtonPressed, label = E.text "Рандомизировать" } 
      else
        EI.button 
          ((Background.color <| E.rgb 220 220 220) :: (Region.description "Укажите текст") :: buttonStyle)
          { onPress = Just DisabledSendRequestButtonPressed, label = E.text "Рандомизировать" } 
    ]
