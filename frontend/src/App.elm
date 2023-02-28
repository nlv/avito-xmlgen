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

type Msg = 
    
    SkipInput String
  | SrcInput String
  | ShortUrlInput Bool

  | DisabledSendRequestButtonPressed
  | SendRequestButtonPressed

  | RequestSent (Result Http.Error ())
  
type alias Model = {
    skip : Int
  , src  : String
  , shortUrl : Bool
  }

initModel : Model
initModel = {
    skip = 0
  , src  = ""
  , shortUrl = False
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

    SkipInput skip -> ({model | skip = String.toInt (if skip == "" then "0" else skip) |> Maybe.withDefault model.skip}, Cmd.none)

    ShortUrlInput shortUrl -> ({model | shortUrl = shortUrl}, Cmd.none)

    SrcInput src -> ({model | src = src}, Cmd.none)

    SendRequestButtonPressed -> 
      let shortUrlParam = if model.shortUrl then "&shortUrl" else ""
      in
      if model.src /= "" then (model, Browser.Navigation.load <| "api/" ++ Url.toQuery [Url.string "src" model.src, Url.int "skip" model.skip] ++ shortUrlParam) else (model, Cmd.none)

    DisabledSendRequestButtonPressed -> (model, Cmd.none)

    RequestSent _ -> (model, Cmd.none)

main : Program () Model Msg
main =  Browser.element { init = \_ -> (initModel, Cmd.none), update = update, view = view, subscriptions = \_ -> Sub.none}

view : Model -> Html.Html Msg
view model = 
    E.layout [] <| E.column [E.padding 10] <| [skipInput model.skip, shortUrlInput model.shortUrl, srcInput model.src, sendRequestButton (model.src /= "")]

lableStyle : List (E.Attribute Msg) 
lableStyle = [E.width <| E.minimum 200 E.fill]

rowStyle : List (E.Attribute Msg) 
rowStyle = [E.padding 10]

skipInput : Int -> E.Element Msg
skipInput skip = 
  E.row 
    rowStyle [
      EI.text 
        []
        {
          onChange = SkipInput, 
          text = String.fromInt skip, 
          placeholder = Nothing, 
          label = EI.labelLeft [] <| E.el lableStyle <| E.text "Строк до заголовка:"
        }
    ]

shortUrlInput : Bool -> E.Element Msg
shortUrlInput shortUrl = 
  E.row 
    rowStyle [
      EI.checkbox 
        []
        {
          onChange = ShortUrlInput, 
          icon = EI.defaultCheckbox,
          checked = shortUrl, 
          label = EI.labelLeft [] <| E.el lableStyle <| E.text "Сокращать ссылки:"
        }
    ]

srcInput : String -> E.Element Msg
srcInput src = 
  E.row 
    rowStyle [
      EI.text 
        [E.width <| E.minimum 1100 E.fill]
        {
          onChange = SrcInput, 
          text = src, 
          placeholder = Nothing, 
          label = EI.labelLeft [] <| E.el lableStyle <| E.text "Адрес таблицы:"
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
          { onPress = Just SendRequestButtonPressed, label = E.text "Скачать" } 
      else
        EI.button 
          ((Background.color <| E.rgb 220 220 220) :: (Region.description "Укажите адрес таблицы") :: buttonStyle)
          { onPress = Just DisabledSendRequestButtonPressed, label = E.text "Скачать" } 
    ]
