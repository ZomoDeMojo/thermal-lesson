module ThermalElement exposing (..)

import Browser
import Html exposing (Html, Attribute, div, text, input, button)
import Html.Events exposing (onClick)
import Browser.Events as E
import Svg
import Svg.Attributes as S
import Helpers exposing(slider, drawArrow, thermalElement)

tmax = 100
tmin = 0

-- Main

main : Program () Model Msg
main =
  Browser.sandbox { init = init, update = update, view = view }
        
-- Model

type alias Model =
  { t : Int
  }

init : Model
init = 
  { t = 50
  }
 -- Update
 
type Msg
  = UpdateT String

update : Msg -> Model -> Model
update msg model =
  case msg of 
    (UpdateT v) ->
      case String.toInt v of
        Just i ->
          { model
            | t = i
          }
        Nothing -> model
    
    
-- View

view : Model -> Html Msg
view model =
  div []
    [ slider tmin tmax model.t ("  T = " ++ String.fromInt model.t ++ " C") UpdateT
    , Svg.svg
      [ S.width "120", S.height "110", S.viewBox "0 0 120 100" ]
      [ thermalElement 20 0 100 (toFloat model.t / tmax ) ]
    ]