module Thermals1 exposing (..)

import Browser
import Html exposing (Html, Attribute, div, text, input)
import Browser.Events as E
import Svg
import Svg.Attributes as S
import Helpers exposing(slider, drawArrow, thermalElement)

tmax = 100
tmin = 0

-- Main

main : Program () Model Msg
main =
  Browser.element
    { init = \() -> (init, Cmd.none)
    , view = view
    , update = \msg model -> ( update msg model, Cmd.none )
    , subscriptions = subscriptions
    }
        
-- Model

type alias Model =
  { t1 : Int
  , t2 : Int
  , time : Float
  }

init : Model
init = 
  { t1 = 10
  , t2 = 15
  , time = 0
  }
 -- Update
 
type Msg
  = UpdateT1 String
  | UpdateT2 String
  | TimeDelta Float

update : Msg -> Model -> Model
update msg model =
  case msg of 
    (UpdateT1 v) ->
      case String.toInt v of
        Just i ->
          { model
            | t1 = i
          }
        Nothing -> model
    (UpdateT2 v) ->
      case String.toInt v of
        Just i ->
          { model
            | t2 = i
          }
        Nothing -> model
    (TimeDelta delta) ->
      { model
        | time = model.time + delta / 1000
      }
    
-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
  E.onAnimationFrameDelta TimeDelta
    
-- View

view : Model -> Html Msg
view model =
  div []
    [ slider tmin tmax model.t1 ("  T1 = " ++ String.fromInt model.t1) UpdateT1
    , slider tmin tmax model.t2 ("  T2 = " ++ String.fromInt model.t2) UpdateT2
    , div [] [ text (String.fromFloat ( toFloat ( round (model.time * 100) ) / 100) ) ]
    , Svg.svg
      [ S.width "240", S.height "120", S.viewBox "0 0 240 120" ]
      [ thermalElement 60 10 100 (toFloat model.t2 / tmax )
      , drawArrow 60 40 10 0]
    ]