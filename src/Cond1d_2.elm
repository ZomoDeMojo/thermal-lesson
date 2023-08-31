module Cond1d_2 exposing (..)

import Browser
import Html exposing (Html, Attribute, div, text, input, button)
import Html.Events exposing (onClick)
import Browser.Events as E
import Svg
import Svg.Attributes as S
import Helpers exposing(..)
import Vector10 exposing(..)
import Vector9 exposing(Vector9)
import Vector8 exposing(Vector8)
import Chart as C
import Chart.Attributes as CA

tmin = 0
tmax = 100
d_x = 0.005
qmax = 6000
alpha = aluminum.k / aluminum.rho / aluminum.c_s / (d_x ^ 2)
c_tot = aluminum.rho * aluminum.c_s * (d_x ^ 3)
material = aluminum

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
  { t : Vector10 Float
  , q_in : Int
  }

init : Model
init = 
  { t = Vector10.from10 50.0 50.0 50.0 50.0 50.0 50.0 50.0 50.0 50.0 50.0
  , q_in = 0
  }

 -- Update
 
type Msg
  = UpdateQin String
  | TimeDelta Float

update : Msg -> Model -> Model
update msg model =
  case msg of 
    (UpdateQin v) ->
      case String.toInt v of
        Just i ->
          { model
            | q_in = i
          }
        Nothing -> model
    (TimeDelta delta) ->
      { model
        | t = updateTemps model.t alpha ((min 1 (delta / 1000))) model.q_in
      }

updateTemps : Vector10 Float -> Float -> Float -> Int -> Vector10 Float
updateTemps t alph delta q_in =
  Vector9.push (Vector10.get Index9 t) (Vector8.cons (t1 t alph delta q_in) (internalT t alph delta))

t1 t alph delta q_in =
  (Vector10.get Index0 t) + alph * delta * ((Vector10.get Index1 t) - (Vector10.get Index0 t)) + delta * (toFloat q_in / 1000) / c_tot

left : Vector10 Float -> Vector8 Float
left t =
  Tuple.first (Vector9.pop (Tuple.first (Vector10.pop t)))

right : Vector10 Float -> Vector8 Float
right t =
  Tuple.second (Vector9.uncons (Tuple.second (Vector10.uncons t)))

center : Vector10 Float -> Vector8 Float
center t =
  Tuple.first (Vector9.pop (Tuple.second (Vector10.uncons t)))

internalT : Vector10 Float -> Float -> Float -> Vector8 Float
internalT t alph delta =
  Vector8.map3 (\a b c -> (a - 2 * b + c) * alph * delta + b) (left t) (center t) (right t)

calculateQ : Model -> Vector9 Float
calculateQ model =
  Vector9.map2 (\a b -> ((a - b) * material.k * d_x))
               (Tuple.first (Vector10.pop model.t))
               (Tuple.second (Vector10.uncons model.t))

  
-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
  E.onAnimationFrameDelta TimeDelta
    
-- View

view : Model -> Html Msg
view model =
  div []
    [ div [] [ text "------------------------------------------------"]
    , div [] [ text ("Material : " ++ material.name)]
    , div [] [ text ("Specific Heat Capacity : " ++ (String.fromFloat material.c_s) ++ " J/Kg-C") ]
    , div [] [ text ("Density : " ++ (String.fromFloat material.rho) ++ " Kg/m^3") ]
    , div [] [ text ("Element Volume : " ++ (String.fromFloat ((d_x * 1000) ^ 3)) ++ " mm^3") ]
    , div [] [ text ("Total Heat Capacity : " ++ (String.fromFloat ( toFloat ( round (c_tot * 1000) ) / 1000)) ++ " J/C") ]
    , div [] [ text ("Thermal Conductivity : " ++ (String.fromFloat material.k) ++ " W/m-C") ]
    , div [] [ text "------------------------------------------------"]
    , slider -qmax qmax model.q_in (" Q' in = " ++ String.fromInt model.q_in ++ " mW") UpdateQin
    , div [] [ text "T2 = 50 C"]
    , Svg.svg
      [ S.width "900", S.height "130", S.viewBox "0 0 900 130" ]
      [ thermalElement 50 10 80 ((Vector10.get Index0 model.t) / tmax )
      , thermalElement 130 10 80 ((Vector10.get Index1 model.t) / tmax )
      , thermalElement 210 10 80 ((Vector10.get Index2 model.t) / tmax )
      , thermalElement 290 10 80 ((Vector10.get Index3 model.t) / tmax )
      , thermalElement 370 10 80 ((Vector10.get Index4 model.t) / tmax )
      , thermalElement 450 10 80 ((Vector10.get Index5 model.t) / tmax )
      , thermalElement 530 10 80 ((Vector10.get Index6 model.t) / tmax )
      , thermalElement 610 10 80 ((Vector10.get Index7 model.t) / tmax )
      , thermalElement 690 10 80 ((Vector10.get Index8 model.t) / tmax )
      , thermalElement 770 10 80 ((Vector10.get Index9 model.t) / tmax )
      , drawArrow 50 50 (model.q_in * 12 // qmax) 0
      ]
    , C.chart
      [ CA.height 250
      , CA.width 900
      , CA.margin { top = 20, bottom = 20, left = 90, right = 90}
      , CA.domain
        [ CA.lowest (tmin - 12) CA.exactly
        , CA.highest (tmax + 12) CA.exactly
        ]
      , CA.range
        [ CA.lowest 1 CA.exactly
        , CA.highest 10 CA.exactly
        ]
      ]
      [ C.yLabels [ CA.withGrid ]
      , C.series .x
          [ C.interpolated .y [ ] []]
          (Vector10.toList (Vector10.indexedMap (\i a -> {x = (toFloat ((indexToInt i) + 1)) , y = a}) model.t))
      , C.labelAt .min CA.middle [ CA.moveLeft 55, CA.rotate 90 ]
        [ Svg.text "Temperature (C)" ]
      ]
    , C.chart
      [ CA.height 250
      , CA.width 900
      , CA.margin { top = 20, bottom = 20, left = 90, right = 90}
      , CA.domain
        [ CA.lowest -7 CA.exactly
        , CA.highest 7 CA.exactly
        ]
      , CA.range
        [ CA.lowest 0.5 CA.exactly
        , CA.highest 9.5 CA.exactly
        ]
      ]
      [ C.yLabels [ CA.withGrid ]
      , C.series .x
          [ C.interpolated .y [ ] []]
          (Vector9.toList (Vector9.indexedMap (\i a -> {x = (toFloat ((Vector9.indexToInt i) + 1)) , y = a}) (calculateQ model)))
      , C.labelAt .min CA.middle [ CA.moveLeft 55, CA.rotate 90 ]
        [ Svg.text "Energy Flow Rate (W)" ]
      ]
    ]