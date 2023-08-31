module Cond1d_1 exposing (..)

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
  , d_x : Float
  , alpha : Float
  , c_tot : Float
  , material : Material
  }

init : Model
init = 
  { t = Vector10.from10 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0
  , d_x = d_x
  , alpha = aluminum.k / aluminum.rho / aluminum.c_s / (d_x ^ 2)
  , c_tot = aluminum.rho * aluminum.c_s * (d_x ^ 3)
  , material = aluminum
  }

 -- Update
 
type Msg
  = UpdateT1 String
  | UpdateT2 String
  | TimeDelta Float
  | UpdateMaterial Material

update : Msg -> Model -> Model
update msg model =
  case msg of 
    (UpdateT1 v) ->
      case String.toInt v of
        Just i ->
          { model
            | t = Vector10.set Index0 (toFloat i) model.t
          }
        Nothing -> model
    (UpdateT2 v) ->
      case String.toInt v of
        Just i ->
          { model
            | t = Vector10.set Index9 (toFloat i) model.t
          }
        Nothing -> model
    (UpdateMaterial mat) ->
      { model
        | material = mat
        , alpha = (mat.k / (mat.rho * mat.c_s * (model.d_x ^ 2)))
        , c_tot = mat.rho * mat.c_s * (model.d_x ^ 3)
      }
    (TimeDelta delta) ->
      { model
        | t = updateTemps model.t (model.alpha * (min 1 (delta / 1000)))
      }

updateTemps : Vector10 Float -> Float -> Vector10 Float
updateTemps t alphadelta =
  Vector9.push (Vector10.get Index9 t) (Vector8.cons (Vector10.get Index0 t) (internalT t alphadelta))

left : Vector10 Float -> Vector8 Float
left t =
  Tuple.first (Vector9.pop (Tuple.first (Vector10.pop t)))

right : Vector10 Float -> Vector8 Float
right t =
  Tuple.second (Vector9.uncons (Tuple.second (Vector10.uncons t)))

center : Vector10 Float -> Vector8 Float
center t =
  Tuple.first (Vector9.pop (Tuple.second (Vector10.uncons t)))

internalT : Vector10 Float -> Float -> Vector8 Float
internalT t alphadelta =
  Vector8.map3 (\a b c -> (a - 2 * b + c) * alphadelta + b) (left t) (center t) (right t)

calculateQ : Model -> Vector9 Float
calculateQ model =
  Vector9.map2 (\a b -> ((a - b) * model.material.k * model.d_x))
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
    [ button [ onClick (UpdateMaterial aluminum) ] [ text "Aluminum" ]
    , button [ onClick (UpdateMaterial steel) ] [ text "Steel" ]
    , button [ onClick (UpdateMaterial glass) ] [ text "Glass" ]
    , button [ onClick (UpdateMaterial nylon) ] [ text "Nylon" ]
    , div [] [ text "------------------------------------------------"]
    , div [] [ text ("Material : " ++ model.material.name)]
    , div [] [ text ("Specific Heat Capacity : " ++ (String.fromFloat model.material.c_s) ++ " J/Kg-C") ]
    , div [] [ text ("Density : " ++ (String.fromFloat model.material.rho) ++ " Kg/m^3") ]
    , div [] [ text ("Element Volume : " ++ (String.fromFloat ((model.d_x * 1000) ^ 3)) ++ " mm^3") ]
    , div [] [ text ("Total Heat Capacity : " ++ (String.fromFloat ( toFloat ( round (model.c_tot * 1000) ) / 1000)) ++ " J/C") ]
    , div [] [ text ("Thermal Conductivity : " ++ (String.fromFloat model.material.k) ++ " W/m-C") ]
    , div [] [ text "------------------------------------------------"]
    , slider tmin tmax (round (Vector10.get Index0 model.t)) ("  T1 = " ++ String.fromInt (round (Vector10.get Index0 model.t)) ++ " C") UpdateT1
    , slider tmin tmax (round (Vector10.get Index9 model.t)) ("  T2 = " ++ String.fromInt (round (Vector10.get Index9 model.t)) ++ " C") UpdateT2
    , Svg.svg
      [ S.width "900", S.height "100", S.viewBox "0 0 900 100" ]
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
      ]
    , C.chart
      [ CA.height 250
      , CA.width 900
      , CA.margin { top = 20, bottom = 20, left = 90, right = 90}
      , CA.domain
        [ CA.lowest tmin CA.exactly
        , CA.highest tmax CA.exactly
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
        [ CA.lowest (model.material.k / -3) CA.exactly
        , CA.highest (model.material.k / 3) CA.exactly
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