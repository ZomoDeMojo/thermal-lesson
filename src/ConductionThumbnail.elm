module ConductionThumbnail exposing (..)

import Browser
import Html exposing (Html, Attribute, div)
import Browser.Events as E
import Svg
import Svg.Attributes as S
import Helpers exposing(..)
import Vector10 exposing(..)
import Vector9 exposing(Vector9)
import Vector8 exposing(Vector8)

tmin = 0
tmax = 100
d_x = 0.005
alpha = aluminum.k / aluminum.rho / aluminum.c_s / (d_x ^ 2)

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
  , time : Float
  }

init : Model
init = 
  { t = Vector10.from10 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 50.0
  , time = 0.0
  }

 -- Update
 
type Msg
  = TimeDelta Float

update : Msg -> Model -> Model
update msg model =
  case msg of 
    (TimeDelta delta) ->
      { model
        | t = updateTemps model.t (alpha * (min 1 (delta / 1000))) model.time
        , time = model.time + (min 1 (delta / 1000))
      }

updateTemps : Vector10 Float -> Float -> Float -> Vector10 Float
updateTemps t alphadelta time =
  Vector9.push (Vector10.get Index9 t) (Vector8.cons (t0 time) (internalT t alphadelta))

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

t0 : Float -> Float
t0 time =
  toFloat (tmax * (modBy 2 (round (time / 5))))
  
-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
  E.onAnimationFrameDelta TimeDelta
    
-- View

view : Model -> Html Msg
view model =
  div []
    [ Svg.svg
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
    ]