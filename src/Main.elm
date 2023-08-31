module Main exposing (..)

import Browser
import Html exposing (Html, Attribute, div, text, input)
import Html.Attributes as H exposing (..)
import Html.Events exposing (on, onInput)
import Browser.Events as E
import Svg
import Svg.Attributes as S
import Color exposing (Color)

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
      
--slider : Int -> Int -> Int -> Msg -> Html msg
slider mn mx v msg =
  div []
    [ input
      [ type_ "range"
      , H.min <| String.fromInt mn
      , H.max <| String.fromInt mx
      , value  <| String.fromInt v
      , onInput msg
      ] []
    , text <|String.fromInt v
    ]
    
-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
  E.onAnimationFrameDelta TimeDelta
    
-- View

cMap : Float -> String
cMap v =
  if v < 0.25 then
    Color.toCssString (Color.rgb 0 (2 * v) (1 - 2 * v))
  else if v < 0.5 then
    Color.toCssString (Color.rgb 0 (0.5 + (v - 0.25) * 2) (0.5 - (v - 0.25) * 2))
  else if v < 0.75 then
    Color.toCssString (Color.rgb (2 * (v - 0.5)) (1 - 2 * (v - 0.5) ) 0)
  else
    Color.toCssString (Color.rgb (0.5 + (v - 0.75) * 2) (0.5 - (v - 0.75) * 2) 0)
  
    
drawArrow : Int -> Int -> Int -> Int -> Svg.Svg msg
drawArrow x y size rotation =
  Svg.polygon
    [ S.points ((String.fromInt (x + 3 * size)) ++ "," ++ (String.fromInt (y)) ++ " "
                ++ (String.fromInt (x + size)) ++ "," ++ (String.fromInt (y + 2 * size)) ++ " "
                ++ (String.fromInt (x + size)) ++ "," ++ (String.fromInt (y + size)) ++ " "
                ++ (String.fromInt (x - 3 * size)) ++ "," ++ (String.fromInt (y + size)) ++ " "
                ++ (String.fromInt (x - 3 * size)) ++ "," ++ (String.fromInt (y - size)) ++ " "
                ++ (String.fromInt (x + size)) ++ "," ++ (String.fromInt (y - size)) ++ " "
                ++ (String.fromInt (x + size)) ++ "," ++ (String.fromInt (y - 2 * size))
                )
    , S.fill "yellow"
    , S.stroke "black"
    , S.transform ( "rotate("
                ++ (String.fromInt rotation)
                ++ " "
                ++ (String.fromInt x)
                ++ ","
                ++ (String.fromInt y)
                ++ ")"
                )
    ]
    []


view : Model -> Html Msg
view model =
  div []
    [ slider tmin tmax model.t1 UpdateT1
    , slider tmin tmax model.t2 UpdateT2
    , div [] [ text (String.fromFloat ( toFloat ( round (model.time * 100) ) / 100) ) ]
    , Svg.svg
      [ S.width "240", S.height "120", S.viewBox "0 0 240 120" ]
      [ Svg.rect [ S.x "60", S.y "10", S.width "100", S.height "100", S.fill (cMap (toFloat model.t2 / tmax ) ) ] []
      , drawArrow 60 40 10 0]
    ]