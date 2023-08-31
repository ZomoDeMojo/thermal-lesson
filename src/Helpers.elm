module Helpers exposing(slider, drawArrow, thermalElement, Material, aluminum, steel, glass, nylon)

import Svg
import Svg.Attributes as S
import Color exposing (Color)
import Html exposing (Html, Attribute, div, text, input)
import Html.Attributes as H exposing (..)
import Html.Events exposing (on, onInput)

--slider : Int -> Int -> Int -> Msg -> Html msg
slider mn mx v txt msg =
  div []
    [ input
      [ type_ "range"
      , H.min <| String.fromInt mn
      , H.max <| String.fromInt mx
      , value  <| String.fromInt v
      , onInput msg
      ] []
    , text <| txt
    ]

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
        ] []

thermalElement : Int -> Int -> Int -> Float -> Svg.Svg msg
thermalElement x y size normalizedT =
    Svg.rect
        [ S.x (String.fromInt x)
        , S.y (String.fromInt y)
        , S.width (String.fromInt size)
        , S.height (String.fromInt size)
        , S.fill (cMap normalizedT )
        ] []

cMap : Float -> String
cMap v =
  Color.toCssString (Color.rgb v 0 (1 - v))

type alias Material =
  { name : String
  , c_s : Float
  , rho : Float
  , k : Float
  }

aluminum =
  Material "Aluminum" 896.0 2700.0 167.0

steel = 
  Material "Steel" 470.0 7600.0 52.0

glass =
  Material "Glass" 830.0 2200.0 1.1

nylon =
  Material "Nylon" 1500.0 1150.0 0.25