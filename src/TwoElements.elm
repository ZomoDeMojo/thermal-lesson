module TwoElements exposing (..)

import Browser
import Html exposing (Html, Attribute, div, text, input, button)
import Html.Events exposing (onClick)
import Browser.Events as E
import Svg
import Svg.Attributes as S
import Helpers exposing(..)

tmax = 100
tmin = 0
ti = 50.0
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
  { t1 : Int
  , t2 : Float
  , d_x : Float
  , alpha : Float
  , c_tot : Float
  , material : Material
  }

init : Model
init = 
  { t1 = (round ti)
  , t2 = ti
  , d_x = d_x
  , alpha = aluminum.k / aluminum.rho / aluminum.c_s / (d_x ^ 2)
  , c_tot = aluminum.rho * aluminum.c_s * (d_x ^ 3)
  , material = aluminum
  }
 -- Update
 
type Msg
  = UpdateT1 String
  | TimeDelta Float
  | UpdateMaterial Material

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
    (UpdateMaterial mat) ->
      { model
        | material = mat
        , alpha = (mat.k / (mat.rho * mat.c_s * (model.d_x ^ 2)))
        , c_tot = mat.rho * mat.c_s * (model.d_x ^ 3)
      }
    (TimeDelta delta) ->
      { model
        | t2 = model.t2 + model.alpha * (min 1 (delta / 1000)) * ((toFloat model.t1) - model.t2)
      }
    
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
    , slider tmin tmax model.t1 (" T1 = " ++ (String.fromInt model.t1) ++ " C") UpdateT1
    , Svg.svg
      [ S.width "220", S.height "120", S.viewBox "0 0 220 120" ]
      [ thermalElement 20 10 100 (toFloat model.t1 / tmax)
      , thermalElement 120 10 100 (model.t2 / tmax)
      , drawArrow 120 60 (max (min (round ((q model) * 100)) 10) -10) 0]
    , div [] [ text ("Q' = " ++ (String.fromInt (round ((q model) * 1000))) ++ " mW") ]
    , div [] [ text ("T2 = " ++ (String.fromFloat ( toFloat ( round (model.t2 * 10) ) / 10)) ++ " C")]
    ]

q : Model -> Float
q model =
  ((toFloat model.t1) - model.t2) * model.material.k * model.d_x