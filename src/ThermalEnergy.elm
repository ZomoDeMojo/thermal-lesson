module ThermalEnergy exposing (..)

import Browser
import Html exposing (Html, Attribute, div, text, input, button)
import Html.Events exposing (onClick)
import Browser.Events as E
import Svg
import Svg.Attributes as S
import Helpers exposing(..)

tmax = 100.0
tmin = 0.0
ti = 50.0
qmax = 2000
qmin = -2000
qi = 0
dxmax = 10
dxmin = 5
d_xi = 5

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
  { t : Float
  , q : Int
  , d_x : Int
  , material : Material
  , c_tot : Float
  }

init : Model
init = 
  { t = ti
  , q = qi
  , d_x = d_xi
  , material = aluminum
  , c_tot = aluminum.rho * aluminum.c_s * ((d_xi / 1000) ^ 3)
  }
 -- Update
 
type Msg
  = UpdateQ String
  | UpdateDx String
  | TimeDelta Float
  | UpdateMaterial Material

update : Msg -> Model -> Model
update msg model =
  case msg of 
    (UpdateQ v) ->
      case String.toInt v of
        Just i ->
          { model
            | q = i
          }
        Nothing -> model
    (UpdateDx v) ->
      case String.toInt v of
        Just i ->
          { model
            | d_x = i
            , c_tot = model.material.rho * model.material.c_s * (((toFloat i) / 1000) ^ 3)
          }
        Nothing -> model
    (UpdateMaterial mat) ->
      { model
        | material = mat
        , c_tot = mat.rho * mat.c_s * (((toFloat model.d_x) / 1000) ^ 3)
      }
    (TimeDelta delta) ->
      { model
        | t = max (min (model.t + ((toFloat model.q) / 1000) / model.c_tot * (delta / 1000)) tmax) tmin
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
    , div [] [ text ("Element Volume : " ++ (String.fromInt (model.d_x ^ 3)) ++ " mm^3") ]
    , div [] [ text ("Total Heat Capacity : " ++ (String.fromFloat ( toFloat ( round (model.c_tot * 1000) ) / 1000)) ++ " J/C") ]
    , div [] [ text "------------------------------------------------"]
    , slider dxmin dxmax model.d_x ("  Element Size = " ++ (String.fromInt model.d_x) ++ " mm") UpdateDx
    , slider qmin qmax model.q ("  Energy Input = " ++ (String.fromInt model.q) ++ " mW") UpdateQ
    , Svg.svg
      [ S.width "250", S.height "200", S.viewBox "0 0 250 200" ]
      [ thermalElement 60 (100 - model.d_x * 10) (model.d_x * 20) (model.t / tmax )
      , drawArrow 60 100 (round ((toFloat model.q) / qmax * 20)) 0]
    , div [] [ text (tMessage model) ]
    ]

tMessage : Model -> String
tMessage model =
  if model.t == tmin then
    ("T = " ++ (String.fromFloat ( toFloat ( round (model.t * 10) ) / 10)) ++ " C - Brrr, that's as cold as this model can get")
  else if model.t == tmax then
    ("T = " ++ (String.fromFloat ( toFloat ( round (model.t * 10) ) / 10)) ++ " C - Oof, that's as hot as this model can get")
  else
    ("T = " ++ (String.fromFloat ( toFloat ( round (model.t * 10) ) / 10)) ++ " C")