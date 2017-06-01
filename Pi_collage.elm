module Pi_collage exposing (main)


-- Add/modify imports if you'd like. ---------------------------------

import Random exposing (Generator, Seed)
import Time exposing (..)
import Html exposing (Html)
import Tuple exposing (..)
import Collage exposing (..)
import Element exposing (..)
import Color exposing (..)
----------------------------------------------------------------------

pixWidth = 500
pointSz = toFloat <| pixWidth // 100
radius = pixWidth / 2

main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias Point = (Float, Float)

type alias Model =
  { hits : List Point
  , misses : List Point
  , hitCount : Int
  , missCount : Int
  , seed : Seed
  }

type Msg = Tick Time

init : (Model, Cmd Msg)
init = (initialModel, Cmd.none)

initialModel : Model
initialModel =
  {
    hits = [] ,
    misses = [] ,
    hitCount = 0 ,
    missCount = 0 ,
    seed = Random.initialSeupdateb Msg
subscriptions model =
  every (Time.millisecond) Tick

pointGenerator : Generator Point
pointGenerator =
  Random.map2 (\x -> \y -> (x,y)) (Random.float -1 1) (Random.float -1 1)

insideCircle : Point -> Bool
insideCircle (x, y) =
  let d = sqrt(x*x + y*y) in
    if (d <= 1) then True else False

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick t ->
      let (p,s) = Random.step pointGenerator model.seed in
        if (insideCircle p) then
          (
            {
              hits = p::model.hits,
              misses = model.misses,
              hitCount = model.hitCount + 1,
              missCount = model.missCount,
              seed = s
            } ,
            Cmd.none
          )
        else
          (
            {
              hits = model.hits ,
              misses = p::model.misses ,
              hitCount = model.hitCount ,
              missCount = model.missCount + 1 ,
              seed = s
            } ,
            Cmd.none
          )

drawHit : Point -> Form
drawHit (x,y) =
  move (x*radius, y*radius) <|
    filled (rgb 0 255 0) <|
      circle pointSz

drawMiss : Point -> Form
drawMiss (x,y) =
  move (x*radius, y*radius) <|
    filled (rgb 255 0 0) <|
      circle pointSz

drawEstimate : Model -> Form
drawEstimate : m =
  

view : Model -> Html Msg
view m =
  toHtml <|
    collage pixWidth pixWidth <|
      (List.map drawHit m.hits) ++ (List.map drawMiss m.misses)


estimatePi : Model -> Float
estimatePi m =
  4.0*((toFloat m.hitCount) / ((toFloat m.hitCount) + (toFloat m.missCount)))
