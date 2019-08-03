module Main exposing (main)

import Browser
import Browser.Events
import Complex exposing (Complex)
import Fourier exposing (Epicycle, EpicycleInfo)
import Html exposing (Html, button, div, text)
import Svg exposing (Svg)
import Svg.Attributes as Attrs
import Time exposing (Posix)


centerX =
    300


centerY =
    300


totalSteps =
    1000


type Msg
    = NextStep Posix


type alias Model =
    { timeStep : Float
    , epis : List EpicycleInfo
    , path : List Complex
    }


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( { timeStep = 0, epis = [], path = [] }, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Time.every 100 NextStep
        }


points : List Complex
points =
    [ Complex.new ( 10, 10 )
    , Complex.new ( 20, 10 )
    , Complex.new ( 30, 10 )
    , Complex.new ( 40, 10 )
    , Complex.new ( 50, 10 )
    , Complex.new ( 60, 10 )
    , Complex.new ( 70, 10 )
    , Complex.new ( 80, 10 )
    , Complex.new ( 90, 10 )
    , Complex.new ( 100, 10 )
    , Complex.new ( 110, 10 )
    , Complex.new ( 120, 10 )
    , Complex.new ( 130, 10 )
    , Complex.new ( 140, 10 )
    , Complex.new ( 150, 10 )
    , Complex.new ( 160, 10 )
    , Complex.new ( 170, 10 )
    , Complex.new ( 180, 10 )
    , Complex.new ( 190, 10 )
    , Complex.new ( 200, 10 )
    , Complex.new ( 210, 10 )
    , Complex.new ( 220, 10 )
    , Complex.new ( 230, 10 )
    , Complex.new ( 240, 10 )
    , Complex.new ( 250, 10 )
    , Complex.new ( 260, 10 )
    , Complex.new ( 270, 10 )
    , Complex.new ( 280, 10 )
    , Complex.new ( 290, 10 )
    , Complex.new ( 300, 10 )
    , Complex.new ( 310, 10 )
    , Complex.new ( 320, 10 )
    , Complex.new ( 330, 10 )
    , Complex.new ( 340, 10 )
    , Complex.new ( 350, 10 )
    , Complex.new ( 350, 20 )
    , Complex.new ( 350, 30 )
    , Complex.new ( 350, 40 )
    , Complex.new ( 350, 50 )
    , Complex.new ( 350, 60 )
    , Complex.new ( 350, 70 )
    , Complex.new ( 350, 80 )
    , Complex.new ( 350, 90 )
    , Complex.new ( 350, 100 )
    , Complex.new ( 350, 110 )
    , Complex.new ( 350, 120 )
    , Complex.new ( 350, 130 )
    , Complex.new ( 350, 140 )
    , Complex.new ( 350, 150 )
    , Complex.new ( 350, 160 )
    , Complex.new ( 350, 170 )
    , Complex.new ( 350, 180 )
    , Complex.new ( 350, 190 )
    , Complex.new ( 350, 200 )
    , Complex.new ( 350, 210 )
    , Complex.new ( 350, 220 )
    , Complex.new ( 350, 230 )
    , Complex.new ( 350, 240 )
    , Complex.new ( 350, 250 )
    , Complex.new ( 350, 260 )
    , Complex.new ( 350, 270 )
    , Complex.new ( 350, 280 )
    , Complex.new ( 350, 290 )
    , Complex.new ( 350, 300 )
    , Complex.new ( 350, 310 )
    , Complex.new ( 350, 320 )
    , Complex.new ( 350, 330 )
    , Complex.new ( 350, 340 )
    , Complex.new ( 350, 350 )
    ]


epicycles : List Epicycle
epicycles =
    points
        |> Fourier.dft
        |> List.sortBy .amplitude
        |> List.reverse


update : Msg -> Model -> ( Model, Cmd Msg )
update (NextStep _) ({ timeStep, path } as model) =
    let
        nextTime =
            timeStep + (2 * pi / toFloat (List.length points))

        finalTime =
            if nextTime > 2 * pi then
                0

            else
                nextTime

        epis =
            Fourier.epicycleInfo ( centerX, centerY ) finalTime epicycles

        nextPath =
            epis
                |> List.reverse
                |> List.head
                |> Maybe.map (\epicycle -> List.append path [ Complex.new ( epicycle.tx, epicycle.ty ) ])
                |> Maybe.map (List.take totalSteps)
                |> Maybe.withDefault path
    in
    ( { model | timeStep = finalTime, epis = epis, path = nextPath }, Cmd.none )


view : Model -> Html Msg
view { timeStep, epis, path } =
    Svg.svg
        [ Attrs.width "1000"
        , Attrs.height "1000"
        ]
        [ epis
            |> List.map epicycleView
            |> List.concat
            |> Svg.g []
        , path
            |> List.map Complex.toCartesian
            |> List.map (\( x, y ) -> circle ( String.fromFloat x, String.fromFloat y ) "1" "red" "1")
            |> Svg.g []
        ]


line : ( String, String ) -> ( String, String ) -> Html Msg
line ( x1, y1 ) ( x2, y2 ) =
    Svg.line
        [ Attrs.x1 x1
        , Attrs.y1 y1
        , Attrs.x2 x2
        , Attrs.y2 y2
        , Attrs.stroke "black"
        , Attrs.strokeWidth "2"
        , Attrs.strokeOpacity "0.7"
        ]
        []


circle : ( String, String ) -> String -> String -> String -> Html Msg
circle ( x, y ) r color opacity =
    Svg.circle
        [ Attrs.cx x
        , Attrs.cy y
        , Attrs.r r
        , Attrs.stroke color
        , Attrs.strokeWidth "1"
        , Attrs.strokeOpacity opacity
        , Attrs.fillOpacity "0"
        ]
        []


epicycleView : EpicycleInfo -> List (Html Msg)
epicycleView info =
    [ circle ( String.fromFloat info.cx, String.fromFloat info.cy ) (String.fromFloat info.radius) "gray" "0.4"
    , line ( String.fromFloat info.cx, String.fromFloat info.cy ) ( String.fromFloat info.tx, String.fromFloat info.ty )
    ]
