module Main exposing (main)

import Browser
import Browser.Events exposing (onMouseDown, onMouseMove, onMouseUp)
import Complex exposing (Complex)
import Fourier exposing (Epicycle, EpicycleInfo)
import Html exposing (Html, button, div, text)
import Json.Decode as Decode
import Svg exposing (Svg)
import Svg.Attributes as Attrs
import Time exposing (Posix)


type Msg
    = NextStep Posix
    | OnMouseDown
    | OnMouseMove Int Int
    | OnMouseUp


type alias Model =
    { mode : Mode
    , handDrawnPoints : List Complex
    , timeStep : Float
    , epis : List EpicycleInfo
    , path : List Complex

    -- This value is cached in the model since it is expensive to compute
    , epicycles : List Epicycle
    }


type Mode
    = Drawing
    | Animating


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.mode of
        Drawing ->
            Sub.batch
                [ onMouseMove
                    (Decode.map2
                        OnMouseMove
                        (Decode.field "pageX" Decode.int)
                        (Decode.field "pageY" Decode.int)
                    )
                , onMouseUp (Decode.succeed OnMouseUp)
                ]

        Animating ->
            Sub.batch
                [ Time.every 30 NextStep
                , onMouseDown (Decode.succeed OnMouseDown)
                ]


initialModel : Model
initialModel =
    { mode = Animating
    , handDrawnPoints = squarePoints
    , timeStep = 0
    , epis = []
    , path = []
    , epicycles =
        squarePoints
            |> Fourier.dft
            |> List.sortBy .amplitude
            |> List.reverse
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ handDrawnPoints } as model) =
    case msg of
        NextStep _ ->
            ( updateAnimation model, Cmd.none )

        OnMouseDown ->
            ( { initialModel
                | mode = Drawing
                , handDrawnPoints = []
                , epicycles = []
              }
            , Cmd.none
            )

        OnMouseMove x y ->
            ( addDrawPoints x y model, Cmd.none )

        OnMouseUp ->
            ( { model
                | mode = Animating
                , epicycles =
                    handDrawnPoints
                        |> Fourier.dft
                        |> List.sortBy .amplitude
                        |> List.reverse
              }
            , Cmd.none
            )


updateAnimation : Model -> Model
updateAnimation ({ timeStep, path, handDrawnPoints, epicycles } as model) =
    let
        nextTime =
            timeStep + (2 * pi / toFloat (List.length handDrawnPoints))

        finalTime =
            if nextTime > 2 * pi then
                0

            else
                nextTime

        epis =
            Fourier.epicycleInfo finalTime epicycles

        nextPath =
            epis
                |> List.reverse
                |> List.head
                |> Maybe.map (\epicycle -> List.append path [ Complex.new ( epicycle.tx, epicycle.ty ) ])
                |> Maybe.map (List.take (List.length handDrawnPoints))
                |> Maybe.withDefault path
    in
    { model | timeStep = finalTime, epis = epis, path = nextPath }


addDrawPoints : Int -> Int -> Model -> Model
addDrawPoints x y model =
    { model
        | handDrawnPoints =
            List.append
                model.handDrawnPoints
                [ Complex.new ( toFloat x, toFloat y ) ]
    }


view : Model -> Html Msg
view { mode, timeStep, epis, path, handDrawnPoints } =
    Svg.svg
        [ Attrs.width "600"
        , Attrs.height "600"
        ]
        (case mode of
            Drawing ->
                [ handDrawnPoints
                    |> List.map Complex.toCartesian
                    |> List.map (\( x, y ) -> circle ( String.fromFloat x, String.fromFloat y ) "2" "blue" "1")
                    |> Svg.g []
                ]

            Animating ->
                [ handDrawnPoints
                    |> List.map Complex.toCartesian
                    |> List.map (\( x, y ) -> circle ( String.fromFloat x, String.fromFloat y ) "2" "blue" "1")
                    |> Svg.g []
                , case epis of
                    _ :: rest ->
                        rest
                            |> List.map epicycleView
                            |> List.concat
                            |> Svg.g []

                    _ ->
                        Html.text ""
                , path
                    |> List.map Complex.toCartesian
                    |> List.map (\( x, y ) -> circle ( String.fromFloat x, String.fromFloat y ) "1" "red" "1")
                    |> Svg.g []
                ]
        )


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


squarePoints : List Complex
squarePoints =
    let
        xs =
            List.repeat 50 0
                |> List.indexedMap (\index _ -> 100 + toFloat index * 5)

        ys =
            List.repeat 50 0
                |> List.indexedMap (\index _ -> 100 + toFloat index * 5)
    in
    List.concat
        [ xs |> List.map (\x -> Complex.new ( x, 100 ))
        , ys |> List.map (\y -> Complex.new ( 350, y ))
        , xs |> List.reverse |> List.map (\x -> Complex.new ( x, 350 ))
        , ys |> List.reverse |> List.map (\y -> Complex.new ( 100, y ))
        ]
