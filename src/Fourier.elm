module Fourier exposing (Epicycle, EpicycleInfo, dft, epicycleInfo)

import Complex exposing (Complex)


type alias Epicycle =
    { freq : Float
    , amplitude : Float
    , phase : Float
    }


type alias EpicycleInfo =
    { cx : Float
    , cy : Float
    , tx : Float
    , ty : Float
    , radius : Float
    }


dft : List Complex -> List Epicycle
dft points =
    let
        pointsLength =
            toFloat <| List.length points
    in
    points
        |> List.foldl
            (\a ( outerIndex, epicycles ) ->
                ( outerIndex + 1
                , points
                    |> List.foldl
                        (\point ( innerIndex, sum ) ->
                            let
                                phi =
                                    (2 * pi * outerIndex * innerIndex) / pointsLength
                            in
                            ( innerIndex + 1
                            , Complex.add sum (Complex.multiply point (Complex.new ( cos phi, -1 * sin phi )))
                            )
                        )
                        ( 0
                        , Complex.new ( 0, 0 )
                        )
                    |> Tuple.second
                    |> Complex.scale (1 / pointsLength)
                    |> newEpicycle outerIndex
                    |> (\epicycle -> epicycle :: epicycles)
                )
            )
            ( 0, [] )
        |> Tuple.second


newEpicycle : Float -> Complex -> Epicycle
newEpicycle freq vector =
    { freq = freq
    , amplitude = Complex.magnitude vector
    , phase = Complex.angle vector
    }


epicycleInfo : Float -> List Epicycle -> List EpicycleInfo
epicycleInfo timeStep =
    .epis
        << List.foldl
            (\{ freq, amplitude, phase } acc ->
                let
                    nextX =
                        acc.x + amplitude * cos (freq * timeStep + phase)

                    nextY =
                        acc.y + amplitude * sin (freq * timeStep + phase)
                in
                { x = nextX
                , y = nextY
                , epis =
                    List.append
                        acc.epis
                        [ { cx = nextX
                          , cy = nextY
                          , tx = acc.x
                          , ty = acc.y
                          , radius = amplitude
                          }
                        ]
                }
            )
            { x = 0
            , y = 0
            , epis = []
            }
