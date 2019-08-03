module Complex exposing (Complex, add, angle, divide, i, imaginary, magnitude, multiply, new, real, scale, subtract, toCartesian)


type Complex
    = Complex ( Float, Float )


new : ( Float, Float ) -> Complex
new =
    Complex


i : Complex
i =
    Complex ( 0, 1 )


real : Complex -> Float
real (Complex ( r1, _ )) =
    r1


imaginary : Complex -> Float
imaginary (Complex ( _, i1 )) =
    i1


add : Complex -> Complex -> Complex
add (Complex ( r1, i1 )) (Complex ( r2, i2 )) =
    Complex ( r1 + r2, i1 + i2 )


subtract : Complex -> Complex -> Complex
subtract (Complex ( r1, i1 )) (Complex ( r2, i2 )) =
    Complex ( r1 - r2, i1 - i2 )


multiply : Complex -> Complex -> Complex
multiply (Complex ( r1, i1 )) (Complex ( r2, i2 )) =
    Complex ( (r1 * r2) - (i1 * i2), (i1 * r2) + (r1 * i2) )


scale : Float -> Complex -> Complex
scale factor (Complex ( r1, i1 )) =
    Complex ( r1 * factor, i1 * factor )


divide : Complex -> Complex -> Complex
divide (Complex ( r1, i1 )) (Complex ( r2, i2 )) =
    Complex
        ( ((r1 * r2) + (i1 * i2)) / ((r2 ^ 2) + (i2 ^ 2))
        , -1 * ((i1 * r2) - (r1 * i2)) / ((r2 ^ 2) + (i2 ^ 2))
        )


magnitude : Complex -> Float
magnitude (Complex ( r1, i1 )) =
    sqrt (r1 * r1 + i1 * i1)


angle : Complex -> Float
angle (Complex ( r1, i1 )) =
    atan2 i1 r1


toCartesian : Complex -> ( Float, Float )
toCartesian (Complex ( r1, i1 )) =
    ( r1, i1 )
