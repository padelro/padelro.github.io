let ``good_enough?`` guess x =
    abs(x - guess * guess) < 0.001

let ``improve`` guess x =
    (guess + (x / guess)) / 2.

let rec ``try_guess`` guess x =
    if ``good_enough?`` guess x
        then guess
        else try_guess (``improve`` guess x) x

let sqrt x = ``try_guess`` 1. x

sqrt 2.