module Theme exposing (bad, black, blueBackground, blueHighlight, good, inactive, veryBad, veryGood, white)

import Element exposing (Color, rgb255)


bad : Color
bad =
    rgb255 255 200 200


good : Color
good =
    rgb255 200 255 200


veryBad : Color
veryBad =
    rgb255 255 100 100


veryGood : Color
veryGood =
    rgb255 100 255 100


inactive : Color
inactive =
    rgb255 240 240 240


blueBackground : Color
blueBackground =
    rgb255 200 200 255


blueHighlight : Color
blueHighlight =
    rgb255 100 100 255


black : Color
black =
    rgb255 0 0 0


white : Color
white =
    rgb255 255 255 255
