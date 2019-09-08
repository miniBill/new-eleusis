module AdorableAvatar exposing (url)

import Sha256


eyes : List String
eyes =
    [ "eyes1", "eyes10", "eyes2", "eyes3", "eyes4", "eyes5", "eyes6", "eyes7", "eyes9" ]


noses : List String
noses =
    [ "nose2", "nose3", "nose4", "nose5", "nose6", "nose7", "nose8", "nose9" ]


mouths : List String
mouths =
    [ "mouth1", "mouth10", "mouth11", "mouth3", "mouth5", "mouth6", "mouth7", "mouth9" ]


getOrEmpty : Int -> List String -> String
getOrEmpty i xs =
    case ( i, xs ) of
        ( _, [] ) ->
            ""

        ( 0, y :: _ ) ->
            y

        ( _, _ :: ys ) ->
            getOrEmpty (i - 1) ys


url : String -> String
url name =
    let
        ints =
            name
                |> Sha256.sha256
                |> String.toList
                |> List.map Char.toCode

        eye =
            ints |> List.take 3 |> List.sum |> modBy (List.length eyes)

        nose =
            ints |> List.take 6 |> List.sum |> modBy (List.length noses)

        mouth =
            ints |> List.take 9 |> List.sum |> modBy (List.length mouths)

        color =
            ints
                |> List.drop 9
                |> List.take 6
                |> List.map Char.fromCode
                |> String.fromList
                |> String.replace "9" "a"
                |> String.replace "8" "a"
                |> String.replace "7" "9"
                |> String.replace "6" "9"
                |> String.replace "5" "8"
                |> String.replace "4" "8"
                |> String.replace "3" "7"
                |> String.replace "2" "7"
                |> String.replace "1" "6"
    in
    String.join "/"
        [ "https://api.adorable.io/avatars/face"
        , getOrEmpty eye eyes
        , getOrEmpty nose noses
        , getOrEmpty mouth mouths
        , color
        ]
