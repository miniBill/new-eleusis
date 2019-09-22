module Model exposing (Card, CardValue(..), Dealer, Player, Suit(..))

import List.Nonempty exposing (Nonempty)


type alias Dealer =
    { name : String
    , rule : String
    , hint : String
    }


type alias Player =
    { hand : Nonempty Card
    , name : String
    }


type alias Card =
    { value : CardValue
    , suit : Suit
    , deck : Int
    }


type CardValue
    = Ace
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King


type Suit
    = Hearth
    | Diamond
    | Club
    | Spade
