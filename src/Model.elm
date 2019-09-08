module Model exposing (Card, CardValue(..), Dealer, Player, Suit(..))


type alias Dealer =
    { name : String
    , rule : String
    , hint : String
    }


type alias Player =
    { hand : List Card
    , name : String
    }


type alias Card =
    { value : CardValue
    , suit : Suit
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
