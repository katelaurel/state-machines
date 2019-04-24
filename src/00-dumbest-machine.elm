import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Random

{---------------------

The dumbest (sorta) state machine! go to any state from any state infinitely the end. does it even qualify? who knows.

----------------------}


-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL


type alias Model =
  { state : StateMachine
  , statestring : String
  }

type StateMachine 
  = StateA
  | StateB
  | StateC


init : () -> (Model, Cmd Msg)
init _ =
  ( Model StateA "about to start from state A, not that that means anything here"
  , Cmd.none
  )

-- UPDATE


type Msg
  = Transition StateMachine String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Transition state statestring ->
      ( Model state statestring
      , Cmd.none
      )

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text ("the most meaningless statemachine") ]
    , h2 [] [ text "A <-<--> B <-->-> C"]
    , h2 [] [ text (model.statestring) ]
    , button [ onClick (Transition StateA "State A") ] [ text "Transition to A" ]
    , button [ onClick (Transition StateB "State B") ] [ text "Transition to B" ]
    , button [ onClick (Transition StateC "State C") ] [ text "Transition to C" ]
    ]


