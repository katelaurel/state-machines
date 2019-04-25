import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Random

{---------------------

The second-dumbest state machine! go to any state from A, but only cycle back and forth between B and C infinitely, the end.
Does it even qualify? Still? who knows.

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

type Transition
  = ToA
  | ToB
  | ToC

init : () -> (Model, Cmd Msg)
init _ =
  ( Model StateA "about to start from state A, not that that means much here"
  , Cmd.none
  )

-- UPDATE


type Msg
  = ChangeState Transition


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangeState transitionType ->
      ( (testTransition transitionType model)
      , Cmd.none
      )


testTransition : Transition -> Model -> Model
testTransition transition currentmodel =
  case transition of
    ToA ->
      if currentmodel.state == StateA then
        { currentmodel | statestring = "Changed to State A" }
      else
        { currentmodel | statestring = "Invalid transition, still in same state" }
    ToB ->
      { currentmodel | state = StateB , statestring = "Changed to State B" }
    ToC ->
      { currentmodel | state = StateC , statestring = "Changed to State C" }

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text ("the second-most meaningless statemachine") ]
    , h2 [] [ text "A --> B <-->-> C"]
    , h2 [] [ text (model.statestring) ]
    , button [ onClick (ChangeState ToA) ] [ text "Transition to A" ]
    , button [ onClick (ChangeState ToB) ] [ text "Transition to B" ]
    , button [ onClick (ChangeState ToC) ] [ text "Transition to C" ]
    ]


