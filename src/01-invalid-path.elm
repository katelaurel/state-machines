import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Random
import Set

-----------------------------------
--  WHERE THIS IS STOPPING FOR NOW:
--    • it seems like just making a practice statemachine that goes from state to state randomly, printing at each iteration,
--    is a good start. that way we can elide the whole emoji-syntax issue and focus on random transitions between states that
--    are not all fully/evenly connected.
  -- UPDATE to below: state machine definitely a type, I think -- or rather, a thing whose current 
  -- values-passing-through-the-machine are of type statemachine with various possible type-values which are states.
--    • not totally clear whether a statemachine is a record, one of whose fields is its current state, which can be one of
--    several types validated by a "state" type definition, or is a type with additional values attached to certain types but
--    not others. This should depend on what characteristics make it easiest to manage the validity of transitions, probably?
--    • possible functions: "produce state", "transition state", "random path" ? the latter two might be collapsible in this
--    simplified version. On the other hand, even with these three, the whole issue of validating emoji string contents is
--    currently elided.
--    • start the next step by sketching out the functions with your imaginary types, THEN try to redefine the types properly
--    based on the new thinking about how these work.
------------------------------------

{--
onclick: makes a random transition
  the functions that govern the state machine's own logic and behavior control how that transition is acted out.
    All state machine functions advance by one step, whether that's to the next state or to a final state.
    If things advance to the final state, that function needs to make it impossible to ask for another update, or (alternatively)
      change the view so that it's clear we're at a final state and any further requests just say no. (see the json example for ways)


update function:
  generaterandomchange (model unchanged, command random.generate)
  changethestate (model gets changed based on value, command none)
^ I *think* it's necessary to make two functions here because generators are themselves handled as a command. But this is tbd:
  so long as nesting generators inside other functions can happen (and why wouldn't it? didn't I do that earlier tonight by
  mistake?), then "changethestate" is the only "update" required, which gets its message from an onclick listener, sends a command
  to generate a random transition and apply it, and updates the model. (Right now to me this sounds like the updated model will
  require a second msg, but see also the possibility of using case statements inside the message thing in the http example?)

STARTING POINT: make a NON-RANDOM state machine. two buttons, cycle back and forth. or multiple paths.

--}



-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL

type Model
  = Start
  | InState StateMachine


--type alias Model =
--  { state : StateMachine
--  , statestring : String
--  }

type StateMachine 
  = StateA StateName ValidTransitions
  | StateB StateName ValidTransitions
  | StateC StateName ValidTransitions

type alias StateName
  = String

type alias ValidTransitions
  = (List String)

stateA = StateA "A" ["A", "B", "C"]
stateB = StateB "B" ["B", "C"]
stateC = StateC "C" ["B", "C"]


init : () -> (Model, Cmd Msg)
init _ =
  ( Start
  , Cmd.none
  )

-- UPDATE


type Msg
  = TestValidity StateMachine
  | Transition StateMachine


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    TestValidity state ->
      ( model
      , testValidity state  --Cmd.none
      )
    Transition state  ->
      ( (InState state)
      , Cmd.none
      )

testValidity : StateMachine -> Model -> Cmd Msg
testValidity state model =
  case model of
    Start ->
      Cmd.none
    InState currentstate ->
      if List.member (getStateName state) (getValidPaths currentstate) then
        update (Transition state)
      else
        Cmd.none

getValidPaths currentstate =
  case currentstate of
    StateA name transitions ->
      transitions
    StateB name transitions ->
      transitions
    StateC name transitions ->
      transitions

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text "the second-most meaningless statemachine" ]
    , h2 [] [ text "A --> B <-->-> C"]
    , displayState model
    , button [ onClick (TestValidity stateA) ] [ text "Transition to A" ]
    , button [ onClick (TestValidity stateB) ] [ text "Transition to B" ]
    , button [ onClick (TestValidity stateC) ] [ text "Transition to C" ]
    ]

displayState model =
  case model of
    Start ->
      div []
        [ h2 [] [ text "about to start from state A, though this still doesn't mean much"]]
    InState statemachine ->
      div []
        [ h2 [] [ text (getStateName statemachine) ]]

getStateName statemachine =
  case statemachine of
    StateA statename _ ->
      statename
    StateB statename _ ->
      statename
    StateC statename _ ->
      statename
