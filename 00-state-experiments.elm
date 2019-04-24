import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Random

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
  the functions that govern the state machine's own logic and behavior control how that transition is
update function:
  start

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


type alias Model =
  { stateresult : String
  }

type StateMachine 
  = Start
  | Unmodifiable String
  | Modifiable String
  | Combinable String
  | BothModAndComb String
  | Done String

--makeEmoji : Random.Generator Int -> Emoji
--makeEmoji statemachine =
--  case statemachine of
--    Start ->
--      addGlyph (statemachine, )
--    Modifiable ->
--      addGlyph (blah)
--    Combinable ->
--      addGlyph (blah)
--    BothModAndComb ->
--      addGlyph (blah)
--    Done
--
--
--addGlyph : StateMachine -> 
--  -- take a statemachine. make a random choice about whether and what kind of modifier to add, depending on statemachine type. 
--
--
--
--function addGlyph: statemachine -> 


init : () -> (Model, Cmd Msg)
init _ =
  ( Model "about to start from state 'start'"
  , Cmd.none
  )

-- UPDATE


type Msg
  = Roll
  | NewText


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Roll ->
      ( model
      , Cmd.none
      )

    NewText ->
      ( Model "blah"
      , Cmd.none
      )


--emojify : String
--emojify = "\u{1F469}\u{1F3FB}\u{200D}\u{1F91D}\u{200D}\u{1F468}\u{1F3FC}"

--emojify : Random.Generator Char
--emojify =
--  Random.map (\n -> Char.fromCode (n)) (Random.int 9728 129685)

--complexify: 

newFace : String -> String
newFace text =
  "text" ++ text

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text (model.stateresult) ]
    , button [ onClick NewText ] [ text "Roll" ]
    ]