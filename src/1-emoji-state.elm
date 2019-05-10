
import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Array
import Random
import Dict


-- start: no codepoints
    -- generate random codepoint
-- set to either:
    -- mod/comb
    -- mod-only
    -- comb-only
    -- non-mod
-- select option from list of allowable transitions: get length of list of possible transitions, gen. random number, then gen. random codepoint based on those
-- transition based on that
-- continue until length of possible transitions is 1; set to "done"


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
  { currentstate : StateMachine
  , statestring : String
  --, codepoints : Codepoints       -- the codepoints in the current state can be represented in the StateMachine type
  }

type StateMachine 
  = Start
  | Unmodifiable Codepoints
  | ModifiableOnly Codepoints
  | CombinableOnly Codepoints
  | BothModAndComb Codepoints
  | Done Codepoints

possibleTransitions : Dict String String
possibleTransitions = 
  Dict.fromList
  [ ("Start" ["Unmodifiable", "ModifiableOnly", "CombinableOnly", "BothModAndComb"])
  , ("Unmodifiable" "Done")
  , ("ModifiableOnly" "Unmodifiable")
  , ("CombinableOnly" "Unmodifiable")
  , ("BothModAndComb" ["BothModAndComb", "ModifiableOnly", "CombinableOnly", "Unmodifiable"])
  ]

type alias Codepoints =
  (List Char)
-- use this type for both the current codepoints in the machine's memory, and the possible codepoints in certain sets of emoji

skintoneModifiers = Array.fromList ['\u{1F3FB}', '\u{1F3FC}', '\u{1F3FD}', '\u{1F3FE}', '\u{1F3FF}']
hairModifiers = Array.fromList ['\u{1F9B0}', '\u{1F9B1}', '\u{1F9B2}', '\u{1F9B3}']

genderCombiners = Array.fromList ['\u{2642}', '\u{2640}']

regularCombiners = Array.fromList ['\u{1F680}', '\u{1F3A8}'] -- for now just rocket and paint-palette
onlyModifiable = Array.fromList ['\u{1F596}', '\u{1F44D}']
bothModifiableCombinable = Array.fromList ['\u{1F468}', '\u{1F469}', '\u{1F926}'] -- basic man/woman can get combined with objects for combiner-statements; humans making gestures can get combined with gender-symbols; modifiers = skin (& hair?)

zwj = '\u{200D}'

unmodifiables = Array.fromList ['\u{1F955}', '\u{1F9E6}', '\u{1F984}', '\u{1F9C0}']


--startingPoints =  Array.append unmodifiables (Array.append regularCombiners (Array.append genderCombiners (Array.append onlyModifiable bothModifiableCombinable)))

{--



--}

init : () -> (Model, Cmd Msg)
init _ =
  ( Model Start ""
  , Cmd.none
  )

-- UPDATE


type Msg
  = StartUp
  | ChangeState
  | Finish

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    StartUp ->
      ( model
      , Random.generate 
      )
    ChangeState ->
      ( model
      , Cmd.none
      )


-- in each case: generate an int and select from the list of available transitions, then generate an int and select from the possible codepoints in that set

generateNextState : Model -> Random.Generator Int
generateNextState model = 
  Random.int 0 (Array.length model.possibleTransitions) -- don't know yet how to model that. hash table, store it separately from the "states" of the state machine?

generateRandomCodepoint : 
generateRandomCodepoint newstate =
  Random.int 0 (Array.length newstate)
  -- generate a random item that's a member of a list


-- maybe possible next states are a dictionary with a key, OR an array from which one can pick a random item.



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [
    {--
    title
    diagram of possible state transitions

    button (depending on model state: get random codepoint, make random transition, dead-button when finished)

    --}
    ]




















