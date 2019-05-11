
import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Array
import Random
import Tuple
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

possibleTransitions : Dict String (List String)
possibleTransitions = 
  Dict.fromList
  [ ("Start", ["Unmodifiable", "ModifiableOnly", "CombinableOnly", "BothModAndComb"])
  , ("Unmodifiable", ["Done"])
  , ("ModifiableOnly", ["Unmodifiable"])
  , ("CombinableOnly", ["Unmodifiable"])
  , ("BothModAndComb", ["BothModAndComb", "ModifiableOnly", "CombinableOnly", "Unmodifiable"])
  ]

possibleCodepointGroups : Dict String (List Array)
possibleCodepointGroups =
  Dict.fromList
  [ ("Start", [regularCombiners, onlyModifiable, bothModifiableCombinable, unmodifiables])
  , ("ModifiableOnly", [skintoneModifiers, hairModifiers])
  , ("CombinableOnly", [genderCombiners, regularCombiners])
  , ("BothModAndComb", [skintoneModifiers, hairModifiers, genderCombiners, regularCombiners])
  ]
  -- excluding Unmodifiable from dict because handled as special case

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
      , 
      )
    GetNewCodeAndState ->
      ( model
      , 
      )
    ApplyStateChange ->
      ( model
      , Cmd.none
      )



{--
- pick a next state from the possible transitions
- pick a codepoint from the possible codepoints in that state
- set the model to the newstate, and append the codepoint to the codepoint list in the model's state-type

generate random place (int) in an array of codepoints in the possiblecodepointgroups valid for the transition.

pick random valid transition
pick random valid code point group
pick random valid codepoint

model (new state new codepoint) name of new state

Random.step (Random.int 0 (Array.length (Array.fromList (Dict.get model.statestring possibleTransitions)))) theseed
^ this gives ([int in the possibletransitions length] newseed)
transition = Tuple.first ^ all that

Random.step (Random.int 0 (Array.length (Array.fromList (Dict.get transition possibleCodePointGroups)))) newseed
^ this gives ([int in the possible codepoint group length] newseed)
codepointgroup = Tuple.first ^ that

Random.generate ApplyStateChange (Random.int 0 (Array.length codepointgroup))
^ produces a cmd msg, but doesn't retain info about which codepointgroup you're pulling from, or what state transition is happening.
  it looks like it MIGHT be possible to produce a... Cmd (String -> Msg) ? but not clear how to make that actually transform a string.

--}


pickNextState : Model -> Int
pickNextState model = 
  newState = Tuple.first (Random.step (Random.int 0 (Array.length (Array.fromList (Dict.get model.statestring possibleTransitions)))) theseed)


pickCodepointGroup newState = 
  cpg = Tuple.first (Random.step (Random.int 0 (Array.length (Array.fromList (Dict.get newState possibleCodepointGroups)))) theseed)


pickCodepoint cpg =
  newpoint = Array.get (Tuple.first (Random.step (Random.int 0 (Array.length cpg)) theseed)) cpg

-- ^^^ the seeding is all fucked up, but chaining these properly (pickcodepoint [pickcodepointgroup [picknextstate model]]) would generate a codepoint
-- .....except: Dict.get gives you a maybe, so either need to use Maybe.withDefault (and have the default be, e.g., a zero-length array or something) or ?? something else.














--generateNextState : Cmd Msg
--generateNextState =
--  Random.generate pickNextState 

--generateStateCodepoint : Array -> 
--generateStateCodepoint newstate =
--  Random.step (Random.int 0 (Array.length newstate)) theseed
  -- generate a random item that's a member of a list


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




















