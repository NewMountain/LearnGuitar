module LearnGuitar exposing (..)

import Html exposing (..)
import Svg exposing (circle, svg, line)
import Svg.Attributes exposing (..)


--import MusicTheory.MusicTypes exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- TYPES


type Pitch
    = A
    | B
    | C
    | D
    | E
    | F
    | G


type Note
    = Note Pitch
    | Accidental Pitch Pitch


type Scale
    = Major
    | MelodicMinor
    | HarmonicMinor


chromaticScale : List Note
chromaticScale =
    [ Note A
    , Accidental A B
    , Note B
    , Note C
    , Accidental C D
    , Note D
    , Accidental D E
    , Note E
    , Note F
    , Accidental F G
    , Note G
    , Accidental G A
    ]


listOfScales : List Scale
listOfScales =
    [ Major
    , MelodicMinor
    , HarmonicMinor
    ]



-- MODEL


type alias Model =
    { currentScale : Note
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )


initModel : Model
initModel =
    { currentScale = Note C }



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div
        []
        [ rootNoteDropdown model
        , scaleDropdown model
        , br [] []
        , div [] [ text "The scale is: " ]
        , displayScale model
        ]


rootNoteDropdown : Model -> Html Msg
rootNoteDropdown model =
    dropDownMaker "Please select a root note" chromaticScale noteMaker


scaleDropdown : Model -> Html Msg
scaleDropdown model =
    dropDownMaker "Please select a root scale type" listOfScales scaleOptionMaker


dropDownMaker : String -> List a -> (a -> Html Msg) -> Html Msg
dropDownMaker labelText stuff stuffMaker =
    div
        []
        [ label
            []
            [ text labelText ]
        , select
            []
            (List.map stuffMaker stuff)
        ]


noteMaker : Note -> Html Msg
noteMaker note =
    option
        []
        [ text <| toString note ]


scaleOptionMaker : Scale -> Html Msg
scaleOptionMaker scale =
    option
        []
        [ text <| toString scale ]


displayScale : Model -> Html Msg
displayScale model =
    div
        []
        [ text <| toString chromaticScale ]


svgThingy : Model -> Html Msg
svgThingy model =
    svg
        [ viewBox "0 0 100 100", width "300px" ]
        [ circle [ cx "50", cy "50", r "45", fill "#0B79CE" ] []
        , line [ x1 "50", y1 "50", x2 "10", y2 "15", stroke "#023963" ] []
        ]
