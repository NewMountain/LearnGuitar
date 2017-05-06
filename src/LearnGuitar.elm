module LearnGuitar exposing (..)

import Html exposing (..)
import Html.Events exposing (onInput)
import Svg exposing (circle, svg, line)
import Svg.Attributes exposing (..)
import Select
import List.Extra as LE


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


type alias Fret =
    Int


type Note
    = Note Pitch
    | Accidental Pitch Pitch


type Scale
    = Major
    | MelodicMinor
    | HarmonicMinor


type AccidentalClass
    = Sharp_
    | Flat_
    | Natural_


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


type UserNote
    = ANatural
    | ASharp
    | BFlat
    | CNatural
    | CSharp
    | DFlat
    | DNatural
    | DSharp
    | EFlat
    | ENatural
    | FNatural
    | FSharp
    | GFlat
    | GNatural
    | GSharp
    | AFlat


listOfUserNotes : List UserNote
listOfUserNotes =
    [ ANatural
    , BFlat
    , CNatural
    , CSharp
    , DFlat
    , DNatural
    , EFlat
    , ENatural
    , FNatural
    , FSharp
    , GFlat
    , GNatural
    , AFlat
    ]


listOfFlatAccidentals : List UserNote
listOfFlatAccidentals =
    [ BFlat
    , DFlat
    , EFlat
    , FNatural
    , GFlat
    , AFlat
    ]


listOfSharpAccidentals : List UserNote
listOfSharpAccidentals =
    [ ANatural
    , CSharp
    , DNatural
    , ENatural
    , FSharp
    , GNatural
    , AFlat
    ]


accidentalFromUserNote : UserNote -> AccidentalClass
accidentalFromUserNote userNote =
    case ( List.member userNote listOfSharpAccidentals, List.member userNote listOfSharpAccidentals ) of
        ( True, _ ) ->
            Sharp_

        ( _, True ) ->
            Flat_

        ( False, False ) ->
            Natural_


userNoteToComputerNote : UserNote -> Note
userNoteToComputerNote userNote =
    case userNote of
        ANatural ->
            Note A

        ASharp ->
            Accidental A B

        BFlat ->
            Accidental A B

        CNatural ->
            Note C

        CSharp ->
            Accidental C D

        DFlat ->
            Accidental C D

        DNatural ->
            Note D

        DSharp ->
            Accidental D E

        EFlat ->
            Accidental D E

        ENatural ->
            Note E

        FNatural ->
            Note F

        FSharp ->
            Accidental F G

        GFlat ->
            Accidental F G

        GNatural ->
            Note G

        GSharp ->
            Accidental G A

        AFlat ->
            Accidental G A



-- MODEL


type alias Model =
    { currentRoot : UserNote
    , computerNote : Note
    , currentScale : Scale
    , accidentalBehavior : AccidentalClass
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )


initModel : Model
initModel =
    { currentRoot = CNatural
    , computerNote = Note C
    , currentScale = Major
    , accidentalBehavior = Natural_
    }



-- UPDATE


type Msg
    = UpdateRoot UserNote
    | UpdateScale Scale


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateRoot note ->
            ( updateNoteAndScaleInfo model note, Cmd.none )

        UpdateScale scale_ ->
            ( { model | currentScale = scale_ }, Cmd.none )


updateNoteAndScaleInfo : Model -> UserNote -> Model
updateNoteAndScaleInfo model userNote =
    let
        newComputerNote =
            userNoteToComputerNote userNote

        newCurrentRoot =
            userNote

        newAccidentalBehavior =
            accidentalFromUserNote userNote
    in
        model
            |> updateComputerNote newComputerNote
            |> updateCurrentRoot newCurrentRoot
            |> updateAccidentalBehavior newAccidentalBehavior


updateComputerNote : Note -> Model -> Model
updateComputerNote newComputerNote model =
    { model | computerNote = newComputerNote }


updateCurrentRoot : UserNote -> Model -> Model
updateCurrentRoot userNote model =
    { model | currentRoot = userNote }


updateAccidentalBehavior : AccidentalClass -> Model -> Model
updateAccidentalBehavior accidentalClass model =
    { model | accidentalBehavior = accidentalClass }



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
    dropDownMaker "Please select a root note" listOfUserNotes UpdateRoot


scaleDropdown : Model -> Html Msg
scaleDropdown model =
    dropDownMaker "Please select a root scale type" listOfScales UpdateScale


dropDownMaker : String -> List a -> (a -> msg) -> Html msg
dropDownMaker labelText stuff msg =
    div
        []
        [ label
            []
            [ text labelText ]
        , Select.from stuff msg
        ]


displayScale : Model -> Html Msg
displayScale model =
    div
        []
        [ text <| toString <| makeScale model ]


makeScale : Model -> List Note
makeScale model =
    model
        |> getRootAndScale
        |> reindexChromaticScale
        |> getScaleIndexes
        |> constructScale


getRootAndScale : Model -> ( Note, Scale )
getRootAndScale model =
    ( userNoteToComputerNote model.currentRoot, model.currentScale )


reindexChromaticScale : ( Note, Scale ) -> ( List Note, Scale )
reindexChromaticScale ( root, scale ) =
    let
        redenominatedScale =
            chromaticScale
                |> List.repeat 2
                |> List.concat
                |> LE.dropWhile ((/=) root)
                |> List.take 13
    in
        ( redenominatedScale, scale )


getScaleIndexes : ( List Note, Scale ) -> ( List Note, List Int )
getScaleIndexes ( notes, scale ) =
    ( notes, getScaleIndexes_ scale )


constructScale : ( List Note, List Int ) -> List Note
constructScale ( notes, indexes ) =
    indexes
        |> List.filterMap (flip LE.getAt notes)


getScaleIndexes_ : Scale -> List Int
getScaleIndexes_ scale =
    case scale of
        Major ->
            --[ W  W  H  W  W  W    H]
            [ 0, 2, 4, 5, 7, 9, 11, 12 ]

        MelodicMinor ->
            --[ W  H  W  W  W  W    H]
            [ 0, 2, 3, 5, 7, 9, 11, 12 ]

        HarmonicMinor ->
            --[ W  H  W  W  H  WH   H]
            [ 0, 2, 3, 5, 7, 8, 11, 12 ]


svgThingy : Model -> Html Msg
svgThingy model =
    svg
        [ viewBox "0 0 100 100", width "300px" ]
        [ circle [ cx "50", cy "50", r "45", fill "#0B79CE" ] []
        , line [ x1 "50", y1 "50", x2 "10", y2 "15", stroke "#023963" ] []
        ]
