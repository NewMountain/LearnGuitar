module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onInput)
import Svg exposing (circle, svg, line)
import Svg.Attributes exposing (..)
import Select
import List.Extra as LE
import Src.Types exposing (..)


--------------------------------------------------------------------
--                         MAIN
--------------------------------------------------------------------


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



--------------------------------------------------------------------
--                           INIT
--------------------------------------------------------------------


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )


initModel : Model
initModel =
    { currentRoot = CNatural
    , computerNote = Note C
    , currentScale = Major
    , accidentalBehavior = Natural_
    , humanScale = [ "C", "D", "E", "F", "G", "A", "B", "C" ]
    }



--------------------------------------------------------------------
--                           UPDATE
--------------------------------------------------------------------


type Msg
    = UpdateRoot UserNote
    | UpdateScale Scale


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateRoot note ->
            ( updateNoteAndScaleInfo model note, Cmd.none )

        UpdateScale scale_ ->
            ( updateScaleAndRecalculate scale_ model, Cmd.none )


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
            |> (\mdl -> updateHumanScale (makeScaleElements mdl) mdl)


updateComputerNote : Note -> Model -> Model
updateComputerNote newComputerNote model =
    { model | computerNote = newComputerNote }


updateCurrentRoot : UserNote -> Model -> Model
updateCurrentRoot userNote model =
    { model | currentRoot = userNote }


updateAccidentalBehavior : AccidentalClass -> Model -> Model
updateAccidentalBehavior accidentalClass model =
    { model | accidentalBehavior = accidentalClass }


updateHumanScale : List String -> Model -> Model
updateHumanScale newHumanScale model =
    { model | humanScale = newHumanScale }


updateScale : Scale -> Model -> Model
updateScale newScale model =
    { model | currentScale = newScale }


updateScaleAndRecalculate : Scale -> Model -> Model
updateScaleAndRecalculate newScale model =
    updateScale newScale model
        |> (\mdl -> updateNoteAndScaleInfo mdl model.currentRoot)



--------------------------------------------------------------------
--                        SUBSCRIPTIONS
--------------------------------------------------------------------


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



--------------------------------------------------------------------
--                           VIEW
--------------------------------------------------------------------


view : Model -> Html Msg
view model =
    div
        []
        [ rootNoteDropdown model
        , scaleDropdown model
        , br [] []
        , div [] [ text "The scale is: " ]
        , displayScale model
        , displayArpeggio model
        , text <| toString model
        ]


displayArpeggio : Model -> Html Msg
displayArpeggio model =
    div
        []
        [ text <| "The arpeggio is a: " ++ calculateChord model
        , br [] []
        , text "The notes are: "
        , div [] (makeArpeggioNotes model.humanScale)
        ]


calculateChord : Model -> String
calculateChord model =
    model
        |> makeScale
        |> flip takeChordTones (Note C)
        |> calculateChordDistance
        |> distancesToChordDescription


distancesToChordDescription : ( Int, Int, Int ) -> String
distancesToChordDescription ( third, fifth, seventh ) =
    classifyThird third ++ classifyFifth fifth ++ classifySeventh seventh


classifyThird : Int -> String
classifyThird int =
    case int of
        2 ->
            "Diminished Third "

        3 ->
            "Minor Third "

        4 ->
            "Major Third "

        5 ->
            "Augmented Third "

        _ ->
            "Never seen a third with interval " ++ toString int ++ "before "


classifyFifth : Int -> String
classifyFifth int =
    case int of
        6 ->
            "Flat 5 "

        7 ->
            ""

        8 ->
            "Augmted Fifth "

        _ ->
            "Never seen a fifth with interval " ++ toString int ++ "before "


classifySeventh : Int -> String
classifySeventh int =
    case int of
        11 ->
            "Major Seventh"

        10 ->
            "Minor Seventh"

        9 ->
            "Diminished Seventh"

        _ ->
            "Never seen a seventh with interval " ++ toString int ++ "before "


calculateChordDistance : List Note -> ( Int, Int, Int )
calculateChordDistance notes =
    case notes of
        root :: third :: fifth :: seventh :: [] ->
            chordDistances ( root, third, fifth, seventh )

        _ ->
            ( 4, 7, 11 )


chordDistances : ( Note, Note, Note, Note ) -> ( Int, Int, Int )
chordDistances ( root, third, fifth, seventh ) =
    List.map (calculateInterval root) [ third, fifth, seventh ]
        |> pluckResults


pluckResults : List Int -> ( Int, Int, Int )
pluckResults lsInts =
    case lsInts of
        thirdInterval :: fifthInterval :: seventhInternal :: [] ->
            ( thirdInterval, fifthInterval, seventhInternal )

        _ ->
            ( 4, 7, 11 )


calculateInterval : Note -> Note -> Int
calculateInterval root note =
    chromaticScale
        |> List.repeat 2
        |> List.concat
        |> LE.dropWhile ((/=) root)
        |> LE.takeWhile ((/=) note)
        |> Debug.log "See the interval scale"
        |> List.length


makeArpeggioNotes : List String -> List (Html Msg)
makeArpeggioNotes humanNotes =
    humanNotes
        |> flip takeChordTones "C"
        |> List.map (\s -> div [] [ text s ])


takeChordTones : List a -> a -> List a
takeChordTones allScalarNotes default =
    let
        giveMeTheNote i =
            LE.getAt i allScalarNotes
                |> Maybe.withDefault default
    in
        [ 0, 2, 4, 6 ]
            |> List.map giveMeTheNote


rootNoteDropdown : Model -> Html Msg
rootNoteDropdown model =
    complexDropDownMaker "Please select a root note" listOfUserNotes UpdateRoot toString showUserNote


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


complexDropDownMaker : String -> List a -> (a -> msg) -> (a -> String) -> (a -> String) -> Html msg
complexDropDownMaker labelText stuff msg toId toLabel =
    div
        []
        [ label
            []
            [ text labelText ]
        , Select.from_ stuff msg toId toLabel
        ]


displayScale : Model -> Html Msg
displayScale model =
    div
        []
        (displayScaleElements model)


displayScaleElements : Model -> List (Html Msg)
displayScaleElements model =
    model
        |> .humanScale
        |> List.map (\s -> div [] [ text s ])


makeScaleElements : Model -> List String
makeScaleElements model =
    model
        |> makeScale
        |> List.map (computerNoteToUserNote model.accidentalBehavior)
        |> List.map showUserNote
        |> enharmonicsSuck


enharmonicsSuck : List String -> List String
enharmonicsSuck notes =
    case notes of
        "D#" :: "F" :: "F#" :: tail_ ->
            "D#" :: "E#" :: enharmonicsSuck ("F#" :: tail_)

        "A#" :: "C" :: "C#" :: tail_ ->
            "A#" :: "B#" :: enharmonicsSuck ("C#" :: tail_)

        "Bb" :: "B" :: "Db" :: tail_ ->
            "Bb" :: "Cb" :: enharmonicsSuck ("Db" :: tail_)

        "Eb" :: "E" :: "Gb" :: tail_ ->
            "Eb" :: "Fb" :: enharmonicsSuck ("Gb" :: tail_)

        a :: tail_ ->
            (a :: enharmonicsSuck tail_)

        [] ->
            []


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
