module Src.Types exposing (..)

--------------------------------------------------------------------
--                          TYPES
--------------------------------------------------------------------


type alias Model =
    { currentRoot : UserNote
    , computerNote : Note
    , currentScale : Scale
    , accidentalBehavior : AccidentalClass
    }


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


type UserNote
    = ANatural
    | ASharp
    | BFlat
    | BNatural
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



--------------------------------------------------------------------
--                     ENUMERARTION OF TYPES
--------------------------------------------------------------------


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


listOfUserNotes : List UserNote
listOfUserNotes =
    [ ANatural
    , BFlat
    , BNatural
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
    , BNatural
    , CSharp
    , DNatural
    , ENatural
    , FSharp
    , GNatural
    , AFlat
    ]



--------------------------------------------------------------------
--                        AD-HOC TYPECLASSES
--------------------------------------------------------------------


showUserNote : UserNote -> String
showUserNote userNote =
    case userNote of
        ANatural ->
            "A"

        ASharp ->
            "A#"

        BFlat ->
            "Bb"

        BNatural ->
            "B"

        CNatural ->
            "C"

        CSharp ->
            "C#"

        DFlat ->
            "Db"

        DNatural ->
            "D"

        DSharp ->
            "D#"

        EFlat ->
            "Eb"

        ENatural ->
            "E"

        FNatural ->
            "F"

        FSharp ->
            "F#"

        GFlat ->
            "Gb"

        GNatural ->
            "G"

        GSharp ->
            "G#"

        AFlat ->
            "Ab"



--------------------------------------------------------------------
--                   CONVERSIONS BETWEEN TYPES
--------------------------------------------------------------------


accidentalFromUserNote : UserNote -> AccidentalClass
accidentalFromUserNote userNote =
    let
        isSharp =
            List.member userNote listOfSharpAccidentals

        isFlat =
            List.member userNote listOfFlatAccidentals
    in
        Debug.log "THis is the accidentalFromUserNote" ( userNote, isSharp, isFlat )
            |> \_ ->
                case ( isSharp, isFlat ) of
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

        BNatural ->
            Note B

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


computerNoteToUserNote : AccidentalClass -> Note -> UserNote
computerNoteToUserNote accidentalBehavior note =
    Debug.log "computerNoteToUserNote" ( accidentalBehavior, note )
        |> \_ ->
            case ( note, accidentalBehavior ) of
                ( Note a, _ ) ->
                    convertNaturalPitchToUserNote a

                ( Accidental a b, Sharp_ ) ->
                    convertSharpPitchToUserNote a

                ( Accidental a b, Flat_ ) ->
                    convertFlatPitchToUserNote b

                _ ->
                    -- This can't happen. Dependent types would be nice for this sort of thing.
                    CNatural


convertNaturalPitchToUserNote : Pitch -> UserNote
convertNaturalPitchToUserNote pitch =
    case pitch of
        A ->
            ANatural

        B ->
            BNatural

        C ->
            CNatural

        D ->
            DNatural

        E ->
            ENatural

        F ->
            FNatural

        G ->
            GNatural


convertSharpPitchToUserNote : Pitch -> UserNote
convertSharpPitchToUserNote pitch =
    case pitch of
        A ->
            ASharp

        C ->
            CSharp

        D ->
            DSharp

        F ->
            FSharp

        G ->
            GSharp

        _ ->
            CNatural


convertFlatPitchToUserNote : Pitch -> UserNote
convertFlatPitchToUserNote pitch =
    case pitch of
        A ->
            AFlat

        B ->
            BFlat

        D ->
            DFlat

        E ->
            EFlat

        G ->
            GFlat

        _ ->
            CNatural
