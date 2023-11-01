module Piano.Chords where

import Piano.Types (ChordShape, Fingering, unfingered)
import Data.Map (Map, fromFoldable, union)
import Data.Tuple (Tuple(..))

type ChordMap = Map String Fingering


-- Major Triads

cMajorTriad :: Fingering
cMajorTriad = [0, 4, 7] -- C, E, G

cSharpMajorTriad :: Fingering
cSharpMajorTriad = [1, 5, 8] -- C#, F, G#

dMajorTriad :: Fingering
dMajorTriad = [2, 6, 9] -- D, F#, A

dSharpMajorTriad :: Fingering
dSharpMajorTriad = [3, 7, 10] -- D#, G, A#

eMajorTriad :: Fingering
eMajorTriad = [4, 8, 11] -- E, G#, B

fMajorTriad :: Fingering
fMajorTriad = [5, 9, 0] -- F, A, C

fSharpMajorTriad :: Fingering
fSharpMajorTriad = [6, 10, 1] -- F#, A#, C#

gMajorTriad :: Fingering
gMajorTriad = [7, 11, 2] -- G, B, D

gSharpMajorTriad :: Fingering
gSharpMajorTriad = [8, 0, 3] -- G#, C, D#

aMajorTriad :: Fingering
aMajorTriad = [9, 1, 4] -- A, C#, E

aSharpMajorTriad :: Fingering
aSharpMajorTriad = [10, 2, 5] -- A#, D, F

bMajorTriad :: Fingering
bMajorTriad = [11, 3, 6] -- B, D#, F#

majorTriads :: ChordMap
majorTriads =
  fromFoldable
    [ Tuple "C" cMajorTriad
    , Tuple "C#" cSharpMajorTriad
    , Tuple "D" dMajorTriad      
    , Tuple "D#" dSharpMajorTriad    
    , Tuple "E" eMajorTriad    
    , Tuple "F" fMajorTriad      
    , Tuple "F#" fSharpMajorTriad  
    , Tuple "G" gMajorTriad      
    , Tuple "G#" gSharpMajorTriad    
    , Tuple "A" aMajorTriad      
    , Tuple "A#" aSharpMajorTriad      
    , Tuple "B" bMajorTriad
    ]

-- Minor Triads

cMinorTriad :: Fingering
cMinorTriad = [0, 3, 7] -- C, E♭, G

cSharpMinorTriad :: Fingering
cSharpMinorTriad = [1, 4, 8] -- C#, E, G#

dMinorTriad :: Fingering
dMinorTriad = [2, 5, 9] -- D, F, A

dSharpMinorTriad :: Fingering
dSharpMinorTriad = [3, 6, 10] -- D#, F#, A#

eMinorTriad :: Fingering
eMinorTriad = [4, 7, 11] -- E, G, B

fMinorTriad :: Fingering
fMinorTriad = [5, 8, 0] -- F, A♭, C

fSharpMinorTriad :: Fingering
fSharpMinorTriad = [6, 9, 1] -- F#, A, C#

gMinorTriad :: Fingering
gMinorTriad = [7, 10, 2] -- G, A#, D

gSharpMinorTriad :: Fingering
gSharpMinorTriad = [8, 11, 3] -- G#, B, D#

aMinorTriad :: Fingering
aMinorTriad = [9, 0, 4] -- A, C, E

aSharpMinorTriad :: Fingering
aSharpMinorTriad = [10, 1, 5] -- A#, C#, F

bMinorTriad :: Fingering
bMinorTriad = [11, 2, 6] -- B, D, F#


minorTriads :: ChordMap
minorTriads = fromFoldable
  [ Tuple "Cm" cMinorTriad
  , Tuple "C#m" cSharpMinorTriad
  , Tuple "Dm" dMinorTriad
  , Tuple "D#m" dSharpMinorTriad
  , Tuple "Em" eMinorTriad
  , Tuple "Fm" fMinorTriad
  , Tuple "F#m" fSharpMinorTriad
  , Tuple "Gm" gMinorTriad
  , Tuple "G#m" gSharpMinorTriad
  , Tuple "Am" aMinorTriad
  , Tuple "A#m" aSharpMinorTriad
  , Tuple "Bm" bMinorTriad
  ]

-- Seventh (Dominant) Chords

cSeventh :: Fingering
cSeventh = [0, 4, 7, 10] -- C, E, G, B♭

cSharpSeventh :: Fingering
cSharpSeventh = [1, 5, 8, 11] -- C#, F, G#, B

dSeventh :: Fingering
dSeventh = [2, 6, 9, 0] -- D, F#, A, C

dSharpSeventh :: Fingering
dSharpSeventh = [3, 7, 10, 1] -- D#, G, A#, C#

eSeventh :: Fingering
eSeventh = [4, 8, 11, 2] -- E, G#, B, D

fSeventh :: Fingering
fSeventh = [5, 9, 0, 3] -- F, A, C, D#

fSharpSeventh :: Fingering
fSharpSeventh = [6, 10, 1, 4] -- F#, A#, C#, E

gSeventh :: Fingering
gSeventh = [7, 11, 2, 5] -- G, B, D, F

gSharpSeventh :: Fingering
gSharpSeventh = [8, 0, 3, 6] -- G#, C, D#, F#

aSeventh :: Fingering
aSeventh = [9, 1, 4, 7] -- A, C#, E, G

aSharpSeventh :: Fingering
aSharpSeventh = [10, 2, 5, 8] -- A#, D, F, G#

bSeventh :: Fingering
bSeventh = [11, 3, 6, 9] -- B, D#, F#, A

seventhChords :: ChordMap
seventhChords = fromFoldable
  [ Tuple "C7" cSeventh
  , Tuple "C#7" cSharpSeventh
  , Tuple "D7" dSeventh
  , Tuple "D#7" dSharpSeventh
  , Tuple "E7" eSeventh
  , Tuple "F7" fSeventh
  , Tuple "F#7" fSharpSeventh
  , Tuple "G7" gSeventh
  , Tuple "G#7" gSharpSeventh
  , Tuple "A7" aSeventh
  , Tuple "A#7" aSharpSeventh
  , Tuple "B7" bSeventh
  ]


chordMap :: ChordMap
chordMap = majorTriads `union` minorTriads `union` seventhChords
