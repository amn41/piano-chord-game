module Piano.Chords where

import Piano.Types (ChordShape, Fingering, unfingered)
import Data.Map (Map, fromFoldable)
import Data.Tuple (Tuple(..))

type ChordMap = Map String Fingering


-- major triads

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

-- minor triads

cMinorTriad :: Fingering
cMinorTriad = [0, 3, 7] -- C, E♭, G

chordMap :: ChordMap
chordMap =
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
    , Tuple "Cm" cMinorTriad
    ]
