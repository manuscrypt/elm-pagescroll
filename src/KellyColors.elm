module KellyColors exposing (..)

import Color exposing (Color)
import Maybe exposing (..)
import List.Extra exposing (..)

type alias KellyDef = { isccNbsNr: Int, hexString: String, r: Int, g: Int, b: Int }

kellyColors : List KellyDef
kellyColors = [ KellyDef 263 "F2F3F4" 242 243 244
              , KellyDef 267 "222222" 34  34  34
              , KellyDef 82  "F3C300" 243 195 0
              , KellyDef 218 "875692" 135 86  146
              , KellyDef 48  "F38400" 243 132 0
              , KellyDef 180 "A1CAF1" 161 202 241
              , KellyDef 11  "BE0032" 190 0 50
              , KellyDef 90  "C2B280" 194 178 128
              , KellyDef 265 "848482" 132 132 130
              , KellyDef 139 "008856" 0 136 86
              , KellyDef 247 "E68FAC" 230 143 172
              , KellyDef 178 "0067A5" 0 103 165
              , KellyDef 26  "F99379" 249 147 121
              , KellyDef 207 "604E97" 96  78  151
              , KellyDef 66  "F6A600" 246 166 0
              , KellyDef 255 "B3446C" 179 68  108
              , KellyDef 97  "DCD300" 220 211 0
              , KellyDef 40  "882D17" 136 45  23
              , KellyDef 115 "8DB600" 141 182 0
              , KellyDef 75  "654522" 101 69  34
              , KellyDef 34  "E25822" 226 88  34
              , KellyDef 126 "2B3D26" 43  61  38
              ]

toColor: KellyDef->Color
toColor kd = Color.rgb kd.r kd.g kd.b

at: Int->Color
at i = Maybe.withDefault Color.black <| Maybe.map toColor <| getAt (i % List.length kellyColors) kellyColors