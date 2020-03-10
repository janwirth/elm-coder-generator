module E2eTest exposing (..)
{-| A module that should not change at all when running the generator over it -}
import Html exposing (text)
import Dict
import Json.Decode as Decode
import Json.Encode as Encode

main = text "Success!"

-- [generator-start]
type Id = Id Int
type Dict_ a = Dict_ (Dict.Dict Int a)

-- [generator-generated-start] -- DO NOT MODIFY or remove this line
decodeDictInt_ParamA_ decodeA =
   let
      decodeDictInt_ParamA_Tuple =
         Decode.map2
            (\a1 a2 -> (a1, a2))
               ( Decode.field "A1" Decode.int )
               ( Decode.field "A2" decodeA )
   in
      Decode.map Dict.fromList (Decode.list decodeDictInt_ParamA_Tuple)

decodeDict_ decodeA =
   Decode.map Dict_ (decodeDictInt_ParamA_ decodeA)

decodeId =
   Decode.map Id Decode.int

encodeDictInt_ParamA_ encodeA a =
   let
      encodeDictInt_ParamA_Tuple (a1,a2) =
         Encode.object
            [ ("A1", Encode.int a1)
            , ("A2", encodeA a2) ]
   in
      (Encode.list encodeDictInt_ParamA_Tuple) (Dict.toList a)

encodeDict_ encodeA (Dict_ a1) =
   encodeDictInt_ParamA_ encodeA a1

encodeId (Id a1) =
   Encode.int a1 
-- [generator-end]
