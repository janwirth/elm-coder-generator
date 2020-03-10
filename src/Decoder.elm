module Decoder exposing
    ( decoder
    -- exposed for testing only
    , decoderOpaque
    , decoderHelp
    )

import Destructuring exposing (bracket, bracketIfSpaced, capitalize, quote, replaceColons, tab, tabLines)
import List exposing (concat, filter, indexedMap, length, map, map2, range)
import Generate.Type
import String exposing (join, split)
import Types exposing (ExtraPackage(..), Type(..), TypeDef, coreType)


decoder : ExtraPackage -> List String -> TypeDef -> List String
decoder extra toLazify typeDef =
    let
        shouldLazify = List.member typeDef.name toLazify
        decoderBody =
            case typeDef.theType of
                TypeUnion _ ->
                    decoderBodyRaw

                _ ->
                    map (tab 1) decoderBodyRaw

        decoderBodyRaw =
            split "\n" <| decoderHelp True typeDef.name typeDef.theType extra

        decoderName =
            "decode" ++ replaceColons typeDef.name ++ makeParameters typeDef ++ " ="
        lazify body =
            case List.member typeDef.name toLazify of
                True -> (tab 1 "Decode.lazy (\\_ ->") :: (map (tab 1) body) ++ [tab 1 ")"]
                False -> body
    in
    decoderName :: (lazify decoderBody)


makeParameters : TypeDef -> String
makeParameters def =
    Types.getParameters def.theType
    |> List.map (\param -> " decode" ++ capitalize param)
    |> String.join " "

{-
    import Types
    decoderHelp True --> ""
-}

{-|

    import Types exposing (Type(..))

    decoderHelp False "" (TypeDict (TypeInt,TypeParameter "a")) Types.Pipeline
    --> """decodeDictInt_ParamA_ decodeA"""

    decoderHelp True "Dict_" (TypeDict (TypeInt,TypeParameter "para")) Types.Pipeline
    --> """let
    -->      decodeDict_Tuple =
    -->         Decode.map2
    -->            (\\a1 a2 -> (a1, a2))
    -->               ( Decode.field \"A1\" Decode.int )
    -->               ( Decode.field \"A2\" decodePara )
    -->   in
    -->      Decode.map Dict.fromList (Decode.list decodeDict_Tuple)"""
    -->         |> String.replace "                  " "" -- adjust to formatting

-}
decoderHelp : Bool -> String -> Type -> ExtraPackage -> String
decoderHelp topLevel rawName a extra =
    let
        recurseOn x y =
            x ++ " " ++ (bracketIfSpaced <| decoderHelp False "" y extra)
            
        name =
            replaceColons rawName
    in
    case a of
        TypeParameter parameter ->
            "decode" ++ capitalize parameter
        TypeArray b ->
            recurseOn "Decode.array" b

        TypeBool ->
            "Decode.bool"

        TypeDict ( b, c ) ->
            case topLevel of
                True ->
                    let
                        subDecoderName =
                            "decode" ++ name ++ "Tuple"
                        subDecoder =
                            decoderHelp True "" (TypeTuple [ b, c ]) extra
                    in
                    join "\n"
                        [ "let"
                        , tab 1 <| subDecoderName ++ " ="
                        , tabLines 2 <| subDecoder
                        , "in"
                        , tab 1 <| "Decode.map Dict.fromList (Decode.list " ++ subDecoderName ++ ")"
                        ]

                False ->
                    let
                        name_ =
                            replaceColons <| case name of
                                "" -> Generate.Type.identifier a
                                _ -> name
                    in
                    case c of
                        TypeParameter parameter ->
                            "decode" ++ name_ ++ " decode" ++ capitalize parameter ++ ""
                        _ -> "decode" ++ name_

        TypeError b ->
            b

        TypeExtendedRecord b ->
            case topLevel of
                True ->
                    case name of
                        "" ->
                            decoderRecord (Generate.Type.identifier a) b extra

                        _ ->
                            decoderRecord (name ++ "Extended") b extra

                False ->
                    case name of
                        "" ->
                            "decode" ++ Generate.Type.identifier a

                        _ ->
                            "decode" ++ name ++ "Extended"

        TypeExtensible _ ->
            "<< Extensible records are not decoded. >>"

        TypeFloat ->
            "Decode.float"

        TypeCustom importedTypeReference parameters ->
            case String.split "." (importedTypeReference) |> List.reverse of
                typeName :: reversedPath ->
                    let
                        path =
                            case reversedPath of
                                [] -> ""
                                _ -> String.join "." (List.reverse reversedPath) ++ "."
                    in
                    path ++ "decode" ++ typeName
                _ ->
                    "decode" ++ name


        TypeInt ->
            "Decode.int"

        TypeList b ->
            recurseOn "Decode.list" b

        TypeMaybe b ->
            recurseOn "Decode.maybe" b

        TypeOpaque b ->
            case topLevel of
                True ->
                    decoderOpaque True b extra

                False ->
                    case name of
                        "" ->
                                "decode" ++ Generate.Type.identifier a

                        _ ->
                            "decode" ++ name

        TypeRecord b ->
            case topLevel of
                True ->
                    case name of
                        "" ->
                            decoderRecord (Generate.Type.identifier a) b extra

                        _ ->
                            decoderRecord name b extra

                False ->
                    case name of
                        "" ->
                            "decode" ++ Generate.Type.identifier a

                        _ ->
                            "decode" ++ name

        TypeString ->
            "Decode.string"

        TypeTuple bs ->
            case topLevel of
                True ->
                    decoderTuple bs extra

                False ->
                    case name of
                        "" ->
                            "decode" ++ Generate.Type.identifier a

                        _ ->
                            "decode" ++ name

        TypeUnion b ->
            case topLevel of
                True ->
                    decoderUnion name b extra

                False ->
                    case name of
                        "" ->
                            let
                                folder x y =
                                    x ++ ", " ++ y

                                typeStr =
                                    "{ " ++ (List.foldl folder "" <| map Tuple.first b) ++ " }"
                            in
                            "Decoder parse error: anonymous union type: " ++ typeStr

                        _ ->
                            "decode" ++ name


{-|

    import Types exposing (Type(..))

    decoderOpaque True ("Dict_", [TypeDict (TypeInt,TypeParameter "a")]) Types.Pipeline
    --> "Decode.map Dict_ (decodeDictInt_ParamA_ decodeA)"

-}
decoderOpaque : Bool -> ( String, List Type ) -> ExtraPackage -> String
decoderOpaque productType ( constructor, subTypes ) extra =
    let
        fieldDecode ( a, b ) =
            field varNum (quote <| capitalize a) (subDecoder b) extra

        fieldDefs =
            map2 (\a b -> ( a, b )) vars subTypes

        subDecoder a =
            a
            |> \a_ -> decoderHelp False "" a_ extra
            |> bracketIfSpaced

        vars =
            map var <| range 1 varNum

        varNum =
            length subTypes

        simpleDecoder x =
            "Decode.map " ++ constructor ++ " " ++ subDecoder x

        complexDecoder =
            join "\n" <|
                [ mapper varNum
                , tab 1 constructor
                ]
                    ++ (map (tab 2) <| map fieldDecode fieldDefs)
    in
    case subTypes of
        [] ->
            "Decode.succeed " ++ constructor

        x :: [] ->
            if productType then
                simpleDecoder x

            else
                complexDecoder

        _ ->
            complexDecoder


decoderRecord : String -> List TypeDef -> ExtraPackage -> String
decoderRecord name xs extra =
    let

        subDecoder x =
            decoderHelp False "" x extra
            |> bracketIfSpaced

        fieldDecode x =
            field fieldNum (quote x.name) (subDecoder x.theType) extra

        fieldNum =
            length xs
    in
    join "\n" <|
        [ mapper fieldNum
        , tab 1 name
        ]
            ++ (map (tab 2) <| map fieldDecode xs)


decoderTuple : List Type -> ExtraPackage -> String
decoderTuple xs extra =
    let
        component ( idx, elem ) =
            tab 2 <| field varNum (quote <| varUpper <| idx + 1) (subDecoder elem) extra

        toTuple =
            "(\\" ++ varList ++ " -> " ++ varListComma ++ ")"

        subDecoder x =
            bracketIfSpaced <| decoderHelp False "" x extra

        vars =
            map var <| range 1 varNum

        varList =
            join " " vars

        varListComma =
            "(" ++ join ", " vars ++ ")"

        varNum =
            length xs
    in
    mapper varNum
        ++ "\n"
        ++ tab 1 toTuple
        ++ "\n"
        ++ (join "\n" <| map component <| indexedMap Tuple.pair xs)


decoderUnion : String -> List ( String, List Type ) -> ExtraPackage -> String
decoderUnion name xs extra =
    let
        complexConstructor ( a, b ) =
            b /= []

        simpleUnion =
            filter complexConstructor xs == []
    in
    case simpleUnion of
        True ->
            decoderUnionSimple name xs

        False ->
            decoderUnionComplex name xs extra



--e.g. type Color = Red | Green | Blue


decoderUnionSimple : String -> List ( String, List Type ) -> String
decoderUnionSimple name xs =
    let
        constructor ( a, b ) =
            (quote a ++ "->\n") ++ (tab 2 <| "Decode.succeed " ++ a)
    in
    join "\n" <|
        map (tab 1) <|
            [ "let", tab 1 "recover x =", tab 2 "case x of" ]
                ++ (map (tabLines 3) <| map constructor xs)
                ++ [ tab 3 "other->", tab 4 "Decode.fail <| \"Unknown constructor for type " ++ name ++ ": \" ++ other" ]
                ++ [ "in", tab 1 "Decode.string |> Decode.andThen recover" ]



--e.g. type Ammo = Bullets Int | Napalm Float


decoderUnionComplex : String -> List ( String, List Type ) -> ExtraPackage -> String
decoderUnionComplex name xs extra =
    let
        decodeConstructor ( constructor, fields ) =
            quote constructor ++ " ->\n" ++ (tabLines 1 <| decoderOpaque False ( constructor, fields ) extra)
    in
    join "\n" <|
        [ tab 1 <| "Decode.field \"Constructor\" Decode.string |> Decode.andThen decode" ++ replaceColons name ++ "Help" ++ "\n"
        , "decode" ++ replaceColons name ++ "Help constructor ="
        , tab 1 "case constructor of"
        ]
            ++ (map (tabLines 2) <| map decodeConstructor xs)
            ++ [ tab 2 "other->"
               , tab 3 <| "Decode.fail <| \"Unknown constructor for type " ++ name ++ ": \" ++ other"
               ]


field : Int -> String -> String -> ExtraPackage -> String
field n name dec extra =
    case n < 9 of
        True ->
            bracket <| " Decode.field " ++ name ++ " " ++ dec ++ " "

        False ->
            case extra of
                Extra ->
                    "|> Extra.andMap (Decode.field " ++ name ++ " " ++ dec ++ ")"

                Pipeline ->
                    "|> Pipeline.required " ++ name ++ " " ++ dec


mapper : Int -> String
mapper n =
    case n < 9 of
        True ->
            let
                suffix =
                    if n == 1 then
                        ""

                    else
                        String.fromInt n
            in
            "Decode.map" ++ suffix

        False ->
            "Decode.succeed"

var : Int -> String
var n =
    "a" ++ String.fromInt n


varOk : String -> String
varOk a =
    a ++ "_"


varUpper n =
    "A" ++ String.fromInt n
