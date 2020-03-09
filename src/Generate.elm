module Generate exposing (both, bothWithImports, decoders, encoders, imports)

import Decoder exposing (decoder)
import Destructuring exposing (bracket, stringify)
import Encoder exposing (encoder)
import List exposing (concat, filter, map, sortBy)
import ParseModules
import ParseType exposing (extractAll, extractAllWithDefs)
import String exposing (contains, join, trim)
import Types exposing (ExtraPackage)

--== Generate encoders/decoders from a list of .elm files ==--
    -- the head of the list is the base module, the rest are its depencies
    -- types defined in the tail are only decoded/encoded if they are
    -- imported by the head, or by something imported by the head etc.




bothWithImports : ExtraPackage -> List String -> String
bothWithImports extra txt =
    decodersWithImports extra txt ++ "\n\n" ++ encodersWithImports txt


decodersWithImports : ExtraPackage -> List String -> String
decodersWithImports extra sources =
    stringify <|
        ( map (decoder extra [])
            <| sortBy .name
                <| ParseModules.parseAll False sources
        )

encodersWithImports : List String -> String
encodersWithImports sources =
    stringify <|
        ( map encoder
            <| sortBy .name
                <| ParseModules.parseAll True sources
        )


--== Generate encoders/decoders from a single .elm file or a bunch of type definitions ==--

{-| Generete encoders/decoders for some source code

    import Types exposing (ExtraPackage(..))

    both Pipeline "type RecursiveExample = RecursiveExample RecursiveData\ntype alias RecursiveData = RecursiveExample"
    --> """decodeRecursiveData =
    -->         Decode.lazy (\\_ ->
    -->            decodeRecursiveExample
    -->         )\n
    -->      decodeRecursiveExample =
    -->         Decode.map RecursiveExample decodeRecursiveData\n
    -->      encodeRecursiveData a =
    -->         encodeRecursiveExample a\n
    -->      encodeRecursiveExample (RecursiveExample a1) =
    -->         encodeRecursiveData a1"""
    -->         |> String.replace "                     " "" -- adjust to formatting
    -->         |> String.replace "                    " "" -- adjust to formatting
    -->         |> String.replace "                   " "" -- adjust to formatting

    both Pipeline "type Parameterized a = Parameterized a"
    --> """decodeParameterized decodeA =
    -->         Decode.map Parameterized decodeA\n
    -->      encodeParameterized encodeA (Parameterized a1) =
    -->         encodeA a1"""
    -->         |> String.replace "                     " "" -- adjust to formatting
    -->         |> String.replace "                    " "" -- adjust to formatting
    -->         |> String.replace "                   " "" -- adjust to formatting

    both Pipeline "type RecursiveExample = RecursiveExample RecursiveData RecursiveData2\ntype alias RecursiveData2 = RecursiveExample\ntype alias RecursiveData = RecursiveExample"
    --> """decodeRecursiveData =
    -->         decodeRecursiveExample\n
    -->      decodeRecursiveData2 =
    -->         decodeRecursiveExample\n
    -->      decodeRecursiveExample =
    -->         Decode.lazy (\\_ ->
    -->            Decode.map2
    -->               RecursiveExample
    -->                  ( Decode.field \"A1\" decodeRecursiveData )
    -->                  ( Decode.field \"A2\" decodeRecursiveData2 )
    -->         )\n
    -->      encodeRecursiveData a =
    -->         encodeRecursiveExample a\n
    -->      encodeRecursiveData2 a =
    -->         encodeRecursiveExample a\n
    -->      encodeRecursiveExample (RecursiveExample a1 a2) =
    -->         Encode.object
    -->            [ (\"A1\", encodeRecursiveData a1)
    -->            , (\"A2\", encodeRecursiveData2 a2)
    -->            ]"""
    -->         |> String.replace "                     " "" -- adjust to formatting
    -->         |> String.replace "                    " "" -- adjust to formatting
    -->         |> String.replace "                   " "" -- adjust to formatting

    both Pipeline "type alias TunaOrTofu = Tuna | Tofu"
    --> """decodeTunaOrTofu =
    -->         let
    -->            recover x =
    -->               case x of
    -->                  \"Tuna\"->
    -->                     Decode.succeed Tuna
    -->                  \"Tofu\"->
    -->                     Decode.succeed Tofu
    -->                  other->
    -->                     Decode.fail <| \"Unknown constructor for type TunaOrTofu: \" ++ other
    -->         in
    -->            Decode.string |> Decode.andThen recover\n
    -->     encodeTunaOrTofu a =
    -->         case a of
    -->            Tuna ->
    -->               Encode.string \"Tuna\"
    -->            Tofu ->
    -->               Encode.string \"Tofu\""""
    -->         |> String.replace "                     " "" -- adjust to formatting
    -->         |> String.replace "                    " "" -- adjust to formatting

    both Pipeline "type alias QualifiedExample = SomeModule.SomeType"

    --> """decodeQualifiedExample =
    -->         SomeModule.decodeSomeType\n
    -->    encodeQualifiedExample a =
    -->         SomeModule.encodeSomeType a"""
    -->         |> String.replace "                     " "" -- adjust to formatting
    -->         |> String.replace "                    " "" -- adjust to formatting
    -->         |> String.replace "                   " "" -- adjust to formatting

    both Pipeline "type alias ListWithAnonymousType = List (Id.Id, String)"

    --> """decodeListWithAnonymousType =
    -->         Decode.list decodeTuple_Id_Id_String_\n
    -->    decodeTuple_Id_Id_String_ =
    -->         Decode.map2
    -->            (\\a1 a2 -> (a1, a2))
    -->               ( Decode.field "A1" Id.decodeId )
    -->               ( Decode.field "A2" Decode.string )\n
    -->    encodeListWithAnonymousType a =
    -->         (Encode.list encodeTuple_Id_Id_String_) a\n
    -->    encodeTuple_Id_Id_String_ (a1, a2) =
    -->         Encode.object
    -->            [ ("A1", Id.encodeId a1)
    -->            , ("A2", Encode.string a2)
    -->            ]"""
    -->         |> String.replace "                     " "" -- adjust to formatting
    -->         |> String.replace "                    " "" -- adjust to formatting
    -->         |> String.replace "                   " "" -- adjust to formatting

    both Pipeline "type alias Record = {a : Int, b : ImportedType}"

    -->    """decodeRecord =
    -->         Decode.map2
    -->            Record
    -->               ( Decode.field \"a\" Decode.int )
    -->               ( Decode.field \"b\" decodeImportedType )\n
    -->    encodeRecord a =
    -->         Encode.object
    -->            [ (\"a\", Encode.int a.a)
    -->            , (\"b\", encodeImportedType a.b)
    -->            ]"""
    -->         |> String.replace "                     " "" -- adjust to formatting
    -->         |> String.replace "                    " "" -- adjust to formatting
    -->         |> String.replace "                   " "" -- adjust to formatting

    both Pipeline "type alias AList = List Qualified.ImportedType"

    -->   """decodeAList =
    -->         Decode.list Qualified.decodeImportedType\n
    -->      encodeAList a =
    -->         (Encode.list Qualified.encodeImportedType) a"""
    -->         |> String.replace "                     " "" -- adjust to formatting
    -->         |> String.replace "                    " "" -- adjust to formatting
    -->         |> String.replace "                   " "" -- adjust to formatting

    both Pipeline "type alias Rects = {items : Dict.Dict Int Rectangle.Rectangle}"

    --> """decodeDictIntRectangle_Rectangle =
    -->        let
    -->           decodeDictIntRectangle_RectangleTuple =
    -->              Decode.map2
    -->                 (\\a1 a2 -> (a1, a2))
    -->                    ( Decode.field \"A1\" Decode.int )
    -->                    ( Decode.field \"A2\" Rectangle.decodeRectangle )
    -->        in
    -->           Decode.map Dict.fromList (Decode.list decodeDictIntRectangle_RectangleTuple)\n
    -->     decodeRects =
    -->        Decode.map
    -->           Rects
    -->              ( Decode.field \"items\" decodeDictIntRectangle_Rectangle )\n
    -->     encodeDictIntRectangle_Rectangle a =
    -->        let
    -->           encodeDictIntRectangle_RectangleTuple (a1,a2) =
    -->              Encode.object
    -->                 [ (\"A1\", Encode.int a1)
    -->                 , (\"A2\", Rectangle.encodeRectangle a2) ]
    -->        in
    -->           (Encode.list encodeDictIntRectangle_RectangleTuple) (Dict.toList a)\n
    -->     encodeRects a =
    -->        Encode.object
    -->           [ (\"items\", encodeDictIntRectangle_Rectangle a.items)
    -->           ]"""
    -->         |> String.replace "                    " "" -- adjust to formatting
    -->         |> String.replace "                   " "" -- adjust to formatting

Note that the last two lines are to make the expected outcome more readable
-}
both : ExtraPackage -> String -> String
both extra txt =
    decoders extra txt ++ "\n\n" ++ encoders txt


decoders : ExtraPackage -> String -> String
decoders extra txt =
    let
        { topLevel, anonymous, toLazify} =
            extractAllWithDefs False txt
    in
    stringify <|
        anonymous
            ++ (map (decoder extra toLazify)
                <| sortBy .name topLevel)


encoders : String -> String
encoders txt =
    let
        allTypes =
            extractAll True txt
    in
    stringify <|
        (map encoder <| sortBy .name allTypes)



--== Decoder/encoder imports ==--


imports output =
    let
        inOutput a =
            contains a output

        maybe modName maybeNick =
            let
                modRef =
                    case maybeNick of
                        Nothing ->
                            modName

                        Just nick ->
                            nick

                relevant =
                    inOutput (modRef ++ ".")
            in
            case relevant of
                True ->
                    case maybeNick of
                        Nothing ->
                            [ "import " ++ modName ]

                        Just nick ->
                            [ "import " ++ modName ++ " as " ++ nick ]

                False ->
                    []

        importDict =
            maybe "Dict" Nothing

        importDec =
            maybe "Json.Decode" (Just "Decode")

        importDecExtra =
            maybe "Json.Decode.Extra" (Just "Extra")

        importDecPipe =
            maybe "Json.Decode.Pipeline" (Just "Pipeline")

        importEnc =
            maybe "Json.Encode" (Just "Encode")
    in
    join "\n" <|
        concat
            [ importDict
            , importDec
            , importDecExtra
            , importDecPipe
            , importEnc
            , [ "\n" ]
            ]

