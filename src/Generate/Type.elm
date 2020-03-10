module Generate.Type exposing (identifier, descriptor, aliasDefinitions )
import Types exposing (TypeDef, Type(..))
import Destructuring exposing 
    ( civilize, bracketIfSpaced, capitalize)

aliasDefinitions : List TypeDef -> List (List String)
aliasDefinitions types =
    let
        name a =
            case a.theType of
                TypeExtendedRecord _ ->
                    a.name ++ "Extended"

                _ ->
                    a.name

        def a =
            [ "type alias " ++ name a ++ " = " ++ descriptor True a.theType ]
    in
    List.map def types



-- @TODO: put this in a separate module
{-| Derive a valid elm identifier from a type definition -}
identifier : Type -> String
identifier theType =
    let
        tag prefix =
            prefix ++ (civilize <| descriptor False theType)
    in
    case theType of
        TypeExtendedRecord _ ->
            tag "Record"

        TypeRecord _ ->
            tag "Record"

        TypeTuple _ ->
            tag "Tuple"

        _ ->
            tag ""



descriptor : Bool -> Type -> String
descriptor bracketIt a =
    let
        wrap x =
            if bracketIt then
                "(" ++ x ++ ")"

            else
                x
    in
    case a of
        TypeParameter parameter -> "Param " ++ capitalize parameter
        TypeArray b ->
            wrap <| "Array " ++ descriptor True b

        TypeBool ->
            "Bool"

        TypeDict ( b, c ) ->
            "Dict " ++ (bracketIfSpaced <| descriptor False b) ++ " " ++ (bracketIfSpaced <| descriptor False c)

        TypeError b ->
            b

        TypeExtendedRecord extendedRecordDefinition ->
            --same as TypeRecord
            let
                fieldString x =
                    x.name ++ ": " ++ descriptor False x.theType ++ ", "

                fields =
                    String.dropRight 2 <| String.concat <| List.map fieldString extendedRecordDefinition
            in
            "{" ++ fields ++ "}"

        TypeExtensible b ->
            let
                fieldString x =
                    x.name ++ ": " ++ descriptor False x.theType ++ ", "

                fields =
                    String.dropRight 2 <| String.concat <| List.map fieldString b
            in
            "{ a | " ++ fields ++ "}"

        TypeFloat ->
            "Float"        
        
        TypeCustom b params ->
            b ++ (String.join "_" (List.map (descriptor False) params))

        TypeInt ->
            "Int"

        TypeList b ->
            wrap <| "List " ++ descriptor True b

        TypeMaybe b ->
            wrap <| "Maybe " ++ descriptor True b

        TypeOpaque ( b, c ) ->
            case c of
                [] ->
                    b

                _ ->
                    b ++ " " ++ (String.concat <| List.map (descriptor True) c)

        TypeRecord b ->
            let
                fieldString x =
                    x.name ++ ": " ++ descriptor False x.theType ++ ", "

                fields =
                    String.dropRight 2 <| String.concat <| List.map fieldString b
            in
            "{" ++ fields ++ "}"

        TypeString ->
            "String"

        TypeTuple bs ->
            "(" ++ (String.join ", " <| List.map (descriptor False) bs) ++ ")"

        TypeUnion b ->
            let
                constructorString ( x, y ) =
                    case y of
                        [] ->
                            x ++ " | "

                        _ ->
                            x ++ " " ++ (String.concat <| List.map (descriptor True) y) ++ " | "

                constructors =
                    String.dropRight 2 <| String.concat <| List.map constructorString b
            in
            constructors

