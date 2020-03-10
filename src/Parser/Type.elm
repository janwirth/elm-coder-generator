module Parser.Type exposing (..)
import Parser exposing (..)
import Pratt
import Set

type Type =
      TypeAlias TypeExpression
    | UnionType Union

type alias ConstructorName = String

type alias Union =
    List Constructor

type alias TypeDef =
    { name : String
    , parameters : List String
    , theType : Type
    }
typeDef : Parser TypeDef
typeDef = oneOf [aliasDef, unionType]

type TypeExpression
    -- primitives
    = TypeBool
    | TypeInt
    | TypeFloat
    | TypeString

    | TypeList Type
    | TypeMaybe Type
    | TypeTuple (List Type)

    -- non Core.Basics
    | TypeDict ( Type, Type )
    | TypeArray Type

    -- records
    | TypeRecord (List (String, Type))
    | TypeExtendedRecord (List Type) --record defined using an extensible one
    | TypeExtensible (List Type) --extensible record

    -- special
    | TypeCustom String (List Type) --type not core and not defined in the input
    | TypeParameter String

{-|
    import Parser
    Parser.run typeExpression "String" --> Ok TypeString
-}
typeExpression : Parser TypeExpression
typeExpression =
    oneOf
    [ parenthesizedExpression
    , typeExpressions
    ]

typeExpressions : Parser TypeExpression
typeExpressions =
    oneOf [
      succeed TypeBool |. keyword "Bool"
    , succeed TypeInt |. keyword "Int"
    , succeed TypeFloat |. keyword "Float"
    , succeed TypeString |. keyword "String"
    ]

parenthesizedExpression =
    succeed identity
        |. symbol "("
        |= lazy (\_ -> typeExpression)
        |. symbol ")"


{-|
    import Parser exposing (..)

    run typeDef "type alias A = String" --> Ok {name = "A", parameters = [], theType = TypeAlias TypeString}
-}
aliasDef : Parser.Parser TypeDef
aliasDef =
    Parser.succeed makeAlias
    |. Parser.keyword "type"
    |. Parser.spaces
    |. Parser.keyword "alias"
    |. Parser.spaces
    |= typeName
    |. Parser.spaces
    |= parameters
    |. Parser.spaces
    |. Parser.keyword "="
    |. Parser.spaces
    |= typeExpression


unionType : Parser.Parser TypeDef
unionType =
    Parser.succeed makeUnion
    |. Parser.keyword "type"
    |. Parser.spaces
    |= typeName
    |= parameters
    |. Parser.keyword "="
    |. Parser.spaces
    |= union


makeUnion : String -> List String -> Union -> TypeDef
makeUnion name parameters_ parsedExpression =
    { name = name
    , parameters = parameters_
    , theType = UnionType parsedExpression
    }

makeAlias : String -> List String -> TypeExpression -> TypeDef
makeAlias name parameters_ parsedExpression =
    { name = name
    , parameters = parameters_
    , theType = TypeAlias parsedExpression
    }

union : Parser Union
union =
    Parser.sequence
    { start = ""
    , separator = "|"
    , end = ""
    , spaces = Parser.spaces
    , item = constructor
    , trailing = Forbidden
    }

constructor : Parser Constructor
constructor =
    succeed Constructor
    |= typeName
    |= tags

tags : Parser (List TypeExpression)
tags =
    sequence { start = ""
    , separator = " "
    , end = " "
    , spaces = Parser.spaces
    , item = typeExpression
    , trailing = Parser.Mandatory
    }


type alias Constructor = {name : String, tags : List TypeExpression}

typeName : Parser.Parser String
typeName =
  Parser.variable
    { start = Char.isUpper
    , inner = \c -> Char.isAlphaNum c || c == '_'
    , reserved = Set.fromList [ "let", "in", "case", "of" ]
    }

typeParameterName : Parser.Parser String
typeParameterName =
  Parser.variable
    { start = Char.isLower
    , inner = \c -> Char.isAlphaNum c || c == '_'
    , reserved = Set.fromList [ "let", "in", "case", "of" ]
    }

parameters : Parser (List String)
parameters =
    Parser.sequence
    { start = ""
    , separator = " "
    , end = " "
    , spaces = Parser.spaces
    , item = typeParameterName
    , trailing = Parser.Mandatory
    }
