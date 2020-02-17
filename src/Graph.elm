module Graph exposing (
      dependencies
    , breakingVertices
    , VertexAndAdjacencies

    -- exposed for testing only
    , break
    , occurences
    , paths
    , makeEdges
    )
{-| A graph module, currently used for detecting vertexes that are parts of cycles-}
import Dict
import List.Extra
import Types exposing (TypeDef, Type(..))
import Generate.Type
import Destructuring exposing 
    ( civilize, bracketIfSpaced)

{-| An edge from node a to node B -}
type alias Edge comparable = (comparable, comparable)

{-| A path as a list of vertices, ordered from LtR -}
type alias Path comparable = List comparable

{-| https://algs4.cs.princeton.edu/42digraph/images/adjacency-lists.png
-}
type alias AdjacencyDict comparable = Dict.Dict comparable (List comparable)

{-|
-}
type alias VertexAndAdjacencies comparable = (comparable, List comparable)

{-|
-}
onlyCycles : List (Path comparable) -> List (Path comparable)
onlyCycles =
    List.filter (\path -> List.head path == List.Extra.last path)

{-|
    import Dict

    occurences [["A", "B", "C", "A"], ["A", "E", "C", "D", "A"]]
    --> Dict.fromList [("C", 2), ("E", 1), ("B", 1), ("D", 1), ("A", 2)]
-}
occurences : (List (Path comparable)) -> Dict.Dict comparable Int
occurences items =
    items
    |> List.filterMap List.Extra.init
    |> List.concat
    |> List.foldl (\next-> Dict.update next increment) Dict.empty

{-|
Find smallest set of nodes that break all the cycles passed

    break [["A", "B", "C", "A"], ["A", "C", "A"], ["B", "C", "B"]] []
    --> ["C"]

    break [["A", "B", "C", "A"], ["A", "C", "A"], ["B", "D", "B"]] []
    --> ["B", "A"]

    break [["X", "Y", "Z", "X"], ["Y", "Z", "Y"], ["W", "Y", "W"]] []
    --> ["Y"]
-}
break : List (Path comparable) -> List comparable -> List comparable
break remainingCycles brokenNodes  =
    case remainingCycles of
        [] -> brokenNodes
        _ ->
            let
                nodeToBreak : Maybe comparable
                nodeToBreak =
                    occurences remainingCycles
                    |> Dict.toList
                    -- sort descending
                    |> List.sortBy (Tuple.second >> negate)
                    |> List.head
                    |> Maybe.map Tuple.first
            in
                case nodeToBreak of
                    Just n ->
                        let
                            -- remove all paths that contained the node
                            remain =
                                remainingCycles
                                |> List.filter (List.member n >> not)
                        in
                            break remain (n :: brokenNodes)
                    Nothing -> brokenNodes

increment : Maybe Int -> Maybe Int
increment val =
    case val of
        Nothing -> Just 1
        Just n -> Just (n + 1)

{-| Find all the

    breakingVertices [("A", ["B", "C"]), ("B", ["C"]), ("B", ["A"]), ("C", ["B"])]
    --> ["B"]

    breakingVertices [("RecursiveExample",["RecursiveData", "RecursiveData2"]),("RecursiveData2",["RecursiveExample"]),("RecursiveData",["RecursiveExample"])]
    --> ["RecursiveExample"]

    breakingVertices [("RecursiveExample",["RecursiveData"]),("RecursiveData",["RecursiveExample"])]
    --> ["RecursiveData"]
-}
breakingVertices : List (VertexAndAdjacencies comparable) -> List comparable
breakingVertices typedefs =
    let
        edges =
            typedefs
            |> List.map makeEdges
            |> List.concat
        cycles =
            edges
            |> paths
            |> onlyCycles
    in
        break cycles []

{-|
    makeEdges ("A", ["B", "C"])
    --> [("A", "B"), ("A", "C")]
-}
makeEdges : VertexAndAdjacencies comparable -> List (Edge comparable)
makeEdges (typeName, typeDeps) =
    List.map (Tuple.pair typeName) typeDeps

{-|
    paths [("A", "B"), ("B", "A")] --> [["A", "B", "A"]]
    paths [("A", "B"), ("B", "A"), ("B", "C"), ("D", "A"), ("C", "D")]
    --> [["A","B","A"],["A","B","C","D","A"],["D","A","B","A"]]
    paths [("A", "B")] --> [["A", "B"]]
-}
paths : List (Edge comparable) -> List (Path comparable)
paths edges =
    let
        init : Edge comparable -> (comparable, Path comparable)
        init (a, b) = (b, [a])
        starts = List.map init edges

        alreadyCycling (current, past) =
            List.member current past

        advance : (comparable, Path comparable) -> (comparable, comparable) -> Maybe (comparable, Path comparable)
        advance (current, past) (edgeFrom, edgeTo) =
            let
                -- we exit the recursion either if we found a cycle or a dead end
                continueConditions =
                    current == edgeFrom
                    && (not <| alreadyCycling (current, past))
            in if continueConditions
                then Just (edgeTo, current :: past)
                else Nothing

        dedupe =
            List.Extra.uniqueBy (List.Extra.init >> Maybe.withDefault [] >> List.sort)
        advanceWithAllEdges start =
            let
                possiblePathsFromHere = List.filterMap (advance start) edges
            in
                case possiblePathsFromHere of
                    [] -> [start]
                    _ ->
                        List.map advanceWithAllEdges possiblePathsFromHere
                        |> List.concat
    in
        -- for each initialized path, try to andvance the path
        List.map advanceWithAllEdges starts
        |> List.concat
        |> List.map (\(current, past) -> List.reverse (current :: past))
        |> dedupe
{-|
Get the Ids of other types
Returns a List of nickNames, which are auto-generated from the structure of a given type or user-defined type names
-}
dependencies : TypeDef -> List String
dependencies typeDef =
    let
        deriveNickOrDeps {theType} =
            case isComplex theType of
                True -> [Generate.Type.nick theType]
                False -> derive theType

        isComplex : Type -> Bool
        isComplex t =
            case t of
              TypeBool -> False
              TypeFloat -> False
              TypeString -> False
              TypeInt -> False
              _ -> True

        derive : Type -> List String
        derive theType =
            case theType of
                  TypeBool -> []
                  TypeFloat -> []
                  TypeString -> []
                  TypeInt -> []

                  TypeError _ -> []
                  TypeExtendedRecord _ -> [] --record defined using an extensible one
                  TypeExtensible _ -> [] --extensible record
                  TypeImported t -> [t] --type not core and not defined in the input

                  TypeArray t -> derive t
                  TypeDict (t1, t2) -> derive t1 ++ derive t2
                  TypeList listType ->
                      derive listType
                  TypeMaybe t -> derive theType
                  TypeProduct (_, t) -> List.map derive t
                    |> List.concat
                  TypeRecord types ->
                    types
                    |> List.map deriveNickOrDeps
                    |> List.concat
                  TypeTuple tupleTypes ->
                    tupleTypes
                    |> List.map derive
                    |> List.concat
                  TypeUnion types ->
                        types
                        |> List.map (Tuple.second >> List.map derive)
                        |> List.concat
                        |> List.concat
    in
        derive typeDef.theType
