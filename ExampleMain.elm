module ExampleMain exposing (Record, Union, Dict)

import Dict
import ExampleDep1 exposing (Union)

type alias Dict = Dict.Dict Int Record

-- [decgen-start]
type alias Record = {
    primitive : String
  , union : Union
  }
-- [decgen-end]

