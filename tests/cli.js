const processAnnotated = require('../cli.js')
const mock = require('mock-fs');
// mock Elm file contents with decgen annotations
const fileMock = `module Main exposing (..)

--| decgen-start
type alias Count = Int
type alias Fruit = Apple Count | Banana Count
--| decgen-end

--| decgen-start
type alias Number = Int
type alias Foo = Bar Number | Baz

--|| decgen-generated-start
THIS SHOULD BE REPLACED
--|| decgen-generated-end

--| decgen-end
`
mock({
  'Main.elm' : fileMock
})



console.log(fs.readFileSync('Main.elm').toString())
