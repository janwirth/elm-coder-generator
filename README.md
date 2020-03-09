# Elm coder generator

Builds JSON decoders and encoders for your Elm types.

Demo using the kakoune editor piping to the CLI.

[![asciicast](https://asciinema.org/a/nZnhdNtLGU33OZ8gbHo3VM5jc.svg)](https://asciinema.org/a/nZnhdNtLGU33OZ8gbHo3VM5jc)


**Coverage**

|Module         | decls        | let decls     | lambdas     | branches      |
|---------------|--------------|---------------|-------------|---------------|
|total          | 96/151 (64%) | 117/168 (70%) | 10/13 (77%) | 190/436 (44%) |

## Installation
```
npm i -g elm-coder-generator
```


## CLI
Generate decoders for Example.elm (available in this repo):

*It grabs and generates code only for type definitions inside these annotations.*

```
elm-coder-generator Example.elm
```

Or pipe to `elm-coder-generator`:
```sh
echo '-- [generator-start]\n type X = Int\n-- [generator-end]' | elm-coder-generator 
```
output
```elm
-- [generator-start]
 type X = Int

-- [elm-generator-start] -- DO NOT MODIFY or remove this line
decodeX =
   Decode.int

encodeX a =
   Encode.int a 
-- [generator-end]
```

## API
```
const generate = require('elm-coder-generator')

generate('-- [generator-start]\n type X = Int\n -- [generator-end]').then(x => console.log(x))

// -> decodeX =
// ->    Decode.int
// -> 
// -> encodeX a =
// ->    Encode.int a
```

## Development

Clone the repo and build Cli.elm:

```
$ git clone https://github.com/franzskuffka/elm-coder-generator
$ cd elm-coder-generator
$ elm make src/Cli.elm --output elm-stuff/Cli.js --optimize
```

## Roadmap
- [ ] support for different data formats
- [ ] IDE plugins
- [ ] codec support for encoder / decoder pairs https://package.elm-lang.org/packages/miniBill/elm-codec/latest/
- [ ] compile plugin / pragma?

## Acknowledgements
Special thanks to Dániel Kodaj @dkodaj for building the meat of the coder generator.

Of course, thanks to @evancz and @stoeffel.

