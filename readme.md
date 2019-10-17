# Elm coder generator

Builds JSON decoders and encoders for your Elm types.

Demo using the kakoune editor piping to the CLI.

[![asciicast](https://asciinema.org/a/NuovOo8YV4G5DivrEkCHhbdNl.svg)](https://asciinema.org/a/NuovOo8YV4G5DivrEkCHhbdNl)

## Installation
```
npm i -g decgen
```


## CLI
Generate decoders for Example.elm (available in this repo):

*It grabs and generates code only for type definitions inside these annotations.*

```
decgen Example.elm
```

Or pipe to `decgen`:
```sh
echo '-- [decgen-start]\n type X = Int\n-- [decgen-end]' | decgen 
```
output
```elm
-- [decgen-start]
 type X = Int

-- [decgen-generated-start] -- DO NOT MODIFY or remove this line
decodeX =
   Decode.int

encodeX a =
   Encode.int a 
-- [decgen-end]
```

## API
```
const generate = require('decgen')

generate('-- [decgen-start]\n type X = Int\n -- [decgen-end]').then(x => console.log(x))

// -> decodeX =
// ->    Decode.int
// -> 
// -> encodeX a =
// ->    Encode.int a
```

## Development

Clone the repo and build Cli.elm:

```
$ git clone https://github.com/dkodaj/decgen
$ cd decgen
$ elm make src/Cli.elm --output src/Cli.js --optimize
```

## Acknowledgements
Special thanks to Dániel Kodaj @dkodaj for building the meat of the coder generator.

Of course, thanks to @evancz and @stoeffel.
