# Elm coders generator

Builds JSON decoders and encoders for your Elm types.

Demo using the kakoune editor piping to the CLI.
[![asciicast](https://asciinema.org/a/NuovOo8YV4G5DivrEkCHhbdNl.svg)](https://asciinema.org/a/NuovOo8YV4G5DivrEkCHhbdNl)

## Installation
```
npm i -g decgen
```


## CLI
Generate decoders for Example.elm (available in this repo):

```
decgen Example.elm
```

Or pipe to `decgen`:

```sh
echo '-- [decgen-start]\n type X = Int\n -- [decgen-end]' | decgen
```
output
```elm
-- [decgen-start]
type X = Int
-- [decgen-end]
```

## API
```
const m = require('.')

m('-- [decgen-start]\n type X = Int\n -- [decgen-end]').then(x => console.log(x))

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
