const mock = require('mock-fs');
const fs = require('fs')
const generate = require('./src/generate')

// mock Elm file contents with decgen annotations
const fileMock = `module Main exposing (..)

--| decgen-start
type alias Count = Int
type alias Fruit = Apple Count | Banana Count
--| decgen-end

--| decgen-start
type alias Number = Int
type alias Foo = Bar Number | Baz

--| decgen-generated-start
THIS SHOULD BE REPLACED
--| decgen-generated-end

--| decgen-end
`
mock({
  'Main.elm' : fileMock
})

// contents of the Elm file
const elmFile = fs.readFileSync('Main.elm').toString()

// REGEXES

// grab annotations, non-greedy

async function run () {
    // grab entire annotated section
    const annotatedGrabber = /(--\| +decgen-start *\n[\s\S]*?\n--\| +decgen-end)/g
    var reg = annotatedGrabber
    var targetText = elmFile
    var transformed = elmFile
    var result;
    while((result = reg.exec(targetText)) !== null) {
        console.log('\n\n')
        let transformedAnnotation = await transformAnnotation(result[0])
        transformed = transformed.replace(result[0], transformedAnnotation)
        // doSomethingWith(result);
    }
    fs.writeFileSync('Main.elm', transformed)
    console.log(fs.readFileSync('Main.elm').toString())
}


async function transformAnnotation (annotatedSection) {
    // grab only the content inside the annotations
    const annotationContentGrabber = /(--\| +decgen-start *\n)([\s\S]*)(\n--\| +decgen-end)/g
    const insideAnnotations = annotationContentGrabber.exec(annotatedSection)[2]

    // separated geenerator input and output
    const inputGrabber = /([\S\s]+)--\|\| +decgen-generated-start/g
    const inputOnly = inputGrabber.exec(insideAnnotations)
    var input
    if (inputOnly) {
        // something was generated before
        input = inputOnly[1]
    } else {
        input = insideAnnotations
    }

    const generated = await makeCoders(input)
    const generatedSection = `\n--|| decgen-generated-start\n${generated}\n--|| decgen-generated-end\n`
    // replace the content
    // $1 and $2 are start markers and input types
    // $3 is end marker
    transformed = annotatedSection.replace(annotationContentGrabber, `$1${input}\n${generatedSection} $3`)
    return transformed
}

function makeCoders (code) {
    return new Promise ((resolve) => {generate(code, resolve)})
}
run()
// console.log(insideAnnotations)
//const sections = separator.exec(insideAnnotations)
//const input = sections[1]
//const generatedBefore = sections[2]
//generate(input, generated => {
//    // console.log(generated)
//    const result = ""
//})
//
//const result = elmFile.replace(annotatedGrabber, '$1test$3')
// console.log(result)


