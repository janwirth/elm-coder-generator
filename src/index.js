#!/usr/bin/env node

const fs = require('fs')
const generate = require('./generate')

// REGEXES

// grab annotations, non-greedy

async function processAnnotated (code) {
    // grab entire annotated section
    const annotatedGrabber = /(-- +\[generator-start\] *\n[\s\S]*?\n-- +\[generator-end\])/g
    var reg = annotatedGrabber
    var transformed = code
    var result;
    while((result = reg.exec(code)) !== null) {
        let transformedAnnotation = await transformAnnotation(result[0])
        transformed = transformed.replace(result[0] + "\n", transformedAnnotation)
        // doSomethingWith(result);
    }
    return transformed
}


async function transformAnnotation (annotatedSection) {
    // grab only the content inside the annotations
    const annotationContentGrabber = /(-- +\[generator-start\] *\n)([\s\S]*)(\n-- +\[generator-end\])/g
    const insideAnnotations = annotationContentGrabber.exec(annotatedSection)[2]

    // separated geenerator input and output
    const inputGrabber = /([\S\s]+)\n\n-- +\[generator-generated-start/g
    const inputOnly = inputGrabber.exec(insideAnnotations)
    var input
    if (inputOnly) {
        // something was generated before
        input = inputOnly[1]
    } else {
        input = insideAnnotations
    }

    const generated = await makeCoders(input)
    const generatedSection =
        `\n-- [generator-generated-start] -- DO NOT MODIFY or remove this line\n${generated}`
    // replace the content
    // $1 and $2 are start markers and input types
    // $3 is end marker
    const transformed =
        annotatedSection.replace(annotationContentGrabber, `$1${input}\n${generatedSection} $3`)
    return transformed
}

async function processFile(fileName) {
    // contents of the Elm file
    const elmFile = fs.readFileSync(fileName).toString()
    const output = await processAnnotated(elmFile)
    fs.writeFileSync(fileName, output)
}

function makeCoders (code) {
    return new Promise ((resolve) => {generate(code, resolve)})
}

const getPipeInput = () => new Promise((resolve) => {
    var data = '';

    process.stdin.resume();
    process.stdin.setEncoding('utf8');

    process.stdin.on('data', function(chunk) {
      data += chunk;
    });

    process.stdin.on('end', function() {
      resolve(data)
    });
})

// run cli when it is called from command line
if (require.main === module) {
    run()
}

// Check whether this module is code piped to or is called with files as argument
async function run () {
    // detectMode
    const isPipedTo = !process.stdin.isTTY
    const filePaths = process.argv.slice(2)
    if (isPipedTo) {
        const input = await getPipeInput()
        const output = await processAnnotated(input)
        console.log(output)
    } else if (filePaths.length > 0) {
        console.log(filePaths)
        filePaths.forEach(processFile)
    } else {
        console.log('Please pipe input or pass filenames')
    }
}

module.exports = makeCoders
