const Module = require('../elm-stuff/Cli.js')

function run (code, callback) {
    if (!(typeof callback == 'function')) {
        console.error('callback is not a function')
        return
    }
    // accept either string of file contents or a list of file contents
    const flags = code
    const generate = Module.Elm.Cli.init
    const process = generate({flags})
    process.ports.done.subscribe(callback)
}

module.exports = run
