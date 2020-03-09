SHELL=/bin/bash -o pipefail

cli:
	elm make --optimize src/Cli.elm --output=src/Cli.js
dev:
	elm make --debug src/Cli.elm --output=src/Cli.js

test:
	elm-verify-examples
	elm-app test
	make e2e
tdd:
	nodemon make -w src -w tests/* -w E2eTest.elm -e elm,js --exec "make test || exit 1"
e2e:
	cat ./E2eTest.elm | decgen > E2eTest_result.elm
	diff -u E2eTest.elm E2eTest_result.elm | colordiff
	elm make E2eTest.elm
