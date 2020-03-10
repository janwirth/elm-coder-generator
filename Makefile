SHELL=/bin/bash -o pipefail

cli:
	elm make --optimize src/Cli.elm --output=elm-stuff/Cli.js
dev:
	elm make --debug src/Cli.elm --output=elm-stuff/Cli.js

test:
	elm-verify-examples
	elm-app test
	make e2e
tdd:
	nodemon make -w src -w tests/* -w E2eTest.elm -e elm,js --exec "make test || exit 1"
e2e:
	rm E2eTest_result.elm
	make dev
	cat ./E2eTest.elm | elm-coder-generator > E2eTest_result.elm
	diff -u E2eTest.elm E2eTest_result.elm | colordiff
	elm make E2eTest.elm --output elm-stuff/e2e.js

release:
	make test
	make cli
	npm version patch -m "Release %s"
	npm publish

homepage:
	rm -rf docs
	elm make src/Homepage.elm --output docs/index.html
