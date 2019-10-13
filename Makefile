cli:
	elm make --optimize src/Cli.elm --output=src/Cli.js
dev:
	elm make --debug src/Cli.elm --output=src/Cli.js
test:
	elm-verify-examples && elm-app test
