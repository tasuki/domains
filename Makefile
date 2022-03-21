release:
	elm make src/Main.elm --optimize --output=domains.js
develop:
	find . -name '*.elm' | entr elm make src/Main.elm --debug --output=domains.js
serve:
	php -S localhost:8000
