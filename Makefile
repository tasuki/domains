react:
	elm reactor
develop:
	watch -n 5 elm make src/Main.elm --debug --output=domains.js
