haskell-exe = .stack-work/install/x86_64-osx/lts-10.5/8.2.2/bin/store-compare-exe

compile: \
	$(haskell-exe) \
	frontend-compiled/index.html \
	frontend-compiled/main.js \
	frontend-compiled/main.css

$(haskell-exe): src/*.hs app/*.hs
	stack build
	touch $(haskell-exe)

frontend-compiled/index.html: frontend-src/index.html
	cp frontend-src/index.html frontend-compiled/

frontend-compiled/main.css: frontend-src/main.css
	cp frontend-src/main.css frontend-compiled/

frontend-compiled/main.js: frontend-src/Main.elm
	elm-make frontend-src/Main.elm --output frontend-compiled/main.js

.PHONY: compile
