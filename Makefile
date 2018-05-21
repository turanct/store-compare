haskell-exe = .stack-work/install/x86_64-osx/lts-10.5/8.2.2/bin/store-compare-exe

compile: $(haskell-exe) frontend-compiled/index.html

$(haskell-exe): src/*.hs app/*.hs
	stack build
	touch $(haskell-exe)

frontend-compiled/index.html: frontend-src/Main.elm
	elm-make frontend-src/Main.elm --output frontend-compiled/index.html

.PHONY: compile
