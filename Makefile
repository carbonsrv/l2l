.PHONY: samples

all: check samples test

repl:
	./bin/l2l

carbon_app:
	zip -r l2l.zip l2l/* l2l.lua LICENSE
	echo "#!`which carbon` -app" > l2l.app
	cat l2l.zip >> l2l.app
	chmod +x l2l.app
	rm l2l.zip

check:
	luacheck --no-color --exclude-files compat.lua sample* \
	  --new-globals TypeException _R _C _D _M symbol resolve setfenv \
	  -- l2l/*.lua

samples:
	bash ./bin/build samples/sample04/main.lisp samples/sample0*.lisp

test: tests/*.lisp tests/init.lua *.lua
	lua tests/init.lua
