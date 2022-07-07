HTML_FILES	= index.html base64.js main.js
IMPLS		= idris2/main.js js/mos6502.js roc/host.js roc/emulator.wasm roc-effectful/host.js roc-effectful/emulator.wasm

.ONESHELL:

all: $(patsubst %, _build/%, $(HTML_FILES)) _build/files.js $(patsubst %,_build/implementations/%, $(IMPLS))

_build/index.html: html/index.html
	mkdir -p _build
	cp -f $< $@

_build/base64.js: html/base64.js
	mkdir -p _build
	cp -f $< $@

_build/main.js: html/main.js
	mkdir -p _build
	cp -f $< $@

_build/files.js: data/program.dat
	mkdir -p _build
	(echo "let files = {"; \
	$(foreach file, $<, \
	        printf "\t'%s': base64ToArrayBuffer('" $(file) ; \
	        base64 $(file) | sed -e 's/$$/\\/' ; \
	        printf "'),\n" ; \
	        ) \
	echo "};"; \
	) > $@

implementations/idris2/build/exec/main.js:
	(cd implementations/idris2 && idris2 --build fp-perf-mos6502-idris2.ipkg)

_build/implementations/idris2/main.js: implementations/idris2/build/exec/main.js
	mkdir -p $(dir $@)
	cp -f $< $@

_build/implementations/js/mos6502.js: implementations/js/mos6502.js
	mkdir -p $(dir $@)
	cp -f $< $@

implementations/roc/emulator.wasm:
	(roc build --target=wasm32 --optimize implementations/roc/emulator.roc)

_build/implementations/roc/emulator.wasm: implementations/roc/emulator.wasm
	mkdir -p $(dir $@)
	cp -f $< $@

_build/implementations/roc/host.js: implementations/roc/platform/host.js
	mkdir -p $(dir $@)
	cp -f $< $@

implementations/roc-effectful/emulator.wasm:
	(roc build --target=wasm32 --optimize implementations/roc-effectful/emulator.roc)

_build/implementations/roc-effectful/emulator.wasm: implementations/roc-effectful/emulator.wasm
	mkdir -p $(dir $@)
	cp -f $< $@

_build/implementations/roc-effectful/host.js: implementations/roc-effectful/platform/host.js
	mkdir -p $(dir $@)
	cp -f $< $@
