
NOSE_REPO=https://github.com/nose-devs/nose.git
NOSE_VERSION=release_1.1.2

all: nutty

check: nutty
	./nutty -v --failed

nutty: combined
	cd ./pkg && zip -q -r nutty .
	echo "#!/usr/bin/env python" > $@
	cat ./pkg/nutty.zip >> $@
	chmod +x $@

combined: erlang python

erlang:
	erlc -o erl/nutty/ebin/ erl/nutty/src/*.erl
	cp -r erl/nutty pkg/

python:
	cp -r py/* pkg/
	mkdir -p tmp/
	if [ ! -d tmp/nose ]; then cd tmp && git clone -q $(NOSE_REPO); fi
	cd tmp/nose && git checkout -q $(NOSE_VERSION)
	rm -rf pkg/nose && cp -r tmp/nose/nose pkg
