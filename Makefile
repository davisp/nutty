
all: nutty

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

