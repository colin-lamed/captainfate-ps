all: build

setup:
	set -e
	npm install
	npm run bower install

build: setup
	npm run build

test: setup
	npm run test

# since `test` is also a directory, it will not be built without this
# (or `make -B test`):
.PHONY: test

clean:
	npm run clean

zip-src:
	rm -f captainfate-ps.zip
	# respects .gitignore, but doesn't include .git
	git archive -o captainfate-ps.zip HEAD
	# update with .git
	zip -b /tmp captainfate-ps.zip -r .git/


run:
	pulp run

runweb:
	firefox html/index.html
