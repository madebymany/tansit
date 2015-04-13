all:
	@echo "nothing to do"

install:
	install -o root -g root -m 0755 dist/build/tansit/tansit /usr/bin/tansit
	install -o root -g root -m 0755 tansit-kms /usr/bin/tansit-kms
