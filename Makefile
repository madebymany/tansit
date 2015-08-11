NAME := tansit
PREFIX := /usr
BIN_PATH := $(PREFIX)/bin
RUNIT_DIR := /etc/sv/$(NAME)
RUNIT_START_DIR := /etc/service/$(NAME)

all:
	@echo "nothing to do"

install:
	install -o root -g root -m 0755 dist/build/tansit/tansit '$(BIN_PATH)/tansit'
	install -o root -g root -m 0755 tansit-kms '$(BIN_PATH)/tansit-kms'

install-runit:
	mkdir -p /etc/sv /etc/service
	cp -R runit '$(RUNIT_DIR)'
	chown -R root:root  '$(RUNIT_DIR)'
	ln -s '$(RUNIT_DIR)' '$(RUNIT_START_DIR)'
