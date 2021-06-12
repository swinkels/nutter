.PHONY: build install unit-test

unit-test:
	# Run unit tests
	cask exec ert-runner -L . -L test

install:
	# Install packages specified in the Cask file
	cask install

build:
	# Byte-compile Emacs Lisp files
	cask build

nutter-pkg.el: nutter.el
	# Generate $@ from $?
	cask pkg-file
