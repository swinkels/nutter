.PHONY: install unit-test

unit-test:
	cask exec ert-runner -L . -L test

install:
	cask install --verbose
