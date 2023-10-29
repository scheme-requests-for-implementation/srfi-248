SCHEME = guile --r6rs -L lib

check:
	$(SCHEME) tests.scm

.PHONY: check
