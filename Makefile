# SPDX-FileCopyrightText: 2025 Marc Nieper-Wißkirchen
# SPDX-License-Identifier: MIT

SCHEME = guile --r6rs -L lib

check:
	$(SCHEME) tests.scm

.PHONY: check
