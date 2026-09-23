.DEFAULT_GOAL := help

EMACS ?= emacs
EL_FILES := $(filter-out azure-evil.el,$(sort $(wildcard *.el)))
EVIL_EL_FILE = $(shell $(EMACS) -Q --batch \
  --eval "(progn (require 'package) (package-initialize) (when (locate-library \"evil\") (princ \"azure-evil.el\")))")
COMPILE_EL_FILES = $(EL_FILES) $(EVIL_EL_FILE)
ORG_FILES := azure.org azure-devops.org azure-devops-outline.org
EVIL_ORG_FILE = $(shell $(EMACS) -Q --batch \
  --eval "(progn (require 'package) (package-initialize) (when (locate-library \"evil\") (princ \"azure-evil.org\")))")
TANGLE_ORG_FILES = $(ORG_FILES) $(EVIL_ORG_FILE)
# Discover tests recursively so adding a test file never requires updating this file.
TEST_FILES := $(sort $(shell find tests -type f -name '*-test.el' -print))
TEST_LOAD_ARGS := $(foreach file,$(TEST_FILES),--load $(file))
export TEST

.PHONY: help test list-tests tangle byte-compile native-compile

help: ## Show available commands.
	@printf '%s\n' \
	  'Usage:' \
	  '  make test            Run all ERT tests' \
	  '  make test TEST=REGEXP Run tests whose names match REGEXP' \
	  '  make list-tests      List all available ERT tests' \
	  '  make tangle          Tangle the Org sources into Elisp files' \
	  '  make byte-compile    Tangle, then byte-compile all Elisp files' \
	  '  make native-compile  Tangle, then native-compile all Elisp files' \
	  '  make help            Show this help' \
	  '' \
	  'Examples:' \
	  '  make test TEST=outline' \
	  '  make test TEST=azure-devops-work-item-get-allows-missing-item'

# Reproduce the Org files' local-variable setup explicitly in batch mode.
# The dev block installs the project's post-tangle processing and lint hook.
tangle: ## Tangle the Org source files into Elisp files.
	$(EMACS) -Q --batch \
	  --eval "(progn (require 'package) (package-initialize) (require 'org) (require 'ob-tangle) (require 's) (require 'package-lint) (setq org-confirm-babel-evaluate nil) (org-babel-lob-ingest \"setup.org\") (with-current-buffer (find-file-noselect \"setup.org\") (org-mode) (org-sbe \"dev\")) (mapc #'org-babel-tangle-file '( $(foreach file,$(TANGLE_ORG_FILES),\"$(file)\") )))"

byte-compile: tangle ## Tangle, then byte-compile all Elisp files.
	$(EMACS) -Q --batch \
	  --eval "(progn (require 'package) (package-initialize))" \
	  -L . --funcall batch-byte-compile $(COMPILE_EL_FILES)

native-compile: tangle ## Tangle, then native-compile all Elisp files.
	$(EMACS) -Q --batch -L . \
	  --eval "(progn (require 'package) (package-initialize) (mapc #'native-compile '( $(foreach file,$(COMPILE_EL_FILES),\"$(file)\") )))"

test: tangle ## Tangle, then run all ERT tests, or those matching TEST=REGEXP.
	$(EMACS) -Q --batch \
	  --eval "(progn (require 'package) (package-initialize) (setq load-prefer-newer t))" \
	  -L . -L tests \
	  $(TEST_LOAD_ARGS) \
	  --eval "(ert-run-tests-batch-and-exit (let ((regexp (getenv \"TEST\"))) (if (and regexp (> (length regexp) 0)) regexp t)))"

list-tests: ## List all available ERT tests without tangling sources.
	@$(EMACS) -Q --batch \
	  --eval "(progn (require 'package) (package-initialize) (setq load-prefer-newer t))" \
	  -L . -L tests \
	  $(TEST_LOAD_ARGS) \
	  --eval "(progn (require 'ert) (mapc (lambda (test) (princ (format \"%s\\n\" (ert-test-name test)))) (sort (ert-select-tests t t) (lambda (left right) (string-lessp (symbol-name (ert-test-name left)) (symbol-name (ert-test-name right)))))))"
