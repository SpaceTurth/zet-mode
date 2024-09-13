#!/bin/bash

emacs -batch -l ert -l tests.el -f ert-run-tests-batch-and-exit
