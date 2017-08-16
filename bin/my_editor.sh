#!/bin/bash
(emacsclient -n $* 2> /dev/null) || emacs $*
