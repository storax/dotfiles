#!/usr/bin/env zsh

for d in *(/); stow -v -t ~/ -S $d
