# My Doom Emacs Config

This is my personal Emacs config. I use Doom Emacs these days, so this
is a .doom.d layout rather than the traditional .emacs.d.

I use this config on both both Windows and Linux, so [config.el](config.el)
is meant to be cross-platform. Machine-specific settings will be
picked up from config.local.el - that file is ignored by git, 
since those settings are not meant to be portable.
My base [init.el](init.el) and [packages.el](packages.el) are included here, but my specific installs
modify them to their needs.

Some particular points of interest:

* I am not an evil user, but I have been experimenting with limited modal
  editing lately, and have settled on a modified [boon](https://github.com/jyp/boon). That setup, along with
  older setups I tried, are in [modal.el](modal.el).
* In service to the boon setup, [redirect-evil-to-boon.el](redirect-evil-to-boon.el) hacks doom's `map!`
  macro to conditionally redirect some evil state keybindings to boon.
  This is set up in [init.el](init.el).
* A doom module, `:config default-bindings`, that replaces 
  doom's `:config default +bindings`.
  While I don't use evil, I prefer doom's evil-mode leader key mappings
  to its emacs ones. `default-bindings` generates a mix of the two
  along with some overrides.
* Integration with a QMK keyboard
* I have org mode set up to find all TODOs in org-roam and periodically
  generate an aggregation file, which will get synced to my phone.
* A package install script for MSYS2 that sets up any packages I need from it.
  
