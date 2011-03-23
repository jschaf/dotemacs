========================
 Joe Schafer's .emacs.d
========================

Setup
=====

A few commands to initialize, populate and correctly place the
repository.

    git clone git@github.com:jschaf/dotemacs.git
    mv dotemacs .emacs.d
    cd .emacs.d
    git submodule update --init
    
    # Emacs errors out if we try to compile js2 from within emacs.    
    emacs --batch --eval '(byte-compile-file "js2-mode.el")'

Structure
=========

init.el
-------

`init.el` is the start point.  `init.el` does some basic customization
and then loads `autoloads.el`, `funcs.el`, `custom.el`, and
`colors.el`.

autoloads.el
------------

Autoloads required files.  Additionally, it sets
some global keys and simple options to prevent customizations from
becoming to spread out.

funcs.el
--------

Various utility functions and their keybindings.

custom.el
---------

Customizations and hooks.  I don't use customize and set all
customizations in `custom.el`.


colors.el
---------

My custom color scheme so I don't have to use the color-theme library.
I only use one color theme and it's easier to deal with one file than
wrestle with the color-theme options.

