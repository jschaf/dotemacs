====================================
 Joe Schafer's Emacs Customizations
====================================

Setup
=====

A few commands to initialize, populate and correctly place the
repository.
::

    git clone git@github.com:jschaf/dotemacs.git
    # Put the folder somewhere Emacs will look
    mv dotemacs .emacs.d
    cd .emacs.d
    # Grab dependencies
    git submodule update --init
    
    # Emacs errors out if we try to compile js2 from within emacs.    
    emacs --batch --eval '(byte-compile-file "js2-mode.el")'

Structure
=========

init.el
-------

``init.el`` is the start point as Emacs looks for this file
(``~/.emacs.d/init.el``) default.  ``init.el`` does some basic
customization up front to avoid jarring changes to the frame
structure.  The bulk of the code is then loaded from ``autoloads.el``,
``functions.el``, ``custom.el``, and ``colors.el``.

autoloads.el
------------

Autoloads required files.  Additionally, it sets some global keys and
simple options to prevent customizations from becoming to spread out.

functions.el
------------

Various utility and custom functions as well as their keybindings.

custom.el
---------

Customizations and hooks.  I think the customize framework is clunky
so I keep my own in ``custom.el``.


colors.el
---------

My custom color scheme so I don't have to use the color-theme library.
I only use one color theme and it's easier to deal with one file than
wrestle with the color-theme options.

