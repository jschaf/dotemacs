;; @(#) ada-support.el --- Provide some Emacs 22 functions for Emacs 21

;; Copyright (C) 2006 Free Software Foundation, Inc.

;; Author: Stephen Leake <stephen_leake@stephe-leake.org>
;; Maintainer: Stephen Leake <stephen_leake@stephe-leake.org>
;; Keywords: languages ada 

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(defvar mode-require-final-newline t)

(defalias 'run-mode-hooks 'run-hooks)

(defalias 'ff-post-load-hook 'ff-post-load-hooks)
(defalias 'ff-file-created-hook 'ff-file-created-hooks)

(defun assoc-string (word substring flag)
  (assoc-ignore-case word substring))

(provide 'ada-support)

;; end of file
