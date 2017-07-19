;;; init.el --- GNU Emacs Setup by Eric James Michael Ritz
;;
;;; Commentary:
;;
;; My personal GNU Emacs configuration.
;;
;;; Code:

(server-start)


;;; Package Support

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(require 'diminish)
(setq use-package-always-ensure t)

;;; TODO: Ideally I would like to have (package-menu-execute t) at the
;;; end of this but it simply does not work.
(defun ejmr-update-available-packages ()
  "Open the list of packages and mark all available for update."
  (interactive)
  (package-list-packages)
  (package-menu-mark-upgrades))

;;; TODO: Setup a local mirror of the Emacs Wiki.
;;; https://emacsmirror.net/manual/epkg/Installation.html#Installation
(use-package epkg :disabled t)


;;; Startup

(let ((file-name-handler-alist nil))
  "~/.emacs.d/init.elc")

(use-package dashboard :disabled t)


;;; Global Custom Keymap Prefixes
;;;
;;; This page defines keymaps which I use throughout for a lot of my
;;; custom key-bindings.  It is important to create these keymaps as
;;; soon as possible so that the rest of the configuration can use
;;; them.  Therefore, this page should always come early.

(defvaralias 'ejmr-custom-bindings-map 'mode-specific-map) ; i.e. `C-c'
(bind-key "h" (define-prefix-command 'ejmr-hydra-map) ejmr-custom-bindings-map)
(bind-key "s-x" (define-prefix-command 'ejmr-command-shortcut-map))


;;; Global Minor Modes

(use-package auto-minor-mode :defer nil)

(transient-mark-mode t)

(progn
  (show-paren-mode t)
  (setq-default show-paren-style 'mixed))

(menu-bar-mode -1)
(scroll-bar-mode -1)
(which-function-mode t)
(global-auto-revert-mode t)
(electric-indent-mode t)
(electric-pair-mode t)
(column-number-mode t)
(tool-bar-mode -1)
(global-prettify-symbols-mode t)
(global-hl-line-mode t)
(pending-delete-mode t)

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode t))

(use-package schrute
  :diminish schrute-mode
  :config
  (use-package bln-mode
    :commands (bln-forward-half bln-backward-half)
    :config
    (bind-key "s-[" #'bln-backward-half)
    (bind-key "s-]" #'bln-forward-half))
  (setq schrute-command-repetitions 10)
  (setq schrute-shortcuts-commands
	'((avy-goto-line . (next-line previous-line))
	  (avy-goto-word-1 . (backward-char forward-char))
	  (kill-buffer . (kill-or-bury-alive))
	  (delete-char . (avy-zap-to-char-dwim))
	  (forward-char . (bln-forward-half))
	  (backward-char . (bln-backward-half))
	  (comment-region . (comment-dwim-2))
	  (isearch . (swiper))
	  (find-file . (counsel-recentf))
	  (zap-char . (avy-zap-up-to-char-dwim))))
  (schrute-mode 1))


;;; Global Variables

(setq backup-inhibited t)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq byte-compile-warnings nil)


;;; Registers

(set-register ?i '(file . "/home/eric/.emacs.d/init.el"))
(set-register ?g '(file . "/home/eric/.gitconfig"))
(set-register ?s '(file . "/home/eric/.config/fish/"))
(set-register ?n '(file . "/home/eric/Documents/Notes.org"))
(set-register ?c '(file . "/home/eric/.conkerorrc/"))
(set-register ?u '(file . "/home/eric/.conkerorrc/saved-buffers.txt"))
(set-register ?p '(file . "/media/eric/ejmr-fillip1/Projects"))


;;; Global Utilities

(use-package with-editor)
(use-package add-hooks)

(use-package historian
  :config
  (use-package ivy-historian
    :config (ivy-historian-mode t)))

(use-package multiple-cursors
  :config
  (defhydra hydra-multiple-cursors (:hint nil)
    "
     ^Up^            ^Down^        ^Other^
----------------------------------------------
[_p_]   Next    [_n_]   Next    [_l_] Edit lines
[_P_]   Skip    [_N_]   Skip    [_a_] Mark all
[_M-p_] Unmark  [_M-n_] Unmark  [_r_] Mark by regexp
^ ^             ^ ^             [_q_] Quit
"
    ("l" mc/edit-lines :exit t)
    ("a" mc/mark-all-like-this :exit t)
    ("n" mc/mark-next-like-this)
    ("N" mc/skip-to-next-like-this)
    ("M-n" mc/unmark-next-like-this)
    ("p" mc/mark-previous-like-this)
    ("P" mc/skip-to-previous-like-this)
    ("M-p" mc/unmark-previous-like-this)
    ("r" mc/mark-all-in-region-regexp :exit t)
    ("q" nil))
  (bind-key "c" #'hydra-multiple-cursors/body ejmr-hydra-map))

(use-package anyins
  :commands (anyins-mode)
  :bind ("C-x r a" . anyins-mode))

(use-package commander)

(use-package helpful
  :config
  (defhydra hydra-helpful (:color blue)
    "Helpful"
    ("f" helpful-function "Function")
    ("c" helpful-command "Command")
    ("m" helpful-macro "Macro"))
  (bind-key "h" #'hydra-helpful/body ejmr-hydra-map))

(use-package neotree
  :commands neotree-toggle
  :bind (:map ejmr-command-shortcut-map ("n" . neotree-toggle))
  :config (setq neo-theme 'state))

(use-package avy-menu)
(use-package mmt)
(use-package el-mock)
(use-package vlf)
(use-package eieio)
(use-package general :disabled t)
(use-package direnv)
(use-package iedit)
(use-package hierarchy)
(use-package lentic)
(use-package rpn-calc)

(use-package kill-or-bury-alive
  :config
  (key-seq-define-global "ZK" #'kill-or-bury-alive)
  (key-seq-define-global "ZP" #'kill-or-bury-alive-purge-buffers))

(use-package refine)
(use-package restart-emacs)
(use-package zone)
(use-package tldr)
(use-package fn)
(use-package find-temp-file)

(use-package editorconfig
  :diminish editorconfig-mode
  :config
  (editorconfig-mode t)
  (use-package editorconfig-custom-majormode
    :config
    (add-hook 'editorconfig-custom-hooks 'editorconfig-custom-majormode)))

(use-package lispy
  :config
  (bind-key "C-c C-y" #'lispy-mode emacs-lisp-mode-map)
  (bind-key "C-c C-y" #'lispy-mode lisp-mode-map)
  (add-hook 'emacs-lisp-mode-hook #'lispy-mode)
  (add-hook       'lisp-mode-hook #'lispy-mode))

(use-package composable
  :diminish composable-mode
  :config
  (bind-key "x" 'er/expand-region composable-object-mode-map)
  (composable-mode t)
  (composable-mark-mode t))

(use-package indent-guide
  :diminish indent-guide-mode
  :config (add-hook 'prog-mode-hook #'indent-guide-mode))

(use-package auto-dim-other-buffers :disabled t)

(use-package iflipb
  :disabled t
  :bind (("s-b" . iflipb-next-buffer)
	 ("C-s-b" . iflipb-previous-buffer)))

(use-package caps-lock
  :bind (:map ejmr-custom-bindings-map ("l" . caps-lock-mode)))

(use-package resize-window
  :bind ("C-x ^" . resize-window))

(use-package tomatinho
  :bind (:map ejmr-command-shortcut-map ("o" . tomatinho)))

(use-package which-key
  :diminish 'which-key-mode
  :config (which-key-mode t))

(use-package free-keys
  :commands (free-keys)
  :bind ("C-~" . free-keys))

(use-package linum
  :diminish 'linum-mode
  :config
  (use-package linum-relative
    :diminish 'linum-relative-mode
    :bind (:map ejmr-custom-bindings-map ("n" . linum-relative-global-mode))))

(use-package tiny
  :bind (:map ejmr-command-shortcut-map ("t" . tiny-expand)))

(use-package origami
  :diminish 'origami-mode
  :config
  (global-origami-mode t)
  (defhydra hydra-origami (:color amaranth :columns 4)
    "Origami Folds"
    ("t" origami-recursively-toggle-node "Toggle")
    ("s" origami-show-only-node "Single")
    ("r" origami-redo "Redo")
    ("u" origami-undo "Undo")
    ("o" origami-open-all-nodes "Open")
    ("c" origami-close-all-nodes "Close")
    ("n" origami-next-fold "Next")
    ("p" origami-previous-fold "Previous")
    ("q" nil "Quit" :color blue))

  (bind-key "o" #'hydra-origami/body ejmr-hydra-map))

(use-package qwe
  :load-path ("/home/eric/.emacs.d/local/qwe-0.9.5/src"
	      "/home/eric/.emacs.d/local/qwe-0.9.5/ext"))


;;; Modal Input

(use-package modalka :disabled t)

(use-package ryo-modal
  :disabled t
  :commands ryo-modal-mode
  ;; TODO: Before using `ryo-modal' I need to choose a different
  ;; key-binding to avoid conflicts.
  :bind (:map ejmr-custom-bindings-map ("SPC" . ryo-modal-mode))
  :init
  (add-hook 'ryo-modal-mode-hook
	    (lambda () (if ryo-modal-mode
		      (selected-minor-mode 1)
		    (selected-minor-mode -1))))
  :config
  (define-key ryo-modal-mode-map (kbd ".") 'ryo-modal-repeat)
  (add-to-list 'ryo-modal-bindings-list '("." "ryo-modal-repeat"))
  (ryo-modal-keys ("q" ryo-modal-mode)
		  ("n" next-line)
		  ("p" previous-line)))

(use-package god-mode
  :bind (:map ejmr-command-shortcut-map
	      ("s-g" . god-mode-all)
	      :map god-local-mode-map
	      ("." . repeat)))


;;; Org Mode

(use-package org
  :config
  (setq org-M-RET-may-split-line '((default . nil)))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN PROGRESS(p)" "|" "DONE(d)")
          (sequence "REPORT(r)" "BUG(b)" "TESTING(t)" "|" "CLOSED(c)")
          (sequence "BRAINSTORMING(b)" "RFC(r)" "FEEDBACK(f)" "|" "ACCEPTED(a) REJECTED(j)")
          (sequence "|" "CANCELED(c)")))
  (use-package org-tree-slide)
  (use-package org-ref)
  (use-package calfw :config (use-package calfw-org))
  (use-package worf
    :config (bind-key "o" #'worf-mode ejmr-custom-bindings-map))
  (use-package yankpad
    :init
    (setq yankpad-file "/home/eric/.emacs.d/org/yankpad.org")
    :config
    (define-prefix-command 'ejmr-yankpad-map)
    (bind-key "y" 'ejmr-yankpad-map ejmr-command-shortcut-map)
    (bind-key "m" #'yankpad-map ejmr-yankpad-map)
    (bind-key "e" #'yankpad-expand ejmr-yankpad-map)
    (add-to-list 'company-backends #'company-yankpad))
  (use-package org-readme)
  (use-package org-parser)
  (use-package org-journal :disabled t)
  (use-package org-wiki
    :disabled t
    :load-path "/home/eric/.emacs.d/local/org-wiki"
    :config
    (setq org-wiki-location "/home/eric/Documents/Wiki")
    (setq org-wiki-server-port "7331")
    (setq org-wiki-server-host "127.0.0.1"))
  (use-package org-board)
  (use-package ob-php)
  (use-package ox-pandoc)
  (use-package ox-gfm)
  (use-package org-brain
    :disabled t
    :init
    (setq org-brain-path "/home/eric/.emacs.d/org")
    :config
    (org-brain-activate-cache-saving)))


;;; Font

(set-frame-font "Bitstream Vera Sans Mono-13" nil t)


;;; Disabled Features

(put 'narrow-to-region 'disabled nil)


;;; Global Hooks

(add-hook 'text-mode-hook 'visual-line-mode)


;;; Global Generic Key-Bindings

(bind-key "<M-return>" #'indent-new-comment-line)
(bind-key "s-o" #'overwrite-mode)

;;; Setup `s-1' as a prefix key for help commands.
(define-prefix-command 'ejmr-help-map)
(bind-key "s-1" 'ejmr-help-map)


;;; Window Management

;;; Use `s-w' as a prefix key for various window commands.
(define-prefix-command 'ejmr-window-map)
(bind-key "s-w" 'ejmr-window-map)
(bind-key "s-w" #'ace-window ejmr-window-map)
(bind-key "0" #'delete-window ejmr-window-map)
(bind-key "1" #'delete-other-windows ejmr-window-map)
(bind-key "3" #'split-window-horizontally ejmr-window-map)
(bind-key "f" #'find-file-other-window ejmr-window-map)
(bind-key "r" #'find-file-read-only-other-window ejmr-window-map)
(bind-key "b" #'ivy-switch-buffer-other-window ejmr-window-map)
(bind-key "d" #'dired-other-window ejmr-window-map)
(bind-key "." #'xref-find-definitions-other-window ejmr-window-map)

(defun ejmr-split-window-vertically-and-balance ()
  "Splits the window vertically then balances all windows.

This is the equivalent of `C-x 2' followed by `C-x +'."
  (interactive)
  (split-window-vertically)
  (balance-windows))

(bind-key "2" #'ejmr-split-window-vertically-and-balance ejmr-window-map)

(defhydra hydra-window (ejmr-window-map "s" :color amaranth)
  "Window Size"
  ("^" enlarge-window "Taller")
  ("{" shrink-window-horizontally "Narrower")
  ("}" enlarge-window-horizontally "Wider")
  ("-" shrink-window-if-larger-than-buffer "Shrink")
  ("+" balance-windows "Balance")
  ("q" nil "Quit" :color blue))


;;; Completion

(use-package git-complete
  :load-path "/home/eric/.emacs.d/local/git-complete"
  :commands git-complete
  :bind ("M-s-/" . git-complete)
  :config
  (setq git-complete-enable-autopair t)
  (setq git-complete-ignore-case nil))

(use-package company-mode
  :diminish 'company-mode
  :bind ("s-/" . company-complete)
  :config
  (global-company-mode t)
  (use-package company-lua)
  (use-package company-quickhelp
    :diminish 'company-quickhelp-mode
    :config
    (company-quickhelp-mode 1)
    (bind-key "M-h" #'company-quickhelp-manual-begin company-active-map)))


;;; Key Chord Mode

(use-package key-chord
  :config
  (use-package key-seq)
  (key-chord-mode 1)
  (setq key-chord-two-keys-delay 0.4))

;;; TODO: Make sure that `:chords` uses the `key-seq` functionality
;;; before I switch any of my current bindings.  And ensure that it
;;; supports `:map` for local chords.
(use-package use-package-chords :disabled t)

;;; General Chords

(key-seq-define-global "ZB" #'ivy-switch-buffer)
(key-seq-define-global "ZW" #'kill-buffer-and-window)

(defun ejmr-server-edit-save-and-kill ()
  "Set current buffer as 'done' for the server, save then kill.

This is equivalent to `C-x C-s' followed by `C-x #'.  The latter
will automatically kill the buffer."
  (interactive)
  (save-buffer)
  (server-edit))

(key-seq-define-global "Z#" #'ejmr-server-edit-save-and-kill)


;;; Page Breaks

(use-package pp-c-l
  :config (pretty-control-l-mode 1))


;;; Hydra

(use-package hydra
  :config
  (setq hydra-verbose nil))

;;; Hydra to Replace C-y and M-y

(defhydra hydra-yank-pop ()
  "Yank"
  ("C-y" yank nil)
  ("M-y" yank-pop nil)
  ("y" (yank-pop 1) "next")
  ("Y" (yank-pop -1) "previous")
  ("c" counsel-yank-pop "counsel")
  ("l" (refine 'kill-ring) "list" :color blue)
  ("w" hydra-webpaste/body "web" :color blue)
  ("q" nil "quit" :color blue))

(bind-key "C-y" #'hydra-yank-pop/yank)
(bind-key "M-y" #'hydra-yank-pop/yank-pop)
(bind-key "s-y" #'hydra-yank-pop/body)
(bind-key "C-s-y" #'counsel-yank-pop)

;;; Hydra for Zooming Text

(defhydra hydra-zoom ()
  "Zoom"
  ("+" text-scale-increase "in")
  ("-" text-scale-decrease "out")
  ("0" (text-scale-increase 0) "default")
  ("q" nil "quit"))

(bind-key "z" #'hydra-zoom/body ejmr-hydra-map)

;;; Hydra for Rectangle Commands

(defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
				     :color pink
				     :post (deactivate-mark))
  "
  ^_k_^     _d_elete    _s_tring
_h_   _l_   _o_k        _y_ank
  ^_j_^     _n_ew-copy  _r_eset
^^^^        _e_xchange  _u_ndo
^^^^        ^ ^         _p_aste
"
  ("h" backward-char nil)
  ("l" forward-char nil)
  ("k" previous-line nil)
  ("j" next-line nil)
  ("e" exchange-point-and-mark nil)
  ("n" copy-rectangle-as-kill nil)
  ("d" delete-rectangle nil)
  ("r" (if (region-active-p)
           (deactivate-mark)
         (rectangle-mark-mode 1)) nil)
  ("y" yank-rectangle nil)
  ("u" undo nil)
  ("s" string-rectangle nil)
  ("p" kill-rectangle nil)
  ("o" nil nil))

(bind-key "r" #'hydra-rectangle/body ejmr-hydra-map)

;;; Buffer Menu

(defhydra hydra-buffer-menu (:color pink :hint nil)
  "
^Mark^             ^Unmark^           ^Actions^          ^Search
^^^^^^^^-----------------------------------------------------------------                        (__)
_m_: mark          _u_: unmark        _x_: execute       _R_: re-isearch                         (oo)
_s_: save          _U_: unmark up     _b_: bury          _I_: isearch                      /------\\/
_d_: delete        ^ ^                _g_: refresh       _O_: multi-occur                 / |    ||
_D_: delete up     ^ ^                _T_: files only: % -28`Buffer-menu-files-only^^    *  /\\---/\\
_~_: modified      ^ ^                ^ ^                ^^                                 ~~   ~~
"
  ("m" Buffer-menu-mark)
  ("u" Buffer-menu-unmark)
  ("U" Buffer-menu-backup-unmark)
  ("d" Buffer-menu-delete)
  ("D" Buffer-menu-delete-backwards)
  ("s" Buffer-menu-save)
  ("~" Buffer-menu-not-modified)
  ("x" Buffer-menu-execute)
  ("b" Buffer-menu-bury)
  ("g" revert-buffer)
  ("T" Buffer-menu-toggle-files-only)
  ("O" Buffer-menu-multi-occur :color blue)
  ("I" Buffer-menu-isearch-buffers :color blue)
  ("R" Buffer-menu-isearch-buffers-regexp :color blue)
  ("c" nil "cancel")
  ("v" Buffer-menu-select "select" :color blue)
  ("o" Buffer-menu-other-window "other-window" :color blue)
  ("q" quit-window "quit" :color blue))

(bind-key "." #'hydra-buffer-menu/body Buffer-menu-mode-map)

;;; Minor Modes

(defhydra hydra-minor-modes (:columns 6)
  "Minor Mode"
  ("a" global-aggressive-indent-mode "Agressive")
  ("A" anyins-mode "Anyins")
  ("c" global-company-mode "Company")
  ("C" cargo-minor-mode "Cargo")
  ("d" darkroom-tentative-mode "Darkroom")
  ("D" direnv-mode "Direnv")
  ("e" emmet-mode "Emmet")
  ("f" global-flycheck-mode "Flycheck")
  ("F" flyspell-mode "Flyspell")
  ("g" god-mode-all "God Mode")
  ("h" nhexl-mode "Hex")
  ("H" global-diff-hl-mode "Diff HL")
  ("i" indent-guide-global-mode "Indent Guide")
  ("l" visual-line-mode "Line")
  ("L" global-lentic-mode "Lentic")
  ("n" nameless-mode "Nameless")
  ("o" global-origami-mode "Origami")
  ("r" rainbow-identifiers-mode "Rainbow")
  ("p" pandoc-mode "Pandoc")
  ("s" firestarter-mode "Firestarter")
  ("S" selected-minor-mode "Selected")
  ("v" view-mode "View")
  ("w" ws-butler-mode "WS Butler")
  ("y" yas-global-mode "YASnippet")
  ("Y" lispy-mode "Lispy")
  ("q" nil "Quit" :color blue))

(bind-key "n" #'hydra-minor-modes/body ejmr-hydra-map)

;;; Major Modes

(defhydra hydra-major-modes (:color blue)
  "Major Mode"
  ("a" adoc-mode "Asciidoc")
  ("m" markdown-mode "Markdown")
  ("n" nasm-mode "NASM")
  ("p" projectile-mode "Projectile")
  ("t" text-mode "Text"))

(bind-key "m" #'hydra-major-modes/body ejmr-hydra-map)

;;; Info Mode

(defhydra hydra-info (:color blue :hint nil)
  "
Info-mode:

  ^^_]_ forward  (next logical node)    ^^_l_ast (←)     _u_p (↑)                          _f_ollow reference    _T_OC
  ^^_[_ backward (prev logical node)    ^^_r_eturn (→)   _m_enu (↓) (C-u for new window)   _i_ndex               _d_irectory
  ^^_n_ext (same level only)            ^^_H_istory      _g_oto (C-u for new window)       _,_ next index item   _c_opy node name
  ^^_p_rev (same level only)            _<_/_t_op        _b_eginning of buffer             virtual _I_ndex       _C_lone buffer
  regex _s_earch (_S_ case sensitive)   ^^_>_ final      _e_nd of buffer                   ^^                    _a_propos

  _1_ .. _9_ Pick first .. ninth item in the node's menu.

"
  ("]"   Info-forward-node)
  ("["   Info-backward-node)
  ("n"   Info-next)
  ("p"   Info-prev)
  ("s"   Info-search)
  ("S"   Info-search-case-sensitively)
  ("l"   Info-history-back)
  ("r"   Info-history-forward)
  ("H"   Info-history)
  ("t"   Info-top-node)
  ("<"   Info-top-node)
  (">"   Info-final-node)
  ("u"   Info-up)
  ("^"   Info-up)
  ("m"   Info-menu)
  ("g"   Info-goto-node)
  ("b"   beginning-of-buffer)
  ("e"   end-of-buffer)
  ("f"   Info-follow-reference)
  ("i"   Info-index)
  (","   Info-index-next)
  ("I"   Info-virtual-index)
  ("T"   Info-toc)
  ("d"   Info-directory)
  ("c"   Info-copy-current-node-name)
  ("C"   clone-buffer)
  ("a"   info-apropos)
  ("1"   Info-nth-menu-item)
  ("2"   Info-nth-menu-item)
  ("3"   Info-nth-menu-item)
  ("4"   Info-nth-menu-item)
  ("5"   Info-nth-menu-item)
  ("6"   Info-nth-menu-item)
  ("7"   Info-nth-menu-item)
  ("8"   Info-nth-menu-item)
  ("9"   Info-nth-menu-item)
  ("?"   Info-summary "Info summary")
  ("h"   Info-help "Info help")
  ("q"   Info-exit "Info exit")
  ("C-g" nil "cancel" :color blue))

(bind-key "?" #'hydra-info/body Info-mode-map)

;;; Flycheck

(defhydra hydra-flycheck (:color blue)
    "
^
^Flycheck^          ^Errors^            ^Checker^
^────────^──────────^──────^────────────^───────^───────────
[_q_] quit          [_c_] check         [_s_] select
[_v_] verify setup  [_n_] next          [_d_] disable
[_m_] manual        [_p_] previous      [_?_] describe
^^                  ^^                  ^^
"
  ("q" nil)
  ("c" flycheck-buffer)
  ("d" flycheck-disable-checker)
  ("m" flycheck-manual)
  ("n" flycheck-next-error :color red)
  ("p" flycheck-previous-error :color red)
  ("s" flycheck-select-checker)
  ("v" flycheck-verify-setup)
  ("?" flycheck-describe-checker))

(bind-key "f" #'hydra-flycheck/body ejmr-hydra-map)

;;; Misc Commands

(defhydra hydra-commands (:color blue :columns 4)
  "Commands"
  ("b" ejmr-browse-current-file "Browse")
  ("c" rpn-calc "RPN Calculator")
  ("e" editorconfig-mode-apply "EditorConfig")
  ("f" elfeed "Elfeed")
  ("l" refine "Refine List")
  ("m" man "Man")
  ("r" revert-buffer "Revert Buffer")
  ("s" ejmr-edit-current-file-as-root "Sudo File")
  ("t" find-temp-file "Temp File")
  ("T" tldr "TL;DR")
  ("u" ejmr-update-available-packages "Update Packages")
  ("v" vlf "View Large File")
  ("w" woman "WoMan")
  ("x" re-builder "Regex Builder")
  ("z" zone "Zone"))

(bind-key "x" #'hydra-commands/body ejmr-hydra-map)
(bind-key "x" #'hydra-commands/body ejmr-command-shortcut-map)
(bind-key "s-x" #'hydra-commands/body ejmr-command-shortcut-map)


;;; Comment DWIM

(use-package comment-dwim-2
  :bind (:map ejmr-command-shortcut-map (";" . comment-dwim-2)))


;;; Expand Region

(use-package expand-region
  :config
  (use-package change-inner)
  (defun ejmr-mark-line ()
    "Mark the current line."
    (interactive)
    (end-of-line)
    (set-mark (point))
    (beginning-of-line))
  (defhydra hydra-mark (:color blue :idle 1.5 :columns 3)
    "Mark"
    ("d" er/mark-defun "Defun / Function")
    ("f" er/mark-defun "Defun / Function")
    ("w" er/mark-word "Word")
    ("u" er/mark-url "Url")
    ("e" mark-sexp "S-Expression")
    ("E" er/mark-email "Email")
    ("b" mark-whole-buffer "Buffer")
    ("l" ejmr-mark-line "Line")
    ("p" er/mark-text-paragraph "Paragraph")
    ("s" er/mark-symbol "Symbol")
    ("S" er/mark-symbol-with-prefix "Prefixed symbol")
    ("q" er/mark-inside-quotes "Inside Quotes")
    ("Q" er/mark-outside-quotes "Outside Quotes")
    ("(" er/mark-inside-pairs "Inside Pairs")
    ("[" er/mark-inside-pairs "Inside Pairs")
    ("{" er/mark-inside-pairs "Inside Pairs")
    (")" er/mark-outside-pairs "Outside Pairs")
    ("]" er/mark-outside-pairs "Outside Pairs")
    ("}" er/mark-outside-pairs "Outside Pairs")
    ("t" er/mark-inner-tag "Inner Tag")
    ("T" er/mark-outer-tag "Outer Tag")
    ("c" er/mark-comment "Comment")
    ("a" er/mark-html-attribute "HTML Attribute")
    ("i" change-inner "Inner")
    ("o" change-outer "Outer")
    ("." er/expand-region "Expand Region" :exit nil)
    ("," er/contract-region "Contract Region" :exit nil))
  (bind-key "SPC" #'hydra-mark/body ejmr-custom-bindings-map))


;;; Avy and Ace

;;; TODO: Check recent Avy commits for any new commands that I may
;;; want to bind to keys.
(use-package avy
  :config
  (setq avy-keys '(?a ?o ?e ?u ?h ?t ?n ?s))
  (setq avy-background t)
  (setq avy-all-windows nil)
  (setq avy-timeout-seconds 1)

  (use-package avy-zap
    :config
    (bind-key "M-g z" #'avy-zap-to-char-dwim))

  (use-package ace-flyspell
    :config (ace-flyspell-setup))

  (use-package link-hint
    :config
    (bind-key "M-g l" #'link-hint-open-link)
    (bind-key "M-g C-l" #'link-hint-copy-link))

  (use-package avy-flycheck
    :config (avy-flycheck-setup))

  (use-package ace-jump-buffer
    :config
    (bind-key "M-g b" #'ace-jump-buffer))

  (bind-key "C-'" #'avy-isearch isearch-mode-map)

  (defhydra hydra-avy-copy (:color blue)
    "Copy"
    ("l" avy-copy-line "Line")
    ("r" avy-copy-region "Region"))
  (defhydra hydra-avy-move (:color blue)
    "Move"
    ("l" avy-move-line "Line")
    ("r" avy-move-region "Region"))
  (defhydra hydra-avy-kill (:color blue)
    "Kill"
    ("l" avy-kill-whole-line "Line")
    ("r" avy-kill-region "Region")
    ("M-l" avy-kill-ring-save-whole-line "Save Line")
    ("M-r" avy-kill-ring-save-region "Save Region"))

  (defhydra hydra-avy (global-map "M-g" :color blue :hint nil)
    "Goto"
    ("c" avy-goto-char-timer "Character")
    ("g" avy-goto-line "Line")
    ("w" avy-goto-word-1 "Word")
    ("s" avy-goto-subword-1 "Subword")
    ("M-c" hydra-avy-copy/body "Copy")
    ("M-m" hydra-avy-move/body "Move")
    ("M-k" hydra-avy-kill/body "Kill")))

(use-package ace-window
  :bind ("C-x o" . ace-window))

(use-package ace-link
  :config (ace-link-setup-default))


;;; Ivy and Swiper

(use-package swiper
  :bind (("C-s" . swiper)
	 ("C-r" . swiper-all)))

(use-package ivy
  :diminish 'ivy-mode
  :config
  (use-package ivy-hydra)
  (use-package ivy-todo
    :commands ivy-todo
    :bind (:map org-mode-map ("C-c C-i" . ivy-todo)))
  (use-package counsel
    :config
    (use-package counsel-dash)
    (use-package counsel-gtags
      :config
      (defhydra hydra-counsel-gtags (:color blue :columns 4)
	"GNU GLOBAL"
	("d" counsel-gtags-find-definition "Definition")
	("r" counsel-gtags-find-reference "Reference")
	("s" counsel-gtags-find-symbol "Symbol")
	("f" counsel-gtags-find-file "File")
	("n" counsel-gtags-go-forward "Next" :color red)
	("p" counsel-gtags-go-backward "Previous" :color red)
	("c" counsel-gtags-create-tags "Create")
	("u" counsel-gtags-update-tags "Update"))
      (bind-key "a" #'hydra-counsel-gtags/body ejmr-hydra-map)
      (bind-key "g" #'counsel-gtags-dwim ejmr-command-shortcut-map))

    (bind-key "r" #'counsel-file-register ejmr-help-map)
    (bind-key "f" #'counsel-describe-function ejmr-help-map)
    (bind-key "d" #'counsel-dash ejmr-help-map)
    (bind-key "v" #'counsel-describe-variable ejmr-help-map)
    (bind-key "l" #'counsel-find-library ejmr-help-map)
    (bind-key "s" #'counsel-info-lookup-symbol ejmr-help-map)
    (bind-key "i" #'counsel-imenu ejmr-help-map)
    (bind-key "b" #'counsel-bookmark ejmr-help-map)
    (bind-key "m" #'counsel-descbinds ejmr-help-map)
    (bind-key "t" #'counsel-tmm ejmr-help-map)
    (bind-key "u" #'counsel-unicode-char ejmr-help-map)
    (bind-key "h" #'counsel-load-theme ejmr-help-map)
    (defhydra hydra-apropos (:color blue :hint nil)
      "
_a_propos        _c_ommand
_d_ocumentation  _l_ibrary
_v_ariable       _u_ser-option
^ ^          valu_e_"
      ("a" apropos)
      ("d" apropos-documentation)
      ("v" apropos-variable)
      ("c" apropos-command)
      ("l" apropos-library)
      ("u" apropos-user-option)
      ("e" apropos-value))
    (bind-key "a" #'hydra-apropos/body ejmr-help-map)
    (bind-key "C-x 8 <return>" #'counsel-unicode-char)

    (defun ejmr-switch-to-info-other-frame ()
      "Opens Info in a new frame."
      (interactive)
      (select-frame-set-input-focus (make-frame-command))
      (info))
    (bind-key "s-i" #'ejmr-switch-to-info-other-frame ejmr-help-map)

    (key-seq-define-global "ZF" #'counsel-recentf)

    (ivy-set-actions
     'counsel-recentf
     '(("v" view-file "view")))

    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-count-format "(%d/%d) ")))


;;; Highlighting

(use-package hl-todo
  :config
  (defhydra hydra-todo (:pre
			(hl-todo-mode 1)
			:post
			(hl-todo-mode -1))
    "Todo"
    ("n" hl-todo-next "Next")
    ("p" hl-todo-previous "Previous")
    ("o" hl-todo-occur "Occur")
    ("q" nil "Quit" :color blue :exit t))
  (bind-key "s-t" #'hydra-todo/body ejmr-command-shortcut-map))

(use-package highlight-blocks)
(use-package rainbow-delimiters)
(use-package rainbow-identifiers)

(use-package symbol-overlay
  :config
  (bind-key "s" #'symbol-overlay-put ejmr-command-shortcut-map))


;;; Quickrun

(use-package quickrun
  :config
  (setq quickrun-focus-p nil)
  (quickrun-set-default "c" "c/clang")
  (quickrun-add-command "asciidoc/html5"
    '((:command . "asciidoc")
      (:exec . "%c -b html5 -a icons -a toc2 -a theme=flask %s"))
    :mode 'asciidoc-mode)
  (defhydra hydra-quickrun (:color blue)
    "Quickrun"
    ("q" quickrun "run")
    ("r" quickrun-region "region")
    ("w" quickrun-with-arg "with-arg")
    ("s" quickrun-shell "shell")
    ("c" quickrun-compile-only "compile")
    ("p" quickrun-replace-region "replace"))
  (bind-key "q" #'hydra-quickrun/body ejmr-hydra-map)
  (bind-key "q" #'hydra-quickrun/body ejmr-command-shortcut-map))

;;; `isend-mode' is somewhat similar to Quickrun, in that I can
;;; roughly accomplish the same thing using the `quickrun-*' commands.
;;; But `isend-mode' makes things easier when working, for example,
;;; with shell scripts.
(use-package isend-mode
  :config
  (setq-default isend-forward-line t)
  (bind-key "i" (defhydra hydra-isend ()
		  "isend"
		  ("a" isend-associate "Associate")
		  ("RET" isend-send "Line")
		  ("f" isend-send-defun "Function")
		  ("b" isend-send-buffer "Buffer")
		  ("d" isend-display-buffer "Display")
		  ("q" nil "Quit" :color blue))
	    ejmr-hydra-map))


;;; Git

(use-package git-modes
  :load-path "/home/eric/.emacs.d/local/git-modes")

(use-package git-timemachine
  :commands git-timemachine-toggle
  :bind ("C-x v T" . git-timemachine-toggle))

(use-package git-link
  :config
  (defhydra hydra-git-link (:color blue)
    "Git Link"
    ("l" git-link "Link")
    ("c" git-link-commit "Commit")
    ("h" git-link-homepage "Homepage"))
  (bind-key "k" #'hydra-git-link/body vc-prefix-map))

(progn
  (defhydra hydra-git (:color blue)
    "Git"
    ("f" counsel-git "File")
    ("g" counsel-git-grep "Grep")
    ("o" counsel-git-grep-occur "Occur")
    ("r" counsel-git-grep-query-replace "Replace")
    ("s" counsel-git-stash "Stash")
    ("l" counsel-git-log "Log")
    ("q" nil "Quit"))

  (bind-key "c" #'hydra-git/body vc-prefix-map))


;;; Programming Modes and Settings

(use-package string-inflection
  :commands (string-inflection-all-cycle)
  :config
  (use-package cycle-quotes)
  (defhydra hydra-string-inflection ()
    "Inflection"
    ("c" capitalize-word "Capitalize")
    ("u" upcase-word "Upcase")
    ("l" downcase-word "Lowercase")
    ("'" cycle-quotes "Quote")
    ("SPC" string-inflection-all-cycle "Cycle")
    ("q" nil "Quit" :color blue))
  (bind-key "M-c" #'hydra-string-inflection/body))

(use-package just-mode
  :load-path "/home/eric/.emacs.d/local/just-mode"
  :mode "Justfile")

(use-package fuel :disabled t)

(use-package aggressive-indent-mode
  :commands (aggressive-indent-mode)
  :config
  (global-aggressive-indent-mode t))

(use-package racket-mode
  :commands (racket-mode)
  :mode ("\\.rkt\\'" . racket-mode))

(use-package scratch)

(use-package indent-tools
  :bind (:map ejmr-hydra-map (">" . indent-tools-hydra/body)))

(setq require-final-newline t)
(setq-default buffer-file-coding-system 'utf-8-unix)
(setq-default default-buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

(use-package assess)
(use-package stupid-indent-mode)

(use-package po-mode
  :mode "\\.po\\'")

(progn
  (defun ejmr-setup-cc-mode ()
    (c-set-style "linux")
    (flycheck-select-checker 'c/c++-clangcheck))
  (add-hook 'c-mode-hook 'ejmr-setup-cc-mode)
  (add-hook 'c++-mode-hook 'ejmr-setup-cc-mode))

(use-package modern-cpp-font-lock
  :config (modern-c++-font-lock-global-mode t))

(use-package polymode)

(use-package neon-mode)
(use-package nhexl-mode)
(use-package restclient)

(use-package diff
  :mode ("COMMIT_EDITMSG" . diff-mode))

(use-package diff-hl
  :bind ("C-x v =" . diff-hl-diff-goto-hunk)
  :config
  (global-diff-hl-mode 1))

(use-package dumb-jump
  :config
  (setq dumb-jump-selector 'ivy)
  (setq dumb-jump-prefer-searcher 'ag)
  (bind-key "d" (defhydra hydra-dumb-jump (:color pink)
		  "Dumb Jump"
		  ("g" dumb-jump-go "Go")
		  ("b" dumb-jump-back "Back")
		  ("l" dumb-jump-quick-look "Look")
		  ("e" dumb-jump-go-prefer-external "External")
		  ("w" dumb-jump-go-other-window "Window" :color blue)
		  ("q" nil "Quit" :color blue))
	    ejmr-command-shortcut-map)
  (dumb-jump-mode 1))

(use-package vdiff
  :disabled t
  :config
  (bind-key "C-c v" vdiff-mode-prefix-map vdiff-mode-map))

(use-package realgud :disabled t)
(use-package cmake-ide :disabled t)
(use-package malinka :disabled t)

(use-package makefile-executor
  :commands (makefile-executor-mode)
  :config
  (add-hook 'makefile-mode-hook 'makefile-executor-mode))

(use-package rtags
  :disabled t
  :config
  (use-package ivy-rtags :disabled t)
  (use-package flycheck-rtags :disabled t)
  (use-package company-rtags :disabled t))

(use-package brainfuck-mode)

(use-package sqlup-mode
  :config (add-hook 'sql-mode-hook 'sqlup-mode))
(use-package emacsql
  :config
  (use-package emacsql-sqlite))

(use-package clang-format)
(use-package irony)
(use-package cov)

(use-package lice)
(use-package forth-mode)
(use-package go-mode)
(use-package fish-mode)
(use-package lua-mode)
(use-package nasm-mode
  :mode (("\\.asm\\'" . nasm-mode)
	 ("\\.s\\'" . nasm-mode)))

(use-package tup-mode
  :load-path "/home/eric/.emacs.d/local/tup-mode")

(use-package yaml-mode
  :mode ("\\.yml\\'" . yaml-mode))

(use-package rust-mode
  :config
  (use-package cargo :diminish cargo-minor-mode))

(use-package php-mode
  :config
  (use-package psysh))

(use-package ini-mode)
(use-package haskell-mode)
(use-package python-mode)
(use-package js2-mode)
(use-package json-navigator)
(use-package tern :disabled t)
(use-package clojure-mode)

(use-package conf-mode
  :mode ("\\.toml\\'" . conf-mode))

(use-package ws-butler
  :diminish 'ws-butler-mode
  :config (add-hook 'prog-mode-hook 'ws-butler-mode))

(use-package yasnippet
  :diminish 'yas-minor-mode
  :config
  (yas-global-mode 1)
  (bind-key "C-?" #'yas-expand yas-minor-mode-map)
  (use-package auto-yasnippet
    :config
    (bind-key "s-x a c" #'aya-create yas-minor-mode-map)
    (bind-key "s-x a e" #'aya-expand yas-minor-mode-map)))

(use-package flycheck
  :config
  (use-package flycheck-clangcheck
    :disabled t
    :config
    (setq flycheck-clangcheck-analyze t))
  (use-package flycheck-mypy :disabled t)
  (use-package flycheck-package
    :config (flycheck-package-setup))
  (use-package flycheck-rust
    :config
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
  (bind-key "<s-up>" #'flycheck-previous-error)
  (bind-key "<s-down>" #'flycheck-next-error))

(use-package solid-mode
  :load-path "/home/eric/.emacs.d/local/solid-mode"
  :config
  (quickrun-add-command "solid"
    '((:command . "solid")
      (:exec . "%c %s")
      (:compile-only . "%c %s")
      (:description . "Compile and execute Solid scripts"))
    :mode 'solid-mode))

(use-package syntactic-close
  :bind ("s-0" . syntactic-close))


;;; Emacs Lisp Programming

(use-package elx)
(use-package face-explorer)
(use-package apiwrap)

(use-package suggest
  :config
  (bind-key "C-c C-s" #'suggest-mode emacs-lisp-mode-map)
  (bind-key "C-c C-e" #'emacs-lisp-mode suggest-mode-map))

(use-package package-lint
  :config
  (bind-key "C-c C-l" #'package-lint-current-buffer emacs-lisp-mode-map))

(use-package el2markdown
  :config
  (defhydra hydra-el2markdown ()
    "el2markdown"
    ("v" el2markdown-view-buffer "View")
    ("f" el2markdown-write-file "Write File")
    ("r" el2markdown-write-readme "Write README")
    ("q" nil "Quit" :color blue))
  (bind-key "C-c C-2" #'hydra-el2markdown/body emacs-lisp-mode-map))

(use-package elisp-refs
  :config
  (bind-key "C-c C-r f" #'elisp-refs-function emacs-lisp-mode-map)
  (bind-key "C-c C-r m" #'elisp-refs-macro emacs-lisp-mode-map)
  (bind-key "C-c C-r v" #'elisp-refs-variable emacs-lisp-mode-map)
  (bind-key "C-c C-r p" #'elisp-refs-special emacs-lisp-mode-map)
  (bind-key "C-c C-r s" #'elisp-refs-symbol emacs-lisp-mode-map))

(use-package nameless
  :diminish 'nameless-mode
  :config
  (setq nameless-private-prefix t)
  (add-hook 'emacs-lisp-mode-hook #'nameless-mode))

(bind-key "C-c C-b" #'emacs-lisp-byte-compile-and-load emacs-lisp-mode-map)
(bind-key "C-c C-t" #'top-level emacs-lisp-mode-map)

(defhydra hydra-lisp-eval (:color blue :columns 2 :idle 1.0)
  "Lisp Eval"
  ("r" eval-region "Region")
  ("b" eval-buffer "Buffer")
  ("e" eval-expression "S-expression")
  ("l" eval-last-sexp "Last S-expression")
  ("L" eval-last-sexp-print-value "Last S-expression and Print Value")
  ("d" eval-defun "Defun / Function")
  ("f" eval-defun "Defun / Function"))

(bind-key "C-c C-e" #'hydra-lisp-eval/body emacs-lisp-mode-map)
(bind-key "C-c C-e" #'hydra-lisp-eval/body lisp-mode-map)


;;; External Tools and System Management

;;; TODO: Configure it for apt and setup a hydra for commands.
(use-package system-packages)

(use-package pass)


;;; Project Management

(defhydra hydra-desktop (:color blue)
  "Desktop"
  ("c" desktop-clear "clear")
  ("s" desktop-save "save")
  ("r" desktop-revert "revert")
  ("d" desktop-change-dir "directory"))

(bind-key "d" #'hydra-desktop/body ejmr-hydra-map)

(use-package projectile
  :diminish projectile-mode
  :config
  (use-package counsel-projectile
    :config (counsel-projectile-on))
  (setq projectile-completion-system 'ivy)
  (projectile-mode nil))

(use-package find-file-in-project)


;;; HTML, XML, CSS

(use-package emmet-mode
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'html-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook 'emmet-mode))

(use-package xmlgen)


;;; Dired

(use-package dired :config

  (setq dired-dwim-target t)

  (defun ejmr-dired-view-readme ()
    "View the README file in the current Dired directory.

    This command will mark files that match the regular expression
    `^README' and then moves to the first such marked file.  The
    command then removes the mark---actually all asterik
    marks---opens the README in View Mode, and finally maximizes the
    size of the window in order to facilitate easier reading."
    (interactive)
    (goto-char (point-min))
    (dired-mark-files-regexp "^README")
    (dired-next-marked-file 1)
    (dired-unmark-all-files ?\*)
    (dired-view-file)
    (delete-other-windows))
  (bind-key "* R" #'ejmr-dired-view-readme dired-mode-map)

  (use-package make-it-so
    :commands make-it-so
    :config (bind-key "C-c ." #'make-it-so dired-mode-map))

  (use-package runner)

  (defun ejmr-dired-find-file (&optional arg)
    "Open file under point or each marked file.

    Also opens the next N files when given the prefix `arg'."
    (interactive "P")
    (let ((fn-list (dired-get-marked-files nil arg)))
      (mapc 'find-file fn-list)))

  (bind-key "F" #'ejmr-dired-find-file dired-mode-map)

  (use-package dired-k
    :config
    (bind-key "K" #'dired-k dired-mode-map)
    (setq dired-k-style 'git))

  (use-package dired-atool
    :config
    (bind-key "z" #'dired-atool-do-unpack dired-mode-map)
    (bind-key "Z" #'dired-atool-do-pack dired-mode-map))

  (use-package dired-efap
    :config
    (bind-key "E" #'dired-efap dired-mode-map)))


;;; General Editing Utilities

(progn
  (defun ejmr-smart-open-line-below ()
    "Insert and indent an empty line after the current line."
    (interactive)
    (move-end-of-line nil)
    (newline-and-indent))
  (defun ejmr-smart-open-line-above ()
    "Insert and indent an empty line above the current line."
    (interactive)
    (move-beginning-of-line nil)
    (newline-and-indent)
    (forward-line -1)
    (indent-according-to-mode))
  (bind-key "C-o" #'ejmr-smart-open-line-below)
  (bind-key "C-S-o" #'ejmr-smart-open-line-above))

(use-package shrink-whitespace
  :commands (shrink-whitespace)
  :bind ("M-SPC" . shrink-whitespace))

(use-package decide)

(use-package linkd
  :load-path "/home/eric/.emacs.d/local/linkd")

(use-package demo-it)

(use-package beginend
  :diminish beginend-global-mode
  :config
  (beginend-global-mode t))

(defun ejmr-edit-current-file-as-root ()
  "Use TRAMP to `sudo' the current file."
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/sudo:root@localhost:" buffer-file-name))))

(defun ejmr-browse-current-file ()
  "Open current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
	     (tramp-tramp-file-p file-name))
	(error "Cannot open TRAMP file")
      (browse-url (concat "file://" file-name)))))

(use-package firestarter
  :diminish 'firestarter-mode
  :config
  (firestarter-mode t))

(use-package duplicate-thing
  :config
  (key-seq-define-global "qd" #'duplicate-thing))

(use-package operate-on-number
  :config
  (key-seq-define-global "qn" #'operate-on-number-at-point))

(use-package corral
  :config
  (setq corral-preserve-point nil)
  (global-set-key (kbd "M-9") 'corral-parentheses-backward)
  (global-set-key (kbd "M-0") 'corral-parentheses-forward)
  (global-set-key (kbd "M-[") 'corral-brackets-backward)
  (global-set-key (kbd "M-]") 'corral-brackets-forward)
  (global-set-key (kbd "M-{") 'corral-braces-backward)
  (global-set-key (kbd "M-}") 'corral-braces-forward)
  (global-set-key (kbd "M-\"") 'corral-double-quotes-backward))

(use-package ace-isearch
  :disabled t
  :diminish 'ace-isearch-mode
  :config
  (global-ace-isearch-mode 1)
  (setq ace-isearch-function 'avy-goto-word-1)
  (setq ace-isearch-use-jump 'printing-char)
  (setq ace-isearch-function-from-isearch 'ace-isearch-swiper-from-isearch))

(use-package recursive-narrow
  :bind
  (("C-x n n" . recursive-narrow-or-widen-dwim)
   ("C-x n w" . recursive-widen)))

(use-package ivy-pages
  :bind ("M-g p" . ivy-pages))

(defhydra hydra-page (ctl-x-map "" :pre (widen))
  "page"
  ("]" forward-page "next")
  ("[" backward-page "previous")
  ("n" narrow-to-page "narrow" :bind nil :exit t)
  ("i" ivy-pages "ivy" :color blue :bind nil)
  ("q" nil "quit" :bind nil :color blue))


;;; Text Reading, Editing, and Writing

;;; TODO: Should I create a simple major mode for dealing with files
;;; containing nothing but lists of URIs and move this function into
;;; that mode's keymap?
(defun ejmr-sort-buffer-lines-and-delete-duplicates ()
  "Sorts all lines in the buffer and deletes duplicates.

    This automates a task I perform very often with my text file of
    saved URIs which Conkeror creates."
  (interactive)
  (mark-whole-buffer)
  (sort-lines nil (point-min) (point-max))
  (delete-duplicate-lines (point-min) (point-max)))

(use-package ids-edit :disabled t)

(use-package selected
  :diminish selected-minor-mode
  :config
  (selected-global-mode t)
  :bind (:map selected-keymap
	      ("n" . narrow-to-region)
	      (";" . comment-dwim-2)
    ("$" . flyspell-region)
    ("u" . upcase-region)
    ("d" . downcase-region)
    ("c" . count-words-region)
    ("\\" . indent-region)
    ("w" . copy-region-as-kill)
    ("W" . copy-as-format)
    ("k" . kill-region)
    ("m" . apply-macro-to-region-lines)))

(use-package latex-mode
  :config
  (use-package magic-latex-buffer
    :config (add-hook 'latex-mode-hook 'magic-latex-buffer)))

(use-package google-translate
  :config
  (use-package google-translate-smooth-ui
    :commands google-translate-smooth-translate
    :bind ("C-M-$" . google-translate-smooth-translate)
    :config
    (setq-default google-translate-translation-directions-alist
		  '(("en" . "ja")
		    ("ja" . "en")))))

(use-package wiki-nav
  :disabled t
  :load-path "/home/eric/.emacs.d/local/button-lock"
  :diminish (wiki-nav-mode button-lock-mode)
  :config (global-wiki-nav-mode 1)

  (defun ejmr-counsel-wiki-nav (prefix)
    "Jump to a wiki-nav link.

This command will not show duplicate link names, which means it
cannot jump to multiple instances of the same link within a
buffer.  It will also not show any link beginning with the
less-than character, i.e. links for jumping back to a previous
buffer location.

If given the universal prefix this command will present links in
all buffers."
    (interactive "P")
    (let* ((links (if prefix (wiki-nav-links) (wiki-nav-links-all-buffers)))
	   (names (mapcar (lambda (name)
			    (unless (string-prefix-p "<" name)
			      name))
			  (delete-dups (mapcar #'first links)))))
      (ivy-read "(%d) Link: "
		names
		:require-match t
		:history 'ejmr-counsel-wiki-nav
		:caller 'ejmr-counsel-wiki-nav
		:action (lambda (link-name)
			  (with-ivy-window
			    (let* ((link-info (cdr (assoc link-name links)))
				   (link-buffer (car link-info))
				   (link-position (cdr link-info)))
			      (switch-to-buffer link-buffer)
			      (goto-char link-position)))))))

  (key-seq-define-global "qw" #'ejmr-counsel-wiki-nav))

(use-package focus
  :commands (focus-mode)
  :config
  (key-seq-define-global "ZC" #'focus-mode))

(use-package copy-as-format
  :config
  (setq-default copy-as-format-default "markdown"))

(use-package bbcode-mode
  :load-path "/home/eric/.emacs.d/local/bbcode-mode")

(use-package epub-mode
  :load-path "/home/eric/.emacs.d/local/epub-mode.el")

;;; TODO: Install the `unfill' package then delete this function.
(defun ejmr-refill-to-one-line ()
  "Refills a paragraph to a single line."
  (interactive)
  (let ((fill-column 100000))
    (fill-individual-paragraphs (point-min) (point-max))))

(use-package wc-mode)
(use-package darkroom
  :config
  (defhydra hydra-darkroom ()
    "Darkroom Margin"
    ("+" darkroom-increase-margins "Increase")
    ("-" darkroom-decrease-margins "Decrease"))
  (bind-key "C-M-+" #'hydra-darkroom/darkroom-increase-margins darkroom-mode-map)
  (bind-key "C-M--" #'hydra-darkroom/darkroom-decrease-margins darkroom-mode-map))
(use-package fountain-mode
  :config (add-hook 'fountain-mode-hook 'darkroom-tentative-mode))

(use-package adoc-mode
  :commands (adoc-mode buffer-face-mode)
  :mode (("\\.adoc\\'" . adoc-mode)
	 ("\\.asciidoc\\'" . adoc-mode)
	 ("\\.txt\\'" . adoc-mode))
  :config
  (bind-key "C-c C-b" #'buffer-face-mode adoc-mode-map))

(use-package flyspell-correct
  :diminish 'flyspell-correct-auto-mode
  :config
  (use-package flyspell-correct-ivy
    :config
    (defhydra hydra-flyspell-correct (:color blue)
      "Flyspell Correct"
      ("w" flyspell-correct-word-generic "Word at Point")
      ("p" flyspell-correct-previous-word-generic "Previous Word")
      ("n" flyspell-correct-next-word-generic "Next Word")
      ("a" flyspell-correct-auto-mode "Auto Mode" :color red))
    (bind-key "C-$" #'hydra-flyspell-correct/body flyspell-mode-map)
    (key-seq-define flyspell-mode-map "Z$" #'flyspell-correct-previous-word-generic)))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("README\\.markdown\\'" . gfm-mode)
	 ("\\.ronn\\'" . markdown-mode)
	 ("\\.md\\'" . gfm-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :config
  (setq markdown-command "pandoc -f markdown -t html5")
  (add-hook 'markdown-mode-hook 'auto-fill-mode)
  (defhydra hydra-markdown (:hint nil :pre (ivy-mode nil) :post (ivy-mode t))
    "
Formatting        C-c C-s    _s_: bold          _e_: italic     _b_: blockquote   _p_: pre-formatted    _c_: code
Headings          C-c C-t    _h_: automatic     _1_: h1         _2_: h2           _3_: h3               _4_: h4
Lists             C-c C-x    _m_: insert item
Demote/Promote    C-c C-x    _l_: promote       _r_: demote     _u_: move up      _d_: move down
Links, footnotes  C-c C-a    _L_: link          _U_: uri        _F_: footnote     _W_: wiki-link      _R_: reference
"
    ("s" markdown-insert-bold)
    ("e" markdown-insert-italic)
    ("b" markdown-insert-blockquote :color blue)
    ("p" markdown-insert-pre :color blue)
    ("c" markdown-insert-code)
    ("h" markdown-insert-header-dwim)
    ("1" markdown-insert-header-atx-1)
    ("2" markdown-insert-header-atx-2)
    ("3" markdown-insert-header-atx-3)
    ("4" markdown-insert-header-atx-4)
    ("m" markdown-insert-list-item)
    ("l" markdown-promote)
    ("r" markdown-demote)
    ("d" markdown-move-down)
    ("u" markdown-move-up)
    ("L" markdown-insert-link :color blue)
    ("U" markdown-insert-uri :color blue)
    ("F" markdown-insert-footnote :color blue)
    ("W" markdown-insert-wiki-link :color blue)
    ("R" markdown-insert-reference-link-dwim :color blue))
  (bind-key "C-c h m" #'hydra-markdown/body markdown-mode-map))

(use-package pandoc-mode
  :config
  (add-hook 'markdown-mode-hook 'pandoc-mode))

(use-package edit-indirect)
(use-package underline-with-char)

(defhydra hydra-text (:color amaranth)
  "
^Major Modes^    ^Minor Modes^    ^Actions^
^───────────^────^───────────^────^───────^──────────
[_T_] Text       [_D_] Darkroom   [_s_] Sort Lines
[_A_] AsciiDoc   [_$_] Flyspell   [_a_] Align Regexp
[_M_] Markdown   [_u_] Auto Fill  [_p_] Delete Duplicates^^
[_G_] GFM                       [_r_] Rectangle Commands^^
[_F_] Fountain                  [_n_] Underline With Character
[_I_] Indirect Edit
^^
"
  ("T" text-mode)
  ("n" underline-with-char :color blue)
  ("I" edit-indirect-region :color blue)
  ("A" adoc-mode)
  ("a" align-regexp)
  ("M" markdown-mode)
  ("F" fountain-mode)
  ("G" gfm-mode)
  ("D" darkroom-mode)
  ("$" flyspell-mode)
  ("s" sort-lines)
  ("u" auto-fill-mode)
  ("p" delete-duplicate-lines)
  ("r" hydra-rectangle/body)
  ("q" nil :color blue))

(bind-key "t" #'hydra-text/body ejmr-hydra-map)


;;; Web and Online Services

(use-package transfer-sh)

(use-package wandbox
  :config
  (defhydra hydra-wandbox (:color amaranth :hint nil)
    "
Wandbox
————————————————————————————————————————
Compile: _F_ile     _L_ist Compilers
         _B_uffer   _I_nsert Template
         _R_egion
"
    ("F" wandbox-compile-file)
    ("B" wandbox-compile-buffer)
    ("R" wandbox-compile-region)
    ("L" wandbox-list-compilers :color red)
    ("I" wandbox-insert-template)
    ("q" nil))
  (bind-key "w" #'hydra-wandbox/body ejmr-command-shortcut-map))

(use-package elfeed)

(progn
  (defun ejmr-search-cheat-sh ()
    "Search `http://cheat.sh/' for help on commands and code."
    (interactive)
    (ivy-read "Command or Topic: "
	      (process-lines "curl" "--silent" "-A \"GNU Emacs (curl)\"" "http://cheat.sh/:list?T&q")
	      :require-match t
	      :sort t
	      :history 'ejmr-search-cheat-sh
	      :action (lambda (input)
			(browse-url (concat "http://cheat.sh/" input "?T&q")))
	      :caller 'ejmr-search-cheat-sh))
  (bind-key "c" #'ejmr-search-cheat-sh ejmr-help-map))

(use-package browse-at-remote
  :bind ("C-x v t" . browse-at-remote))

(use-package numbers)

(use-package webpaste
  :config
  (defhydra hydra-webpaste (:color blue)
    "Paste to Web"
    ("b" webpaste-paste-buffer "Buffer")
    ("r" webpaste-paste-region "Region"))
  (bind-key "p" #'hydra-webpaste/body ejmr-command-shortcut-map))


;;; Custom File

(setq custom-file "/home/eric/.emacs.d/custom.el")
(load custom-file)

;;; init.el ends here

;; Local Variables:
;; firestarter: (byte-compile-file (buffer-file-name))
;; End:
