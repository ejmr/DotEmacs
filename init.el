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


;;; Global Minor Modes

(transient-mark-mode t)
(show-paren-mode t)
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


;;; Global Variables

(setq backup-inhibited t)
(setq make-backup-files nil)
(setq auto-save-default nil)


;;; Registers

(set-register ?i '(file . "/home/eric/.emacs.d/init.el"))
(set-register ?g '(file . "/home/eric/.gitconfig"))
(set-register ?s '(file . "/home/eric/.config/fish/"))
(set-register ?n '(file . "/home/eric/Documents/Notes.org"))
(set-register ?c '(file . "/home/eric/.conkerorrc/"))


;;; Global Utilities

(use-package vlf)
(use-package eieio)
(use-package general :disabled t)
(use-package direnv)
(use-package iedit)
(use-package hierarchy)
(use-package lentic)
(use-package god-mode
  :bind ("s-g" . god-mode-all))
(use-package refine)
(use-package restart-emacs)
(use-package zone)
(use-package tldr)
(use-package fn)
(use-package find-temp-file)
(use-package rainbow-delimiters)
(use-package rainbow-identifiers)
(use-package editorconfig
  :diminish editorconfig-mode
  :config
  (editorconfig-mode t)
  (use-package editorconfig-custom-majormode
    :config
    (add-hook 'editorconfig-custom-hooks 'editorconfig-custom-majormode)))

(use-package lispy
  :config
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
  :config (indent-guide-global-mode t))

(use-package auto-dim-other-buffers :disabled t)

(use-package caps-lock
  :bind ("s-l" . caps-lock-mode))

(use-package ace-window
  :bind ("C-x o" . ace-window))
(use-package ace-link
  :config (ace-link-setup-default))
(use-package resize-window
  :bind ("C-x ^" . resize-window))

(use-package cycle-quotes
  :bind ("C-'" . cycle-quotes))

(use-package tomatinho
  :bind ("s-8" . tomatinho))

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
    :bind ("s-9" . linum-relative-global-mode)))

(use-package tiny
  :bind ("s-t" . tiny-expand))

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

  (bind-key "C-c i" #'hydra-origami/body))

(use-package qwe
  :load-path ("/home/eric/.emacs.d/local/qwe-0.9.5/src"
	      "/home/eric/.emacs.d/local/qwe-0.9.5/ext"))


;;; Org Mode

(use-package org
  :config
  (setq initial-major-mode 'org-mode)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN PROGRESS(p)" "|" "DONE(d)")
          (sequence "REPORT(r)" "BUG(b)" "TESTING(t)" "|" "CLOSED(c)")
          (sequence "BRAINSTORMING(b)" "RFC(r)" "FEEDBACK(f)" "|" "ACCEPTED(a) REJECTED(j)")
          (sequence "|" "CANCELED(c)")))
  (defhydra hydra-org-movement (:color red)
    "Org Mode Movements"
    ("n" outline-next-visible-heading "Next")
    ("p" outline-previous-visible-heading "Previous")
    ("N" org-forward-heading-same-level "Next Level")
    ("P" org-backward-heading-same-level "Previous Level")
    ("u" outline-up-heading "Up")
    ("g" org-goto "Goto" :exit t))
  (bind-key "C-c o" #'hydra-org-movement/body))


;;; Font

(set-frame-font "Bitstream Vera Sans Mono-13" nil t)


;;; Disabled Features

(put 'narrow-to-region 'disabled nil)


;;; Global Hooks

(add-hook 'text-mode-hook 'visual-line-mode)


;;; Global Generic Key-Bindings

(bind-key "<M-return>" #'indent-new-comment-line)
(bind-key "s-a" #'align-regexp)

;;; Setup `s-1' as a prefix key for help commands.
(define-prefix-command 'super-1-map)
(bind-key "s-1" 'super-1-map)


;;; Window Management

;;; Use `s-w' as a prefix key for various window commands.
(define-prefix-command 'super-w-map)
(bind-key "s-w" 'super-w-map)
(bind-key "s-w" #'ace-window super-w-map)
(bind-key "0" #'delete-window super-w-map)
(bind-key "1" #'delete-other-windows super-w-map)
(bind-key "3" #'split-window-horizontally super-w-map)
(bind-key "f" #'find-file-other-window super-w-map)
(bind-key "r" #'find-file-read-only-other-window super-w-map)
(bind-key "b" #'ivy-switch-buffer-other-window super-w-map)
(bind-key "d" #'dired-other-window super-w-map)
(bind-key "." #'xref-find-definitions-other-window super-w-map)

(defun ejmr-split-window-vertically-and-balance ()
  "Splits the window vertically then balances all windows.

This is the equivalent of `C-x 2' followed by `C-x +'."
  (interactive)
  (split-window-vertically)
  (balance-windows))

(bind-key "2" #'ejmr-split-window-vertically-and-balance super-w-map)

(defhydra hydra-window (super-w-map "s" :color amaranth)
  "Window Size"
  ("^" enlarge-window "Taller")
  ("{" shrink-window-horizontally "Narrower")
  ("}" enlarge-window-horizontally "Wider")
  ("-" shrink-window-if-larger-than-buffer "Shrink")
  ("+" balance-windows "Balance")
  ("q" nil "Quit" :color blue))


;;; Completion via Company

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

;;; General Chords

(key-seq-define-global "ZK" #'kill-this-buffer)
(key-seq-define-global "ZB" #'ivy-switch-buffer)
(key-seq-define-global "ZW" #'kill-buffer-and-window)
(key-seq-define-global "ZS" #'save-buffer)


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

(bind-key "C-c z" #'hydra-zoom/body)

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

(bind-key "C-c r" #'hydra-rectangle/body)

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

;;; Desktops

(defhydra hydra-desktop (:color blue)
  "Desktop"
  ("c" desktop-clear "clear")
  ("s" desktop-save "save")
  ("r" desktop-revert "revert")
  ("d" desktop-change-dir "directory"))

(bind-key "C-c d" #'hydra-desktop/body)

;;; Minor Modes

(defhydra hydra-minor-modes (:columns 6)
  "Minor Mode"
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

(bind-key "C-c n" #'hydra-minor-modes/body)

;;; Major Modes

(defhydra hydra-major-modes (:color blue)
  "Major Mode"
  ("a" adoc-mode "Asciidoc")
  ("m" markdown-mode "Markdown")
  ("n" nasm-mode "NASM")
  ("p" projectile-mode "Projectile")
  ("t" text-mode "Text"))

(bind-key "C-c m" #'hydra-major-modes/body)

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

(bind-key "C-c f" #'hydra-flycheck/body)

;;; Misc Commands

(defhydra hydra-commands (:color blue :columns 2)
  "Commands"
  ("b" ejmr-browse-current-file "Browse")
  ("e" editorconfig-mode-apply "EditorConfig")
  ("r" revert-buffer "Revert Buffer")
  ("R" refine "Refine List")
  ("s" ejmr-edit-current-file-as-root "Sudo File")
  ("t" find-temp-file "Temp File")
  ("T" tldr "TL;DR")
  ("v" vlf "View Large File")
  ("w" woman "WoMan")
  ("z" zone "Zone"))

(bind-key "C-c x" #'hydra-commands/body)


;;; Rebind M-; to Comment DWIM

(use-package comment-dwim-2
  :bind ("M-;" . comment-dwim-2))


;;; Expand Region

(use-package expand-region
  :bind ("C-=" . er/expand-region)
  :config
  (use-package change-inner
    :config
    (defhydra hydra-change-inner (:color blue)
      "Change"
      ("i" change-inner "inner")
      ("o" change-outer "outer"))
    (key-seq-define-global "qc" 'hydra-change-inner/body)))


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
    :bind (:map org-mode-map ("C-c t" . ivy-todo)))
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
      (bind-key "C-c a" #'hydra-counsel-gtags/body)
      (bind-key "C-s-g" #'counsel-gtags-dwim))

    (defun ejmr-counsel-file-register ()
      "Jump to a file register with completion."
      (interactive)
      (ivy-read "File Register: "
		(mapcar (lambda (x)
			  (if (eq 'file (cadr x))
			      (cddr x)))
			register-alist)
		:require-match t
		:action (lambda (x) (find-file x))
		:caller 'ejmr-counsel-file-register))

    (bind-key "r" #'ejmr-counsel-file-register super-1-map)
    (bind-key "f" #'counsel-describe-function super-1-map)
    (bind-key "d" #'counsel-dash super-1-map)
    (bind-key "v" #'counsel-describe-variable super-1-map)
    (bind-key "l" #'counsel-find-library super-1-map)
    (bind-key "s" #'counsel-info-lookup-symbol super-1-map)
    (bind-key "i" #'counsel-imenu super-1-map)
    (bind-key "b" #'counsel-bookmark super-1-map)
    (bind-key "m" #'counsel-descbinds super-1-map)
    (bind-key "t" #'counsel-tmm super-1-map)
    (bind-key "u" #'counsel-unicode-char super-1-map)
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
    (bind-key "a" #'hydra-apropos/body super-1-map)
    (bind-key "C-x 8 <return>" #'counsel-unicode-char)

    (key-seq-define-global "ZF" #'counsel-recentf)

    (ivy-set-actions
     'counsel-recentf
     '(("v" view-file "view")))

    (defhydra hydra-git (:color blue)
      "Git"
      ("f" counsel-git "File")
      ("g" counsel-git-grep "Grep")
      ("o" counsel-git-grep-occur "Occur")
      ("r" counsel-git-grep-query-replace "Replace")
      ("s" counsel-git-stash "Stash")
      ("l" counsel-git-log "Log")
      ("q" nil "Quit"))
    (bind-key "C-c g" #'hydra-git/body))
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))


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
  (bind-key "C-c q" #'hydra-quickrun/body))


;;; Programming Modes and Settings

(setq require-final-newline t)
(setq-default buffer-file-coding-system 'utf-8-unix)
(setq-default default-buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

(use-package assess)

(use-package po-mode
  :mode "\\.po\\'")

(progn
  (defun ejmr-setup-cc-mode ()
    (c-set-style "linux")
    (flycheck-select-checker 'c/c++-clangcheck))
  (add-hook 'c-mode-hook 'ejmr-setup-cc-mode)
  (add-hook 'c++-mode-hook 'ejmr-setup-cc-mode))

;;; TODO: Make a hydra for highlight-*, hl-*, and rainbow-* commands
(use-package highlight-blocks)

(use-package polymode)

(use-package nhexl-mode)

(use-package restclient)

(use-package git-timemachine
  :commands git-timemachine-toggle
  :bind ("C-x v T" . git-timemachine-toggle))

(use-package diff-hl
  :config
  (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
  (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode))

(use-package dumb-jump
  :config
  (setq dumb-jump-selector 'ivy)
  (setq dumb-jump-prefer-searcher 'ag)
  (bind-key "s-d" (defhydra hydra-dumb-jump (:color pink)
		    "Dumb Jump"
		    ("g" dumb-jump-go "Go")
		    ("b" dumb-jump-back "Back")
		    ("l" dumb-jump-quick-look "Look")
		    ("e" dumb-jump-go-prefer-external "External")
		    ("q" nil "Quit" :color blue)))
  (dumb-jump-mode 1))

(use-package vdiff
  :config
  (bind-key "C-c v" vdiff-mode-prefix-map vdiff-mode-map))

(use-package projectile
  :diminish projectile-mode
  :config
  (use-package counsel-projectile
    :config (counsel-projectile-on))
  (setq projectile-completion-system 'ivy)
  (projectile-mode nil))

(use-package realgud
  :disabled t)

(use-package brainfuck-mode)

(use-package clang-format)
(use-package irony)
(use-package cov)

(use-package selected
  :diminish selected-minor-mode
  :config
  (selected-global-mode t)
  :bind (:map selected-keymap
              ("u" . upcase-region)
              ("d" . downcase-region)
              ("c" . count-words-region)
	      ("\\" . indent-region)
	      ("w" . copy-region-as-kill)
	      ("k" . kill-region)
              ("m" . apply-macro-to-region-lines)))

(use-package lice)
(use-package forth-mode)
(use-package go-mode)
(use-package fish-mode)
(use-package lua-mode)
(use-package nasm-mode
  :mode (("\\.asm\\'" . nasm-mode)
	 ("\\.s\\'" . nasm-mode)))
(use-package tup-mode)
(use-package yaml-mode
  :mode ("\\.yml\\'" . yaml-mode))

(use-package rust-mode
  :config
  (use-package cargo :diminish cargo-minor-mode))

(use-package symbol-overlay
  :config
  (bind-key "C-c s" #'symbol-overlay-put))

(use-package php-mode)
(use-package ini-mode)
(use-package haskell-mode)
(use-package python-mode)
(use-package js2-mode)
(use-package tern :disabled t)
(use-package clojure-mode)

(use-package conf-mode
  :mode (("\\.gitignore\\'" . conf-mode)
	 ("\\.gitconfig\\'" . conf-mode)
	 ("\\.toml\\'" . conf-mode)))

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
    (bind-key "C-s-a c" #'aya-create yas-minor-mode-map)
    (bind-key "C-s-a e" #'aya-expand yas-minor-mode-map)))

(use-package flycheck
  :config
  (use-package flycheck-clangcheck
    :disabled t
    :config
    (setq flycheck-clangcheck-analyze t))
  (use-package flycheck-mypy :disabled t)
  (use-package flycheck-rust
    :config
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
  (bind-key "<s-up>" #'flycheck-previous-error)
  (bind-key "<s-down>" #'flycheck-next-error))

(use-package nameless
  :diminish 'nameless-mode
  :config
  (setq nameless-private-prefix t)
  (add-hook 'emacs-lisp-mode-hook #'nameless-mode))

(bind-key "C-c C-b" #'emacs-lisp-byte-compile-and-load emacs-lisp-mode-map)

(use-package syntactic-close
  :bind ("s-0" . syntactic-close))


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
  (bind-key "C-s-t" #'hydra-todo/body))

(use-package duplicate-thing
  :config
  (key-seq-define-global "qd" #'duplicate-thing))

(use-package operate-on-number
  :config
  (key-seq-define-global "qn" #'operate-on-number-at-point))

(use-package corral
  :config
  (setq corral-preserve-point t)
  (global-set-key (kbd "M-9") 'corral-parentheses-backward)
  (global-set-key (kbd "M-0") 'corral-parentheses-forward)
  (global-set-key (kbd "M-[") 'corral-brackets-backward)
  (global-set-key (kbd "M-]") 'corral-brackets-forward)
  (global-set-key (kbd "M-{") 'corral-braces-backward)
  (global-set-key (kbd "M-}") 'corral-braces-forward)
  (global-set-key (kbd "M-\"") 'corral-double-quotes-backward))

(use-package avy
  :config
  (use-package avy-zap
    :config
    (key-seq-define-global "jz" #'avy-zap-up-to-char-dwim)
    (bind-key "M-g z" #'avy-zap-to-char-dwim))
  (key-seq-define-global "jw" #'avy-goto-word-1)
  (key-seq-define-global "js" #'avy-goto-subword-1)
  (key-seq-define-global "jc" #'avy-goto-char-timer)
  (key-seq-define-global "jl" #'avy-goto-line)
  (bind-key "C-'" #'avy-isearch isearch-mode-map)
  (defhydra hydra-avy (global-map "M-g" :color blue :hint nil)
    "Goto"
    ("c" avy-goto-char-timer "Character")
    ("g" avy-goto-line "Line")
    ("w" avy-goto-word-1 "Word")
    ("s" avy-goto-subword-1 "Subword")))

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

(use-package epub-mode
  :load-path "/home/eric/.emacs.d/local/epub-mode.el")

(defun ejmr-refill-to-one-line ()
  "Refills a paragraph to a single line."
  (interactive)
  (let ((fill-column 100000))
    (fill-individual-paragraphs (point-min) (point-max))))

(use-package fountain-mode)
(use-package darkroom)
(use-package wc-mode)

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
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :config
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
  (bind-key "C-c C-m" #'hydra-markdown/body markdown-mode-map))

(use-package pandoc-mode
  :config
  (add-hook 'markdown-mode-hook 'pandoc-mode))

(defhydra hydra-text (:color amaranth)
  "
^Major Modes^    ^Minor Modes^    ^Actions^
^───────────^────^───────────^────^───────^──────────
[_T_] Text       [_D_] Darkroom   [_s_] Sort Lines
[_A_] AsciiDoc   [_$_] Flyspell   [_a_] Align Regexp
[_M_] Markdown   [_u_] Auto Fill  ^^
[_G_] GFM        ^^
[_F_] Fountain
^^
"
  ("T" text-mode)
  ("A" adoc-mode)
  ("a" align-regexp)
  ("M" markdown-mode)
  ("F" fountain-mode)
  ("G" gfm-mode)
  ("D" darkroom-mode)
  ("$" flyspell-mode)
  ("s" sort-lines)
  ("u" auto-fill-mode)
  ("q" nil :color blue))

(bind-key "C-c t" #'hydra-text/body)


;;; Online Services

(use-package webpaste
  :config
  (defhydra hydra-webpaste (:color blue)
    "Paste to Web"
    ("b" webpaste-paste-buffer "Buffer")
    ("r" webpaste-paste-region "Region"))
  (bind-key "C-c w" #'hydra-webpaste/body))


;;; Custom File

(setq custom-file "/home/eric/.emacs.d/custom.el")
(load custom-file)

;;; init.el ends here

;; Local Variables:
;; firestarter: (byte-compile-file (buffer-file-name))
;; End:
