;;linux only!

(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://melpa.org/packages/"))
;; (when (< emacs-major-version 24)
;;   ;; For important compatibility libraries like cl-lib
;;   (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

(or (file-exists-p package-user-dir)
    (package-refresh-contents))

;; (load-file "~/.emacs.d/my-package-list.el")

; install the missing packages
;; (dolist (package package-list)
;;   (unless (package-installed-p package)
;;     (package-install package)))

(package-install-selected-packages)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(add-to-list 'load-path "~/.emacs.d/themes/")
(add-to-list 'load-path "~/.emacs.d/evil-plugins")
(add-to-list 'load-path "~/.emacs.d/submodules/evil-easymotion")

;;(add-to-list 'load-path "~/.emacs.d/non_elpa/i3-emacs")

;;(add-to-list 'load-path "~/.emacs.d/small-libs")

(server-start)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-item-indent 0)
 '(ac-auto-show-menu nil)
 '(ac-auto-start nil)
 '(ac-candidate-menu-min 0)
 '(ac-delay 0.001)
 '(ac-disable-inline t)
 '(ac-use-menu-map nil)
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(avy-background t)
 '(avy-highlight-first nil)
 '(avy-style 'at-full)
 '(backup-directory-alist '(("" . "~/.emacs_backups")))
 '(blink-matching-paren nil)
 '(c-basic-offset 3)
 '(c-hanging-braces-alist 'set-from-style)
 '(column-number-mode t)
 '(company-begin-commands nil)
 '(company-frontends
   '(company-pseudo-tooltip-frontend company-echo-metadata-frontend))
 '(company-idle-delay nil)
 '(company-lighter-base "")
 '(company-show-numbers t)
 '(create-lockfiles nil)
 '(cua-enable-cua-keys nil)
 '(cua-remap-control-z t)
 '(current-language-environment "IPA")
 '(cursor-type 'bar)
 '(custom-enabled-themes '(dracula))
 '(custom-safe-themes
   '("d19ce793bbbb150829d028f26a71993d3a410c4bc65e9a51c677ce8eaa834dab" "1f994a818358561585e11fea17246bb1bd5000b9298706a57216dc80cd91ee46" "4dbabd0f0450f5baca0265f8ce0335fd15d0d581a3ef82692139183ede4e5851" "868ef4caa6b12606bfe97d528d5a287ee3c00ab58e755ab9e09f4c77c23615e5" "1c43c033c22036fb6a723558987b4693d94bbd04848124537203335101190bbe" "4dd93829533c7dc513fdc6ef684d9037312ecad8db669e177ebb68a2b13468e8" "ab389cd23ef61d03c13ef257e78941ae31d5a0850fa1f64e34ea30c7c6cf533f" "b319facc4d6ca2b0fd9f8a846419837d6c0de035b52c2225fc2117e08e5c8a0e" "421cc5fdb7a071684abcd0448671a3ae5a0857de84fced2aea934ef8d992a559" "f83396f830e55520bfe6ce87b41f7216c060475fe314529fff6fa37d43388ee9" "d8130f52454981b00854b1d9a8d8e1afd90b5eaebf54c0625b01ca6e6cda8058" "1ec751fd3959b808684d378c631fa813e1846ab7211e508a4895516dd77e88a4" "06e4e8633af9c9f4f62b3d2e49dbda9ec2780c67aaf1b9b8e8328479eea8081a" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "1157a4055504672be1df1232bed784ba575c60ab44d8e6c7b3800ae76b42f8bd" "795bfbd35dc1f1492041ff27a9af4207d57d6fb438696377f02f33bfbe7b01f5" "b8fda2e478dbaaaff0600969f42d015f4f557c7fc91494a0e49287f5ff240e81" "237b8e54647b20d1f159dadeec0176839942389ca19ece24d5332df7cfd20ea7" "c8e1726d0d31b3564914d28f8e1849e30fcb2b457834f73b04765491503c3ffb" "82d79ee356fd38fe81deb201e8cb575931406a816b1415b245f2670303d50b07" "85f950051876972d2e5d0c45d0057b02cdd7420f31e5b7efc1b230bcccf4c62a" "09ecc80176744f57ddf75914df698a8e2daef2c3e1713dbdfe2fd539fa6dcfb5" "3c7fef838368f3eb596ba2a66bcd8b26b94ec41090d04bcbda24cdcb0dcf5a76" "cca41afaaa77fea8b6c4ee97b3fe4ef0b87b389a31e481abaa10fe1d4c673a01" "f8cf128fa0ef7e61b5546d12bb8ea1584c80ac313db38867b6e774d1d38c73db" "7f72dd635d7078f5c4152b138aefe8f73dfa55b731245fbf4b7793c72b633519" "28ec8ccf6190f6a73812df9bc91df54ce1d6132f18b4c8fcc85d45298569eb53" "f9574c9ede3f64d57b3aa9b9cef621d54e2e503f4d75d8613cbcc4ca1c962c21" default))
 '(default-input-method "ipa")
 '(dired-dwim-target t)
 '(dmoccur-recursive-search t)
 '(eclim-auto-save nil)
 '(eclim-eclipse-dirs '("~/bin/eclipse-neon"))
 '(eclim-executable "~/bin/eclipse-neon/eclim")
 '(evil-cross-lines t)
 '(evil-default-state 'emacs)
 '(evil-move-beyond-eol t)
 '(evil-move-cursor-back nil)
 '(evil-repeat-move-cursor nil)
 '(evil-search-module 'evil-search)
 '(evil-shift-width 1)
 '(evil-want-C-d-scroll nil)
 '(evil-want-Y-yank-to-eol t)
 '(expand-region-fast-keys-enabled t)
 '(eyebrowse-mode-line-style 'always)
 '(fci-rule-color "#14151E")
 '(flycheck-global-modes nil)
 '(ggtags-highlight-tag nil)
 '(git-gutter+-lighter "")
 '(git-gutter-fr+-side 'right-fringe)
 '(global-eldoc-mode nil)
 '(global-hl-line-mode t)
 '(global-linum-mode t)
 '(indent-tabs-mode nil)
 '(js-indent-level 3)
 '(linum-relative-current-symbol "")
 '(linum-relative-lighter "")
 '(menu-bar-mode nil)
 '(modalka-excluded-modes '(dired-mode))
 '(my-keys-minor-mode t)
 '(neo-show-hidden-files t)
 '(neo-window-fixed-size t)
 '(org-src-window-setup 'current-window)
 '(package-selected-packages
   '(switch-window evil-collection git-timemachine eyebrowse atomic-chrome evil-exchange git-gutter-fringe+ yasnippet-snippets wgrep-ag ag drag-stuff which-key smartparens smart-mode-line rainbow-mode rainbow-delimiters projectile outshine org-bullets multiple-cursors markdown-mode magit linum-relative hydra haskell-snippets haskell-mode golden-ratio-scroll-screen ggtags framemove flycheck expand-region evil-surround evil-nerd-commenter evil-mc-extras evil-lion evil-indent-plus evil-extra-operator disable-mouse diminish counsel-projectile counsel company-emacs-eclim color-moccur charmap buffer-move browse-kill-ring auctex))
 '(perl-continued-brace-offset -3)
 '(perl-continued-statement-offset 3)
 '(perl-indent-level 3)
 '(powerline-display-buffer-size nil)
 '(python-indent-offset 3)
 '(rm-base-text-properties
   '('help-echo 'rm--help-echo 'mouse-face 'mode-line-highlight 'local-map mode-line-minor-mode-keymap))
 '(rm-whitelist nil)
 '(sentence-end-double-space nil)
 '(setq global-linum-mode t)
 '(show-trailing-whitespace nil)
 '(size-indication-mode nil)
 '(sml/theme 'dark-erik)
 '(sp-autodelete-closing-pair nil)
 '(sp-autodelete-opening-pair nil)
 '(sp-autodelete-pair nil)
 '(sp-echo-match-when-invisible nil)
 '(sp-escape-quotes-after-insert t)
 '(sp-escape-wrapped-region t)
 '(sp-highlight-pair-overlay nil)
 '(sp-navigate-consider-sgml-tags
   '(html-erb-mode jinja2-mode web-mode nxml-mode nxhtml-mode rhtml-mode sgml-mode html-mode mhtml-mode))
 '(sp-navigate-reindent-after-up nil)
 '(sp-navigate-reindent-after-up-in-string nil)
 '(split-height-threshold nil)
 '(split-width-threshold 100)
 '(standard-indent 3)
 '(tab-width 4)
 '(text-scale-mode-step 1.05)
 '(tool-bar-mode nil)
 '(undo-tree-mode-lighter " untree")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#d54e53")
     (40 . "goldenrod")
     (60 . "#e7c547")
     (80 . "DarkOliveGreen3")
     (100 . "#70c0b1")
     (120 . "DeepSkyBlue1")
     (140 . "#c397d8")
     (160 . "#d54e53")
     (180 . "goldenrod")
     (200 . "#e7c547")
     (220 . "DarkOliveGreen3")
     (240 . "#70c0b1")
     (260 . "DeepSkyBlue1")
     (280 . "#c397d8")
     (300 . "#d54e53")
     (320 . "goldenrod")
     (340 . "#e7c547")
     (360 . "DarkOliveGreen3")))
 '(vc-annotate-very-old-color nil)
 '(yas-also-auto-indent-first-line t)
 '(yas-expand-only-for-last-commands '(self-insert-command org-self-insert-command)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#000000" :foreground "#bbe0f0" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "DAMA" :family "Ubuntu Mono"))))
 '(eyebrowse-mode-line-active ((t (:inherit mode-line-emphasis :foreground "white smoke"))))
 '(moccur-face ((t nil))))

;;init files
;;(load-file "~/.emacs.d/inits/keybindings.el")
;;(load-file "~/.emacs.d/inits/c-saker.el")

;;load init
(load-file "~/.emacs.d/myinit.el")

;; (toggle-frame-maximized)
;;only two vertical windows will sensibly created
;; (setq split-width-threshold (+ 1 (/ (frame-width) 2)))
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
