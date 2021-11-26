
:tanat

"27.2"

#s(hash-table size 145 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("org-elpa" ("2021-11-23 17:17:28" nil (:local-repo nil :package "org-elpa" :type git)) "melpa" ("2021-11-23 17:17:29" nil (:type git :host github :repo "melpa/melpa" :build nil :package "melpa" :local-repo "melpa")) "gnu-elpa-mirror" ("2021-11-23 17:17:29" nil (:type git :host github :repo "emacs-straight/gnu-elpa-mirror" :build nil :package "gnu-elpa-mirror" :local-repo "gnu-elpa-mirror")) "el-get" ("2021-11-23 17:17:29" nil (:type git :host github :repo "dimitri/el-get" :build nil :files ("*.el" ("recipes" "recipes/el-get.rcp") "methods" "el-get-pkg.el") :flavor melpa :package "el-get" :local-repo "el-get")) "emacsmirror-mirror" ("2021-11-23 17:17:29" nil (:type git :host github :repo "emacs-straight/emacsmirror-mirror" :build nil :package "emacsmirror-mirror" :local-repo "emacsmirror-mirror")) "straight" ("2021-11-23 17:17:29" ("emacs") (:type git :host github :repo "raxod502/straight.el" :files ("straight*.el") :branch "master" :package "straight" :local-repo "straight.el")) "use-package" ("2021-11-23 17:17:29" ("emacs" "bind-key") (:type git :flavor melpa :files (:defaults (:exclude "bind-key.el" "bind-chord.el" "use-package-chords.el" "use-package-ensure-system-package.el") "use-package-pkg.el") :host github :repo "jwiegley/use-package" :package "use-package" :local-repo "use-package")) "bind-key" ("2021-11-23 17:17:29" nil (:flavor melpa :files ("bind-key.el" "bind-key-pkg.el") :package "bind-key" :local-repo "use-package" :type git :repo "jwiegley/use-package" :host github)) "s" ("2021-11-23 17:17:29" nil (:type git :flavor melpa :files ("s.el" "s-pkg.el") :host github :repo "magnars/s.el" :package "s" :local-repo "s.el")) "hydra" ("2021-11-23 17:17:29" ("cl-lib" "lv") (:type git :host github :repo "abo-abo/hydra" :files (:defaults (:exclude "lv.el") "hydra-pkg.el") :flavor melpa :package "hydra" :local-repo "hydra")) "lv" ("2021-11-23 17:17:29" nil (:flavor melpa :files ("lv.el" "lv-pkg.el") :package "lv" :local-repo "hydra" :type git :repo "abo-abo/hydra" :host github)) "swiper" ("2021-11-23 17:17:29" ("emacs" "ivy") (:type git :flavor melpa :files ("swiper.el" "swiper-pkg.el") :host github :repo "abo-abo/swiper" :package "swiper" :local-repo "swiper")) "ivy" ("2021-11-23 17:17:29" ("emacs") (:flavor melpa :files (:defaults (:exclude "swiper.el" "counsel.el" "ivy-hydra.el" "ivy-avy.el") "doc/ivy-help.org" "ivy-pkg.el") :package "ivy" :local-repo "swiper" :type git :repo "abo-abo/swiper" :host github)) "counsel" ("2021-11-23 17:17:29" ("emacs" "ivy" "swiper") (:flavor melpa :files ("counsel.el" "counsel-pkg.el") :package "counsel" :local-repo "swiper" :type git :repo "abo-abo/swiper" :host github)) "evil-escape" ("2021-11-23 17:17:29" ("emacs" "evil" "cl-lib") (:type git :host github :repo "syl20bnr/evil-escape" :flavor melpa :package "evil-escape" :local-repo "evil-escape")) "evil" ("2021-11-23 17:17:29" ("emacs" "goto-chg" "cl-lib") (:type git :flavor melpa :files (:defaults "doc/build/texinfo/evil.texi" (:exclude "evil-test-helpers.el") "evil-pkg.el") :host github :repo "emacs-evil/evil" :package "evil" :local-repo "evil")) "goto-chg" ("2021-11-23 17:17:29" ("emacs") (:type git :flavor melpa :host github :repo "emacs-evil/goto-chg" :package "goto-chg" :local-repo "goto-chg")) "evil-leader" ("2021-11-23 17:17:29" ("evil") (:type git :host github :repo "cofi/evil-leader" :flavor melpa :package "evil-leader" :local-repo "evil-leader")) "projectile" ("2021-11-23 17:17:29" ("emacs") (:type git :host github :repo "bbatsov/projectile" :files ("projectile.el" "projectile-pkg.el") :flavor melpa :package "projectile" :local-repo "projectile")) "all-the-icons" ("2021-11-23 17:17:30" ("emacs") (:type git :host github :repo "domtronn/all-the-icons.el" :files (:defaults "data" "all-the-icons-pkg.el") :flavor melpa :package "all-the-icons" :local-repo "all-the-icons.el")) "doom-themes" ("2021-11-23 17:17:30" ("emacs" "cl-lib") (:type git :host github :repo "hlissner/emacs-doom-themes" :files (:defaults "themes/*.el" "doom-themes-pkg.el") :flavor melpa :package "doom-themes" :local-repo "emacs-doom-themes")) "cycle-themes" ("2021-11-23 17:17:30" ("cl-lib") (:type git :host github :repo "toroidal-code/cycle-themes.el" :flavor melpa :package "cycle-themes" :local-repo "cycle-themes.el")) "smooth-scrolling" ("2021-11-23 17:17:30" nil (:type git :host github :repo "aspiers/smooth-scrolling" :flavor melpa :package "smooth-scrolling" :local-repo "smooth-scrolling")) "magit" ("2021-11-23 17:17:30" ("emacs" "dash" "git-commit" "magit-section" "transient" "with-editor") (:type git :host github :repo "magit/magit" :files ("lisp/magit" "lisp/magit*.el" "lisp/git-rebase.el" "Documentation/magit.texi" "Documentation/AUTHORS.md" "LICENSE" (:exclude "lisp/magit-libgit.el" "lisp/magit-section.el") "magit-pkg.el") :flavor melpa :package "magit" :local-repo "magit")) "dash" ("2021-11-23 17:17:29" ("emacs") (:type git :flavor melpa :files ("dash.el" "dash.texi" "dash-pkg.el") :host github :repo "magnars/dash.el" :package "dash" :local-repo "dash.el")) "git-commit" ("2021-11-23 17:17:30" ("emacs" "dash" "transient" "with-editor") (:flavor melpa :files ("lisp/git-commit.el" "LICENSE" "git-commit-pkg.el") :package "git-commit" :local-repo "magit" :type git :repo "magit/magit" :host github)) "transient" ("2021-11-23 17:17:30" ("emacs") (:type git :flavor melpa :files ("lisp/*.el" "docs/transient.texi" "transient-pkg.el") :host github :repo "magit/transient" :package "transient" :local-repo "transient")) "with-editor" ("2021-11-23 17:17:30" ("emacs") (:type git :flavor melpa :host github :repo "magit/with-editor" :package "with-editor" :local-repo "with-editor")) "magit-section" ("2021-11-23 17:17:30" ("emacs" "dash") (:flavor melpa :files ("lisp/magit-section.el" "Documentation/magit-section.texi" "magit-section-pkg.el") :package "magit-section" :local-repo "magit" :type git :repo "magit/magit" :host github)) "git-timemachine" ("2021-11-23 17:17:31" ("emacs" "transient") (:type git :host gitlab :repo "pidu/git-timemachine" :flavor melpa :package "git-timemachine" :local-repo "git-timemachine")) "org-bullets" ("2021-11-23 17:17:37" nil (:type git :host github :repo "sabof/org-bullets" :flavor melpa :package "org-bullets" :local-repo "org-bullets")) "org" ("2021-11-23 17:17:31" ("emacs") (:type git :repo "https://git.savannah.gnu.org/git/emacs/org-mode.git" :local-repo "org" :depth full :pre-build (straight-recipes-org-elpa--build) :build (:not autoloads) :files (:defaults "lisp/*.el" ("etc/styles/" "etc/styles/*")) :package "org")) "smart-quotes" ("2021-11-23 17:17:37" nil (:type git :host github :repo "gareth-rees/smart-quotes" :files (:defaults) :package "smart-quotes" :local-repo "smart-quotes")) "guess-language" ("2021-11-23 17:17:37" ("cl-lib" "emacs") (:type git :host github :repo "tmalsburg/guess-language.el" :files ("guess-language.el" "trigrams/*" "guess-language-pkg.el") :flavor melpa :package "guess-language" :local-repo "guess-language.el")) "writeroom-mode" ("2021-11-23 17:17:37" ("emacs" "visual-fill-column") (:type git :host github :repo "joostkremers/writeroom-mode" :flavor melpa :package "writeroom-mode" :local-repo "writeroom-mode")) "visual-fill-column" ("2021-11-23 17:17:37" ("emacs") (:type git :host github :repo "joostkremers/visual-fill-column" :flavor melpa :package "visual-fill-column" :local-repo "visual-fill-column")) "flycheck" ("2021-11-23 17:17:37" ("dash" "pkg-info" "let-alist" "seq" "emacs") (:type git :flavor melpa :host github :repo "flycheck/flycheck" :package "flycheck" :local-repo "flycheck")) "pkg-info" ("2021-11-23 17:17:37" ("epl") (:type git :flavor melpa :host github :repo "emacsorphanage/pkg-info" :package "pkg-info" :local-repo "pkg-info")) "epl" ("2021-11-23 17:17:37" ("cl-lib") (:type git :flavor melpa :host github :repo "cask/epl" :package "epl" :local-repo "epl")) "let-alist" ("2021-11-23 17:17:31" ("emacs") (:type git :host github :repo "emacs-straight/let-alist" :files ("*" (:exclude ".git")) :package "let-alist" :local-repo "let-alist")) "company" ("2021-11-23 17:17:37" ("emacs") (:type git :flavor melpa :files (:defaults "icons" "company-pkg.el") :host github :repo "company-mode/company-mode" :package "company" :local-repo "company-mode")) "company-box" ("2021-11-23 17:17:37" ("emacs" "dash" "dash-functional" "company" "frame-local") (:type git :host github :repo "sebastiencs/company-box" :files (:defaults "images" "company-box-pkg.el") :flavor melpa :package "company-box" :local-repo "company-box")) "dash-functional" ("2021-11-23 17:17:37" ("dash") (:flavor melpa :files ("dash-functional.el" "dash-functional-pkg.el") :package "dash-functional" :local-repo "dash.el" :type git :repo "magnars/dash.el" :host github)) "frame-local" ("2021-11-23 17:17:37" ("emacs") (:type git :flavor melpa :host github :repo "sebastiencs/frame-local" :package "frame-local" :local-repo "frame-local")) "web-mode" ("2021-11-23 17:17:37" ("emacs") (:type git :host github :repo "fxbois/web-mode" :flavor melpa :package "web-mode" :local-repo "web-mode")) "yasnippet" ("2021-11-23 17:17:38" ("cl-lib") (:type git :host github :repo "joaotavora/yasnippet" :files ("yasnippet.el" "snippets" "yasnippet-pkg.el") :flavor melpa :package "yasnippet" :local-repo "yasnippet")) "markdown-mode" ("2021-11-23 17:17:32" ("emacs") (:type git :flavor melpa :host github :repo "jrblevin/markdown-mode" :package "markdown-mode" :local-repo "markdown-mode")) "bibtex-completion" ("2021-11-23 17:17:31" ("parsebib" "s" "dash" "f" "cl-lib" "biblio" "emacs") (:flavor melpa :files ("bibtex-completion.el" "bibtex-completion-pkg.el") :package "bibtex-completion" :local-repo "helm-bibtex" :type git :repo "tmalsburg/helm-bibtex" :host github)) "parsebib" ("2021-11-23 17:17:31" ("emacs") (:type git :flavor melpa :host github :repo "joostkremers/parsebib" :package "parsebib" :local-repo "parsebib")) "f" ("2021-11-23 17:17:31" ("s" "dash") (:type git :flavor melpa :files ("f.el" "f-pkg.el") :host github :repo "rejeep/f.el" :package "f" :local-repo "f.el")) "biblio" ("2021-11-23 17:17:31" ("emacs" "biblio-core") (:type git :flavor melpa :files (:defaults (:exclude "biblio-core.el") "biblio-pkg.el") :host github :repo "cpitclaudel/biblio.el" :package "biblio" :local-repo "biblio.el")) "biblio-core" ("2021-11-23 17:17:31" ("emacs" "let-alist" "seq" "dash") (:flavor melpa :files ("biblio-core.el" "biblio-core-pkg.el") :package "biblio-core" :local-repo "biblio.el" :type git :repo "cpitclaudel/biblio.el" :host github)) "pdf-tools" ("2021-11-23 17:17:31" ("emacs" "tablist" "let-alist") (:type git :flavor melpa :files ("lisp/*.el" "README" ("build" "Makefile") ("build" "server") (:exclude "lisp/tablist.el" "lisp/tablist-filter.el") "pdf-tools-pkg.el") :host github :repo "vedang/pdf-tools" :package "pdf-tools" :local-repo "pdf-tools")) "tablist" ("2021-11-23 17:17:31" ("emacs") (:type git :flavor melpa :host github :repo "politza/tablist" :package "tablist" :local-repo "tablist")) "ivy-bibtex" ("2021-11-23 17:17:38" ("bibtex-completion" "ivy" "cl-lib") (:flavor melpa :files ("ivy-bibtex.el" "ivy-bibtex-pkg.el") :package "ivy-bibtex" :local-repo "helm-bibtex" :type git :repo "tmalsburg/helm-bibtex" :host github)) "undo-tree" ("2021-11-23 17:17:29" nil (:type git :host github :repo "emacs-straight/undo-tree" :files ("*" (:exclude ".git")) :package "undo-tree" :local-repo "undo-tree")) "which-key" ("2021-11-23 17:17:29" ("emacs") (:type git :host github :repo "justbur/emacs-which-key" :flavor melpa :package "which-key" :local-repo "emacs-which-key")) "focus" ("2021-10-06 00:58:12" ("emacs" "cl-lib") (:type git :host github :repo "larstvei/Focus" :flavor melpa :package "focus" :local-repo "Focus")) "emacs-async" ("2021-10-06 01:02:06" nil (:type git :host github :repo "jwiegley/emacs-async" :files (:defaults) :package "emacs-async" :local-repo "emacs-async")) "org-ref" ("2021-11-23 17:17:31" ("dash" "htmlize" "helm" "helm-bibtex" "ivy" "hydra" "key-chord" "s" "f" "pdf-tools" "bibtex-completion") (:type git :flavor melpa :files (:defaults "org-ref.org" "org-ref.bib" "citeproc" "org-ref-pkg.el") :host github :repo "jkitchin/org-ref" :package "org-ref" :local-repo "org-ref")) "htmlize" ("2021-11-23 17:17:31" nil (:type git :flavor melpa :host github :repo "hniksic/emacs-htmlize" :package "htmlize" :local-repo "emacs-htmlize")) "helm" ("2021-11-23 17:17:31" ("emacs" "async" "popup" "helm-core") (:type git :flavor melpa :files ("*.el" "emacs-helm.sh" (:exclude "helm.el" "helm-lib.el" "helm-source.el" "helm-multi-match.el" "helm-core-pkg.el") "helm-pkg.el") :host github :repo "emacs-helm/helm" :package "helm" :local-repo "helm")) "async" ("2021-11-23 17:17:31" ("emacs") (:type git :flavor melpa :host github :repo "jwiegley/emacs-async" :package "async" :local-repo "emacs-async")) "popup" ("2021-11-23 17:17:31" ("emacs") (:type git :flavor melpa :files ("popup.el" "popup-pkg.el") :host github :repo "auto-complete/popup-el" :package "popup" :local-repo "popup-el")) "helm-core" ("2021-11-23 17:17:31" ("emacs" "async") (:flavor melpa :files ("helm-core-pkg.el" "helm.el" "helm-lib.el" "helm-source.el" "helm-multi-match.el" "helm-core-pkg.el") :package "helm-core" :local-repo "helm" :type git :repo "emacs-helm/helm" :host github)) "helm-bibtex" ("2021-11-23 17:17:31" ("bibtex-completion" "helm" "cl-lib" "emacs") (:type git :flavor melpa :files ("helm-bibtex.el" "helm-bibtex-pkg.el") :host github :repo "tmalsburg/helm-bibtex" :package "helm-bibtex" :local-repo "helm-bibtex")) "key-chord" ("2021-11-23 17:17:31" ("emacs") (:type git :flavor melpa :host github :repo "emacsorphanage/key-chord" :package "key-chord" :local-repo "key-chord")) "esxml" ("2021-11-23 17:17:37" ("emacs" "kv" "cl-lib") (:type git :host github :repo "tali713/esxml" :files ("esxml.el" "esxml-query.el" "esxml-pkg.el") :flavor melpa :package "esxml" :local-repo "esxml")) "kv" ("2021-11-23 17:17:37" nil (:type git :flavor melpa :files ("kv.el" "kv-pkg.el") :host github :repo "nicferrier/emacs-kv" :package "kv" :local-repo "emacs-kv")) "deft" ("2021-11-23 17:17:37" nil (:type git :flavor melpa :host github :repo "jrblevin/deft" :package "deft" :local-repo "deft")) "neotree" ("2021-11-23 17:17:37" ("cl-lib") (:type git :flavor melpa :files (:defaults "icons" "neotree-pkg.el") :host github :repo "jaypei/emacs-neotree" :package "neotree" :local-repo "emacs-neotree")) "emmet-mode" ("2021-11-23 17:17:37" nil (:type git :flavor melpa :host github :repo "smihica/emmet-mode" :package "emmet-mode" :local-repo "emmet-mode")) "lsp-mode" ("2021-11-23 17:17:37" ("emacs" "dash" "f" "ht" "spinner" "markdown-mode" "lv") (:type git :flavor melpa :files (:defaults "clients/*.el" "lsp-mode-pkg.el") :host github :repo "emacs-lsp/lsp-mode" :package "lsp-mode" :local-repo "lsp-mode")) "ht" ("2021-11-23 17:17:37" ("dash") (:type git :flavor melpa :files ("ht.el" "ht-pkg.el") :host github :repo "Wilfred/ht.el" :package "ht" :local-repo "ht.el")) "spinner" ("2021-11-23 17:17:37" ("emacs") (:type git :host github :repo "emacs-straight/spinner" :files ("*" (:exclude ".git")) :package "spinner" :local-repo "spinner")) "lsp-ui" ("2021-11-23 17:17:37" ("emacs" "dash" "lsp-mode" "markdown-mode") (:type git :flavor melpa :files (:defaults "lsp-ui-doc.html" "resources" "lsp-ui-pkg.el") :host github :repo "emacs-lsp/lsp-ui" :package "lsp-ui" :local-repo "lsp-ui")) "lsp-ivy" ("2021-11-23 17:17:37" ("emacs" "dash" "lsp-mode" "ivy") (:type git :flavor melpa :host github :repo "emacs-lsp/lsp-ivy" :package "lsp-ivy" :local-repo "lsp-ivy")) "lsp-treemacs" ("2021-11-23 17:17:37" ("emacs" "dash" "f" "ht" "treemacs" "lsp-mode") (:type git :flavor melpa :files (:defaults "icons" "lsp-treemacs-pkg.el") :host github :repo "emacs-lsp/lsp-treemacs" :package "lsp-treemacs" :local-repo "lsp-treemacs")) "treemacs" ("2021-11-23 17:17:37" ("emacs" "cl-lib" "dash" "s" "ace-window" "pfuture" "hydra" "ht" "cfrs") (:type git :flavor melpa :files (:defaults "Changelog.org" "icons" "src/elisp/treemacs*.el" "src/scripts/treemacs*.py" (:exclude "src/extra/*") "treemacs-pkg.el") :host github :repo "Alexander-Miller/treemacs" :package "treemacs" :local-repo "treemacs")) "ace-window" ("2021-11-23 17:17:37" ("avy") (:type git :flavor melpa :host github :repo "abo-abo/ace-window" :package "ace-window" :local-repo "ace-window")) "avy" ("2021-11-23 17:17:37" ("emacs" "cl-lib") (:type git :flavor melpa :host github :repo "abo-abo/avy" :package "avy" :local-repo "avy")) "pfuture" ("2021-11-23 17:17:37" ("emacs") (:type git :flavor melpa :host github :repo "Alexander-Miller/pfuture" :package "pfuture" :local-repo "pfuture")) "cfrs" ("2021-11-23 17:17:37" ("emacs" "dash" "s" "posframe") (:type git :flavor melpa :host github :repo "Alexander-Miller/cfrs" :package "cfrs" :local-repo "cfrs")) "posframe" ("2021-11-23 17:17:37" ("emacs") (:type git :flavor melpa :host github :repo "tumashu/posframe" :package "posframe" :local-repo "posframe")) "dap-mode" ("2021-11-23 17:17:37" ("emacs" "dash" "lsp-mode" "bui" "f" "s" "lsp-treemacs" "posframe" "ht") (:type git :flavor melpa :files (:defaults "icons" "dap-mode-pkg.el") :host github :repo "emacs-lsp/dap-mode" :package "dap-mode" :local-repo "dap-mode")) "bui" ("2021-11-23 17:17:37" ("emacs" "dash") (:type git :flavor melpa :host github :repo "alezost/bui.el" :package "bui" :local-repo "bui.el")) "company-lsp" ("2021-11-23 17:17:37" ("emacs" "lsp-mode" "company" "s" "dash") (:type git :host github :repo "emacsattic/company-lsp" :package "company-lsp" :local-repo "company-lsp")) "exec-path-from-shell" ("2021-11-23 17:17:29" ("emacs" "cl-lib") (:type git :host github :repo "purcell/exec-path-from-shell" :flavor melpa :package "exec-path-from-shell" :local-repo "exec-path-from-shell")) "yaml-mode" ("2021-11-23 17:17:37" ("emacs") (:type git :flavor melpa :host github :repo "yoshiki/yaml-mode" :package "yaml-mode" :local-repo "yaml-mode")) "smartparens" ("2021-11-23 17:17:37" ("dash" "cl-lib") (:type git :host github :repo "Fuco1/smartparens" :flavor melpa :package "smartparens" :local-repo "smartparens")) "lsp-jedi" ("2021-11-23 17:17:38" ("emacs" "lsp-mode") (:type git :host github :repo "fredcamps/lsp-jedi" :flavor melpa :package "lsp-jedi" :local-repo "lsp-jedi")) "py-autopep8" ("2021-11-23 17:17:38" nil (:type git :host github :repo "paetzke/py-autopep8.el" :flavor melpa :package "py-autopep8" :local-repo "py-autopep8.el")) "company-jedi" ("2021-10-23 15:13:29" ("emacs" "cl-lib" "company" "jedi-core") (:type git :host github :repo "emacsorphanage/company-jedi" :flavor melpa :package "company-jedi" :local-repo "company-jedi")) "jedi-core" ("2021-10-23 15:13:29" ("emacs" "epc" "python-environment" "cl-lib") (:type git :flavor melpa :files ("jedi-core.el" "jediepcserver.py" "Makefile" "setup.py" "jedi-core-pkg.el") :host github :repo "tkf/emacs-jedi" :package "jedi-core" :local-repo "emacs-jedi")) "epc" ("2021-10-23 15:13:28" ("concurrent" "ctable") (:type git :flavor melpa :files ("epc.el" "epcs.el" "epc-pkg.el") :host github :repo "kiwanami/emacs-epc" :package "epc" :local-repo "emacs-epc")) "concurrent" ("2021-10-23 15:13:26" ("emacs" "deferred") (:type git :flavor melpa :files ("concurrent.el" "concurrent-pkg.el") :host github :repo "kiwanami/emacs-deferred" :package "concurrent" :local-repo "emacs-deferred")) "deferred" ("2021-10-23 15:13:26" ("emacs") (:flavor melpa :files ("deferred.el" "deferred-pkg.el") :package "deferred" :local-repo "emacs-deferred" :type git :repo "kiwanami/emacs-deferred" :host github)) "ctable" ("2021-10-23 15:13:28" ("emacs" "cl-lib") (:type git :flavor melpa :files ("ctable.el" "ctable-pkg.el") :host github :repo "kiwanami/emacs-ctable" :package "ctable" :local-repo "emacs-ctable")) "python-environment" ("2021-10-23 15:13:29" ("deferred") (:type git :flavor melpa :host github :repo "tkf/emacs-python-environment" :package "python-environment" :local-repo "emacs-python-environment")) "graphviz-dot-mode" ("2021-11-23 17:17:37" ("emacs") (:type git :host github :repo "ppareit/graphviz-dot-mode" :flavor melpa :package "graphviz-dot-mode" :local-repo "graphviz-dot-mode")) "org-roam" ("2021-11-23 17:17:31" ("emacs" "dash" "f" "org" "emacsql" "emacsql-sqlite" "magit-section") (:type git :host github :repo "org-roam/org-roam" :files (:defaults "extensions/*" "org-roam-pkg.el") :flavor melpa :package "org-roam" :local-repo "org-roam")) "emacsql" ("2021-11-23 17:17:31" ("emacs") (:type git :flavor melpa :files ("emacsql.el" "emacsql-compiler.el" "emacsql-system.el" "README.md" "emacsql-pkg.el") :host github :repo "skeeto/emacsql" :package "emacsql" :local-repo "emacsql")) "emacsql-sqlite" ("2021-11-23 17:17:31" ("emacs" "emacsql") (:flavor melpa :files ("emacsql-sqlite.el" "sqlite" "emacsql-sqlite-pkg.el") :package "emacsql-sqlite" :local-repo "emacsql" :type git :repo "skeeto/emacsql" :host github)) "org-noter" ("2021-11-23 17:17:37" ("emacs" "cl-lib" "org") (:type git :host github :repo "weirdNox/org-noter" :flavor melpa :package "org-noter" :local-repo "org-noter")) "ox-epub" ("2021-11-23 17:17:37" ("emacs" "org") (:type git :host github :repo "ofosos/ox-epub" :flavor melpa :package "ox-epub" :local-repo "ox-epub")) "org-download" ("2021-11-23 17:17:37" ("emacs" "async") (:type git :host github :repo "abo-abo/org-download" :flavor melpa :package "org-download" :local-repo "org-download")) "md-roam" ("2021-11-23 17:17:32" ("emacs" "org-roam" "markdown-mode") (:type git :host github :repo "nobiot/md-roam" :package "md-roam" :local-repo "md-roam")) "major-mode-hydra" ("2021-11-23 17:17:29" ("dash" "pretty-hydra" "emacs") (:type git :host github :repo "jerrypnz/major-mode-hydra.el" :files ("major-mode-hydra.el" "major-mode-hydra-pkg.el") :flavor melpa :package "major-mode-hydra" :local-repo "major-mode-hydra.el")) "pretty-hydra" ("2021-11-23 17:17:29" ("hydra" "s" "dash" "emacs") (:flavor melpa :files ("pretty-hydra.el" "pretty-hydra-pkg.el") :package "pretty-hydra" :local-repo "major-mode-hydra.el" :type git :repo "jerrypnz/major-mode-hydra.el" :host github))))

#s(hash-table size 145 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("straight" ((straight straight-autoloads straight-x) (autoload 'straight-remove-unused-repos "straight" "Remove unused repositories from the repos directory.
A repo is considered \"unused\" if it was not explicitly requested via
`straight-use-package' during the current Emacs session.
If FORCE is non-nil do not prompt before deleting repos.

(fn &optional FORCE)" t nil) (autoload 'straight-get-recipe "straight" "Interactively select a recipe from one of the recipe repositories.
All recipe repositories in `straight-recipe-repositories' will
first be cloned. After the recipe is selected, it will be copied
to the kill ring. With a prefix argument, first prompt for a
recipe repository to search. Only that repository will be
cloned.

From Lisp code, SOURCES should be a subset of the symbols in
`straight-recipe-repositories'. Only those recipe repositories
are cloned and searched. If it is nil or omitted, then the value
of `straight-recipe-repositories' is used. If SOURCES is the
symbol `interactive', then the user is prompted to select a
recipe repository, and a list containing that recipe repository
is used for the value of SOURCES. ACTION may be `copy' (copy
recipe to the kill ring), `insert' (insert at point), or nil (no
action, just return it).

(fn &optional SOURCES ACTION)" t nil) (autoload 'straight-visit-package-website "straight" "Interactively select a recipe, and visit the package's website." t nil) (autoload 'straight-use-package "straight" "Register, clone, build, and activate a package and its dependencies.
This is the main entry point to the functionality of straight.el.

MELPA-STYLE-RECIPE is either a symbol naming a package, or a list
whose car is a symbol naming a package and whose cdr is a
property list containing e.g. `:type', `:local-repo', `:files',
and VC backend specific keywords.

First, the package recipe is registered with straight.el. If
NO-CLONE is a function, then it is called with two arguments: the
package name as a string, and a boolean value indicating whether
the local repository for the package is available. In that case,
the return value of the function is used as the value of NO-CLONE
instead. In any case, if NO-CLONE is non-nil, then processing
stops here.

Otherwise, the repository is cloned, if it is missing. If
NO-BUILD is a function, then it is called with one argument: the
package name as a string. In that case, the return value of the
function is used as the value of NO-BUILD instead. In any case,
if NO-BUILD is non-nil, then processing halts here. Otherwise,
the package is built and activated. Note that if the package
recipe has a nil `:build' entry, then NO-BUILD is ignored
and processing always stops before building and activation
occurs.

CAUSE is a string explaining the reason why
`straight-use-package' has been called. It is for internal use
only, and is used to construct progress messages. INTERACTIVE is
non-nil if the function has been called interactively. It is for
internal use only, and is used to determine whether to show a
hint about how to install the package permanently.

Return non-nil if package was actually installed, and nil
otherwise (this can only happen if NO-CLONE is non-nil).

(fn MELPA-STYLE-RECIPE &optional NO-CLONE NO-BUILD CAUSE INTERACTIVE)" t nil) (autoload 'straight-register-package "straight" "Register a package without cloning, building, or activating it.
This function is equivalent to calling `straight-use-package'
with a non-nil argument for NO-CLONE. It is provided for
convenience. MELPA-STYLE-RECIPE is as for
`straight-use-package'.

(fn MELPA-STYLE-RECIPE)" nil nil) (autoload 'straight-use-package-no-build "straight" "Register and clone a package without building it.
This function is equivalent to calling `straight-use-package'
with nil for NO-CLONE but a non-nil argument for NO-BUILD. It is
provided for convenience. MELPA-STYLE-RECIPE is as for
`straight-use-package'.

(fn MELPA-STYLE-RECIPE)" nil nil) (autoload 'straight-use-package-lazy "straight" "Register, build, and activate a package if it is already cloned.
This function is equivalent to calling `straight-use-package'
with symbol `lazy' for NO-CLONE. It is provided for convenience.
MELPA-STYLE-RECIPE is as for `straight-use-package'.

(fn MELPA-STYLE-RECIPE)" nil nil) (autoload 'straight-use-recipes "straight" "Register a recipe repository using MELPA-STYLE-RECIPE.
This registers the recipe and builds it if it is already cloned.
Note that you probably want the recipe for a recipe repository to
include a nil `:build' property, to unconditionally
inhibit the build phase.

This function also adds the recipe repository to
`straight-recipe-repositories', at the end of the list.

(fn MELPA-STYLE-RECIPE)" nil nil) (autoload 'straight-override-recipe "straight" "Register MELPA-STYLE-RECIPE as a recipe override.
This puts it in `straight-recipe-overrides', depending on the
value of `straight-current-profile'.

(fn MELPA-STYLE-RECIPE)" nil nil) (autoload 'straight-check-package "straight" "Rebuild a PACKAGE if it has been modified.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. See also `straight-rebuild-package' and
`straight-check-all'.

(fn PACKAGE)" t nil) (autoload 'straight-check-all "straight" "Rebuild any packages that have been modified.
See also `straight-rebuild-all' and `straight-check-package'.
This function should not be called during init." t nil) (autoload 'straight-rebuild-package "straight" "Rebuild a PACKAGE.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument RECURSIVE, rebuild
all dependencies as well. See also `straight-check-package' and
`straight-rebuild-all'.

(fn PACKAGE &optional RECURSIVE)" t nil) (autoload 'straight-rebuild-all "straight" "Rebuild all packages.
See also `straight-check-all' and `straight-rebuild-package'." t nil) (autoload 'straight-prune-build-cache "straight" "Prune the build cache.
This means that only packages that were built in the last init
run and subsequent interactive session will remain; other
packages will have their build mtime information and any cached
autoloads discarded." nil nil) (autoload 'straight-prune-build-directory "straight" "Prune the build directory.
This means that only packages that were built in the last init
run and subsequent interactive session will remain; other
packages will have their build directories deleted." nil nil) (autoload 'straight-prune-build "straight" "Prune the build cache and build directory.
This means that only packages that were built in the last init
run and subsequent interactive session will remain; other
packages will have their build mtime information discarded and
their build directories deleted." t nil) (autoload 'straight-normalize-package "straight" "Normalize a PACKAGE's local repository to its recipe's configuration.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'.

(fn PACKAGE)" t nil) (autoload 'straight-normalize-all "straight" "Normalize all packages. See `straight-normalize-package'.
Return a list of recipes for packages that were not successfully
normalized. If multiple packages come from the same local
repository, only one is normalized.

PREDICATE, if provided, filters the packages that are normalized.
It is called with the package name as a string, and should return
non-nil if the package should actually be normalized.

(fn &optional PREDICATE)" t nil) (autoload 'straight-fetch-package "straight" "Try to fetch a PACKAGE from the primary remote.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM,
fetch not just from primary remote but also from upstream (for
forked packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t nil) (autoload 'straight-fetch-package-and-deps "straight" "Try to fetch a PACKAGE and its (transitive) dependencies.
PACKAGE, its dependencies, their dependencies, etc. are fetched
from their primary remotes.

PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM,
fetch not just from primary remote but also from upstream (for
forked packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t nil) (autoload 'straight-fetch-all "straight" "Try to fetch all packages from their primary remotes.
With prefix argument FROM-UPSTREAM, fetch not just from primary
remotes but also from upstreams (for forked packages).

Return a list of recipes for packages that were not successfully
fetched. If multiple packages come from the same local
repository, only one is fetched.

PREDICATE, if provided, filters the packages that are fetched. It
is called with the package name as a string, and should return
non-nil if the package should actually be fetched.

(fn &optional FROM-UPSTREAM PREDICATE)" t nil) (autoload 'straight-merge-package "straight" "Try to merge a PACKAGE from the primary remote.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM,
merge not just from primary remote but also from upstream (for
forked packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t nil) (autoload 'straight-merge-package-and-deps "straight" "Try to merge a PACKAGE and its (transitive) dependencies.
PACKAGE, its dependencies, their dependencies, etc. are merged
from their primary remotes.

PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM,
merge not just from primary remote but also from upstream (for
forked packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t nil) (autoload 'straight-merge-all "straight" "Try to merge all packages from their primary remotes.
With prefix argument FROM-UPSTREAM, merge not just from primary
remotes but also from upstreams (for forked packages).

Return a list of recipes for packages that were not successfully
merged. If multiple packages come from the same local
repository, only one is merged.

PREDICATE, if provided, filters the packages that are merged. It
is called with the package name as a string, and should return
non-nil if the package should actually be merged.

(fn &optional FROM-UPSTREAM PREDICATE)" t nil) (autoload 'straight-pull-package "straight" "Try to pull a PACKAGE from the primary remote.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM, pull
not just from primary remote but also from upstream (for forked
packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t nil) (autoload 'straight-pull-package-and-deps "straight" "Try to pull a PACKAGE and its (transitive) dependencies.
PACKAGE, its dependencies, their dependencies, etc. are pulled
from their primary remotes.

PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM,
pull not just from primary remote but also from upstream (for
forked packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t nil) (autoload 'straight-pull-all "straight" "Try to pull all packages from their primary remotes.
With prefix argument FROM-UPSTREAM, pull not just from primary
remotes but also from upstreams (for forked packages).

Return a list of recipes for packages that were not successfully
pulled. If multiple packages come from the same local repository,
only one is pulled.

PREDICATE, if provided, filters the packages that are pulled. It
is called with the package name as a string, and should return
non-nil if the package should actually be pulled.

(fn &optional FROM-UPSTREAM PREDICATE)" t nil) (autoload 'straight-push-package "straight" "Push a PACKAGE to its primary remote, if necessary.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'.

(fn PACKAGE)" t nil) (autoload 'straight-push-all "straight" "Try to push all packages to their primary remotes.

Return a list of recipes for packages that were not successfully
pushed. If multiple packages come from the same local repository,
only one is pushed.

PREDICATE, if provided, filters the packages that are normalized.
It is called with the package name as a string, and should return
non-nil if the package should actually be normalized.

(fn &optional PREDICATE)" t nil) (autoload 'straight-freeze-versions "straight" "Write version lockfiles for currently activated packages.
This implies first pushing all packages that have unpushed local
changes. If the package management system has been used since the
last time the init-file was reloaded, offer to fix the situation
by reloading the init-file again. If FORCE is
non-nil (interactively, if a prefix argument is provided), skip
all checks and write the lockfile anyway.

Currently, writing version lockfiles requires cloning all lazily
installed packages. Hopefully, this inconvenient requirement will
be removed in the future.

Multiple lockfiles may be written (one for each profile),
according to the value of `straight-profiles'.

(fn &optional FORCE)" t nil) (autoload 'straight-thaw-versions "straight" "Read version lockfiles and restore package versions to those listed." t nil) (autoload 'straight-bug-report "straight" "Test straight.el in a clean environment.
ARGS may be any of the following keywords and their respective values:
  - :pre-bootstrap (Form)...
      Forms evaluated before bootstrapping straight.el
      e.g. (setq straight-repository-branch \"develop\")
      Note this example is already in the default bootstrapping code.

  - :post-bootstrap (Form)...
      Forms evaluated in the testing environment after boostrapping.
      e.g. (straight-use-package '(example :type git :host github))

  - :interactive Boolean
      If nil, the subprocess will immediately exit after the test.
      Output will be printed to `straight-bug-report--process-buffer'
      Otherwise, the subprocess will be interactive.

  - :preserve Boolean
      If non-nil, the test directory is left in the directory stored in the
      variable `temporary-file-directory'. Otherwise, it is
      immediately removed after the test is run.

  - :executable String
      Indicate the Emacs executable to launch.
      Defaults to the path of the current Emacs executable.

  - :raw Boolean
      If non-nil, the raw process output is sent to
      `straight-bug-report--process-buffer'. Otherwise, it is
      formatted as markdown for submitting as an issue.

  - :user-dir String
      If non-nil, the test is run with `user-emacs-directory' set to STRING.
      Otherwise, a temporary directory is created and used.
      Unless absolute, paths are expanded relative to the variable
      `temporary-file-directory'.

ARGS are accessible within the :pre/:post-bootsrap phases via the
locally bound plist, straight-bug-report-args.

(fn &rest ARGS)" nil t) (function-put 'straight-bug-report 'lisp-indent-function '0) (autoload 'straight-dependencies "straight" "Return a list of PACKAGE's dependencies.

(fn &optional PACKAGE)" t nil) (autoload 'straight-dependents "straight" "Return a list PACKAGE's dependents.

(fn &optional PACKAGE)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "straight" '("straight-"))) (defvar straight-x-pinned-packages nil "List of pinned packages.") (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "straight-x" '("straight-x-"))) (provide 'straight-autoloads)) "bind-key" ((bind-key-autoloads bind-key) (autoload 'bind-key "bind-key" "Bind KEY-NAME to COMMAND in KEYMAP (`global-map' if not passed).

KEY-NAME may be a vector, in which case it is passed straight to
`define-key'. Or it may be a string to be interpreted as
spelled-out keystrokes, e.g., \"C-c C-z\". See documentation of
`edmacro-mode' for details.

COMMAND must be an interactive function or lambda form.

KEYMAP, if present, should be a keymap variable or symbol.
For example:

  (bind-key \"M-h\" #'some-interactive-function my-mode-map)

  (bind-key \"M-h\" #'some-interactive-function 'my-mode-map)

If PREDICATE is non-nil, it is a form evaluated to determine when
a key should be bound. It must return non-nil in such cases.
Emacs can evaluate this form at any time that it does redisplay
or operates on menu data structures, so you should write it so it
can safely be called at any time.

(fn KEY-NAME COMMAND &optional KEYMAP PREDICATE)" nil t) (autoload 'unbind-key "bind-key" "Unbind the given KEY-NAME, within the KEYMAP (if specified).
See `bind-key' for more details.

(fn KEY-NAME &optional KEYMAP)" nil t) (autoload 'bind-key* "bind-key" "Similar to `bind-key', but overrides any mode-specific bindings.

(fn KEY-NAME COMMAND &optional PREDICATE)" nil t) (autoload 'bind-keys "bind-key" "Bind multiple keys at once.

Accepts keyword arguments:
:map MAP               - a keymap into which the keybindings should be
                         added
:prefix KEY            - prefix key for these bindings
:prefix-map MAP        - name of the prefix map that should be created
                         for these bindings
:prefix-docstring STR  - docstring for the prefix-map variable
:menu-name NAME        - optional menu string for prefix map
:filter FORM           - optional form to determine when bindings apply

The rest of the arguments are conses of keybinding string and a
function symbol (unquoted).

(fn &rest ARGS)" nil t) (autoload 'bind-keys* "bind-key" "

(fn &rest ARGS)" nil t) (autoload 'describe-personal-keybindings "bind-key" "Display all the personal keybindings defined by `bind-key'." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "bind-key" '("bind-key" "compare-keybindings" "get-binding-description" "override-global-m" "personal-keybindings"))) (provide 'bind-key-autoloads)) "use-package" ((use-package-lint use-package-ensure use-package-core use-package-jump use-package use-package-diminish use-package-bind-key use-package-delight use-package-autoloads) (autoload 'use-package-autoload-keymap "use-package-bind-key" "Loads PACKAGE and then binds the key sequence used to invoke
this function to KEYMAP-SYMBOL. It then simulates pressing the
same key sequence a again, so that the next key pressed is routed
to the newly loaded keymap.

This function supports use-package's :bind-keymap keyword. It
works by binding the given key sequence to an invocation of this
function for a particular keymap. The keymap is expected to be
defined by the package. In this way, loading the package is
deferred until the prefix key sequence is pressed.

(fn KEYMAP-SYMBOL PACKAGE OVERRIDE)" nil nil) (autoload 'use-package-normalize-binder "use-package-bind-key" "

(fn NAME KEYWORD ARGS)" nil nil) (defalias 'use-package-normalize/:bind 'use-package-normalize-binder) (defalias 'use-package-normalize/:bind* 'use-package-normalize-binder) (defalias 'use-package-autoloads/:bind 'use-package-autoloads-mode) (defalias 'use-package-autoloads/:bind* 'use-package-autoloads-mode) (autoload 'use-package-handler/:bind "use-package-bind-key" "

(fn NAME KEYWORD ARGS REST STATE &optional BIND-MACRO)" nil nil) (defalias 'use-package-normalize/:bind-keymap 'use-package-normalize-binder) (defalias 'use-package-normalize/:bind-keymap* 'use-package-normalize-binder) (autoload 'use-package-handler/:bind-keymap "use-package-bind-key" "

(fn NAME KEYWORD ARGS REST STATE &optional OVERRIDE)" nil nil) (autoload 'use-package-handler/:bind-keymap* "use-package-bind-key" "

(fn NAME KEYWORD ARG REST STATE)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "use-package-bind-key" '("use-package-handler/:bind*"))) (autoload 'use-package "use-package-core" "Declare an Emacs package by specifying a group of configuration options.

For full documentation, please see the README file that came with
this file.  Usage:

  (use-package package-name
     [:keyword [option]]...)

:init            Code to run before PACKAGE-NAME has been loaded.
:config          Code to run after PACKAGE-NAME has been loaded.  Note that
                 if loading is deferred for any reason, this code does not
                 execute until the lazy load has occurred.
:preface         Code to be run before everything except `:disabled'; this
                 can be used to define functions for use in `:if', or that
                 should be seen by the byte-compiler.

:mode            Form to be added to `auto-mode-alist'.
:magic           Form to be added to `magic-mode-alist'.
:magic-fallback  Form to be added to `magic-fallback-mode-alist'.
:interpreter     Form to be added to `interpreter-mode-alist'.

:commands        Define autoloads for commands that will be defined by the
                 package.  This is useful if the package is being lazily
                 loaded, and you wish to conditionally call functions in your
                 `:init' block that are defined in the package.
:hook            Specify hook(s) to attach this package to.

:bind            Bind keys, and define autoloads for the bound commands.
:bind*           Bind keys, and define autoloads for the bound commands,
                 *overriding all minor mode bindings*.
:bind-keymap     Bind a key prefix to an auto-loaded keymap defined in the
                 package.  This is like `:bind', but for keymaps.
:bind-keymap*    Like `:bind-keymap', but overrides all minor mode bindings

:defer           Defer loading of a package -- this is implied when using
                 `:commands', `:bind', `:bind*', `:mode', `:magic', `:hook',
                 `:magic-fallback', or `:interpreter'.  This can be an integer,
                 to force loading after N seconds of idle time, if the package
                 has not already been loaded.
:after           Delay the use-package declaration until after the named modules
                 have loaded. Once load, it will be as though the use-package
                 declaration (without `:after') had been seen at that moment.
:demand          Prevent the automatic deferred loading introduced by constructs
                 such as `:bind' (see `:defer' for the complete list).

:if EXPR         Initialize and load only if EXPR evaluates to a non-nil value.
:disabled        The package is ignored completely if this keyword is present.
:defines         Declare certain variables to silence the byte-compiler.
:functions       Declare certain functions to silence the byte-compiler.
:load-path       Add to the `load-path' before attempting to load the package.
:diminish        Support for diminish.el (if installed).
:delight         Support for delight.el (if installed).
:custom          Call `custom-set' or `set-default' with each variable
                 definition without modifying the Emacs `custom-file'.
                 (compare with `custom-set-variables').
:custom-face     Call `customize-set-faces' with each face definition.
:ensure          Loads the package using package.el if necessary.
:pin             Pin the package to an archive.

(fn NAME &rest ARGS)" nil t) (function-put 'use-package 'lisp-indent-function '1) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "use-package-core" '("use-package-"))) (autoload 'use-package-normalize/:delight "use-package-delight" "Normalize arguments to delight.

(fn NAME KEYWORD ARGS)" nil nil) (autoload 'use-package-handler/:delight "use-package-delight" "

(fn NAME KEYWORD ARGS REST STATE)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "use-package-delight" '("use-package-normalize-delight"))) (autoload 'use-package-normalize/:diminish "use-package-diminish" "

(fn NAME KEYWORD ARGS)" nil nil) (autoload 'use-package-handler/:diminish "use-package-diminish" "

(fn NAME KEYWORD ARG REST STATE)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "use-package-diminish" '("use-package-normalize-diminish"))) (autoload 'use-package-normalize/:ensure "use-package-ensure" "

(fn NAME KEYWORD ARGS)" nil nil) (autoload 'use-package-handler/:ensure "use-package-ensure" "

(fn NAME KEYWORD ENSURE REST STATE)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "use-package-ensure" '("use-package-"))) (autoload 'use-package-jump-to-package-form "use-package-jump" "Attempt to find and jump to the `use-package' form that loaded
PACKAGE. This will only find the form if that form actually
required PACKAGE. If PACKAGE was previously required then this
function will jump to the file that originally required PACKAGE
instead.

(fn PACKAGE)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "use-package-jump" '("use-package-find-require"))) (autoload 'use-package-lint "use-package-lint" "Check for errors in use-package declarations.
For example, if the module's `:if' condition is met, but even
with the specified `:load-path' the module cannot be found." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "use-package-lint" '("use-package-lint-declaration"))) (provide 'use-package-autoloads)) "s" ((s-autoloads s) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "s" '("s-"))) (provide 's-autoloads)) "lv" ((lv-autoloads lv) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lv" '("lv-"))) (provide 'lv-autoloads)) "hydra" ((hydra-examples hydra hydra-ox hydra-autoloads) (autoload 'defhydra "hydra" "Create a Hydra - a family of functions with prefix NAME.

NAME should be a symbol, it will be the prefix of all functions
defined here.

BODY has the format:

    (BODY-MAP BODY-KEY &rest BODY-PLIST)

DOCSTRING will be displayed in the echo area to identify the
Hydra.  When DOCSTRING starts with a newline, special Ruby-style
substitution will be performed by `hydra--format'.

Functions are created on basis of HEADS, each of which has the
format:

    (KEY CMD &optional HINT &rest PLIST)

BODY-MAP is a keymap; `global-map' is used quite often.  Each
function generated from HEADS will be bound in BODY-MAP to
BODY-KEY + KEY (both are strings passed to `kbd'), and will set
the transient map so that all following heads can be called
though KEY only.  BODY-KEY can be an empty string.

CMD is a callable expression: either an interactive function
name, or an interactive lambda, or a single sexp (it will be
wrapped in an interactive lambda).

HINT is a short string that identifies its head.  It will be
printed beside KEY in the echo erea if `hydra-is-helpful' is not
nil.  If you don't even want the KEY to be printed, set HINT
explicitly to nil.

The heads inherit their PLIST from BODY-PLIST and are allowed to
override some keys.  The keys recognized are :exit, :bind, and :column.
:exit can be:

- nil (default): this head will continue the Hydra state.
- t: this head will stop the Hydra state.

:bind can be:
- nil: this head will not be bound in BODY-MAP.
- a lambda taking KEY and CMD used to bind a head.

:column is a string that sets the column for all subsequent heads.

It is possible to omit both BODY-MAP and BODY-KEY if you don't
want to bind anything.  In that case, typically you will bind the
generated NAME/body command.  This command is also the return
result of `defhydra'.

(fn NAME BODY &optional DOCSTRING &rest HEADS)" nil t) (function-put 'defhydra 'lisp-indent-function 'defun) (function-put 'defhydra 'doc-string-elt '3) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "hydra" '("defhydra" "hydra-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "hydra-examples" '("hydra-" "org-agenda-cts" "whitespace-mode"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "hydra-ox" '("hydra-ox"))) (provide 'hydra-autoloads)) "ivy" ((ivy-autoloads ivy-overlay ivy-faces ivy colir elpa) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "colir" '("colir-"))) (autoload 'ivy-resume "ivy" "Resume the last completion session, or SESSION if non-nil.
With a prefix arg, try to restore a recorded completion session,
if one exists.

(fn &optional SESSION)" t nil) (autoload 'ivy-read "ivy" "Read a string in the minibuffer, with completion.

PROMPT is a string, normally ending in a colon and a space.
`ivy-count-format' is prepended to PROMPT during completion.

COLLECTION is either a list of strings, a function, an alist, or
a hash table, supplied for `minibuffer-completion-table'.

PREDICATE is applied to filter out the COLLECTION immediately.
This argument is for compatibility with `completing-read'.

When REQUIRE-MATCH is non-nil, only members of COLLECTION can be
selected.

If INITIAL-INPUT is non-nil, then insert that input in the
minibuffer initially.

HISTORY is a name of a variable to hold the completion session
history.

KEYMAP is composed with `ivy-minibuffer-map'.

PRESELECT, when non-nil, determines which one of the candidates
matching INITIAL-INPUT to select initially.  An integer stands
for the position of the desired candidate in the collection,
counting from zero.  Otherwise, use the first occurrence of
PRESELECT in the collection.  Comparison is first done with
`equal'.  If that fails, and when applicable, match PRESELECT as
a regular expression.

DEF is for compatibility with `completing-read'.

UPDATE-FN is called each time the candidate list is re-displayed.

When SORT is non-nil, `ivy-sort-functions-alist' determines how
to sort candidates before displaying them.

ACTION is a function to call after selecting a candidate.
It takes one argument, the selected candidate. If COLLECTION is
an alist, the argument is a cons cell, otherwise it's a string.

MULTI-ACTION, when non-nil, is called instead of ACTION when
there are marked candidates. It takes the list of candidates as
its only argument. When it's nil, ACTION is called on each marked
candidate.

UNWIND is a function of no arguments to call before exiting.

RE-BUILDER is a function transforming input text into a regex
pattern.

MATCHER is a function which can override how candidates are
filtered based on user input.  It takes a regex pattern and a
list of candidates, and returns the list of matching candidates.

DYNAMIC-COLLECTION is a boolean specifying whether the list of
candidates is updated after each input by calling COLLECTION.

EXTRA-PROPS is a plist that can be used to store
collection-specific session-specific data.

CALLER is a symbol to uniquely identify the caller to `ivy-read'.
It is used, along with COLLECTION, to determine which
customizations apply to the current completion session.

(fn PROMPT COLLECTION &key PREDICATE REQUIRE-MATCH INITIAL-INPUT HISTORY PRESELECT DEF KEYMAP UPDATE-FN SORT ACTION MULTI-ACTION UNWIND RE-BUILDER MATCHER DYNAMIC-COLLECTION EXTRA-PROPS CALLER)" nil nil) (autoload 'ivy-completing-read "ivy" "Read a string in the minibuffer, with completion.

This interface conforms to `completing-read' and can be used for
`completing-read-function'.

PROMPT is a string that normally ends in a colon and a space.
COLLECTION is either a list of strings, an alist, an obarray, or a hash table.
PREDICATE limits completion to a subset of COLLECTION.
REQUIRE-MATCH is a boolean value or a symbol.  See `completing-read'.
INITIAL-INPUT is a string inserted into the minibuffer initially.
HISTORY is a list of previously selected inputs.
DEF is the default value.
INHERIT-INPUT-METHOD is currently ignored.

(fn PROMPT COLLECTION &optional PREDICATE REQUIRE-MATCH INITIAL-INPUT HISTORY DEF INHERIT-INPUT-METHOD)" nil nil) (defvar ivy-mode nil "Non-nil if Ivy mode is enabled.
See the `ivy-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `ivy-mode'.") (custom-autoload 'ivy-mode "ivy" nil) (autoload 'ivy-mode "ivy" "Toggle Ivy mode on or off.
Turn Ivy mode on if ARG is positive, off otherwise.
Turning on Ivy mode sets `completing-read-function' to
`ivy-completing-read'.

Global bindings:
\\{ivy-mode-map}

Minibuffer bindings:
\\{ivy-minibuffer-map}

(fn &optional ARG)" t nil) (autoload 'ivy-switch-buffer "ivy" "Switch to another buffer." t nil) (autoload 'ivy-switch-view "ivy" "Switch to one of the window views stored by `ivy-push-view'." t nil) (autoload 'ivy-switch-buffer-other-window "ivy" "Switch to another buffer in another window." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ivy" '("ivy-" "with-ivy-window"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ivy-overlay" '("ivy-"))) (provide 'ivy-autoloads)) "swiper" ((swiper swiper-autoloads) (autoload 'swiper-avy "swiper" "Jump to one of the current swiper candidates with `avy'." t nil) (autoload 'swiper-backward "swiper" "`isearch-backward' with an overview.
When non-nil, INITIAL-INPUT is the initial search pattern.

(fn &optional INITIAL-INPUT)" t nil) (autoload 'swiper-thing-at-point "swiper" "`swiper' with `ivy-thing-at-point'." t nil) (autoload 'swiper-all-thing-at-point "swiper" "`swiper-all' with `ivy-thing-at-point'." t nil) (autoload 'swiper "swiper" "`isearch-forward' with an overview.
When non-nil, INITIAL-INPUT is the initial search pattern.

(fn &optional INITIAL-INPUT)" t nil) (autoload 'swiper-all "swiper" "Run `swiper' for all open buffers.

(fn &optional INITIAL-INPUT)" t nil) (autoload 'swiper-isearch "swiper" "A `swiper' that's not line-based.

(fn &optional INITIAL-INPUT)" t nil) (autoload 'swiper-isearch-backward "swiper" "Like `swiper-isearch' but the first result is before the point.

(fn &optional INITIAL-INPUT)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "swiper" '("swiper-"))) (provide 'swiper-autoloads)) "counsel" ((counsel-autoloads counsel) (autoload 'counsel-company "counsel" "Complete using `company-candidates'." t nil) (autoload 'counsel-irony "counsel" "Inline C/C++ completion using Irony." t nil) (autoload 'counsel-describe-variable "counsel" "Forward to `describe-variable'.

Variables declared using `defcustom' are highlighted according to
`ivy-highlight-face'." t nil) (autoload 'counsel-describe-function "counsel" "Forward to `describe-function'.

Interactive functions (i.e., commands) are highlighted according
to `ivy-highlight-face'." t nil) (autoload 'counsel-describe-symbol "counsel" "Forward to `describe-symbol'." t nil) (autoload 'counsel-set-variable "counsel" "Set a variable SYM, with completion.

When the selected variable is a `defcustom' with the type boolean
or radio, offer completion of all possible values.

Otherwise, offer a variant of `eval-expression', with the initial
input corresponding to the chosen variable.

With a prefix arg, restrict list to variables defined using
`defcustom'.

(fn SYM)" t nil) (autoload 'counsel-apropos "counsel" "Show all matching symbols.
See `apropos' for further information on what is considered
a symbol and how to search for them." t nil) (autoload 'counsel-info-lookup-symbol "counsel" "Forward SYMBOL to `info-lookup-symbol' with ivy completion.
With prefix arg MODE a query for the symbol help mode is offered.

(fn SYMBOL &optional MODE)" t nil) (autoload 'counsel-M-x "counsel" "Ivy version of `execute-extended-command'.
Optional INITIAL-INPUT is the initial input in the minibuffer.
This function integrates with either the `amx' or `smex' package
when available, in that order of precedence.

(fn &optional INITIAL-INPUT)" t nil) (autoload 'counsel-command-history "counsel" "Show the history of commands." t nil) (autoload 'counsel-load-library "counsel" "Load a selected the Emacs Lisp library.
The libraries are offered from `load-path'." t nil) (autoload 'counsel-find-library "counsel" "Visit a selected the Emacs Lisp library.
The libraries are offered from `load-path'." t nil) (autoload 'counsel-load-theme "counsel" "Forward to `load-theme'.
Usable with `ivy-resume', `ivy-next-line-and-call' and
`ivy-previous-line-and-call'." t nil) (autoload 'counsel-descbinds "counsel" "Show a list of all defined keys and their definitions.
If non-nil, show only bindings that start with PREFIX.
BUFFER defaults to the current one.

(fn &optional PREFIX BUFFER)" t nil) (autoload 'counsel-describe-face "counsel" "Completion for `describe-face'." t nil) (autoload 'counsel-faces "counsel" "Complete faces with preview.
Actions are provided by default for describing or customizing the
selected face." t nil) (autoload 'counsel-git "counsel" "Find file in the current Git repository.
INITIAL-INPUT can be given as the initial minibuffer input.

(fn &optional INITIAL-INPUT)" t nil) (autoload 'counsel-git-grep "counsel" "Grep for a string in the current Git repository.
INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search.
When CMD is a string, use it as a \"git grep\" command.
When CMD is non-nil, prompt for a specific \"git grep\" command.

(fn &optional INITIAL-INPUT INITIAL-DIRECTORY CMD)" t nil) (autoload 'counsel-git-stash "counsel" "Search through all available git stashes." t nil) (autoload 'counsel-git-change-worktree "counsel" "Find the file corresponding to the current buffer on a different worktree." t nil) (autoload 'counsel-git-checkout "counsel" "Call the \"git checkout\" command." t nil) (autoload 'counsel-git-log "counsel" "Call the \"git log --grep\" shell command." t nil) (autoload 'counsel-find-file "counsel" "Forward to `find-file'.
When INITIAL-INPUT is non-nil, use it in the minibuffer during completion.

(fn &optional INITIAL-INPUT INITIAL-DIRECTORY)" t nil) (autoload 'counsel-dired "counsel" "Forward to `dired'.
When INITIAL-INPUT is non-nil, use it in the minibuffer during completion.

(fn &optional INITIAL-INPUT)" t nil) (autoload 'counsel-recentf "counsel" "Find a file on `recentf-list'." t nil) (autoload 'counsel-buffer-or-recentf "counsel" "Find a buffer visiting a file or file on `recentf-list'." t nil) (autoload 'counsel-bookmark "counsel" "Forward to `bookmark-jump' or `bookmark-set' if bookmark doesn't exist." t nil) (autoload 'counsel-bookmarked-directory "counsel" "Ivy interface for bookmarked directories.

With a prefix argument, this command creates a new bookmark which points to the
current value of `default-directory'." t nil) (autoload 'counsel-file-register "counsel" "Search file in register.

You cannot use Emacs' normal register commands to create file
registers.  Instead you must use the `set-register' function like
so: `(set-register ?i \"/home/eric/.emacs.d/init.el\")'.  Now you
can use `C-x r j i' to open that file." t nil) (autoload 'counsel-locate-action-extern "counsel" "Pass X to `xdg-open' or equivalent command via the shell.

(fn X)" t nil) (autoload 'counsel-locate "counsel" "Call a \"locate\" style shell command.
INITIAL-INPUT can be given as the initial minibuffer input.

(fn &optional INITIAL-INPUT)" t nil) (autoload 'counsel-tracker "counsel" nil t nil) (autoload 'counsel-fzf "counsel" "Open a file using the fzf shell command.
INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search.
FZF-PROMPT, if non-nil, is passed as `ivy-read' prompt argument.

(fn &optional INITIAL-INPUT INITIAL-DIRECTORY FZF-PROMPT)" t nil) (autoload 'counsel-dpkg "counsel" "Call the \"dpkg\" shell command." t nil) (autoload 'counsel-rpm "counsel" "Call the \"rpm\" shell command." t nil) (autoload 'counsel-file-jump "counsel" "Jump to a file below the current directory.
List all files within the current directory or any of its sub-directories.
INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search.

(fn &optional INITIAL-INPUT INITIAL-DIRECTORY)" t nil) (autoload 'counsel-dired-jump "counsel" "Jump to a directory (see `dired-jump') below the current directory.
List all sub-directories within the current directory.
INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search.

(fn &optional INITIAL-INPUT INITIAL-DIRECTORY)" t nil) (autoload 'counsel-ag "counsel" "Grep for a string in a root directory using `ag'.

By default, the root directory is the first directory containing
a .git subdirectory.

INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search.
EXTRA-AG-ARGS, if non-nil, is appended to `counsel-ag-base-command'.
AG-PROMPT, if non-nil, is passed as `ivy-read' prompt argument.
CALLER is passed to `ivy-read'.

With a `\\[universal-argument]' prefix argument, prompt for INITIAL-DIRECTORY.
With a `\\[universal-argument] \\[universal-argument]' prefix argument, prompt additionally for EXTRA-AG-ARGS.

(fn &optional INITIAL-INPUT INITIAL-DIRECTORY EXTRA-AG-ARGS AG-PROMPT &key CALLER)" t nil) (autoload 'counsel-pt "counsel" "Grep for a string in the current directory using pt.
INITIAL-INPUT can be given as the initial minibuffer input.
This uses `counsel-ag' with `counsel-pt-base-command' instead of
`counsel-ag-base-command'.

(fn &optional INITIAL-INPUT)" t nil) (autoload 'counsel-ack "counsel" "Grep for a string in the current directory using ack.
INITIAL-INPUT can be given as the initial minibuffer input.
This uses `counsel-ag' with `counsel-ack-base-command' replacing
`counsel-ag-base-command'.

(fn &optional INITIAL-INPUT)" t nil) (autoload 'counsel-rg "counsel" "Grep for a string in the current directory using `rg'.
INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search.
EXTRA-RG-ARGS string, if non-nil, is appended to `counsel-rg-base-command'.
RG-PROMPT, if non-nil, is passed as `ivy-read' prompt argument.

Example input with inclusion and exclusion file patterns:
    require i -- -g*.el

(fn &optional INITIAL-INPUT INITIAL-DIRECTORY EXTRA-RG-ARGS RG-PROMPT)" t nil) (autoload 'counsel-grep "counsel" "Grep for a string in the file visited by the current buffer.
When non-nil, INITIAL-INPUT is the initial search pattern.

(fn &optional INITIAL-INPUT)" t nil) (autoload 'counsel-grep-backward "counsel" "Grep for a string in the file visited by the current buffer going
backward similar to `swiper-backward'. When non-nil, INITIAL-INPUT is
the initial search pattern.

(fn &optional INITIAL-INPUT)" t nil) (autoload 'counsel-grep-or-swiper "counsel" "Call `swiper' for small buffers and `counsel-grep' for large ones.
When non-nil, INITIAL-INPUT is the initial search pattern.

(fn &optional INITIAL-INPUT)" t nil) (autoload 'counsel-grep-or-swiper-backward "counsel" "Call `swiper-backward' for small buffers and `counsel-grep-backward' for
large ones.  When non-nil, INITIAL-INPUT is the initial search pattern.

(fn &optional INITIAL-INPUT)" t nil) (autoload 'counsel-recoll "counsel" "Search for a string in the recoll database.
You'll be given a list of files that match.
Selecting a file will launch `swiper' for that file.
INITIAL-INPUT can be given as the initial minibuffer input.

(fn &optional INITIAL-INPUT)" t nil) (autoload 'counsel--org-get-tags "counsel" nil nil nil) (autoload 'counsel-org-tag "counsel" "Add or remove tags in `org-mode'." t nil) (autoload 'counsel-org-tag-agenda "counsel" "Set tags for the current agenda item." t nil) (defalias 'counsel-org-goto #'counsel-outline) (autoload 'counsel-org-goto-all "counsel" "Go to a different location in any org file." t nil) (autoload 'counsel-org-file "counsel" "Browse all attachments for current Org file." t nil) (autoload 'counsel-org-entity "counsel" "Complete Org entities using Ivy." t nil) (autoload 'counsel-org-capture "counsel" "Capture something." t nil) (autoload 'counsel-org-agenda-headlines "counsel" "Choose from headers of `org-mode' files in the agenda." t nil) (autoload 'counsel-org-link "counsel" "Insert a link to an headline with completion." t nil) (autoload 'counsel-mark-ring "counsel" "Browse `mark-ring' interactively.
Obeys `widen-automatically', which see." t nil) (autoload 'counsel-evil-marks "counsel" "Ivy replacement for `evil-show-marks'.
By default, this function respects `counsel-evil-marks-exclude-registers'.
When ARG is non-nil, display all active evil registers.

(fn &optional ARG)" t nil) (autoload 'counsel-package "counsel" "Install or delete packages.

Packages not currently installed are prefixed with \"+\", and
selecting one of these will try to install it.
Packages currently installed are prefixed with \"-\", and
selecting one of these will try to delete it.

Additional actions:\\<ivy-minibuffer-map>

  \\[ivy-dispatching-done] d: Describe package
  \\[ivy-dispatching-done] h: Visit package's homepage" t nil) (autoload 'counsel-tmm "counsel" "Text-mode emulation of looking and choosing from a menu bar." t nil) (autoload 'counsel-yank-pop "counsel" "Ivy replacement for `yank-pop'.
With a plain prefix argument (\\[universal-argument]),
temporarily toggle the value of `counsel-yank-pop-after-point'.
Any other value of ARG has the same meaning as in `yank-pop', but
`counsel-yank-pop-preselect-last' determines its default value.
See also `counsel-yank-pop-filter' for how to filter candidates.

Note: Duplicate elements of `kill-ring' are always deleted.

(fn &optional ARG)" t nil) (autoload 'counsel-register "counsel" "Interactively choose a register." t nil) (autoload 'counsel-evil-registers "counsel" "Ivy replacement for `evil-show-registers'." t nil) (autoload 'counsel-imenu "counsel" "Jump to a buffer position indexed by imenu." t nil) (autoload 'counsel-list-processes "counsel" "Offer completion for `process-list'.
The default action deletes the selected process.
An extra action allows to switch to the process buffer." t nil) (autoload 'counsel-minibuffer-history "counsel" "Browse minibuffer history." t nil) (autoload 'counsel-esh-history "counsel" "Browse Eshell history." t nil) (autoload 'counsel-shell-history "counsel" "Browse shell history." t nil) (autoload 'counsel-slime-repl-history "counsel" "Browse Slime REPL history." t nil) (autoload 'counsel-hydra-heads "counsel" "Call a head of the current/last hydra." t nil) (autoload 'counsel-semantic "counsel" "Jump to a semantic tag in the current buffer." t nil) (autoload 'counsel-semantic-or-imenu "counsel" nil t nil) (autoload 'counsel-outline "counsel" "Jump to an outline heading with completion." t nil) (autoload 'counsel-ibuffer "counsel" "Use ibuffer to switch to another buffer.
NAME specifies the name of the buffer (defaults to \"*Ibuffer*\").

(fn &optional NAME)" t nil) (autoload 'counsel-switch-to-shell-buffer "counsel" "Switch to a shell buffer, or create one." t nil) (autoload 'counsel-unicode-char "counsel" "Insert COUNT copies of a Unicode character at point.
COUNT defaults to 1.

(fn &optional COUNT)" t nil) (autoload 'counsel-colors-emacs "counsel" "Show a list of all supported colors for a particular frame.

You can insert or kill the name or hexadecimal RGB value of the
selected color." t nil) (autoload 'counsel-colors-web "counsel" "Show a list of all W3C web colors for use in CSS.

You can insert or kill the name or hexadecimal RGB value of the
selected color." t nil) (autoload 'counsel-fonts "counsel" "Show a list of all supported font families for a particular frame.

You can insert or kill the name of the selected font." t nil) (autoload 'counsel-kmacro "counsel" "Interactively choose and run a keyboard macro.

With prefix argument, run macro that many times.

Macros are run using the current value of `kmacro-counter-value'
and their respective counter format. Displayed next to each macro is
the counter's format and initial value.

One can use actions to copy the counter format or initial counter
value of a macro, using them for a new macro." t nil) (autoload 'counsel-geiser-doc-look-up-manual "counsel" "Search Scheme documentation." t nil) (autoload 'counsel-rhythmbox "counsel" "Choose a song from the Rhythmbox library to play or enqueue.

(fn &optional ARG)" t nil) (autoload 'counsel-linux-app "counsel" "Launch a Linux desktop application, similar to Alt-<F2>.
When ARG is non-nil, ignore NoDisplay property in *.desktop files.

(fn &optional ARG)" t nil) (autoload 'counsel-wmctrl "counsel" "Select a desktop window using wmctrl." t nil) (autoload 'counsel-switch-buffer "counsel" "Switch to another buffer.
Display a preview of the selected ivy completion candidate buffer
in the current window." t nil) (autoload 'counsel-switch-buffer-other-window "counsel" "Switch to another buffer in another window.
Display a preview of the selected ivy completion candidate buffer
in the current window." t nil) (autoload 'counsel-compile "counsel" "Call `compile' completing with smart suggestions, optionally for DIR.

Additional actions:

\\{counsel-compile-map}

(fn &optional DIR)" t nil) (autoload 'counsel-compile-env "counsel" "Update `counsel-compile-env' interactively." t nil) (autoload 'counsel-minor "counsel" "Enable or disable minor mode.

Disabled minor modes are prefixed with \"+\", and
selecting one of these will enable it.
Enabled minor modes are prefixed with \"-\", and
selecting one of these will enable it.

Additional actions:\\<ivy-minibuffer-map>

  \\[ivy-dispatching-done] d: Go to minor mode definition
  \\[ivy-dispatching-done] h: Describe minor mode" t nil) (autoload 'counsel-major "counsel" nil t nil) (autoload 'counsel-compilation-errors "counsel" "Compilation errors." t nil) (autoload 'counsel-flycheck "counsel" "Flycheck errors." t nil) (defvar counsel-mode nil "Non-nil if Counsel mode is enabled.
See the `counsel-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `counsel-mode'.") (custom-autoload 'counsel-mode "counsel" nil) (autoload 'counsel-mode "counsel" "Toggle Counsel mode on or off.
Turn Counsel mode on if ARG is positive, off otherwise. Counsel
mode remaps built-in emacs functions that have counsel
replacements.

Local bindings (`counsel-mode-map'):
\\{counsel-mode-map}

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "counsel" '("counsel-" "ivy-function-called-at-point" "tmm-km-list"))) (provide 'counsel-autoloads)) "goto-chg" ((goto-chg-autoloads goto-chg) (autoload 'goto-last-change "goto-chg" "Go to the point where the last edit was made in the current buffer.
Repeat the command to go to the second last edit, etc.

To go back to more recent edit, the reverse of this command, use \\[goto-last-change-reverse]
or precede this command with \\[universal-argument] - (minus).

It does not go to the same point twice even if there has been many edits
there. I call the minimal distance between distinguishable edits \"span\".
Set variable `glc-default-span' to control how close is \"the same point\".
Default span is 8.
The span can be changed temporarily with \\[universal-argument] right before \\[goto-last-change]:
\\[universal-argument] <NUMBER> set current span to that number,
\\[universal-argument] (no number) multiplies span by 4, starting with default.
The so set span remains until it is changed again with \\[universal-argument], or the consecutive
repetition of this command is ended by any other command.

When span is zero (i.e. \\[universal-argument] 0) subsequent \\[goto-last-change] visits each and
every point of edit and a message shows what change was made there.
In this case it may go to the same point twice.

This command uses undo information. If undo is disabled, so is this command.
At times, when undo information becomes too large, the oldest information is
discarded. See variable `undo-limit'.

(fn ARG)" t nil) (autoload 'goto-last-change-reverse "goto-chg" "Go back to more recent changes after \\[goto-last-change] have been used.
See `goto-last-change' for use of prefix argument.

(fn ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "goto-chg" '("glc-"))) (provide 'goto-chg-autoloads)) "evil" ((evil-jumps evil-states evil-autoloads evil-keybindings evil-core evil-digraphs evil evil-commands evil-command-window evil-search evil-maps evil-vars evil-development evil-macros evil-pkg evil-repeat evil-ex evil-integration evil-types evil-common) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-command-window" '("evil-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-commands" '("evil-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-common" '("bounds-of-evil-" "evil-" "forward-evil-"))) (autoload 'evil-mode "evil" nil t) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-core" '("evil-" "turn-o"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-digraphs" '("evil-digraph"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-ex" '("evil-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-integration" '("evil-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-jumps" '("evil-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-macros" '("evil-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-maps" '("evil-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-repeat" '("evil-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-search" '("evil-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-states" '("evil-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-types" '("evil-ex-get-optional-register-and-count"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-vars" '("evil-"))) (provide 'evil-autoloads)) "evil-escape" ((evil-escape evil-escape-autoloads) (defvar evil-escape-mode nil "Non-nil if Evil-Escape mode is enabled.
See the `evil-escape-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `evil-escape-mode'.") (custom-autoload 'evil-escape-mode "evil-escape" nil) (autoload 'evil-escape-mode "evil-escape" "Buffer-local minor mode to escape insert state and everything else
with a key sequence.

If called interactively, enable Evil-Escape mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-escape" '("evil-escape"))) (provide 'evil-escape-autoloads)) "evil-leader" ((evil-leader evil-leader-autoloads) (autoload 'global-evil-leader-mode "evil-leader" "Global minor mode for <leader> support.

If called interactively, enable Global Evil-Leader mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (autoload 'evil-leader-mode "evil-leader" "Minor mode to enable <leader> support.

If called interactively, enable Evil-Leader mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (autoload 'evil-leader/set-key "evil-leader" "Bind `key' to command `def' in `evil-leader/default-map'.

Key has to be readable by `read-kbd-macro' and `def' a command.
Accepts further `key' `def' pairs.

(fn KEY DEF &rest BINDINGS)" t nil) (autoload 'evil-leader/set-key-for-mode "evil-leader" "Create keybindings for major-mode `mode' with `key' bound to command `def'.

See `evil-leader/set-key'.

(fn MODE KEY DEF &rest BINDINGS)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-leader" '("evil-leader"))) (provide 'evil-leader-autoloads)) "projectile" ((projectile projectile-autoloads) (autoload 'projectile-version "projectile" "Get the Projectile version as string.

If called interactively or if SHOW-VERSION is non-nil, show the
version in the echo area and the messages buffer.

The returned string includes both, the version from package.el
and the library version, if both a present and different.

If the version number could not be determined, signal an error,
if called interactively, or if SHOW-VERSION is non-nil, otherwise
just return nil.

(fn &optional SHOW-VERSION)" t nil) (autoload 'projectile-invalidate-cache "projectile" "Remove the current project's files from `projectile-projects-cache'.

With a prefix argument PROMPT prompts for the name of the project whose cache
to invalidate.

(fn PROMPT)" t nil) (autoload 'projectile-purge-file-from-cache "projectile" "Purge FILE from the cache of the current project.

(fn FILE)" t nil) (autoload 'projectile-purge-dir-from-cache "projectile" "Purge DIR from the cache of the current project.

(fn DIR)" t nil) (autoload 'projectile-cache-current-file "projectile" "Add the currently visited file to the cache." t nil) (autoload 'projectile-discover-projects-in-directory "projectile" "Discover any projects in DIRECTORY and add them to the projectile cache.

If DEPTH is non-nil recursively descend exactly DEPTH levels below DIRECTORY and
discover projects there.

(fn DIRECTORY &optional DEPTH)" nil nil) (autoload 'projectile-discover-projects-in-search-path "projectile" "Discover projects in `projectile-project-search-path'.
Invoked automatically when `projectile-mode' is enabled." t nil) (autoload 'projectile-switch-to-buffer "projectile" "Switch to a project buffer." t nil) (autoload 'projectile-switch-to-buffer-other-window "projectile" "Switch to a project buffer and show it in another window." t nil) (autoload 'projectile-switch-to-buffer-other-frame "projectile" "Switch to a project buffer and show it in another frame." t nil) (autoload 'projectile-display-buffer "projectile" "Display a project buffer in another window without selecting it." t nil) (autoload 'projectile-project-buffers-other-buffer "projectile" "Switch to the most recently selected buffer project buffer.
Only buffers not visible in windows are returned." t nil) (autoload 'projectile-multi-occur "projectile" "Do a `multi-occur' in the project's buffers.
With a prefix argument, show NLINES of context.

(fn &optional NLINES)" t nil) (autoload 'projectile-find-other-file "projectile" "Switch between files with the same name but different extensions.
With FLEX-MATCHING, match any file that contains the base name of current file.
Other file extensions can be customized with the variable `projectile-other-file-alist'.

(fn &optional FLEX-MATCHING)" t nil) (autoload 'projectile-find-other-file-other-window "projectile" "Switch between files with the same name but different extensions in other window.
With FLEX-MATCHING, match any file that contains the base name of current file.
Other file extensions can be customized with the variable `projectile-other-file-alist'.

(fn &optional FLEX-MATCHING)" t nil) (autoload 'projectile-find-other-file-other-frame "projectile" "Switch between files with the same name but different extensions in other frame.
With FLEX-MATCHING, match any file that contains the base name of current file.
Other file extensions can be customized with the variable `projectile-other-file-alist'.

(fn &optional FLEX-MATCHING)" t nil) (autoload 'projectile-find-file-dwim "projectile" "Jump to a project's files using completion based on context.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

If point is on a filename, Projectile first tries to search for that
file in project:

- If it finds just a file, it switches to that file instantly.  This works even
if the filename is incomplete, but there's only a single file in the current project
that matches the filename at point.  For example, if there's only a single file named
\"projectile/projectile.el\" but the current filename is \"projectile/proj\" (incomplete),
`projectile-find-file-dwim' still switches to \"projectile/projectile.el\" immediately
 because this is the only filename that matches.

- If it finds a list of files, the list is displayed for selecting.  A list of
files is displayed when a filename appears more than one in the project or the
filename at point is a prefix of more than two files in a project.  For example,
if `projectile-find-file-dwim' is executed on a filepath like \"projectile/\", it lists
the content of that directory.  If it is executed on a partial filename like
 \"projectile/a\", a list of files with character 'a' in that directory is presented.

- If it finds nothing, display a list of all files in project for selecting.

(fn &optional INVALIDATE-CACHE)" t nil) (autoload 'projectile-find-file-dwim-other-window "projectile" "Jump to a project's files using completion based on context in other window.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

If point is on a filename, Projectile first tries to search for that
file in project:

- If it finds just a file, it switches to that file instantly.  This works even
if the filename is incomplete, but there's only a single file in the current project
that matches the filename at point.  For example, if there's only a single file named
\"projectile/projectile.el\" but the current filename is \"projectile/proj\" (incomplete),
`projectile-find-file-dwim-other-window' still switches to \"projectile/projectile.el\"
immediately because this is the only filename that matches.

- If it finds a list of files, the list is displayed for selecting.  A list of
files is displayed when a filename appears more than one in the project or the
filename at point is a prefix of more than two files in a project.  For example,
if `projectile-find-file-dwim-other-window' is executed on a filepath like \"projectile/\", it lists
the content of that directory.  If it is executed on a partial filename
like \"projectile/a\", a list of files with character 'a' in that directory
is presented.

- If it finds nothing, display a list of all files in project for selecting.

(fn &optional INVALIDATE-CACHE)" t nil) (autoload 'projectile-find-file-dwim-other-frame "projectile" "Jump to a project's files using completion based on context in other frame.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

If point is on a filename, Projectile first tries to search for that
file in project:

- If it finds just a file, it switches to that file instantly.  This works even
if the filename is incomplete, but there's only a single file in the current project
that matches the filename at point.  For example, if there's only a single file named
\"projectile/projectile.el\" but the current filename is \"projectile/proj\" (incomplete),
`projectile-find-file-dwim-other-frame' still switches to \"projectile/projectile.el\"
immediately because this is the only filename that matches.

- If it finds a list of files, the list is displayed for selecting.  A list of
files is displayed when a filename appears more than one in the project or the
filename at point is a prefix of more than two files in a project.  For example,
if `projectile-find-file-dwim-other-frame' is executed on a filepath like \"projectile/\", it lists
the content of that directory.  If it is executed on a partial filename
like \"projectile/a\", a list of files with character 'a' in that directory
is presented.

- If it finds nothing, display a list of all files in project for selecting.

(fn &optional INVALIDATE-CACHE)" t nil) (autoload 'projectile-find-file "projectile" "Jump to a project's file using completion.
With a prefix arg INVALIDATE-CACHE invalidates the cache first.

(fn &optional INVALIDATE-CACHE)" t nil) (autoload 'projectile-find-file-other-window "projectile" "Jump to a project's file using completion and show it in another window.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

(fn &optional INVALIDATE-CACHE)" t nil) (autoload 'projectile-find-file-other-frame "projectile" "Jump to a project's file using completion and show it in another frame.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

(fn &optional INVALIDATE-CACHE)" t nil) (autoload 'projectile-toggle-project-read-only "projectile" "Toggle project read only." t nil) (autoload 'projectile-find-dir "projectile" "Jump to a project's directory using completion.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

(fn &optional INVALIDATE-CACHE)" t nil) (autoload 'projectile-find-dir-other-window "projectile" "Jump to a project's directory in other window using completion.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

(fn &optional INVALIDATE-CACHE)" t nil) (autoload 'projectile-find-dir-other-frame "projectile" "Jump to a project's directory in other frame using completion.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

(fn &optional INVALIDATE-CACHE)" t nil) (autoload 'projectile-find-test-file "projectile" "Jump to a project's test file using completion.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

(fn &optional INVALIDATE-CACHE)" t nil) (autoload 'projectile-find-related-file-other-window "projectile" "Open related file in other window." t nil) (autoload 'projectile-find-related-file-other-frame "projectile" "Open related file in other frame." t nil) (autoload 'projectile-find-related-file "projectile" "Open related file." t nil) (autoload 'projectile-related-files-fn-groups "projectile" "Generate a related-files-fn which relates as KIND for files in each of GROUPS.

(fn KIND GROUPS)" nil nil) (autoload 'projectile-related-files-fn-extensions "projectile" "Generate a related-files-fn which relates as KIND for files having EXTENSIONS.

(fn KIND EXTENSIONS)" nil nil) (autoload 'projectile-related-files-fn-test-with-prefix "projectile" "Generate a related-files-fn which relates tests and impl for files with EXTENSION based on TEST-PREFIX.

(fn EXTENSION TEST-PREFIX)" nil nil) (autoload 'projectile-related-files-fn-test-with-suffix "projectile" "Generate a related-files-fn which relates tests and impl for files with EXTENSION based on TEST-SUFFIX.

(fn EXTENSION TEST-SUFFIX)" nil nil) (autoload 'projectile-project-info "projectile" "Display info for current project." t nil) (autoload 'projectile-find-implementation-or-test-other-window "projectile" "Open matching implementation or test file in other window." t nil) (autoload 'projectile-find-implementation-or-test-other-frame "projectile" "Open matching implementation or test file in other frame." t nil) (autoload 'projectile-toggle-between-implementation-and-test "projectile" "Toggle between an implementation file and its test file." t nil) (autoload 'projectile-grep "projectile" "Perform rgrep in the project.

With a prefix ARG asks for files (globbing-aware) which to grep in.
With prefix ARG of `-' (such as `M--'), default the files (without prompt),
to `projectile-grep-default-files'.

With REGEXP given, don't query the user for a regexp.

(fn &optional REGEXP ARG)" t nil) (autoload 'projectile-ag "projectile" "Run an ag search with SEARCH-TERM in the project.

With an optional prefix argument ARG SEARCH-TERM is interpreted as a
regular expression.

(fn SEARCH-TERM &optional ARG)" t nil) (autoload 'projectile-ripgrep "projectile" "Run a ripgrep (rg) search with `SEARCH-TERM' at current project root.

With an optional prefix argument ARG SEARCH-TERM is interpreted as a
regular expression.

This command depends on of the Emacs packages ripgrep or rg being
installed to work.

(fn SEARCH-TERM &optional ARG)" t nil) (autoload 'projectile-regenerate-tags "projectile" "Regenerate the project's [e|g]tags." t nil) (autoload 'projectile-find-tag "projectile" "Find tag in project." t nil) (autoload 'projectile-run-command-in-root "projectile" "Invoke `execute-extended-command' in the project's root." t nil) (autoload 'projectile-run-shell-command-in-root "projectile" "Invoke `shell-command' in the project's root.

(fn COMMAND &optional OUTPUT-BUFFER ERROR-BUFFER)" t nil) (autoload 'projectile-run-async-shell-command-in-root "projectile" "Invoke `async-shell-command' in the project's root.

(fn COMMAND &optional OUTPUT-BUFFER ERROR-BUFFER)" t nil) (autoload 'projectile-run-gdb "projectile" "Invoke `gdb' in the project's root." t nil) (autoload 'projectile-run-shell "projectile" "Invoke `shell' in the project's root.

Switch to the project specific shell buffer if it already exists.

Use a prefix argument ARG to indicate creation of a new process instead.

(fn &optional ARG)" t nil) (autoload 'projectile-run-eshell "projectile" "Invoke `eshell' in the project's root.

Switch to the project specific eshell buffer if it already exists.

Use a prefix argument ARG to indicate creation of a new process instead.

(fn &optional ARG)" t nil) (autoload 'projectile-run-ielm "projectile" "Invoke `ielm' in the project's root.

Switch to the project specific ielm buffer if it already exists.

Use a prefix argument ARG to indicate creation of a new process instead.

(fn &optional ARG)" t nil) (autoload 'projectile-run-term "projectile" "Invoke `term' in the project's root.

Switch to the project specific term buffer if it already exists.

Use a prefix argument ARG to indicate creation of a new process instead.

(fn &optional ARG)" t nil) (autoload 'projectile-run-vterm "projectile" "Invoke `vterm' in the project's root.

Switch to the project specific term buffer if it already exists.

Use a prefix argument ARG to indicate creation of a new process instead.

(fn &optional ARG)" t nil) (autoload 'projectile-replace "projectile" "Replace literal string in project using non-regexp `tags-query-replace'.

With a prefix argument ARG prompts you for a directory on which
to run the replacement.

(fn &optional ARG)" t nil) (autoload 'projectile-replace-regexp "projectile" "Replace a regexp in the project using `tags-query-replace'.

With a prefix argument ARG prompts you for a directory on which
to run the replacement.

(fn &optional ARG)" t nil) (autoload 'projectile-kill-buffers "projectile" "Kill project buffers.

The buffer are killed according to the value of
`projectile-kill-buffers-filter'." t nil) (autoload 'projectile-save-project-buffers "projectile" "Save all project buffers." t nil) (autoload 'projectile-dired "projectile" "Open `dired' at the root of the project." t nil) (autoload 'projectile-dired-other-window "projectile" "Open `dired'  at the root of the project in another window." t nil) (autoload 'projectile-dired-other-frame "projectile" "Open `dired' at the root of the project in another frame." t nil) (autoload 'projectile-vc "projectile" "Open `vc-dir' at the root of the project.

For git projects `magit-status-internal' is used if available.
For hg projects `monky-status' is used if available.

If PROJECT-ROOT is given, it is opened instead of the project
root directory of the current buffer file.  If interactively
called with a prefix argument, the user is prompted for a project
directory to open.

(fn &optional PROJECT-ROOT)" t nil) (autoload 'projectile-recentf "projectile" "Show a list of recently visited files in a project." t nil) (autoload 'projectile-configure-project "projectile" "Run project configure command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG.

(fn ARG)" t nil) (autoload 'projectile-compile-project "projectile" "Run project compilation command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG.

(fn ARG)" t nil) (autoload 'projectile-test-project "projectile" "Run project test command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG.

(fn ARG)" t nil) (autoload 'projectile-install-project "projectile" "Run project install command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG.

(fn ARG)" t nil) (autoload 'projectile-package-project "projectile" "Run project package command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG.

(fn ARG)" t nil) (autoload 'projectile-run-project "projectile" "Run project run command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG.

(fn ARG)" t nil) (autoload 'projectile-repeat-last-command "projectile" "Run last projectile external command.

External commands are: `projectile-configure-project',
`projectile-compile-project', `projectile-test-project',
`projectile-install-project', `projectile-package-project',
and `projectile-run-project'.

If the prefix argument SHOW_PROMPT is non nil, the command can be edited.

(fn SHOW-PROMPT)" t nil) (autoload 'projectile-switch-project "projectile" "Switch to a project we have visited before.
Invokes the command referenced by `projectile-switch-project-action' on switch.
With a prefix ARG invokes `projectile-commander' instead of
`projectile-switch-project-action.'

(fn &optional ARG)" t nil) (autoload 'projectile-switch-open-project "projectile" "Switch to a project we have currently opened.
Invokes the command referenced by `projectile-switch-project-action' on switch.
With a prefix ARG invokes `projectile-commander' instead of
`projectile-switch-project-action.'

(fn &optional ARG)" t nil) (autoload 'projectile-find-file-in-directory "projectile" "Jump to a file in a (maybe regular) DIRECTORY.

This command will first prompt for the directory the file is in.

(fn &optional DIRECTORY)" t nil) (autoload 'projectile-find-file-in-known-projects "projectile" "Jump to a file in any of the known projects." t nil) (autoload 'projectile-cleanup-known-projects "projectile" "Remove known projects that don't exist anymore." t nil) (autoload 'projectile-clear-known-projects "projectile" "Clear both `projectile-known-projects' and `projectile-known-projects-file'." t nil) (autoload 'projectile-reset-known-projects "projectile" "Clear known projects and rediscover." t nil) (autoload 'projectile-remove-known-project "projectile" "Remove PROJECT from the list of known projects.

(fn &optional PROJECT)" t nil) (autoload 'projectile-remove-current-project-from-known-projects "projectile" "Remove the current project from the list of known projects." t nil) (autoload 'projectile-add-known-project "projectile" "Add PROJECT-ROOT to the list of known projects.

(fn PROJECT-ROOT)" t nil) (autoload 'projectile-ibuffer "projectile" "Open an IBuffer window showing all buffers in the current project.

Let user choose another project when PROMPT-FOR-PROJECT is supplied.

(fn PROMPT-FOR-PROJECT)" t nil) (autoload 'projectile-commander "projectile" "Execute a Projectile command with a single letter.
The user is prompted for a single character indicating the action to invoke.
The `?' character describes then
available actions.

See `def-projectile-commander-method' for defining new methods." t nil) (autoload 'projectile-browse-dirty-projects "projectile" "Browse dirty version controlled projects.

With a prefix argument, or if CACHED is non-nil, try to use the cached
dirty project list.

(fn &optional CACHED)" t nil) (autoload 'projectile-edit-dir-locals "projectile" "Edit or create a .dir-locals.el file of the project." t nil) (defvar projectile-mode nil "Non-nil if Projectile mode is enabled.
See the `projectile-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `projectile-mode'.") (custom-autoload 'projectile-mode "projectile" nil) (autoload 'projectile-mode "projectile" "Minor mode to assist project management and navigation.

When called interactively, toggle `projectile-mode'.  With prefix
ARG, enable `projectile-mode' if ARG is positive, otherwise disable
it.

When called from Lisp, enable `projectile-mode' if ARG is omitted,
nil or positive.  If ARG is `toggle', toggle `projectile-mode'.
Otherwise behave as if called interactively.

\\{projectile-mode-map}

(fn &optional ARG)" t nil) (define-obsolete-function-alias 'projectile-global-mode 'projectile-mode "1.0") (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "projectile" '("??" "compilation-find-file-projectile-find-compilation-buffer" "def-projectile-commander-method" "delete-file-projectile-remove-from-cache" "projectile-"))) (provide 'projectile-autoloads)) "all-the-icons" ((all-the-icons-faces all-the-icons-autoloads all-the-icons) (autoload 'all-the-icons-icon-for-dir "all-the-icons" "Get the formatted icon for DIR.
ARG-OVERRIDES should be a plist containining `:height',
`:v-adjust' or `:face' properties like in the normal icon
inserting functions.

Note: You want chevron, please use `all-the-icons-icon-for-dir-with-chevron'.

(fn DIR &rest ARG-OVERRIDES)" nil nil) (autoload 'all-the-icons-icon-for-file "all-the-icons" "Get the formatted icon for FILE.
ARG-OVERRIDES should be a plist containining `:height',
`:v-adjust' or `:face' properties like in the normal icon
inserting functions.

(fn FILE &rest ARG-OVERRIDES)" nil nil) (autoload 'all-the-icons-icon-for-mode "all-the-icons" "Get the formatted icon for MODE.
ARG-OVERRIDES should be a plist containining `:height',
`:v-adjust' or `:face' properties like in the normal icon
inserting functions.

(fn MODE &rest ARG-OVERRIDES)" nil nil) (autoload 'all-the-icons-icon-for-url "all-the-icons" "Get the formatted icon for URL.
If an icon for URL isn't found in `all-the-icons-url-alist', a globe is used.
ARG-OVERRIDES should be a plist containining `:height',
`:v-adjust' or `:face' properties like in the normal icon
inserting functions.

(fn URL &rest ARG-OVERRIDES)" nil nil) (autoload 'all-the-icons-install-fonts "all-the-icons" "Helper function to download and install the latests fonts based on OS.
When PFX is non-nil, ignore the prompt and just install

(fn &optional PFX)" t nil) (autoload 'all-the-icons-insert "all-the-icons" "Interactive icon insertion function.
When Prefix ARG is non-nil, insert the propertized icon.
When FAMILY is non-nil, limit the candidates to the icon set matching it.

(fn &optional ARG FAMILY)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "all-the-icons" '("all-the-icons-"))) (provide 'all-the-icons-autoloads)) "doom-themes" ((doom-tomorrow-day-theme doom-laserwave-theme doom-monokai-pro-theme doom-themes doom-vibrant-theme doom-molokai-theme doom-gruvbox-theme doom-opera-light-theme doom-solarized-light-theme doom-moonlight-theme doom-1337-theme doom-themes-ext-org doom-solarized-dark-high-contrast-theme doom-one-light-theme doom-badger-theme doom-monokai-machine-theme doom-dracula-theme doom-sourcerer-theme doom-henna-theme doom-monokai-classic-theme doom-tomorrow-night-theme doom-acario-dark-theme doom-material-theme doom-outrun-electric-theme doom-peacock-theme doom-manegarm-theme doom-plain-dark-theme doom-themes-autoloads doom-ayu-light-theme doom-gruvbox-light-theme doom-fairy-floss-theme doom-city-lights-theme doom-monokai-spectrum-theme doom-ir-black-theme doom-xcode-theme doom-oceanic-next-theme doom-palenight-theme doom-spacegrey-theme doom-wilmersdorf-theme doom-horizon-theme doom-monokai-octagon-theme doom-themes-ext-treemacs doom-themes-base doom-rouge-theme doom-nord-theme doom-challenger-deep-theme doom-plain-theme doom-flatwhite-theme doom-homage-white-theme doom-Iosvkem-theme doom-themes-ext-visual-bell doom-monokai-ristretto-theme doom-themes-ext-neotree doom-one-theme doom-nova-theme doom-homage-black-theme doom-zenburn-theme doom-dark+-theme doom-acario-light-theme doom-ephemeral-theme doom-miramare-theme doom-shades-of-purple-theme doom-opera-theme doom-nord-light-theme doom-snazzy-theme doom-old-hope-theme doom-ayu-mirage-theme doom-solarized-dark-theme) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-1337-theme" '("doom-1337"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-Iosvkem-theme" '("doom-Iosvkem"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-acario-dark-theme" '("doom-acario-dark"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-acario-light-theme" '("doom-acario-light"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-ayu-light-theme" '("doom-ayu-light"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-ayu-mirage-theme" '("doom-ayu-mirage"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-badger-theme" '("doom-badger"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-challenger-deep-theme" '("doom-challenger-deep"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-city-lights-theme" '("doom-city-lights"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-dark+-theme" '("doom-dark+"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-dracula-theme" '("doom-dracula"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-ephemeral-theme" '("doom-ephemeral"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-fairy-floss-theme" '("doom-fairy-floss"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-flatwhite-theme" '("doom-f"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-gruvbox-light-theme" '("doom-gruvbox-light"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-gruvbox-theme" '("doom-gruvbox"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-henna-theme" '("doom-henna"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-homage-black-theme" '("doom-homage-black"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-homage-white-theme" '("doom-homage-white"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-horizon-theme" '("doom-horizon"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-ir-black-theme" '("doom-ir-black"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-laserwave-theme" '("doom-laserwave"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-manegarm-theme" '("doom-manegarm"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-material-theme" '("doom-material"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-miramare-theme" '("doom-miramare"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-molokai-theme" '("doom-molokai"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-monokai-classic-theme" '("doom-monokai-classic"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-monokai-machine-theme" '("doom-monokai-machine"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-monokai-octagon-theme" '("doom-monokai-octagon"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-monokai-pro-theme" '("doom-monokai-pro"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-monokai-ristretto-theme" '("doom-monokai-ristretto"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-monokai-spectrum-theme" '("doom-monokai-spectrum"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-moonlight-theme" '("doom-moonlight"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-nord-light-theme" '("doom-nord-light"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-nord-theme" '("doom-nord"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-nova-theme" '("doom-nova"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-oceanic-next-theme" '("doom-oceanic-next"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-old-hope-theme" '("doom-old-hope"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-one-light-theme" '("doom-one-light"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-one-theme" '("doom-one"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-opera-light-theme" '("doom-opera-light"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-opera-theme" '("doom-opera"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-outrun-electric-theme" '("doom-outrun-electric"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-palenight-theme" '("doom-palenight"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-peacock-theme" '("doom-peacock"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-plain-dark-theme" '("doom-plain-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-plain-theme" '("doom-plain"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-rouge-theme" '("doom-rouge"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-shades-of-purple-theme" '("doom-shades-of-purple"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-snazzy-theme" '("doom-snazzy"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-solarized-dark-high-contrast-theme" '("doom-solarized-dark-high-contrast"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-solarized-dark-theme" '("doom-solarized-dark"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-solarized-light-theme" '("doom-solarized-light"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-sourcerer-theme" '("doom-sourcerer"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-spacegrey-theme" '("doom-spacegrey"))) (autoload 'doom-name-to-rgb "doom-themes" "Retrieves the hexidecimal string repesented the named COLOR (e.g. \"red\")
for FRAME (defaults to the current frame).

(fn COLOR)" nil nil) (autoload 'doom-blend "doom-themes" "Blend two colors (hexidecimal strings) together by a coefficient ALPHA (a
float between 0 and 1)

(fn COLOR1 COLOR2 ALPHA)" nil nil) (autoload 'doom-darken "doom-themes" "Darken a COLOR (a hexidecimal string) by a coefficient ALPHA (a float between
0 and 1).

(fn COLOR ALPHA)" nil nil) (autoload 'doom-lighten "doom-themes" "Brighten a COLOR (a hexidecimal string) by a coefficient ALPHA (a float
between 0 and 1).

(fn COLOR ALPHA)" nil nil) (autoload 'doom-color "doom-themes" "Retrieve a specific color named NAME (a symbol) from the current theme.

(fn NAME &optional TYPE)" nil nil) (autoload 'doom-ref "doom-themes" "TODO

(fn FACE PROP &optional CLASS)" nil nil) (autoload 'doom-themes-set-faces "doom-themes" "Customize THEME (a symbol) with FACES.

If THEME is nil, it applies to all themes you load. FACES is a list of Doom
theme face specs. These is a simplified spec. For example:

  (doom-themes-set-faces 'user
    '(default :background red :foreground blue)
    '(doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
    '(doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
    '(doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
    '(doom-modeline-buffer-project-root :foreground green :weight 'bold))

(fn THEME &rest FACES)" nil nil) (function-put 'doom-themes-set-faces 'lisp-indent-function 'defun) (when (and (boundp 'custom-theme-load-path) load-file-name) (let* ((base (file-name-directory load-file-name)) (dir (expand-file-name "themes/" base))) (add-to-list 'custom-theme-load-path (or (and (file-directory-p dir) dir) base)))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-themes" '("def-doom-theme" "doom-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-themes-base" '("doom-themes-base-"))) (autoload 'doom-themes-neotree-config "doom-themes-ext-neotree" "Install doom-themes' neotree configuration.

Includes an Atom-esque icon theme and highlighting based on filetype." nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-themes-ext-neotree" '("doom-"))) (autoload 'doom-themes-org-config "doom-themes-ext-org" "Load `doom-themes-ext-org'." nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-themes-ext-org" '("doom-themes-"))) (autoload 'doom-themes-treemacs-config "doom-themes-ext-treemacs" "Install doom-themes' treemacs configuration.

Includes an Atom-esque icon theme and highlighting based on filetype." nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-themes-ext-treemacs" '("doom-themes-"))) (autoload 'doom-themes-visual-bell-fn "doom-themes-ext-visual-bell" "Blink the mode-line red briefly. Set `ring-bell-function' to this to use it." nil nil) (autoload 'doom-themes-visual-bell-config "doom-themes-ext-visual-bell" "Enable flashing the mode-line on error." nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-tomorrow-day-theme" '("doom-tomorrow-day"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-tomorrow-night-theme" '("doom-tomorrow-night"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-vibrant-theme" '("doom-vibrant"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-wilmersdorf-theme" '("doom-wilmersdorf"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-xcode-theme" '("doom-xcode"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-zenburn-theme" '("doom-zenburn"))) (provide 'doom-themes-autoloads)) "cycle-themes" ((cycle-themes-autoloads cycle-themes) (defvar cycle-themes-mode nil "Non-nil if Cycle-Themes mode is enabled.
See the `cycle-themes-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `cycle-themes-mode'.") (custom-autoload 'cycle-themes-mode "cycle-themes" nil) (autoload 'cycle-themes-mode "cycle-themes" "Minor mode for cycling between themes.

If called interactively, enable Cycle-Themes mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "cycle-themes" '("cycle-themes"))) (provide 'cycle-themes-autoloads)) "smooth-scrolling" ((smooth-scrolling-autoloads smooth-scrolling) (defvar smooth-scrolling-mode nil "Non-nil if Smooth-Scrolling mode is enabled.
See the `smooth-scrolling-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `smooth-scrolling-mode'.") (custom-autoload 'smooth-scrolling-mode "smooth-scrolling" nil) (autoload 'smooth-scrolling-mode "smooth-scrolling" "Make emacs scroll smoothly

If called interactively, enable Smooth-Scrolling mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (defvar smooth-scroll-margin 10 "Number of lines of visible margin at the top and bottom of a window.
If the point is within these margins, then scrolling will occur
smoothly for `previous-line' at the top of the window, and for
`next-line' at the bottom.

This is very similar in its goal to `scroll-margin'.  However, it
is implemented by activating `smooth-scroll-down' and
`smooth-scroll-up' advise via `defadvice' for `previous-line' and
`next-line' respectively.  As a result it avoids problems
afflicting `scroll-margin', such as a sudden jump and unexpected
highlighting of a region when the mouse is clicked in the margin.

Scrolling only occurs when the point is closer to the window
boundary it is heading for (top or bottom) than the middle of the
window.  This is to intelligently handle the case where the
margins cover the whole buffer (e.g. `smooth-scroll-margin' set
to 5 and `window-height' returning 10 or less).

See also `smooth-scroll-strict-margins'.") (custom-autoload 'smooth-scroll-margin "smooth-scrolling" t) (defvar smooth-scroll-strict-margins t "If true, the advice code supporting `smooth-scroll-margin'
will use `count-screen-lines' to determine the number of
*visible* lines between the point and the window top/bottom,
rather than `count-lines' which obtains the number of actual
newlines.  This is because there might be extra newlines hidden
by a mode such as folding-mode, outline-mode, org-mode etc., or
fewer due to very long lines being displayed wrapped when
`truncate-lines' is nil.

However, using `count-screen-lines' can supposedly cause
performance issues in buffers with extremely long lines.  Setting
`cache-long-line-scans' may be able to address this;
alternatively you can set this variable to nil so that the advice
code uses `count-lines', and put up with the fact that sometimes
the point will be allowed to stray into the margin.") (custom-autoload 'smooth-scroll-strict-margins "smooth-scrolling" t) (autoload 'enable-smooth-scroll-for-function "smooth-scrolling" "Define advice on FUNC to do smooth scrolling.

This adds after advice with name `smooth-scroll' to FUNC.

Note that the advice will not have an effect unless
`smooth-scrolling-mode' is enabled.

(fn FUNC)" nil t) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smooth-scrolling" '("disable-smooth-scroll-for-function" "do-smooth-scroll" "enable-smooth-scroll-for-function-conditionally" "smooth-scroll-" "window-is-at-bob-p"))) (provide 'smooth-scrolling-autoloads)) "dash" ((dash-autoloads dash) (autoload 'dash-fontify-mode "dash" "Toggle fontification of Dash special variables.

If called interactively, enable Dash-Fontify mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

Dash-Fontify mode is a buffer-local minor mode intended for Emacs
Lisp buffers.  Enabling it causes the special variables bound in
anaphoric Dash macros to be fontified.  These anaphoras include
`it', `it-index', `acc', and `other'.  In older Emacs versions
which do not dynamically detect macros, Dash-Fontify mode
additionally fontifies Dash macro calls.

See also `dash-fontify-mode-lighter' and
`global-dash-fontify-mode'.

(fn &optional ARG)" t nil) (put 'global-dash-fontify-mode 'globalized-minor-mode t) (defvar global-dash-fontify-mode nil "Non-nil if Global Dash-Fontify mode is enabled.
See the `global-dash-fontify-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-dash-fontify-mode'.") (custom-autoload 'global-dash-fontify-mode "dash" nil) (autoload 'global-dash-fontify-mode "dash" "Toggle Dash-Fontify mode in all buffers.
With prefix ARG, enable Global Dash-Fontify mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Dash-Fontify mode is enabled in all buffers where
`dash--turn-on-fontify-mode' would do it.
See `dash-fontify-mode' for more information on Dash-Fontify mode.

(fn &optional ARG)" t nil) (autoload 'dash-register-info-lookup "dash" "Register the Dash Info manual with `info-lookup-symbol'.
This allows Dash symbols to be looked up with \\[info-lookup-symbol]." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dash" '("!cdr" "!cons" "--" "->" "-a" "-butlast" "-c" "-d" "-e" "-f" "-gr" "-i" "-juxt" "-keep" "-l" "-m" "-no" "-o" "-p" "-r" "-s" "-t" "-u" "-value-to-list" "-when-let" "-zip" "dash-"))) (provide 'dash-autoloads)) "transient" ((transient transient-autoloads) (autoload 'transient-insert-suffix "transient" "Insert a SUFFIX into PREFIX before LOC.
PREFIX is a prefix command, a symbol.
SUFFIX is a suffix command or a group specification (of
  the same forms as expected by `transient-define-prefix').
LOC is a command, a key vector, a key description (a string
  as returned by `key-description'), or a coordination list
  (whose last element may also be a command or key).
See info node `(transient)Modifying Existing Transients'.

(fn PREFIX LOC SUFFIX)" nil nil) (function-put 'transient-insert-suffix 'lisp-indent-function 'defun) (autoload 'transient-append-suffix "transient" "Insert a SUFFIX into PREFIX after LOC.
PREFIX is a prefix command, a symbol.
SUFFIX is a suffix command or a group specification (of
  the same forms as expected by `transient-define-prefix').
LOC is a command, a key vector, a key description (a string
  as returned by `key-description'), or a coordination list
  (whose last element may also be a command or key).
See info node `(transient)Modifying Existing Transients'.

(fn PREFIX LOC SUFFIX)" nil nil) (function-put 'transient-append-suffix 'lisp-indent-function 'defun) (autoload 'transient-replace-suffix "transient" "Replace the suffix at LOC in PREFIX with SUFFIX.
PREFIX is a prefix command, a symbol.
SUFFIX is a suffix command or a group specification (of
  the same forms as expected by `transient-define-prefix').
LOC is a command, a key vector, a key description (a string
  as returned by `key-description'), or a coordination list
  (whose last element may also be a command or key).
See info node `(transient)Modifying Existing Transients'.

(fn PREFIX LOC SUFFIX)" nil nil) (function-put 'transient-replace-suffix 'lisp-indent-function 'defun) (autoload 'transient-remove-suffix "transient" "Remove the suffix or group at LOC in PREFIX.
PREFIX is a prefix command, a symbol.
LOC is a command, a key vector, a key description (a string
  as returned by `key-description'), or a coordination list
  (whose last element may also be a command or key).
See info node `(transient)Modifying Existing Transients'.

(fn PREFIX LOC)" nil nil) (function-put 'transient-remove-suffix 'lisp-indent-function 'defun) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "transient" '("magit--fit-window-to-buffer" "transient-"))) (provide 'transient-autoloads)) "with-editor" ((with-editor with-editor-autoloads) (autoload 'with-editor-export-editor "with-editor" "Teach subsequent commands to use current Emacs instance as editor.

Set and export the environment variable ENVVAR, by default
\"EDITOR\".  The value is automatically generated to teach
commands to use the current Emacs instance as \"the editor\".

This works in `shell-mode', `term-mode', `eshell-mode' and
`vterm'.

(fn &optional (ENVVAR \"EDITOR\"))" t nil) (autoload 'with-editor-export-git-editor "with-editor" "Like `with-editor-export-editor' but always set `$GIT_EDITOR'." t nil) (autoload 'with-editor-export-hg-editor "with-editor" "Like `with-editor-export-editor' but always set `$HG_EDITOR'." t nil) (defvar shell-command-with-editor-mode nil "Non-nil if Shell-Command-With-Editor mode is enabled.
See the `shell-command-with-editor-mode' command
for a description of this minor mode.") (custom-autoload 'shell-command-with-editor-mode "with-editor" nil) (autoload 'shell-command-with-editor-mode "with-editor" "Teach `shell-command' to use current Emacs instance as editor.

If called interactively, enable Shell-Command-With-Editor mode if
ARG is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

Teach `shell-command', and all commands that ultimately call that
command, to use the current Emacs instance as editor by executing
\"EDITOR=CLIENT COMMAND&\" instead of just \"COMMAND&\".

CLIENT is automatically generated; EDITOR=CLIENT instructs
COMMAND to use to the current Emacs instance as \"the editor\",
assuming no other variable overrides the effect of \"$EDITOR\".
CLIENT may be the path to an appropriate emacsclient executable
with arguments, or a script which also works over Tramp.

Alternatively you can use the `with-editor-async-shell-command',
which also allows the use of another variable instead of
\"EDITOR\".

(fn &optional ARG)" t nil) (autoload 'with-editor-async-shell-command "with-editor" "Like `async-shell-command' but with `$EDITOR' set.

Execute string \"ENVVAR=CLIENT COMMAND\" in an inferior shell;
display output, if any.  With a prefix argument prompt for an
environment variable, otherwise the default \"EDITOR\" variable
is used.  With a negative prefix argument additionally insert
the COMMAND's output at point.

CLIENT is automatically generated; ENVVAR=CLIENT instructs
COMMAND to use to the current Emacs instance as \"the editor\",
assuming it respects ENVVAR as an \"EDITOR\"-like variable.
CLIENT may be the path to an appropriate emacsclient executable
with arguments, or a script which also works over Tramp.

Also see `async-shell-command' and `shell-command'.

(fn COMMAND &optional OUTPUT-BUFFER ERROR-BUFFER ENVVAR)" t nil) (autoload 'with-editor-shell-command "with-editor" "Like `shell-command' or `with-editor-async-shell-command'.
If COMMAND ends with \"&\" behave like the latter,
else like the former.

(fn COMMAND &optional OUTPUT-BUFFER ERROR-BUFFER ENVVAR)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "with-editor" '("server-" "shell-command--shell-command-with-editor-mode" "start-file-process--with-editor-process-filter" "with-editor"))) (provide 'with-editor-autoloads)) "git-commit" ((git-commit git-commit-autoloads) (put 'git-commit-major-mode 'safe-local-variable (lambda (val) (memq val '(text-mode markdown-mode org-mode fundamental-mode git-commit-elisp-text-mode)))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "git-commit" '("git-commit-" "global-git-commit-mode"))) (provide 'git-commit-autoloads)) "magit-section" ((magit-section-autoloads magit-section) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-section" '("isearch-clean-overlays@magit-mode" "magit-"))) (provide 'magit-section-autoloads)) "magit" ((magit-sequence magit-libgit-pkg magit-bookmark magit-branch magit magit-imenu magit-ediff magit-apply magit-notes magit-transient magit-wip magit-process magit-subtree magit-files magit-fetch magit-reset magit-remote magit-stash git-rebase magit-gitignore magit-section-pkg magit-extras magit-log magit-utils magit-repos magit-pull magit-reflog magit-commit magit-mode magit-bundle magit-pkg magit-tag magit-bisect magit-merge magit-submodule magit-core magit-margin magit-worktree magit-status magit-git magit-autorevert magit-obsolete magit-push magit-patch magit-refs magit-clone magit-autoloads magit-blame magit-diff) (autoload 'git-rebase-current-line "git-rebase" "Parse current line into a `git-rebase-action' instance.
If the current line isn't recognized as a rebase line, an
instance with all nil values is returned." nil nil) (autoload 'git-rebase-mode "git-rebase" "Major mode for editing of a Git rebase file.

Rebase files are generated when you run 'git rebase -i' or run
`magit-interactive-rebase'.  They describe how Git should perform
the rebase.  See the documentation for git-rebase (e.g., by
running 'man git-rebase' at the command line) for details.

(fn)" t nil) (defconst git-rebase-filename-regexp "/git-rebase-todo\\'") (add-to-list 'auto-mode-alist (cons git-rebase-filename-regexp 'git-rebase-mode)) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "git-rebase" '("git-rebase-"))) (define-obsolete-variable-alias 'global-magit-file-mode 'magit-define-global-key-bindings "Magit 3.0.0") (defvar magit-define-global-key-bindings t "Whether to bind some Magit commands in the global keymap.

If this variable is non-nil, then the following bindings may
be added to the global keymap.  The default is t.

key             binding
---             -------
C-x g           magit-status
C-x M-g         magit-dispatch
C-c M-g         magit-file-dispatch

These bindings may be added when `after-init-hook' is run.
Each binding is added if and only if at that time no other key
is bound to the same command and no other command is bound to
the same key.  In other words we try to avoid adding bindings
that are unnecessary, as well as bindings that conflict with
other bindings.

Adding the above bindings is delayed until `after-init-hook'
is called to allow users to set the variable anywhere in their
init file (without having to make sure to do so before `magit'
is loaded or autoloaded) and to increase the likelihood that
all the potentially conflicting user bindings have already
been added.

To set this variable use either `setq' or the Custom interface.
Do not use the function `customize-set-variable' because doing
that would cause Magit to be loaded immediately when that form
is evaluated (this differs from `custom-set-variables', which
doesn't load the libraries that define the customized variables).

Setting this variable to nil has no effect if that is done after
the key bindings have already been added.

We recommend that you bind \"C-c g\" instead of \"C-c M-g\" to
`magit-file-dispatch'.  The former is a much better binding
but the \"C-c <letter>\" namespace is strictly reserved for
users; preventing Magit from using it by default.

Also see info node `(magit)Commands for Buffers Visiting Files'.") (custom-autoload 'magit-define-global-key-bindings "magit" t) (defun magit-maybe-define-global-key-bindings nil (when magit-define-global-key-bindings (let ((map (current-global-map))) (dolist (elt '(("C-x g" . magit-status) ("C-x M-g" . magit-dispatch) ("C-c M-g" . magit-file-dispatch))) (let ((key (kbd (car elt))) (def (cdr elt))) (unless (or (lookup-key map key) (where-is-internal def (make-sparse-keymap) t)) (define-key map key def))))))) (if after-init-time (magit-maybe-define-global-key-bindings) (add-hook 'after-init-hook 'magit-maybe-define-global-key-bindings t)) (autoload 'magit-dispatch "magit" nil t) (autoload 'magit-run "magit" nil t) (autoload 'magit-git-command "magit" "Execute COMMAND asynchronously; display output.

Interactively, prompt for COMMAND in the minibuffer. \"git \" is
used as initial input, but can be deleted to run another command.

With a prefix argument COMMAND is run in the top-level directory
of the current working tree, otherwise in `default-directory'.

(fn COMMAND)" t nil) (autoload 'magit-git-command-topdir "magit" "Execute COMMAND asynchronously; display output.

Interactively, prompt for COMMAND in the minibuffer. \"git \" is
used as initial input, but can be deleted to run another command.

COMMAND is run in the top-level directory of the current
working tree.

(fn COMMAND)" t nil) (autoload 'magit-shell-command "magit" "Execute COMMAND asynchronously; display output.

Interactively, prompt for COMMAND in the minibuffer.  With a
prefix argument COMMAND is run in the top-level directory of
the current working tree, otherwise in `default-directory'.

(fn COMMAND)" t nil) (autoload 'magit-shell-command-topdir "magit" "Execute COMMAND asynchronously; display output.

Interactively, prompt for COMMAND in the minibuffer.  COMMAND
is run in the top-level directory of the current working tree.

(fn COMMAND)" t nil) (autoload 'magit-version "magit" "Return the version of Magit currently in use.
If optional argument PRINT-DEST is non-nil, output
stream (interactively, the echo area, or the current buffer with
a prefix argument), also print the used versions of Magit, Git,
and Emacs to it.

(fn &optional PRINT-DEST)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit" '("magit-"))) (autoload 'magit-stage-file "magit-apply" "Stage all changes to FILE.
With a prefix argument or when there is no file at point ask for
the file to be staged.  Otherwise stage the file at point without
requiring confirmation.

(fn FILE)" t nil) (autoload 'magit-stage-modified "magit-apply" "Stage all changes to files modified in the worktree.
Stage all new content of tracked files and remove tracked files
that no longer exist in the working tree from the index also.
With a prefix argument also stage previously untracked (but not
ignored) files.

(fn &optional ALL)" t nil) (autoload 'magit-unstage-file "magit-apply" "Unstage all changes to FILE.
With a prefix argument or when there is no file at point ask for
the file to be unstaged.  Otherwise unstage the file at point
without requiring confirmation.

(fn FILE)" t nil) (autoload 'magit-unstage-all "magit-apply" "Remove all changes from the staging area." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-apply" '("magit-"))) (put 'magit-auto-revert-mode 'globalized-minor-mode t) (defvar magit-auto-revert-mode (not (or global-auto-revert-mode noninteractive)) "Non-nil if Magit-Auto-Revert mode is enabled.
See the `magit-auto-revert-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `magit-auto-revert-mode'.") (custom-autoload 'magit-auto-revert-mode "magit-autorevert" nil) (autoload 'magit-auto-revert-mode "magit-autorevert" "Toggle Auto-Revert mode in all buffers.
With prefix ARG, enable Magit-Auto-Revert mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Auto-Revert mode is enabled in all buffers where
`magit-turn-on-auto-revert-mode-if-desired' would do it.
See `auto-revert-mode' for more information on Auto-Revert mode.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-autorevert" '("auto-revert-buffer" "magit-"))) (autoload 'magit-bisect "magit-bisect" nil t) (autoload 'magit-bisect-start "magit-bisect" "Start a bisect session.

Bisecting a bug means to find the commit that introduced it.
This command starts such a bisect session by asking for a known
good and a known bad commit.  To move the session forward use the
other actions from the bisect transient command (\\<magit-status-mode-map>\\[magit-bisect]).

(fn BAD GOOD ARGS)" t nil) (autoload 'magit-bisect-reset "magit-bisect" "After bisecting, cleanup bisection state and return to original `HEAD'." t nil) (autoload 'magit-bisect-good "magit-bisect" "While bisecting, mark the current commit as good.
Use this after you have asserted that the commit does not contain
the bug in question." t nil) (autoload 'magit-bisect-bad "magit-bisect" "While bisecting, mark the current commit as bad.
Use this after you have asserted that the commit does contain the
bug in question." t nil) (autoload 'magit-bisect-mark "magit-bisect" "While bisecting, mark the current commit with a bisect term.
During a bisect using alternate terms, commits can still be
marked with `magit-bisect-good' and `magit-bisect-bad', as those
commands map to the correct term (\"good\" to --term-old's value
and \"bad\" to --term-new's).  However, in some cases, it can be
difficult to keep that mapping straight in your head; this
command provides an interface that exposes the underlying terms." t nil) (autoload 'magit-bisect-skip "magit-bisect" "While bisecting, skip the current commit.
Use this if for some reason the current commit is not a good one
to test.  This command lets Git choose a different one." t nil) (autoload 'magit-bisect-run "magit-bisect" "Bisect automatically by running commands after each step.

Unlike `git bisect run' this can be used before bisecting has
begun.  In that case it behaves like `git bisect start; git
bisect run'.

(fn CMDLINE &optional BAD GOOD ARGS)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-bisect" '("magit-"))) (autoload 'magit-blame-echo "magit-blame" nil t) (autoload 'magit-blame-addition "magit-blame" nil t) (autoload 'magit-blame-removal "magit-blame" nil t) (autoload 'magit-blame-reverse "magit-blame" nil t) (autoload 'magit-blame "magit-blame" nil t) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-blame" '("magit-"))) (autoload 'magit--handle-bookmark "magit-bookmark" "Open a bookmark created by `magit--make-bookmark'.
Call the `magit-*-setup-buffer' function of the the major-mode
with the variables' values as arguments, which were recorded by
`magit--make-bookmark'.  Ignore `magit-display-buffer-function'.

(fn BOOKMARK)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-bookmark" '("magit--make-bookmark"))) (autoload 'magit-branch "magit" nil t) (autoload 'magit-checkout "magit-branch" "Checkout REVISION, updating the index and the working tree.
If REVISION is a local branch, then that becomes the current
branch.  If it is something else, then `HEAD' becomes detached.
Checkout fails if the working tree or the staging area contain
changes.

(git checkout REVISION).

(fn REVISION &optional ARGS)" t nil) (autoload 'magit-branch-create "magit-branch" "Create BRANCH at branch or revision START-POINT.

(fn BRANCH START-POINT)" t nil) (autoload 'magit-branch-and-checkout "magit-branch" "Create and checkout BRANCH at branch or revision START-POINT.

(fn BRANCH START-POINT &optional ARGS)" t nil) (autoload 'magit-branch-or-checkout "magit-branch" "Hybrid between `magit-checkout' and `magit-branch-and-checkout'.

Ask the user for an existing branch or revision.  If the user
input actually can be resolved as a branch or revision, then
check that out, just like `magit-checkout' would.

Otherwise create and checkout a new branch using the input as
its name.  Before doing so read the starting-point for the new
branch.  This is similar to what `magit-branch-and-checkout'
does.

(fn ARG &optional START-POINT)" t nil) (autoload 'magit-branch-checkout "magit-branch" "Checkout an existing or new local branch.

Read a branch name from the user offering all local branches and
a subset of remote branches as candidates.  Omit remote branches
for which a local branch by the same name exists from the list
of candidates.  The user can also enter a completely new branch
name.

- If the user selects an existing local branch, then check that
  out.

- If the user selects a remote branch, then create and checkout
  a new local branch with the same name.  Configure the selected
  remote branch as push target.

- If the user enters a new branch name, then create and check
  that out, after also reading the starting-point from the user.

In the latter two cases the upstream is also set.  Whether it is
set to the chosen START-POINT or something else depends on the
value of `magit-branch-adjust-remote-upstream-alist', just like
when using `magit-branch-and-checkout'.

(fn BRANCH &optional START-POINT)" t nil) (autoload 'magit-branch-orphan "magit-branch" "Create and checkout an orphan BRANCH with contents from revision START-POINT.

(fn BRANCH START-POINT)" t nil) (autoload 'magit-branch-spinout "magit-branch" "Create new branch from the unpushed commits.
Like `magit-branch-spinoff' but remain on the current branch.
If there are any uncommitted changes, then behave exactly like
`magit-branch-spinoff'.

(fn BRANCH &optional FROM)" t nil) (autoload 'magit-branch-spinoff "magit-branch" "Create new branch from the unpushed commits.

Create and checkout a new branch starting at and tracking the
current branch.  That branch in turn is reset to the last commit
it shares with its upstream.  If the current branch has no
upstream or no unpushed commits, then the new branch is created
anyway and the previously current branch is not touched.

This is useful to create a feature branch after work has already
began on the old branch (likely but not necessarily \"master\").

If the current branch is a member of the value of option
`magit-branch-prefer-remote-upstream' (which see), then the
current branch will be used as the starting point as usual, but
the upstream of the starting-point may be used as the upstream
of the new branch, instead of the starting-point itself.

If optional FROM is non-nil, then the source branch is reset
to `FROM~', instead of to the last commit it shares with its
upstream.  Interactively, FROM is only ever non-nil, if the
region selects some commits, and among those commits, FROM is
the commit that is the fewest commits ahead of the source
branch.

The commit at the other end of the selection actually does not
matter, all commits between FROM and `HEAD' are moved to the new
branch.  If FROM is not reachable from `HEAD' or is reachable
from the source branch's upstream, then an error is raised.

(fn BRANCH &optional FROM)" t nil) (autoload 'magit-branch-reset "magit-branch" "Reset a branch to the tip of another branch or any other commit.

When the branch being reset is the current branch, then do a
hard reset.  If there are any uncommitted changes, then the user
has to confirm the reset because those changes would be lost.

This is useful when you have started work on a feature branch but
realize it's all crap and want to start over.

When resetting to another branch and a prefix argument is used,
then also set the target branch as the upstream of the branch
that is being reset.

(fn BRANCH TO &optional SET-UPSTREAM)" t nil) (autoload 'magit-branch-delete "magit-branch" "Delete one or multiple branches.
If the region marks multiple branches, then offer to delete
those, otherwise prompt for a single branch to be deleted,
defaulting to the branch at point.

(fn BRANCHES &optional FORCE)" t nil) (autoload 'magit-branch-rename "magit-branch" "Rename the branch named OLD to NEW.

With a prefix argument FORCE, rename even if a branch named NEW
already exists.

If `branch.OLD.pushRemote' is set, then unset it.  Depending on
the value of `magit-branch-rename-push-target' (which see) maybe
set `branch.NEW.pushRemote' and maybe rename the push-target on
the remote.

(fn OLD NEW &optional FORCE)" t nil) (autoload 'magit-branch-shelve "magit-branch" "Shelve a BRANCH.
Rename \"refs/heads/BRANCH\" to \"refs/shelved/BRANCH\",
and also rename the respective reflog file.

(fn BRANCH)" t nil) (autoload 'magit-branch-unshelve "magit-branch" "Unshelve a BRANCH
Rename \"refs/shelved/BRANCH\" to \"refs/heads/BRANCH\",
and also rename the respective reflog file.

(fn BRANCH)" t nil) (autoload 'magit-branch-configure "magit-branch" nil t) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-branch" '("magit-"))) (autoload 'magit-bundle "magit-bundle" nil t) (autoload 'magit-bundle-import "magit-bundle" nil t) (autoload 'magit-bundle-create-tracked "magit-bundle" "Create and track a new bundle.

(fn FILE TAG BRANCH REFS ARGS)" t nil) (autoload 'magit-bundle-update-tracked "magit-bundle" "Update a bundle that is being tracked using TAG.

(fn TAG)" t nil) (autoload 'magit-bundle-verify "magit-bundle" "Check whether FILE is valid and applies to the current repository.

(fn FILE)" t nil) (autoload 'magit-bundle-list-heads "magit-bundle" "List the refs in FILE.

(fn FILE)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-bundle" '("magit-"))) (autoload 'magit-clone "magit-clone" nil t) (autoload 'magit-clone-regular "magit-clone" "Create a clone of REPOSITORY in DIRECTORY.
Then show the status buffer for the new repository.

(fn REPOSITORY DIRECTORY ARGS)" t nil) (autoload 'magit-clone-shallow "magit-clone" "Create a shallow clone of REPOSITORY in DIRECTORY.
Then show the status buffer for the new repository.
With a prefix argument read the DEPTH of the clone;
otherwise use 1.

(fn REPOSITORY DIRECTORY ARGS DEPTH)" t nil) (autoload 'magit-clone-shallow-since "magit-clone" "Create a shallow clone of REPOSITORY in DIRECTORY.
Then show the status buffer for the new repository.
Exclude commits before DATE, which is read from the
user.

(fn REPOSITORY DIRECTORY ARGS DATE)" t nil) (autoload 'magit-clone-shallow-exclude "magit-clone" "Create a shallow clone of REPOSITORY in DIRECTORY.
Then show the status buffer for the new repository.
Exclude commits reachable from EXCLUDE, which is a
branch or tag read from the user.

(fn REPOSITORY DIRECTORY ARGS EXCLUDE)" t nil) (autoload 'magit-clone-bare "magit-clone" "Create a bare clone of REPOSITORY in DIRECTORY.
Then show the status buffer for the new repository.

(fn REPOSITORY DIRECTORY ARGS)" t nil) (autoload 'magit-clone-mirror "magit-clone" "Create a mirror of REPOSITORY in DIRECTORY.
Then show the status buffer for the new repository.

(fn REPOSITORY DIRECTORY ARGS)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-clone" '("magit-clone-"))) (autoload 'magit-commit "magit-commit" nil t) (autoload 'magit-commit-create "magit-commit" "Create a new commit on `HEAD'.
With a prefix argument, amend to the commit at `HEAD' instead.

(git commit [--amend] ARGS)

(fn &optional ARGS)" t nil) (autoload 'magit-commit-amend "magit-commit" "Amend the last commit.

(git commit --amend ARGS)

(fn &optional ARGS)" t nil) (autoload 'magit-commit-extend "magit-commit" "Amend the last commit, without editing the message.

With a prefix argument keep the committer date, otherwise change
it.  The option `magit-commit-extend-override-date' can be used
to inverse the meaning of the prefix argument.  
(git commit
--amend --no-edit)

(fn &optional ARGS OVERRIDE-DATE)" t nil) (autoload 'magit-commit-reword "magit-commit" "Reword the last commit, ignoring staged changes.

With a prefix argument keep the committer date, otherwise change
it.  The option `magit-commit-reword-override-date' can be used
to inverse the meaning of the prefix argument.

Non-interactively respect the optional OVERRIDE-DATE argument
and ignore the option.

(git commit --amend --only)

(fn &optional ARGS OVERRIDE-DATE)" t nil) (autoload 'magit-commit-fixup "magit-commit" "Create a fixup commit.

With a prefix argument the target COMMIT has to be confirmed.
Otherwise the commit at point may be used without confirmation
depending on the value of option `magit-commit-squash-confirm'.

(fn &optional COMMIT ARGS)" t nil) (autoload 'magit-commit-squash "magit-commit" "Create a squash commit, without editing the squash message.

With a prefix argument the target COMMIT has to be confirmed.
Otherwise the commit at point may be used without confirmation
depending on the value of option `magit-commit-squash-confirm'.

If you want to immediately add a message to the squash commit,
then use `magit-commit-augment' instead of this command.

(fn &optional COMMIT ARGS)" t nil) (autoload 'magit-commit-augment "magit-commit" "Create a squash commit, editing the squash message.

With a prefix argument the target COMMIT has to be confirmed.
Otherwise the commit at point may be used without confirmation
depending on the value of option `magit-commit-squash-confirm'.

(fn &optional COMMIT ARGS)" t nil) (autoload 'magit-commit-instant-fixup "magit-commit" "Create a fixup commit targeting COMMIT and instantly rebase.

(fn &optional COMMIT ARGS)" t nil) (autoload 'magit-commit-instant-squash "magit-commit" "Create a squash commit targeting COMMIT and instantly rebase.

(fn &optional COMMIT ARGS)" t nil) (autoload 'magit-commit-reshelve "magit-commit" "Change the committer date and possibly the author date of `HEAD'.

The current time is used as the initial minibuffer input and the
original author or committer date is available as the previous
history element.

Both the author and the committer dates are changes, unless one
of the following is true, in which case only the committer date
is updated:
- You are not the author of the commit that is being reshelved.
- The command was invoked with a prefix argument.
- Non-interactively if UPDATE-AUTHOR is nil.

(fn DATE UPDATE-AUTHOR &optional ARGS)" t nil) (autoload 'magit-commit-absorb-modules "magit-commit" "Spread modified modules across recent commits.

(fn PHASE COMMIT)" t nil) (autoload 'magit-commit-absorb "magit-commit" nil t) (autoload 'magit-commit-autofixup "magit-commit" nil t) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-commit" '("magit-"))) (autoload 'magit-diff "magit-diff" nil t) (autoload 'magit-diff-refresh "magit-diff" nil t) (autoload 'magit-diff-dwim "magit-diff" "Show changes for the thing at point.

(fn &optional ARGS FILES)" t nil) (autoload 'magit-diff-range "magit-diff" "Show differences between two commits.

REV-OR-RANGE should be a range or a single revision.  If it is a
revision, then show changes in the working tree relative to that
revision.  If it is a range, but one side is omitted, then show
changes relative to `HEAD'.

If the region is active, use the revisions on the first and last
line of the region as the two sides of the range.  With a prefix
argument, instead of diffing the revisions, choose a revision to
view changes along, starting at the common ancestor of both
revisions (i.e., use a \"...\" range).

(fn REV-OR-RANGE &optional ARGS FILES)" t nil) (autoload 'magit-diff-working-tree "magit-diff" "Show changes between the current working tree and the `HEAD' commit.
With a prefix argument show changes between the working tree and
a commit read from the minibuffer.

(fn &optional REV ARGS FILES)" t nil) (autoload 'magit-diff-staged "magit-diff" "Show changes between the index and the `HEAD' commit.
With a prefix argument show changes between the index and
a commit read from the minibuffer.

(fn &optional REV ARGS FILES)" t nil) (autoload 'magit-diff-unstaged "magit-diff" "Show changes between the working tree and the index.

(fn &optional ARGS FILES)" t nil) (autoload 'magit-diff-unmerged "magit-diff" "Show changes that are being merged.

(fn &optional ARGS FILES)" t nil) (autoload 'magit-diff-while-committing "magit-diff" "While committing, show the changes that are about to be committed.
While amending, invoking the command again toggles between
showing just the new changes or all the changes that will
be committed.

(fn &optional ARGS)" t nil) (autoload 'magit-diff-buffer-file "magit-diff" "Show diff for the blob or file visited in the current buffer.

When the buffer visits a blob, then show the respective commit.
When the buffer visits a file, then show the differenced between
`HEAD' and the working tree.  In both cases limit the diff to
the file or blob." t nil) (autoload 'magit-diff-paths "magit-diff" "Show changes between any two files on disk.

(fn A B)" t nil) (autoload 'magit-show-commit "magit-diff" "Visit the revision at point in another buffer.
If there is no revision at point or with a prefix argument prompt
for a revision.

(fn REV &optional ARGS FILES MODULE)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-diff" '("magit-"))) (autoload 'magit-ediff "magit-ediff" nil) (autoload 'magit-ediff-resolve "magit-ediff" "Resolve outstanding conflicts in FILE using Ediff.
FILE has to be relative to the top directory of the repository.

In the rare event that you want to manually resolve all
conflicts, including those already resolved by Git, use
`ediff-merge-revisions-with-ancestor'.

(fn FILE)" t nil) (autoload 'magit-ediff-stage "magit-ediff" "Stage and unstage changes to FILE using Ediff.
FILE has to be relative to the top directory of the repository.

(fn FILE)" t nil) (autoload 'magit-ediff-compare "magit-ediff" "Compare REVA:FILEA with REVB:FILEB using Ediff.

FILEA and FILEB have to be relative to the top directory of the
repository.  If REVA or REVB is nil, then this stands for the
working tree state.

If the region is active, use the revisions on the first and last
line of the region.  With a prefix argument, instead of diffing
the revisions, choose a revision to view changes along, starting
at the common ancestor of both revisions (i.e., use a \"...\"
range).

(fn REVA REVB FILEA FILEB)" t nil) (autoload 'magit-ediff-dwim "magit-ediff" "Compare, stage, or resolve using Ediff.
This command tries to guess what file, and what commit or range
the user wants to compare, stage, or resolve using Ediff.  It
might only be able to guess either the file, or range or commit,
in which case the user is asked about the other.  It might not
always guess right, in which case the appropriate `magit-ediff-*'
command has to be used explicitly.  If it cannot read the user's
mind at all, then it asks the user for a command to run." t nil) (autoload 'magit-ediff-show-staged "magit-ediff" "Show staged changes using Ediff.

This only allows looking at the changes; to stage, unstage,
and discard changes using Ediff, use `magit-ediff-stage'.

FILE must be relative to the top directory of the repository.

(fn FILE)" t nil) (autoload 'magit-ediff-show-unstaged "magit-ediff" "Show unstaged changes using Ediff.

This only allows looking at the changes; to stage, unstage,
and discard changes using Ediff, use `magit-ediff-stage'.

FILE must be relative to the top directory of the repository.

(fn FILE)" t nil) (autoload 'magit-ediff-show-working-tree "magit-ediff" "Show changes between `HEAD' and working tree using Ediff.
FILE must be relative to the top directory of the repository.

(fn FILE)" t nil) (autoload 'magit-ediff-show-commit "magit-ediff" "Show changes introduced by COMMIT using Ediff.

(fn COMMIT)" t nil) (autoload 'magit-ediff-show-stash "magit-ediff" "Show changes introduced by STASH using Ediff.
`magit-ediff-show-stash-with-index' controls whether a
three-buffer Ediff is used in order to distinguish changes in the
stash that were staged.

(fn STASH)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-ediff" '("magit-ediff-"))) (autoload 'magit-run-git-gui "magit-extras" "Run `git gui' for the current git repository." t nil) (autoload 'magit-run-git-gui-blame "magit-extras" "Run `git gui blame' on the given FILENAME and COMMIT.
Interactively run it for the current file and the `HEAD', with a
prefix or when the current file cannot be determined let the user
choose.  When the current buffer is visiting FILENAME instruct
blame to center around the line point is on.

(fn COMMIT FILENAME &optional LINENUM)" t nil) (autoload 'magit-run-gitk "magit-extras" "Run `gitk' in the current repository." t nil) (autoload 'magit-run-gitk-branches "magit-extras" "Run `gitk --branches' in the current repository." t nil) (autoload 'magit-run-gitk-all "magit-extras" "Run `gitk --all' in the current repository." t nil) (autoload 'ido-enter-magit-status "magit-extras" "Drop into `magit-status' from file switching.

This command does not work in Emacs 26.1.
See https://github.com/magit/magit/issues/3634
and https://debbugs.gnu.org/cgi/bugreport.cgi?bug=31707.

To make this command available use something like:

  (add-hook \\='ido-setup-hook
            (lambda ()
              (define-key ido-completion-map
                (kbd \"C-x g\") \\='ido-enter-magit-status)))

Starting with Emacs 25.1 the Ido keymaps are defined just once
instead of every time Ido is invoked, so now you can modify it
like pretty much every other keymap:

  (define-key ido-common-completion-map
    (kbd \"C-x g\") \\='ido-enter-magit-status)" t nil) (autoload 'magit-project-status "magit-extras" "Run `magit-status' in the current project's root." t nil) (autoload 'magit-dired-jump "magit-extras" "Visit file at point using Dired.
With a prefix argument, visit in another window.  If there
is no file at point, then instead visit `default-directory'.

(fn &optional OTHER-WINDOW)" t nil) (autoload 'magit-dired-log "magit-extras" "Show log for all marked files, or the current file.

(fn &optional FOLLOW)" t nil) (autoload 'magit-dired-am-apply-patches "magit-extras" "In Dired, apply the marked (or next ARG) files as patches.
If inside a repository, then apply in that.  Otherwise prompt
for a repository.

(fn REPO &optional ARG)" t nil) (autoload 'magit-do-async-shell-command "magit-extras" "Open FILE with `dired-do-async-shell-command'.
Interactively, open the file at point.

(fn FILE)" t nil) (autoload 'magit-previous-line "magit-extras" "Like `previous-line' but with Magit-specific shift-selection.

Magit's selection mechanism is based on the region but selects an
area that is larger than the region.  This causes `previous-line'
when invoked while holding the shift key to move up one line and
thereby select two lines.  When invoked inside a hunk body this
command does not move point on the first invocation and thereby
it only selects a single line.  Which inconsistency you prefer
is a matter of preference.

(fn &optional ARG TRY-VSCROLL)" t nil) (function-put 'magit-previous-line 'interactive-only '"use `forward-line' with negative argument instead.") (autoload 'magit-next-line "magit-extras" "Like `next-line' but with Magit-specific shift-selection.

Magit's selection mechanism is based on the region but selects
an area that is larger than the region.  This causes `next-line'
when invoked while holding the shift key to move down one line
and thereby select two lines.  When invoked inside a hunk body
this command does not move point on the first invocation and
thereby it only selects a single line.  Which inconsistency you
prefer is a matter of preference.

(fn &optional ARG TRY-VSCROLL)" t nil) (function-put 'magit-next-line 'interactive-only 'forward-line) (autoload 'magit-clean "magit-extras" "Remove untracked files from the working tree.
With a prefix argument also remove ignored files,
with two prefix arguments remove ignored files only.

(git clean -f -d [-x|-X])

(fn &optional ARG)" t nil) (autoload 'magit-add-change-log-entry "magit-extras" "Find change log file and add date entry and item for current change.
This differs from `add-change-log-entry' (which see) in that
it acts on the current hunk in a Magit buffer instead of on
a position in a file-visiting buffer.

(fn &optional WHOAMI FILE-NAME OTHER-WINDOW)" t nil) (autoload 'magit-add-change-log-entry-other-window "magit-extras" "Find change log file in other window and add entry and item.
This differs from `add-change-log-entry-other-window' (which see)
in that it acts on the current hunk in a Magit buffer instead of
on a position in a file-visiting buffer.

(fn &optional WHOAMI FILE-NAME)" t nil) (autoload 'magit-edit-line-commit "magit-extras" "Edit the commit that added the current line.

With a prefix argument edit the commit that removes the line,
if any.  The commit is determined using `git blame' and made
editable using `git rebase --interactive' if it is reachable
from `HEAD', or by checking out the commit (or a branch that
points at it) otherwise.

(fn &optional TYPE)" t nil) (autoload 'magit-diff-edit-hunk-commit "magit-extras" "From a hunk, edit the respective commit and visit the file.

First visit the file being modified by the hunk at the correct
location using `magit-diff-visit-file'.  This actually visits a
blob.  When point is on a diff header, not within an individual
hunk, then this visits the blob the first hunk is about.

Then invoke `magit-edit-line-commit', which uses an interactive
rebase to make the commit editable, or if that is not possible
because the commit is not reachable from `HEAD' by checking out
that commit directly.  This also causes the actual worktree file
to be visited.

Neither the blob nor the file buffer are killed when finishing
the rebase.  If that is undesirable, then it might be better to
use `magit-rebase-edit-command' instead of this command.

(fn FILE)" t nil) (autoload 'magit-reshelve-since "magit-extras" "Change the author and committer dates of the commits since REV.

Ask the user for the first reachable commit whose dates should
be changed.  Then read the new date for that commit.  The initial
minibuffer input and the previous history element offer good
values.  The next commit will be created one minute later and so
on.

This command is only intended for interactive use and should only
be used on highly rearranged and unpublished history.

If KEYID is non-nil, then use that to sign all reshelved commits.
Interactively use the value of the \"--gpg-sign\" option in the
list returned by `magit-rebase-arguments'.

(fn REV KEYID)" t nil) (autoload 'magit-pop-revision-stack "magit-extras" "Insert a representation of a revision into the current buffer.

Pop a revision from the `magit-revision-stack' and insert it into
the current buffer according to `magit-pop-revision-stack-format'.
Revisions can be put on the stack using `magit-copy-section-value'
and `magit-copy-buffer-revision'.

If the stack is empty or with a prefix argument, instead read a
revision in the minibuffer.  By using the minibuffer history this
allows selecting an item which was popped earlier or to insert an
arbitrary reference or revision without first pushing it onto the
stack.

When reading the revision from the minibuffer, then it might not
be possible to guess the correct repository.  When this command
is called inside a repository (e.g. while composing a commit
message), then that repository is used.  Otherwise (e.g. while
composing an email) then the repository recorded for the top
element of the stack is used (even though we insert another
revision).  If not called inside a repository and with an empty
stack, or with two prefix arguments, then read the repository in
the minibuffer too.

(fn REV TOPLEVEL)" t nil) (autoload 'magit-copy-section-value "magit-extras" "Save the value of the current section for later use.

Save the section value to the `kill-ring', and, provided that
the current section is a commit, branch, or tag section, push
the (referenced) revision to the `magit-revision-stack' for use
with `magit-pop-revision-stack'.

When `magit-copy-revision-abbreviated' is non-nil, save the
abbreviated revision to the `kill-ring' and the
`magit-revision-stack'.

When the current section is a branch or a tag, and a prefix
argument is used, then save the revision at its tip to the
`kill-ring' instead of the reference name.

When the region is active, then save that to the `kill-ring',
like `kill-ring-save' would, instead of behaving as described
above.  If a prefix argument is used and the region is within
a hunk, then strip the diff marker column and keep only either
the added or removed lines, depending on the sign of the prefix
argument.

(fn ARG)" t nil) (autoload 'magit-copy-buffer-revision "magit-extras" "Save the revision of the current buffer for later use.

Save the revision shown in the current buffer to the `kill-ring'
and push it to the `magit-revision-stack'.

This command is mainly intended for use in `magit-revision-mode'
buffers, the only buffers where it is always unambiguous exactly
which revision should be saved.

Most other Magit buffers usually show more than one revision, in
some way or another, so this command has to select one of them,
and that choice might not always be the one you think would have
been the best pick.

In such buffers it is often more useful to save the value of
the current section instead, using `magit-copy-section-value'.

When the region is active, then save that to the `kill-ring',
like `kill-ring-save' would, instead of behaving as described
above.

When `magit-copy-revision-abbreviated' is non-nil, save the
abbreviated revision to the `kill-ring' and the
`magit-revision-stack'." t nil) (autoload 'magit-display-repository-buffer "magit-extras" "Display a Magit buffer belonging to the current Git repository.
The buffer is displayed using `magit-display-buffer', which see.

(fn BUFFER)" t nil) (autoload 'magit-switch-to-repository-buffer "magit-extras" "Switch to a Magit buffer belonging to the current Git repository.

(fn BUFFER)" t nil) (autoload 'magit-switch-to-repository-buffer-other-window "magit-extras" "Switch to a Magit buffer belonging to the current Git repository.

(fn BUFFER)" t nil) (autoload 'magit-switch-to-repository-buffer-other-frame "magit-extras" "Switch to a Magit buffer belonging to the current Git repository.

(fn BUFFER)" t nil) (autoload 'magit-abort-dwim "magit-extras" "Abort current operation.
Depending on the context, this will abort a merge, a rebase, a
patch application, a cherry-pick, a revert, or a bisect." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-extras" '("magit-"))) (autoload 'magit-fetch "magit-fetch" nil t) (autoload 'magit-fetch-from-pushremote "magit-fetch" nil t) (autoload 'magit-fetch-from-upstream "magit-fetch" nil t) (autoload 'magit-fetch-other "magit-fetch" "Fetch from another repository.

(fn REMOTE ARGS)" t nil) (autoload 'magit-fetch-branch "magit-fetch" "Fetch a BRANCH from a REMOTE.

(fn REMOTE BRANCH ARGS)" t nil) (autoload 'magit-fetch-refspec "magit-fetch" "Fetch a REFSPEC from a REMOTE.

(fn REMOTE REFSPEC ARGS)" t nil) (autoload 'magit-fetch-all "magit-fetch" "Fetch from all remotes.

(fn ARGS)" t nil) (autoload 'magit-fetch-all-prune "magit-fetch" "Fetch from all remotes, and prune.
Prune remote tracking branches for branches that have been
removed on the respective remote." t nil) (autoload 'magit-fetch-all-no-prune "magit-fetch" "Fetch from all remotes." t nil) (autoload 'magit-fetch-modules "magit-fetch" "Fetch all submodules.

Option `magit-fetch-modules-jobs' controls how many submodules
are being fetched in parallel.  Also fetch the super-repository,
because `git-fetch' does not support not doing that.  With a
prefix argument fetch all remotes.

(fn &optional ALL)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-fetch" '("magit-"))) (autoload 'magit-find-file "magit-files" "View FILE from REV.
Switch to a buffer visiting blob REV:FILE, creating one if none
already exists.  If prior to calling this command the current
buffer and/or cursor position is about the same file, then go
to the line and column corresponding to that location.

(fn REV FILE)" t nil) (autoload 'magit-find-file-other-window "magit-files" "View FILE from REV, in another window.
Switch to a buffer visiting blob REV:FILE, creating one if none
already exists.  If prior to calling this command the current
buffer and/or cursor position is about the same file, then go to
the line and column corresponding to that location.

(fn REV FILE)" t nil) (autoload 'magit-find-file-other-frame "magit-files" "View FILE from REV, in another frame.
Switch to a buffer visiting blob REV:FILE, creating one if none
already exists.  If prior to calling this command the current
buffer and/or cursor position is about the same file, then go to
the line and column corresponding to that location.

(fn REV FILE)" t nil) (autoload 'magit-file-dispatch "magit" nil t) (autoload 'magit-blob-visit-file "magit-files" "View the file from the worktree corresponding to the current blob.
When visiting a blob or the version from the index, then go to
the same location in the respective file in the working tree." t nil) (autoload 'magit-file-checkout "magit-files" "Checkout FILE from REV.

(fn REV FILE)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-files" '("magit-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-git" '("magit-"))) (autoload 'magit-gitignore "magit-gitignore" nil t) (autoload 'magit-gitignore-in-topdir "magit-gitignore" "Add the Git ignore RULE to the top-level \".gitignore\" file.
Since this file is tracked, it is shared with other clones of the
repository.  Also stage the file.

(fn RULE)" t nil) (autoload 'magit-gitignore-in-subdir "magit-gitignore" "Add the Git ignore RULE to a \".gitignore\" file in DIRECTORY.
Prompt the user for a directory and add the rule to the
\".gitignore\" file in that directory.  Since such files are
tracked, they are shared with other clones of the repository.
Also stage the file.

(fn RULE DIRECTORY)" t nil) (autoload 'magit-gitignore-in-gitdir "magit-gitignore" "Add the Git ignore RULE to \"$GIT_DIR/info/exclude\".
Rules in that file only affects this clone of the repository.

(fn RULE)" t nil) (autoload 'magit-gitignore-on-system "magit-gitignore" "Add the Git ignore RULE to the file specified by `core.excludesFile'.
Rules that are defined in that file affect all local repositories.

(fn RULE)" t nil) (autoload 'magit-skip-worktree "magit-gitignore" "Call \"git update-index --skip-worktree -- FILE\".

(fn FILE)" t nil) (autoload 'magit-no-skip-worktree "magit-gitignore" "Call \"git update-index --no-skip-worktree -- FILE\".

(fn FILE)" t nil) (autoload 'magit-assume-unchanged "magit-gitignore" "Call \"git update-index --assume-unchanged -- FILE\".

(fn FILE)" t nil) (autoload 'magit-no-assume-unchanged "magit-gitignore" "Call \"git update-index --no-assume-unchanged -- FILE\".

(fn FILE)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-gitignore" '("magit-"))) (autoload 'magit-imenu--log-prev-index-position-function "magit-imenu" "Move point to previous line in current buffer.
This function is used as a value for
`imenu-prev-index-position-function'." nil nil) (autoload 'magit-imenu--log-extract-index-name-function "magit-imenu" "Return imenu name for line at point.
This function is used as a value for
`imenu-extract-index-name-function'.  Point should be at the
beginning of the line." nil nil) (autoload 'magit-imenu--diff-prev-index-position-function "magit-imenu" "Move point to previous file line in current buffer.
This function is used as a value for
`imenu-prev-index-position-function'." nil nil) (autoload 'magit-imenu--diff-extract-index-name-function "magit-imenu" "Return imenu name for line at point.
This function is used as a value for
`imenu-extract-index-name-function'.  Point should be at the
beginning of the line." nil nil) (autoload 'magit-imenu--status-create-index-function "magit-imenu" "Return an alist of all imenu entries in current buffer.
This function is used as a value for
`imenu-create-index-function'." nil nil) (autoload 'magit-imenu--refs-create-index-function "magit-imenu" "Return an alist of all imenu entries in current buffer.
This function is used as a value for
`imenu-create-index-function'." nil nil) (autoload 'magit-imenu--cherry-create-index-function "magit-imenu" "Return an alist of all imenu entries in current buffer.
This function is used as a value for
`imenu-create-index-function'." nil nil) (autoload 'magit-imenu--submodule-prev-index-position-function "magit-imenu" "Move point to previous line in magit-submodule-list buffer.
This function is used as a value for
`imenu-prev-index-position-function'." nil nil) (autoload 'magit-imenu--submodule-extract-index-name-function "magit-imenu" "Return imenu name for line at point.
This function is used as a value for
`imenu-extract-index-name-function'.  Point should be at the
beginning of the line." nil nil) (autoload 'magit-imenu--repolist-prev-index-position-function "magit-imenu" "Move point to previous line in magit-repolist buffer.
This function is used as a value for
`imenu-prev-index-position-function'." nil nil) (autoload 'magit-imenu--repolist-extract-index-name-function "magit-imenu" "Return imenu name for line at point.
This function is used as a value for
`imenu-extract-index-name-function'.  Point should be at the
beginning of the line." nil nil) (autoload 'magit-imenu--process-prev-index-position-function "magit-imenu" "Move point to previous process in magit-process buffer.
This function is used as a value for
`imenu-prev-index-position-function'." nil nil) (autoload 'magit-imenu--process-extract-index-name-function "magit-imenu" "Return imenu name for line at point.
This function is used as a value for
`imenu-extract-index-name-function'.  Point should be at the
beginning of the line." nil nil) (autoload 'magit-imenu--rebase-prev-index-position-function "magit-imenu" "Move point to previous commit in git-rebase buffer.
This function is used as a value for
`imenu-prev-index-position-function'." nil nil) (autoload 'magit-imenu--rebase-extract-index-name-function "magit-imenu" "Return imenu name for line at point.
This function is used as a value for
`imenu-extract-index-name-function'.  Point should be at the
beginning of the line." nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-imenu" '("magit-imenu--index-function"))) (autoload 'magit-log "magit-log" nil t) (autoload 'magit-log-refresh "magit-log" nil t) (autoload 'magit-log-current "magit-log" "Show log for the current branch.
When `HEAD' is detached or with a prefix argument show log for
one or more revs read from the minibuffer.

(fn REVS &optional ARGS FILES)" t nil) (autoload 'magit-log-other "magit-log" "Show log for one or more revs read from the minibuffer.
The user can input any revision or revisions separated by a
space, or even ranges, but only branches and tags, and a
representation of the commit at point, are available as
completion candidates.

(fn REVS &optional ARGS FILES)" t nil) (autoload 'magit-log-head "magit-log" "Show log for `HEAD'.

(fn &optional ARGS FILES)" t nil) (autoload 'magit-log-branches "magit-log" "Show log for all local branches and `HEAD'.

(fn &optional ARGS FILES)" t nil) (autoload 'magit-log-matching-branches "magit-log" "Show log for all branches matching PATTERN and `HEAD'.

(fn PATTERN &optional ARGS FILES)" t nil) (autoload 'magit-log-matching-tags "magit-log" "Show log for all tags matching PATTERN and `HEAD'.

(fn PATTERN &optional ARGS FILES)" t nil) (autoload 'magit-log-all-branches "magit-log" "Show log for all local and remote branches and `HEAD'.

(fn &optional ARGS FILES)" t nil) (autoload 'magit-log-all "magit-log" "Show log for all references and `HEAD'.

(fn &optional ARGS FILES)" t nil) (autoload 'magit-log-buffer-file "magit-log" "Show log for the blob or file visited in the current buffer.
With a prefix argument or when `--follow' is an active log
argument, then follow renames.  When the region is active,
restrict the log to the lines that the region touches.

(fn &optional FOLLOW BEG END)" t nil) (autoload 'magit-log-trace-definition "magit-log" "Show log for the definition at point.

(fn FILE FN REV)" t nil) (autoload 'magit-log-merged "magit-log" "Show log for the merge of COMMIT into BRANCH.

More precisely, find merge commit M that brought COMMIT into
BRANCH, and show the log of the range \"M^1..M\".  If COMMIT is
directly on BRANCH, then show approximately twenty surrounding
commits instead.

This command requires git-when-merged, which is available from
https://github.com/mhagger/git-when-merged.

(fn COMMIT BRANCH &optional ARGS FILES)" t nil) (autoload 'magit-log-move-to-parent "magit-log" "Move to the Nth parent of the current commit.

(fn &optional N)" t nil) (autoload 'magit-shortlog "magit-log" nil t) (autoload 'magit-shortlog-since "magit-log" "Show a history summary for commits since REV.

(fn REV ARGS)" t nil) (autoload 'magit-shortlog-range "magit-log" "Show a history summary for commit or range REV-OR-RANGE.

(fn REV-OR-RANGE ARGS)" t nil) (autoload 'magit-cherry "magit-log" "Show commits in a branch that are not merged in the upstream branch.

(fn HEAD UPSTREAM)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-log" '("magit-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-margin" '("magit-"))) (autoload 'magit-merge "magit" nil t) (autoload 'magit-merge-plain "magit-merge" "Merge commit REV into the current branch; using default message.

Unless there are conflicts or a prefix argument is used create a
merge commit using a generic commit message and without letting
the user inspect the result.  With a prefix argument pretend the
merge failed to give the user the opportunity to inspect the
merge.

(git merge --no-edit|--no-commit [ARGS] REV)

(fn REV &optional ARGS NOCOMMIT)" t nil) (autoload 'magit-merge-editmsg "magit-merge" "Merge commit REV into the current branch; and edit message.
Perform the merge and prepare a commit message but let the user
edit it.

(git merge --edit --no-ff [ARGS] REV)

(fn REV &optional ARGS)" t nil) (autoload 'magit-merge-nocommit "magit-merge" "Merge commit REV into the current branch; pretending it failed.
Pretend the merge failed to give the user the opportunity to
inspect the merge and change the commit message.

(git merge --no-commit --no-ff [ARGS] REV)

(fn REV &optional ARGS)" t nil) (autoload 'magit-merge-into "magit-merge" "Merge the current branch into BRANCH and remove the former.

Before merging, force push the source branch to its push-remote,
provided the respective remote branch already exists, ensuring
that the respective pull-request (if any) won't get stuck on some
obsolete version of the commits that are being merged.  Finally
if `forge-branch-pullreq' was used to create the merged branch,
branch, then also remove the respective remote branch.

(fn BRANCH &optional ARGS)" t nil) (autoload 'magit-merge-absorb "magit-merge" "Merge BRANCH into the current branch and remove the former.

Before merging, force push the source branch to its push-remote,
provided the respective remote branch already exists, ensuring
that the respective pull-request (if any) won't get stuck on some
obsolete version of the commits that are being merged.  Finally
if `forge-branch-pullreq' was used to create the merged branch,
then also remove the respective remote branch.

(fn BRANCH &optional ARGS)" t nil) (autoload 'magit-merge-squash "magit-merge" "Squash commit REV into the current branch; don't create a commit.

(git merge --squash REV)

(fn REV)" t nil) (autoload 'magit-merge-preview "magit-merge" "Preview result of merging REV into the current branch.

(fn REV)" t nil) (autoload 'magit-merge-abort "magit-merge" "Abort the current merge operation.

(git merge --abort)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-merge" '("magit-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-mode" '("disable-magit-save-buffers" "magit-"))) (autoload 'magit-notes "magit" nil t) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-notes" '("magit-notes-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-obsolete" '("magit--magit-popup-warning"))) (autoload 'magit-patch "magit-patch" nil t) (autoload 'magit-patch-create "magit-patch" nil t) (autoload 'magit-patch-apply "magit-patch" nil t) (autoload 'magit-patch-save "magit-patch" "Write current diff into patch FILE.

What arguments are used to create the patch depends on the value
of `magit-patch-save-arguments' and whether a prefix argument is
used.

If the value is the symbol `buffer', then use the same arguments
as the buffer.  With a prefix argument use no arguments.

If the value is a list beginning with the symbol `exclude', then
use the same arguments as the buffer except for those matched by
entries in the cdr of the list.  The comparison is done using
`string-prefix-p'.  With a prefix argument use the same arguments
as the buffer.

If the value is a list of strings (including the empty list),
then use those arguments.  With a prefix argument use the same
arguments as the buffer.

Of course the arguments that are required to actually show the
same differences as those shown in the buffer are always used.

(fn FILE &optional ARG)" t nil) (autoload 'magit-request-pull "magit-patch" "Request upstream to pull from your public repository.

URL is the url of your publicly accessible repository.
START is a commit that already is in the upstream repository.
END is the last commit, usually a branch name, which upstream
is asked to pull.  START has to be reachable from that commit.

(fn URL START END)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-patch" '("magit-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-process" '("magit-" "tramp-sh-handle-"))) (autoload 'magit-pull "magit-pull" nil t) (autoload 'magit-pull-from-pushremote "magit-pull" nil t) (autoload 'magit-pull-from-upstream "magit-pull" nil t) (autoload 'magit-pull-branch "magit-pull" "Pull from a branch read in the minibuffer.

(fn SOURCE ARGS)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-pull" '("magit-pull-"))) (autoload 'magit-push "magit-push" nil t) (autoload 'magit-push-current-to-pushremote "magit-push" nil t) (autoload 'magit-push-current-to-upstream "magit-push" nil t) (autoload 'magit-push-current "magit-push" "Push the current branch to a branch read in the minibuffer.

(fn TARGET ARGS)" t nil) (autoload 'magit-push-other "magit-push" "Push an arbitrary branch or commit somewhere.
Both the source and the target are read in the minibuffer.

(fn SOURCE TARGET ARGS)" t nil) (autoload 'magit-push-refspecs "magit-push" "Push one or multiple REFSPECS to a REMOTE.
Both the REMOTE and the REFSPECS are read in the minibuffer.  To
use multiple REFSPECS, separate them with commas.  Completion is
only available for the part before the colon, or when no colon
is used.

(fn REMOTE REFSPECS ARGS)" t nil) (autoload 'magit-push-matching "magit-push" "Push all matching branches to another repository.
If multiple remotes exist, then read one from the user.
If just one exists, use that without requiring confirmation.

(fn REMOTE &optional ARGS)" t nil) (autoload 'magit-push-tags "magit-push" "Push all tags to another repository.
If only one remote exists, then push to that.  Otherwise prompt
for a remote, offering the remote configured for the current
branch as default.

(fn REMOTE &optional ARGS)" t nil) (autoload 'magit-push-tag "magit-push" "Push a tag to another repository.

(fn TAG REMOTE &optional ARGS)" t nil) (autoload 'magit-push-notes-ref "magit-push" "Push a notes ref to another repository.

(fn REF REMOTE &optional ARGS)" t nil) (autoload 'magit-push-implicitly "magit-push" nil t) (autoload 'magit-push-to-remote "magit-push" "Push to REMOTE without using an explicit refspec.
The REMOTE is read in the minibuffer.

This command simply runs \"git push -v [ARGS] REMOTE\".  ARGS
are the arguments specified in the popup buffer.  No refspec
arguments are used.  Instead the behavior depends on at least
these Git variables: `push.default', `remote.pushDefault',
`branch.<branch>.pushRemote', `branch.<branch>.remote',
`branch.<branch>.merge', and `remote.<remote>.push'.

(fn REMOTE ARGS)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-push" '("magit-"))) (autoload 'magit-reflog-current "magit-reflog" "Display the reflog of the current branch.
If `HEAD' is detached, then show the reflog for that instead." t nil) (autoload 'magit-reflog-other "magit-reflog" "Display the reflog of a branch or another ref.

(fn REF)" t nil) (autoload 'magit-reflog-head "magit-reflog" "Display the `HEAD' reflog." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-reflog" '("magit-reflog-"))) (autoload 'magit-show-refs "magit-refs" nil t) (autoload 'magit-show-refs-head "magit-refs" "List and compare references in a dedicated buffer.
Compared with `HEAD'.

(fn &optional ARGS)" t nil) (autoload 'magit-show-refs-current "magit-refs" "List and compare references in a dedicated buffer.
Compare with the current branch or `HEAD' if it is detached.

(fn &optional ARGS)" t nil) (autoload 'magit-show-refs-other "magit-refs" "List and compare references in a dedicated buffer.
Compared with a branch read from the user.

(fn &optional REF ARGS)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-refs" '("magit-"))) (autoload 'magit-remote "magit-remote" nil t) (autoload 'magit-remote-add "magit-remote" "Add a remote named REMOTE and fetch it.

(fn REMOTE URL &optional ARGS)" t nil) (autoload 'magit-remote-rename "magit-remote" "Rename the remote named OLD to NEW.

(fn OLD NEW)" t nil) (autoload 'magit-remote-remove "magit-remote" "Delete the remote named REMOTE.

(fn REMOTE)" t nil) (autoload 'magit-remote-prune "magit-remote" "Remove stale remote-tracking branches for REMOTE.

(fn REMOTE)" t nil) (autoload 'magit-remote-prune-refspecs "magit-remote" "Remove stale refspecs for REMOTE.

A refspec is stale if there no longer exists at least one branch
on the remote that would be fetched due to that refspec.  A stale
refspec is problematic because its existence causes Git to refuse
to fetch according to the remaining non-stale refspecs.

If only stale refspecs remain, then offer to either delete the
remote or to replace the stale refspecs with the default refspec.

Also remove the remote-tracking branches that were created due to
the now stale refspecs.  Other stale branches are not removed.

(fn REMOTE)" t nil) (autoload 'magit-remote-set-head "magit-remote" "Set the local representation of REMOTE's default branch.
Query REMOTE and set the symbolic-ref refs/remotes/<remote>/HEAD
accordingly.  With a prefix argument query for the branch to be
used, which allows you to select an incorrect value if you fancy
doing that.

(fn REMOTE &optional BRANCH)" t nil) (autoload 'magit-remote-unset-head "magit-remote" "Unset the local representation of REMOTE's default branch.
Delete the symbolic-ref \"refs/remotes/<remote>/HEAD\".

(fn REMOTE)" t nil) (autoload 'magit-remote-unshallow "magit-remote" "Convert a shallow remote into a full one.
If only a single refspec is set and it does not contain a
wildcard, then also offer to replace it with the standard
refspec.

(fn REMOTE)" t nil) (autoload 'magit-remote-configure "magit-remote" nil t) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-remote" '("magit-"))) (autoload 'magit-list-repositories "magit-repos" "Display a list of repositories.

Use the options `magit-repository-directories' to control which
repositories are displayed." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-repos" '("magit-"))) (autoload 'magit-reset "magit" nil t) (autoload 'magit-reset-mixed "magit-reset" "Reset the `HEAD' and index to COMMIT, but not the working tree.

(git reset --mixed COMMIT)

(fn COMMIT)" t nil) (autoload 'magit-reset-soft "magit-reset" "Reset the `HEAD' to COMMIT, but not the index and working tree.

(git reset --soft REVISION)

(fn COMMIT)" t nil) (autoload 'magit-reset-hard "magit-reset" "Reset the `HEAD', index, and working tree to COMMIT.

(git reset --hard REVISION)

(fn COMMIT)" t nil) (autoload 'magit-reset-keep "magit-reset" "Reset the `HEAD' and index to COMMIT, while keeping uncommitted changes.

(git reset --keep REVISION)

(fn COMMIT)" t nil) (autoload 'magit-reset-index "magit-reset" "Reset the index to COMMIT.
Keep the `HEAD' and working tree as-is, so if COMMIT refers to the
head this effectively unstages all changes.

(git reset COMMIT .)

(fn COMMIT)" t nil) (autoload 'magit-reset-worktree "magit-reset" "Reset the worktree to COMMIT.
Keep the `HEAD' and index as-is.

(fn COMMIT)" t nil) (autoload 'magit-reset-quickly "magit-reset" "Reset the `HEAD' and index to COMMIT, and possibly the working tree.
With a prefix argument reset the working tree otherwise don't.

(git reset --mixed|--hard COMMIT)

(fn COMMIT &optional HARD)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-reset" '("magit-reset-"))) (autoload 'magit-sequencer-continue "magit-sequence" "Resume the current cherry-pick or revert sequence." t nil) (autoload 'magit-sequencer-skip "magit-sequence" "Skip the stopped at commit during a cherry-pick or revert sequence." t nil) (autoload 'magit-sequencer-abort "magit-sequence" "Abort the current cherry-pick or revert sequence.
This discards all changes made since the sequence started." t nil) (autoload 'magit-cherry-pick "magit-sequence" nil t) (autoload 'magit-cherry-copy "magit-sequence" "Copy COMMITS from another branch onto the current branch.
Prompt for a commit, defaulting to the commit at point.  If
the region selects multiple commits, then pick all of them,
without prompting.

(fn COMMITS &optional ARGS)" t nil) (autoload 'magit-cherry-apply "magit-sequence" "Apply the changes in COMMITS but do not commit them.
Prompt for a commit, defaulting to the commit at point.  If
the region selects multiple commits, then apply all of them,
without prompting.

(fn COMMITS &optional ARGS)" t nil) (autoload 'magit-cherry-harvest "magit-sequence" "Move COMMITS from another BRANCH onto the current branch.
Remove the COMMITS from BRANCH and stay on the current branch.
If a conflict occurs, then you have to fix that and finish the
process manually.

(fn COMMITS BRANCH &optional ARGS)" t nil) (autoload 'magit-cherry-donate "magit-sequence" "Move COMMITS from the current branch onto another existing BRANCH.
Remove COMMITS from the current branch and stay on that branch.
If a conflict occurs, then you have to fix that and finish the
process manually.

(fn COMMITS BRANCH &optional ARGS)" t nil) (autoload 'magit-cherry-spinout "magit-sequence" "Move COMMITS from the current branch onto a new BRANCH.
Remove COMMITS from the current branch and stay on that branch.
If a conflict occurs, then you have to fix that and finish the
process manually.

(fn COMMITS BRANCH START-POINT &optional ARGS)" t nil) (autoload 'magit-cherry-spinoff "magit-sequence" "Move COMMITS from the current branch onto a new BRANCH.
Remove COMMITS from the current branch and checkout BRANCH.
If a conflict occurs, then you have to fix that and finish
the process manually.

(fn COMMITS BRANCH START-POINT &optional ARGS)" t nil) (autoload 'magit-revert "magit-sequence" nil t) (autoload 'magit-revert-and-commit "magit-sequence" "Revert COMMIT by creating a new commit.
Prompt for a commit, defaulting to the commit at point.  If
the region selects multiple commits, then revert all of them,
without prompting.

(fn COMMIT &optional ARGS)" t nil) (autoload 'magit-revert-no-commit "magit-sequence" "Revert COMMIT by applying it in reverse to the worktree.
Prompt for a commit, defaulting to the commit at point.  If
the region selects multiple commits, then revert all of them,
without prompting.

(fn COMMIT &optional ARGS)" t nil) (autoload 'magit-am "magit-sequence" nil t) (autoload 'magit-am-apply-patches "magit-sequence" "Apply the patches FILES.

(fn &optional FILES ARGS)" t nil) (autoload 'magit-am-apply-maildir "magit-sequence" "Apply the patches from MAILDIR.

(fn &optional MAILDIR ARGS)" t nil) (autoload 'magit-am-continue "magit-sequence" "Resume the current patch applying sequence." t nil) (autoload 'magit-am-skip "magit-sequence" "Skip the stopped at patch during a patch applying sequence." t nil) (autoload 'magit-am-abort "magit-sequence" "Abort the current patch applying sequence.
This discards all changes made since the sequence started." t nil) (autoload 'magit-rebase "magit-sequence" nil t) (autoload 'magit-rebase-onto-pushremote "magit-sequence" nil t) (autoload 'magit-rebase-onto-upstream "magit-sequence" nil t) (autoload 'magit-rebase-branch "magit-sequence" "Rebase the current branch onto a branch read in the minibuffer.
All commits that are reachable from `HEAD' but not from the
selected branch TARGET are being rebased.

(fn TARGET ARGS)" t nil) (autoload 'magit-rebase-subset "magit-sequence" "Rebase a subset of the current branch's history onto a new base.
Rebase commits from START to `HEAD' onto NEWBASE.
START has to be selected from a list of recent commits.

(fn NEWBASE START ARGS)" t nil) (autoload 'magit-rebase-interactive "magit-sequence" "Start an interactive rebase sequence.

(fn COMMIT ARGS)" t nil) (autoload 'magit-rebase-autosquash "magit-sequence" "Combine squash and fixup commits with their intended targets.

(fn ARGS)" t nil) (autoload 'magit-rebase-edit-commit "magit-sequence" "Edit a single older commit using rebase.

(fn COMMIT ARGS)" t nil) (autoload 'magit-rebase-reword-commit "magit-sequence" "Reword a single older commit using rebase.

(fn COMMIT ARGS)" t nil) (autoload 'magit-rebase-remove-commit "magit-sequence" "Remove a single older commit using rebase.

(fn COMMIT ARGS)" t nil) (autoload 'magit-rebase-continue "magit-sequence" "Restart the current rebasing operation.
In some cases this pops up a commit message buffer for you do
edit.  With a prefix argument the old message is reused as-is.

(fn &optional NOEDIT)" t nil) (autoload 'magit-rebase-skip "magit-sequence" "Skip the current commit and restart the current rebase operation." t nil) (autoload 'magit-rebase-edit "magit-sequence" "Edit the todo list of the current rebase operation." t nil) (autoload 'magit-rebase-abort "magit-sequence" "Abort the current rebase operation, restoring the original branch." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-sequence" '("magit-"))) (autoload 'magit-stash "magit-stash" nil t) (autoload 'magit-stash-both "magit-stash" "Create a stash of the index and working tree.
Untracked files are included according to infix arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'.

(fn MESSAGE &optional INCLUDE-UNTRACKED)" t nil) (autoload 'magit-stash-index "magit-stash" "Create a stash of the index only.
Unstaged and untracked changes are not stashed.  The stashed
changes are applied in reverse to both the index and the
worktree.  This command can fail when the worktree is not clean.
Applying the resulting stash has the inverse effect.

(fn MESSAGE)" t nil) (autoload 'magit-stash-worktree "magit-stash" "Create a stash of unstaged changes in the working tree.
Untracked files are included according to infix arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'.

(fn MESSAGE &optional INCLUDE-UNTRACKED)" t nil) (autoload 'magit-stash-keep-index "magit-stash" "Create a stash of the index and working tree, keeping index intact.
Untracked files are included according to infix arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'.

(fn MESSAGE &optional INCLUDE-UNTRACKED)" t nil) (autoload 'magit-snapshot-both "magit-stash" "Create a snapshot of the index and working tree.
Untracked files are included according to infix arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'.

(fn &optional INCLUDE-UNTRACKED)" t nil) (autoload 'magit-snapshot-index "magit-stash" "Create a snapshot of the index only.
Unstaged and untracked changes are not stashed." t nil) (autoload 'magit-snapshot-worktree "magit-stash" "Create a snapshot of unstaged changes in the working tree.
Untracked files are included according to infix arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'.

(fn &optional INCLUDE-UNTRACKED)" t nil) (autoload 'magit-stash-apply "magit-stash" "Apply a stash to the working tree.
Try to preserve the stash index.  If that fails because there
are staged changes, apply without preserving the stash index.

(fn STASH)" t nil) (autoload 'magit-stash-pop "magit-stash" "Apply a stash to the working tree and remove it from stash list.
Try to preserve the stash index.  If that fails because there
are staged changes, apply without preserving the stash index
and forgo removing the stash.

(fn STASH)" t nil) (autoload 'magit-stash-drop "magit-stash" "Remove a stash from the stash list.
When the region is active offer to drop all contained stashes.

(fn STASH)" t nil) (autoload 'magit-stash-clear "magit-stash" "Remove all stashes saved in REF's reflog by deleting REF.

(fn REF)" t nil) (autoload 'magit-stash-branch "magit-stash" "Create and checkout a new BRANCH from STASH.

(fn STASH BRANCH)" t nil) (autoload 'magit-stash-branch-here "magit-stash" "Create and checkout a new BRANCH and apply STASH.
The branch is created using `magit-branch-and-checkout', using the
current branch or `HEAD' as the start-point.

(fn STASH BRANCH)" t nil) (autoload 'magit-stash-format-patch "magit-stash" "Create a patch from STASH

(fn STASH)" t nil) (autoload 'magit-stash-list "magit-stash" "List all stashes in a buffer." t nil) (autoload 'magit-stash-show "magit-stash" "Show all diffs of a stash in a buffer.

(fn STASH &optional ARGS FILES)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-stash" '("magit-"))) (autoload 'magit-init "magit-status" "Initialize a Git repository, then show its status.

If the directory is below an existing repository, then the user
has to confirm that a new one should be created inside.  If the
directory is the root of the existing repository, then the user
has to confirm that it should be reinitialized.

Non-interactively DIRECTORY is (re-)initialized unconditionally.

(fn DIRECTORY)" t nil) (autoload 'magit-status "magit-status" "Show the status of the current Git repository in a buffer.

If the current directory isn't located within a Git repository,
then prompt for an existing repository or an arbitrary directory,
depending on option `magit-repository-directories', and show the
status of the selected repository instead.

* If that option specifies any existing repositories, then offer
  those for completion and show the status buffer for the
  selected one.

* Otherwise read an arbitrary directory using regular file-name
  completion.  If the selected directory is the top-level of an
  existing working tree, then show the status buffer for that.

* Otherwise offer to initialize the selected directory as a new
  repository.  After creating the repository show its status
  buffer.

These fallback behaviors can also be forced using one or more
prefix arguments:

* With two prefix arguments (or more precisely a numeric prefix
  value of 16 or greater) read an arbitrary directory and act on
  it as described above.  The same could be accomplished using
  the command `magit-init'.

* With a single prefix argument read an existing repository, or
  if none can be found based on `magit-repository-directories',
  then fall back to the same behavior as with two prefix
  arguments.

(fn &optional DIRECTORY CACHE)" t nil) (defalias 'magit 'magit-status "An alias for `magit-status' for better discoverability.

Instead of invoking this alias for `magit-status' using
\"M-x magit RET\", you should bind a key to `magit-status'
and read the info node `(magit)Getting Started', which
also contains other useful hints.") (autoload 'magit-status-here "magit-status" "Like `magit-status' but with non-nil `magit-status-goto-file-position'." t nil) (autoload 'magit-status-quick "magit-status" "Show the status of the current Git repository, maybe without refreshing.

If the status buffer of the current Git repository exists but
isn't being displayed in the selected frame, then display it
without refreshing it.

If the status buffer is being displayed in the selected frame,
then also refresh it.

Prefix arguments have the same meaning as for `magit-status',
and additionally cause the buffer to be refresh.

To use this function instead of `magit-status', add this to your
init file: (global-set-key (kbd \"C-x g\") 'magit-status-quick)." t nil) (autoload 'magit-status-setup-buffer "magit-status" "

(fn &optional DIRECTORY)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-status" '("magit-"))) (autoload 'magit-submodule "magit-submodule" nil t) (autoload 'magit-submodule-add "magit-submodule" nil t) (autoload 'magit-submodule-read-name-for-path "magit-submodule" "

(fn PATH &optional PREFER-SHORT)" nil nil) (autoload 'magit-submodule-register "magit-submodule" nil t) (autoload 'magit-submodule-populate "magit-submodule" nil t) (autoload 'magit-submodule-update "magit-submodule" nil t) (autoload 'magit-submodule-synchronize "magit-submodule" nil t) (autoload 'magit-submodule-unpopulate "magit-submodule" nil t) (autoload 'magit-submodule-remove "magit-submodule" "Unregister MODULES and remove their working directories.

For safety reasons, do not remove the gitdirs and if a module has
uncommitted changes, then do not remove it at all.  If a module's
gitdir is located inside the working directory, then move it into
the gitdir of the superproject first.

With the \"--force\" argument offer to remove dirty working
directories and with a prefix argument offer to delete gitdirs.
Both actions are very dangerous and have to be confirmed.  There
are additional safety precautions in place, so you might be able
to recover from making a mistake here, but don't count on it.

(fn MODULES ARGS TRASH-GITDIRS)" t nil) (autoload 'magit-insert-modules "magit-submodule" "Insert submodule sections.
Hook `magit-module-sections-hook' controls which module sections
are inserted, and option `magit-module-sections-nested' controls
whether they are wrapped in an additional section." nil nil) (autoload 'magit-insert-modules-overview "magit-submodule" "Insert sections for all modules.
For each section insert the path and the output of `git describe --tags',
or, failing that, the abbreviated HEAD commit hash." nil nil) (autoload 'magit-insert-modules-unpulled-from-upstream "magit-submodule" "Insert sections for modules that haven't been pulled from the upstream.
These sections can be expanded to show the respective commits." nil nil) (autoload 'magit-insert-modules-unpulled-from-pushremote "magit-submodule" "Insert sections for modules that haven't been pulled from the push-remote.
These sections can be expanded to show the respective commits." nil nil) (autoload 'magit-insert-modules-unpushed-to-upstream "magit-submodule" "Insert sections for modules that haven't been pushed to the upstream.
These sections can be expanded to show the respective commits." nil nil) (autoload 'magit-insert-modules-unpushed-to-pushremote "magit-submodule" "Insert sections for modules that haven't been pushed to the push-remote.
These sections can be expanded to show the respective commits." nil nil) (autoload 'magit-list-submodules "magit-submodule" "Display a list of the current repository's submodules." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-submodule" '("magit-"))) (autoload 'magit-subtree "magit-subtree" nil t) (autoload 'magit-subtree-import "magit-subtree" nil t) (autoload 'magit-subtree-export "magit-subtree" nil t) (autoload 'magit-subtree-add "magit-subtree" "Add REF from REPOSITORY as a new subtree at PREFIX.

(fn PREFIX REPOSITORY REF ARGS)" t nil) (autoload 'magit-subtree-add-commit "magit-subtree" "Add COMMIT as a new subtree at PREFIX.

(fn PREFIX COMMIT ARGS)" t nil) (autoload 'magit-subtree-merge "magit-subtree" "Merge COMMIT into the PREFIX subtree.

(fn PREFIX COMMIT ARGS)" t nil) (autoload 'magit-subtree-pull "magit-subtree" "Pull REF from REPOSITORY into the PREFIX subtree.

(fn PREFIX REPOSITORY REF ARGS)" t nil) (autoload 'magit-subtree-push "magit-subtree" "Extract the history of the subtree PREFIX and push it to REF on REPOSITORY.

(fn PREFIX REPOSITORY REF ARGS)" t nil) (autoload 'magit-subtree-split "magit-subtree" "Extract the history of the subtree PREFIX.

(fn PREFIX COMMIT ARGS)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-subtree" '("magit-"))) (autoload 'magit-tag "magit" nil t) (autoload 'magit-tag-create "magit-tag" "Create a new tag with the given NAME at REV.
With a prefix argument annotate the tag.

(git tag [--annotate] NAME REV)

(fn NAME REV &optional ARGS)" t nil) (autoload 'magit-tag-delete "magit-tag" "Delete one or more tags.
If the region marks multiple tags (and nothing else), then offer
to delete those, otherwise prompt for a single tag to be deleted,
defaulting to the tag at point.

(git tag -d TAGS)

(fn TAGS)" t nil) (autoload 'magit-tag-prune "magit-tag" "Offer to delete tags missing locally from REMOTE, and vice versa.

(fn TAGS REMOTE-TAGS REMOTE)" t nil) (autoload 'magit-tag-release "magit-tag" "Create a release tag.

Assume that release tags match `magit-release-tag-regexp'.

First prompt for the name of the new tag using the highest
existing tag as initial input and leaving it to the user to
increment the desired part of the version string.

If `--annotate' is enabled, then prompt for the message of the
new tag.  Base the proposed tag message on the message of the
highest tag, provided that that contains the corresponding
version string and substituting the new version string for that.
Otherwise propose something like \"Foo-Bar 1.2.3\", given, for
example, a TAG \"v1.2.3\" and a repository located at something
like \"/path/to/foo-bar\".

(fn TAG MSG &optional ARGS)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-tag" '("magit-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-transient" '("magit-"))) (autoload 'magit-emacs-Q-command "magit-utils" "Show a shell command that runs an uncustomized Emacs with only Magit loaded.
See info node `(magit)Debugging Tools' for more information." t nil) (autoload 'Info-follow-nearest-node--magit-gitman "magit-utils" "

(fn FN &optional FORK)" nil nil) (advice-add 'Info-follow-nearest-node :around 'Info-follow-nearest-node--magit-gitman) (autoload 'org-man-export--magit-gitman "magit-utils" "

(fn FN LINK DESCRIPTION FORMAT)" nil nil) (advice-add 'org-man-export :around 'org-man-export--magit-gitman) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-utils" '("magit-"))) (defvar magit-wip-mode nil "Non-nil if Magit-Wip mode is enabled.
See the `magit-wip-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `magit-wip-mode'.") (custom-autoload 'magit-wip-mode "magit-wip" nil) (autoload 'magit-wip-mode "magit-wip" "Save uncommitted changes to work-in-progress refs.

If called interactively, enable Magit-Wip mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

Whenever appropriate (i.e. when dataloss would be a possibility
otherwise) this mode causes uncommitted changes to be committed
to dedicated work-in-progress refs.

For historic reasons this mode is implemented on top of four
other `magit-wip-*' modes, which can also be used individually,
if you want finer control over when the wip refs are updated;
but that is discouraged.

(fn &optional ARG)" t nil) (put 'magit-wip-after-save-mode 'globalized-minor-mode t) (defvar magit-wip-after-save-mode nil "Non-nil if Magit-Wip-After-Save mode is enabled.
See the `magit-wip-after-save-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `magit-wip-after-save-mode'.") (custom-autoload 'magit-wip-after-save-mode "magit-wip" nil) (autoload 'magit-wip-after-save-mode "magit-wip" "Toggle Magit-Wip-After-Save-Local mode in all buffers.
With prefix ARG, enable Magit-Wip-After-Save mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Magit-Wip-After-Save-Local mode is enabled in all buffers where
`magit-wip-after-save-local-mode-turn-on' would do it.
See `magit-wip-after-save-local-mode' for more information on Magit-Wip-After-Save-Local mode.

(fn &optional ARG)" t nil) (defvar magit-wip-after-apply-mode nil "Non-nil if Magit-Wip-After-Apply mode is enabled.
See the `magit-wip-after-apply-mode' command
for a description of this minor mode.") (custom-autoload 'magit-wip-after-apply-mode "magit-wip" nil) (autoload 'magit-wip-after-apply-mode "magit-wip" "Commit to work-in-progress refs.

If called interactively, enable Magit-Wip-After-Apply mode if ARG
is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

After applying a change using any \"apply variant\"
command (apply, stage, unstage, discard, and reverse) commit the
affected files to the current wip refs.  For each branch there
may be two wip refs; one contains snapshots of the files as found
in the worktree and the other contains snapshots of the entries
in the index.

(fn &optional ARG)" t nil) (defvar magit-wip-before-change-mode nil "Non-nil if Magit-Wip-Before-Change mode is enabled.
See the `magit-wip-before-change-mode' command
for a description of this minor mode.") (custom-autoload 'magit-wip-before-change-mode "magit-wip" nil) (autoload 'magit-wip-before-change-mode "magit-wip" "Commit to work-in-progress refs before certain destructive changes.

If called interactively, enable Magit-Wip-Before-Change mode if
ARG is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

Before invoking a revert command or an \"apply variant\"
command (apply, stage, unstage, discard, and reverse) commit the
affected tracked files to the current wip refs.  For each branch
there may be two wip refs; one contains snapshots of the files
as found in the worktree and the other contains snapshots of the
entries in the index.

Only changes to files which could potentially be affected by the
command which is about to be called are committed.

(fn &optional ARG)" t nil) (autoload 'magit-wip-commit-initial-backup "magit-wip" "Before saving, commit current file to a worktree wip ref.

The user has to add this function to `before-save-hook'.

Commit the current state of the visited file before saving the
current buffer to that file.  This backs up the same version of
the file as `backup-buffer' would, but stores the backup in the
worktree wip ref, which is also used by the various Magit Wip
modes, instead of in a backup file as `backup-buffer' would.

This function ignores the variables that affect `backup-buffer'
and can be used along-side that function, which is recommended
because this function only backs up files that are tracked in
a Git repository." nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-wip" '("magit-"))) (autoload 'magit-worktree "magit-worktree" nil t) (autoload 'magit-worktree-checkout "magit-worktree" "Checkout BRANCH in a new worktree at PATH.

(fn PATH BRANCH)" t nil) (autoload 'magit-worktree-branch "magit-worktree" "Create a new BRANCH and check it out in a new worktree at PATH.

(fn PATH BRANCH START-POINT &optional FORCE)" t nil) (autoload 'magit-worktree-move "magit-worktree" "Move WORKTREE to PATH.

(fn WORKTREE PATH)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-worktree" '("magit-"))) (provide 'magit-autoloads)) "git-timemachine" ((git-timemachine-autoloads git-timemachine) (autoload 'git-timemachine-toggle "git-timemachine" "Toggle git timemachine mode." t nil) (autoload 'git-timemachine "git-timemachine" "Enable git timemachine for file of current buffer." t nil) (autoload 'git-timemachine-switch-branch "git-timemachine" "Enable git timemachine for current buffer, switching to GIT-BRANCH.

(fn GIT-BRANCH)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "git-timemachine" '("git-timemachine-"))) (provide 'git-timemachine-autoloads)) "org-bullets" ((org-bullets-autoloads org-bullets) (autoload 'org-bullets-mode "org-bullets" "UTF8 Bullets for org-mode

If called interactively, enable Org-Bullets mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-bullets" '("org-bullets-"))) (provide 'org-bullets-autoloads)) "org" ((ob-eval ob-ocaml oc-natbib org-id ox-html org-goto org-clock ob-css ob-sqlite ox-man org-habit ox-md org-lint ob-sass org-crypt ob-processing org-archive ol-irc org-table ol ob-lisp ob-groovy ob-eshell ob-js ob-sql org-duration org-feed ob-dot org-num ob-emacs-lisp org-attach-git ox-texinfo ob-R ob-forth org-install ob-java ol-eshell org-ctags org-faces oc-csl ox-beamer ox-publish org ol-bibtex ob-tangle ob-awk ol-rmail org-mouse ob-gnuplot ob-lua ob-haskell ol-mhe ol-bbdb ob-core ox org-compat org-entities org-keys ob-octave ob-ref ob-lob ol-info ob-ruby ob-org ox-koma-letter ob-makefile org-datetree ox-icalendar ob-comint ob-python ob-shell ob-exp org-list ol-gnus org-agenda org-inlinetask oc-biblatex org-timer ob-julia ol-docview ol-doi ob ob-fortran org-loaddefs org-mobile ob-matlab ob-calc ob-lilypond org-version org-colview ob-plantuml org-macro ob-screen ox-odt org-tempo org-attach org-src org-element ob-scheme org-pcomplete org-protocol org-capture ob-C org-macs ob-latex org-plot org-refile ob-perl ob-maxima ob-clojure ob-ditaa oc-basic ox-ascii org-footnote ox-latex ob-table ol-eww ol-w3m ob-sed ox-org org-indent oc)) "smart-quotes" ((smart-quotes-autoloads smart-quotes) (defvar smart-quotes-mode nil "Toggle smart-quotes-mode.
Setting this variable directly does not take effect;
use either \\[customize] or the function `smart-quotes-mode'.") (custom-autoload 'smart-quotes-mode "smart-quotes" nil) (autoload 'smart-quotes-mode "smart-quotes" "Minor mode that makes the ' and \" keys insert left and right
quotation marks automatically according to the context before point;
see `smart-quotes-insert-single' and `smart-quotes-insert-double'.
With no argument, this command toggles Smart Quotes mode.
With a prefix argument ARG, turn Smart Quotes minor mode on if ARG
is positive, otherwise turn it off.

(fn &optional ARG)" t nil) (autoload 'turn-on-smart-quotes "smart-quotes" "Unconditionally turn on Smart Quotes mode." nil nil) (autoload 'turn-off-smart-quotes "smart-quotes" "Unconditionally turn off Smart Quotes mode." nil nil) (autoload 'smart-quotes-smarten "smart-quotes" "Turn quotes into smart quotes in region or buffer." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smart-quotes" '("smart-quotes-"))) (provide 'smart-quotes-autoloads)) "guess-language" ((guess-language guess-language-autoloads) (autoload 'guess-language-mode "guess-language" "Toggle guess-language mode.

If called interactively, enable Guess-Language mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

Guess-language is a buffer-local minor mode.  It guesses the
language of the current paragraph when flyspell detects an
incorrect word and changes ispell's dictionary and typo-mode
accordingly.  If the language settings change, flyspell is rerun
on the current paragraph.  If the paragraph is shorter than
`guess-language-min-paragraph-length', none of the above happens
because there is likely not enough text to guess the language
correctly.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "guess-language" '("guess-language"))) (provide 'guess-language-autoloads)) "visual-fill-column" ((visual-fill-column-autoloads visual-fill-column) (autoload 'visual-fill-column-mode "visual-fill-column" "Wrap lines according to `fill-column' in `visual-line-mode'.

If called interactively, enable Visual-Fill-Column mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (put 'global-visual-fill-column-mode 'globalized-minor-mode t) (defvar global-visual-fill-column-mode nil "Non-nil if Global Visual-Fill-Column mode is enabled.
See the `global-visual-fill-column-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-visual-fill-column-mode'.") (custom-autoload 'global-visual-fill-column-mode "visual-fill-column" nil) (autoload 'global-visual-fill-column-mode "visual-fill-column" "Toggle Visual-Fill-Column mode in all buffers.
With prefix ARG, enable Global Visual-Fill-Column mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Visual-Fill-Column mode is enabled in all buffers where
`turn-on-visual-fill-column-mode' would do it.
See `visual-fill-column-mode' for more information on Visual-Fill-Column mode.

(fn &optional ARG)" t nil) (autoload 'visual-fill-column-split-window-sensibly "visual-fill-column" "Split WINDOW sensibly, unsetting its margins first.
This function unsets the window margins and calls
`split-window-sensibly'.

By default, `split-window-sensibly' does not split a window in
two side-by-side windows if it has wide margins, even if there is
enough space for a vertical split.  This function is used as the
value of `split-window-preferred-function' to allow
`display-buffer' to split such windows.

(fn &optional WINDOW)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "visual-fill-column" '("turn-on-visual-fill-column-mode" "visual-fill-column-"))) (provide 'visual-fill-column-autoloads)) "writeroom-mode" ((writeroom-mode writeroom-mode-autoloads) (autoload 'writeroom-mode "writeroom-mode" "Minor mode for distraction-free writing.

If called interactively, enable Writeroom mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (put 'global-writeroom-mode 'globalized-minor-mode t) (defvar global-writeroom-mode nil "Non-nil if Global Writeroom mode is enabled.
See the `global-writeroom-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-writeroom-mode'.") (custom-autoload 'global-writeroom-mode "writeroom-mode" nil) (autoload 'global-writeroom-mode "writeroom-mode" "Toggle Writeroom mode in all buffers.
With prefix ARG, enable Global Writeroom mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Writeroom mode is enabled in all buffers where
`turn-on-writeroom-mode' would do it.
See `writeroom-mode' for more information on Writeroom mode.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "writeroom-mode" '("alpha" "bottom-divider-width" "define-writeroom-global-effect" "fullscreen" "internal-border-width" "menu-bar-lines" "sticky" "tool-bar-lines" "turn-on-writeroom-mode" "vertical-scroll-bars" "writeroom-"))) (provide 'writeroom-mode-autoloads)) "epl" ((epl epl-autoloads) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "epl" '("epl-"))) (provide 'epl-autoloads)) "pkg-info" ((pkg-info-autoloads pkg-info) (autoload 'pkg-info-library-original-version "pkg-info" "Get the original version in the header of LIBRARY.

The original version is stored in the X-Original-Version header.
This header is added by the MELPA package archive to preserve
upstream version numbers.

LIBRARY is either a symbol denoting a named feature, or a library
name as string.

If SHOW is non-nil, show the version in the minibuffer.

Return the version from the header of LIBRARY as list.  Signal an
error if the LIBRARY was not found or had no X-Original-Version
header.

See Info node `(elisp)Library Headers' for more information
about library headers.

(fn LIBRARY &optional SHOW)" t nil) (autoload 'pkg-info-library-version "pkg-info" "Get the version in the header of LIBRARY.

LIBRARY is either a symbol denoting a named feature, or a library
name as string.

If SHOW is non-nil, show the version in the minibuffer.

Return the version from the header of LIBRARY as list.  Signal an
error if the LIBRARY was not found or had no proper header.

See Info node `(elisp)Library Headers' for more information
about library headers.

(fn LIBRARY &optional SHOW)" t nil) (autoload 'pkg-info-defining-library-original-version "pkg-info" "Get the original version of the library defining FUNCTION.

The original version is stored in the X-Original-Version header.
This header is added by the MELPA package archive to preserve
upstream version numbers.

If SHOW is non-nil, show the version in mini-buffer.

This function is mainly intended to find the version of a major
or minor mode, i.e.

   (pkg-info-defining-library-version 'flycheck-mode)

Return the version of the library defining FUNCTION.  Signal an
error if FUNCTION is not a valid function, if its defining
library was not found, or if the library had no proper version
header.

(fn FUNCTION &optional SHOW)" t nil) (autoload 'pkg-info-defining-library-version "pkg-info" "Get the version of the library defining FUNCTION.

If SHOW is non-nil, show the version in mini-buffer.

This function is mainly intended to find the version of a major
or minor mode, i.e.

   (pkg-info-defining-library-version 'flycheck-mode)

Return the version of the library defining FUNCTION.  Signal an
error if FUNCTION is not a valid function, if its defining
library was not found, or if the library had no proper version
header.

(fn FUNCTION &optional SHOW)" t nil) (autoload 'pkg-info-package-version "pkg-info" "Get the version of an installed PACKAGE.

If SHOW is non-nil, show the version in the minibuffer.

Return the version as list, or nil if PACKAGE is not installed.

(fn PACKAGE &optional SHOW)" t nil) (autoload 'pkg-info-version-info "pkg-info" "Obtain complete version info for LIBRARY and PACKAGE.

LIBRARY is a symbol denoting a named feature, or a library name
as string.  PACKAGE is a symbol denoting an ELPA package.  If
omitted or nil, default to LIBRARY.

If SHOW is non-nil, show the version in the minibuffer.

When called interactively, prompt for LIBRARY.  When called
interactively with prefix argument, prompt for PACKAGE as well.

Return a string with complete version information for LIBRARY.
This version information contains the version from the headers of
LIBRARY, and the version of the installed PACKAGE, the LIBRARY is
part of.  If PACKAGE is not installed, or if the PACKAGE version
is the same as the LIBRARY version, do not include a package
version.

(fn LIBRARY &optional PACKAGE SHOW)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pkg-info" '("pkg-info-"))) (provide 'pkg-info-autoloads)) "let-alist" ((let-alist-autoloads let-alist) (autoload 'let-alist "let-alist" "Let-bind dotted symbols to their cdrs in ALIST and execute BODY.
Dotted symbol is any symbol starting with a `.'.  Only those present
in BODY are let-bound and this search is done at compile time.

For instance, the following code

  (let-alist alist
    (if (and .title .body)
        .body
      .site
      .site.contents))

essentially expands to

  (let ((.title (cdr (assq \\='title alist)))
        (.body  (cdr (assq \\='body alist)))
        (.site  (cdr (assq \\='site alist)))
        (.site.contents (cdr (assq \\='contents (cdr (assq \\='site alist))))))
    (if (and .title .body)
        .body
      .site
      .site.contents))

If you nest `let-alist' invocations, the inner one can't access
the variables of the outer one. You can, however, access alists
inside the original alist by using dots inside the symbol, as
displayed in the example above.

(fn ALIST &rest BODY)" nil t) (function-put 'let-alist 'lisp-indent-function '1) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "let-alist" '("let-alist--"))) (provide 'let-alist-autoloads)) "flycheck" ((flycheck-buttercup flycheck flycheck-ert flycheck-autoloads) (autoload 'flycheck-manual "flycheck" "Open the Flycheck manual." t nil) (autoload 'flycheck-mode "flycheck" "Flycheck is a minor mode for on-the-fly syntax checking.

In `flycheck-mode' the buffer is automatically syntax-checked
using the first suitable syntax checker from `flycheck-checkers'.
Use `flycheck-select-checker' to select a checker for the current
buffer manually.

If you run into issues, use `\\[flycheck-verify-setup]' to get help.

Flycheck supports many languages out of the box, and many
additional ones are available on MELPA.  Adding new ones is very
easy.  Complete documentation is available online at URL
`https://www.flycheck.org/en/latest/'.  Please report issues and
request features at URL `https://github.com/flycheck/flycheck'.

Flycheck displays its status in the mode line.  In the default
configuration, it looks like this:

`FlyC'     This buffer has not been checked yet.
`FlyC-'    Flycheck doesn't have a checker for this buffer.
`FlyC*'    Flycheck is running.  Expect results soon!
`FlyC:3|2' This buffer contains three warnings and two errors.
           Use `\\[flycheck-list-errors]' to see the list.

You may also see the following icons:
`FlyC!'    The checker crashed.
`FlyC.'    The last syntax check was manually interrupted.
`FlyC?'    The checker did something unexpected, like exiting with 1
           but returning no errors.

The following keybindings are available in `flycheck-mode':

\\{flycheck-mode-map}
(you can change the prefix by customizing
`flycheck-keymap-prefix')

If called interactively, enable Flycheck mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is \342\200\230toggle\342\200\231; disable the mode otherwise.

(fn &optional ARG)" t nil) (put 'global-flycheck-mode 'globalized-minor-mode t) (defvar global-flycheck-mode nil "Non-nil if Global Flycheck mode is enabled.
See the `global-flycheck-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-flycheck-mode'.") (custom-autoload 'global-flycheck-mode "flycheck" nil) (autoload 'global-flycheck-mode "flycheck" "Toggle Flycheck mode in all buffers.
With prefix ARG, enable Global Flycheck mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Flycheck mode is enabled in all buffers where
`flycheck-mode-on-safe' would do it.
See `flycheck-mode' for more information on Flycheck mode.

(fn &optional ARG)" t nil) (autoload 'flycheck-define-error-level "flycheck" "Define a new error LEVEL with PROPERTIES.

The following PROPERTIES constitute an error level:

`:severity SEVERITY'
     A number denoting the severity of this level.  The higher
     the number, the more severe is this level compared to other
     levels.  Defaults to 0; info is -10, warning is 10, and
     error is 100.

     The severity is used by `flycheck-error-level-<' to
     determine the ordering of errors according to their levels.

`:compilation-level LEVEL'

     A number indicating the broad class of messages that errors
     at this level belong to: one of 0 (info), 1 (warning), or
     2 or nil (error).  Defaults to nil.

     This is used by `flycheck-checker-pattern-to-error-regexp'
     to map error levels into `compilation-mode''s hierarchy and
     to get proper highlighting of errors in `compilation-mode'.

`:overlay-category CATEGORY'
     A symbol denoting the overlay category to use for error
     highlight overlays for this level.  See Info
     node `(elisp)Overlay Properties' for more information about
     overlay categories.

     A category for an error level overlay should at least define
     the `face' property, for error highlighting.  Another useful
     property for error level categories is `priority', to
     influence the stacking of multiple error level overlays.

`:fringe-bitmap BITMAPS'
     A fringe bitmap symbol denoting the bitmap to use for fringe
     indicators for this level, or a cons of two bitmaps (one for
     narrow fringes and one for wide fringes).  See Info node
     `(elisp)Fringe Bitmaps' for more information about fringe
     bitmaps, including a list of built-in fringe bitmaps.

`:fringe-face FACE'
     A face symbol denoting the face to use for fringe indicators
     for this level.

`:margin-spec SPEC'
     A display specification indicating what to display in the
     margin when `flycheck-indication-mode' is `left-margin' or
     `right-margin'.  See Info node `(elisp)Displaying in the
     Margins'.  If omitted, Flycheck generates an image spec from
     the fringe bitmap.

`:error-list-face FACE'
     A face symbol denoting the face to use for messages of this
     level in the error list.  See `flycheck-list-errors'.

(fn LEVEL &rest PROPERTIES)" nil nil) (function-put 'flycheck-define-error-level 'lisp-indent-function '1) (autoload 'flycheck-define-command-checker "flycheck" "Define SYMBOL as syntax checker to run a command.

Define SYMBOL as generic syntax checker via
`flycheck-define-generic-checker', which uses an external command
to check the buffer.  SYMBOL and DOCSTRING are the same as for
`flycheck-define-generic-checker'.

In addition to the properties understood by
`flycheck-define-generic-checker', the following PROPERTIES
constitute a command syntax checker.  Unless otherwise noted, all
properties are mandatory.  Note that the default `:error-filter'
of command checkers is `flycheck-sanitize-errors'.

`:command COMMAND'
     The command to run for syntax checking.

     COMMAND is a list of the form `(EXECUTABLE [ARG ...])'.

     EXECUTABLE is a string with the executable of this syntax
     checker.  It can be overridden with the variable
     `flycheck-SYMBOL-executable'.  Note that this variable is
     NOT implicitly defined by this function.  Use
     `flycheck-def-executable-var' to define this variable.

     Each ARG is an argument to the executable, either as string,
     or as special symbol or form for
     `flycheck-substitute-argument', which see.

`:error-patterns PATTERNS'
     A list of patterns to parse the output of the `:command'.

     Each ITEM in PATTERNS is a list `(LEVEL SEXP ...)', where
     LEVEL is a Flycheck error level (see
     `flycheck-define-error-level'), followed by one or more RX
     `SEXP's which parse an error of that level and extract line,
     column, file name and the message.

     See `rx' for general information about RX, and
     `flycheck-rx-to-string' for some special RX forms provided
     by Flycheck.

     All patterns are applied in the order of declaration to the
     whole output of the syntax checker.  Output already matched
     by a pattern will not be matched by subsequent patterns.  In
     other words, the first pattern wins.

     This property is optional.  If omitted, however, an
     `:error-parser' is mandatory.

`:error-parser FUNCTION'
     A function to parse errors with.

     The function shall accept three arguments OUTPUT CHECKER
     BUFFER.  OUTPUT is the syntax checker output as string,
     CHECKER the syntax checker that was used, and BUFFER a
     buffer object representing the checked buffer.  The function
     must return a list of `flycheck-error' objects parsed from
     OUTPUT.

     This property is optional.  If omitted, it defaults to
     `flycheck-parse-with-patterns'.  In this case,
     `:error-patterns' is mandatory.

`:standard-input t'
     Whether to send the buffer contents on standard input.

     If this property is given and has a non-nil value, send the
     contents of the buffer on standard input.

     Defaults to nil.

Note that you may not give `:start', `:interrupt', and
`:print-doc' for a command checker.  You can give a custom
`:verify' function, though, whose results will be appended to the
default `:verify' function of command checkers.

(fn SYMBOL DOCSTRING &rest PROPERTIES)" nil nil) (function-put 'flycheck-define-command-checker 'lisp-indent-function '1) (function-put 'flycheck-define-command-checker 'doc-string-elt '2) (autoload 'flycheck-def-config-file-var "flycheck" "Define SYMBOL as config file variable for CHECKER, with default FILE-NAME.

SYMBOL is declared as customizable variable using `defcustom', to
provide configuration files for the given syntax CHECKER.
CUSTOM-ARGS are forwarded to `defcustom'.

FILE-NAME is the initial value of the new variable.  If omitted,
the default value is nil.  It can be either a string or a list of
strings.

Use this together with the `config-file' form in the `:command'
argument to `flycheck-define-checker'.

(fn SYMBOL CHECKER &optional FILE-NAME &rest CUSTOM-ARGS)" nil t) (function-put 'flycheck-def-config-file-var 'lisp-indent-function '3) (autoload 'flycheck-def-option-var "flycheck" "Define SYMBOL as option variable with INIT-VALUE for CHECKER.

SYMBOL is declared as customizable variable using `defcustom', to
provide an option for the given syntax CHECKERS (a checker or a
list of checkers).  INIT-VALUE is the initial value of the
variable, and DOCSTRING is its docstring.  CUSTOM-ARGS are
forwarded to `defcustom'.

Use this together with the `option', `option-list' and
`option-flag' forms in the `:command' argument to
`flycheck-define-checker'.

(fn SYMBOL INIT-VALUE CHECKERS DOCSTRING &rest CUSTOM-ARGS)" nil t) (function-put 'flycheck-def-option-var 'lisp-indent-function '3) (function-put 'flycheck-def-option-var 'doc-string-elt '4) (autoload 'flycheck-define-checker "flycheck" "Define SYMBOL as command syntax checker with DOCSTRING and PROPERTIES.

Like `flycheck-define-command-checker', but PROPERTIES must not
be quoted.  Also, implicitly define the executable variable for
SYMBOL with `flycheck-def-executable-var'.

(fn SYMBOL DOCSTRING &rest PROPERTIES)" nil t) (function-put 'flycheck-define-checker 'lisp-indent-function '1) (function-put 'flycheck-define-checker 'doc-string-elt '2) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "flycheck" '("flycheck-" "help-flycheck-checker-d" "list-flycheck-errors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "flycheck-buttercup" '("flycheck-buttercup-format-error-list"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "flycheck-ert" '("flycheck-er"))) (provide 'flycheck-autoloads)) "company" ((company-clang company-dabbrev company-autoloads company-yasnippet company-dabbrev-code company-capf company-tng company-semantic company-css company-template company-keywords company company-abbrev company-nxml company-cmake company-elisp company-bbdb company-etags company-files company-gtags company-ispell company-oddmuse company-tempo) (autoload 'company-mode "company" "\"complete anything\"; is an in-buffer completion framework.
Completion starts automatically, depending on the values
`company-idle-delay' and `company-minimum-prefix-length'.

If called interactively, enable Company mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

Completion can be controlled with the commands:
`company-complete-common', `company-complete-selection', `company-complete',
`company-select-next', `company-select-previous'.  If these commands are
called before `company-idle-delay', completion will also start.

Completions can be searched with `company-search-candidates' or
`company-filter-candidates'.  These can be used while completion is
inactive, as well.

The completion data is retrieved using `company-backends' and displayed
using `company-frontends'.  If you want to start a specific backend, call
it interactively or use `company-begin-backend'.

By default, the completions list is sorted alphabetically, unless the
backend chooses otherwise, or `company-transformers' changes it later.

regular keymap (`company-mode-map'):

\\{company-mode-map}
keymap during active completions (`company-active-map'):

\\{company-active-map}

(fn &optional ARG)" t nil) (put 'global-company-mode 'globalized-minor-mode t) (defvar global-company-mode nil "Non-nil if Global Company mode is enabled.
See the `global-company-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-company-mode'.") (custom-autoload 'global-company-mode "company" nil) (autoload 'global-company-mode "company" "Toggle Company mode in all buffers.
With prefix ARG, enable Global Company mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Company mode is enabled in all buffers where
`company-mode-on' would do it.
See `company-mode' for more information on Company mode.

(fn &optional ARG)" t nil) (autoload 'company-manual-begin "company" nil t nil) (autoload 'company-complete "company" "Insert the common part of all candidates or the current selection.
The first time this is called, the common part is inserted, the second
time, or when the selection has been changed, the selected candidate is
inserted." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company" '("company-"))) (autoload 'company-abbrev "company-abbrev" "`company-mode' completion backend for abbrev.

(fn COMMAND &optional ARG &rest IGNORED)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-abbrev" '("company-abbrev-insert"))) (autoload 'company-bbdb "company-bbdb" "`company-mode' completion backend for BBDB.

(fn COMMAND &optional ARG &rest IGNORE)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-bbdb" '("company-bbdb-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-capf" '("company-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-clang" '("company-clang"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-cmake" '("company-cmake"))) (autoload 'company-css "company-css" "`company-mode' completion backend for `css-mode'.

(fn COMMAND &optional ARG &rest IGNORED)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-css" '("company-css-"))) (autoload 'company-dabbrev "company-dabbrev" "dabbrev-like `company-mode' completion backend.

(fn COMMAND &optional ARG &rest IGNORED)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-dabbrev" '("company-dabbrev-"))) (autoload 'company-dabbrev-code "company-dabbrev-code" "dabbrev-like `company-mode' backend for code.
The backend looks for all symbols in the current buffer that aren't in
comments or strings.

(fn COMMAND &optional ARG &rest IGNORED)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-dabbrev-code" '("company-dabbrev-code-"))) (autoload 'company-elisp "company-elisp" "`company-mode' completion backend for Emacs Lisp.

(fn COMMAND &optional ARG &rest IGNORED)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-elisp" '("company-elisp-"))) (autoload 'company-etags "company-etags" "`company-mode' completion backend for etags.

(fn COMMAND &optional ARG &rest IGNORED)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-etags" '("company-etags-"))) (autoload 'company-files "company-files" "`company-mode' completion backend existing file names.
Completions works for proper absolute and relative files paths.
File paths with spaces are only supported inside strings.

(fn COMMAND &optional ARG &rest IGNORED)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-files" '("company-file"))) (autoload 'company-gtags "company-gtags" "`company-mode' completion backend for GNU Global.

(fn COMMAND &optional ARG &rest IGNORED)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-gtags" '("company-gtags-"))) (autoload 'company-ispell "company-ispell" "`company-mode' completion backend using Ispell.

(fn COMMAND &optional ARG &rest IGNORED)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-ispell" '("company-ispell-"))) (autoload 'company-keywords "company-keywords" "`company-mode' backend for programming language keywords.

(fn COMMAND &optional ARG &rest IGNORED)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-keywords" '("company-keywords-"))) (autoload 'company-nxml "company-nxml" "`company-mode' completion backend for `nxml-mode'.

(fn COMMAND &optional ARG &rest IGNORED)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-nxml" '("company-nxml-"))) (autoload 'company-oddmuse "company-oddmuse" "`company-mode' completion backend for `oddmuse-mode'.

(fn COMMAND &optional ARG &rest IGNORED)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-oddmuse" '("company-oddmuse-"))) (autoload 'company-semantic "company-semantic" "`company-mode' completion backend using CEDET Semantic.

(fn COMMAND &optional ARG &rest IGNORED)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-semantic" '("company-semantic-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-template" '("company-template-"))) (autoload 'company-tempo "company-tempo" "`company-mode' completion backend for tempo.

(fn COMMAND &optional ARG &rest IGNORED)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-tempo" '("company-tempo-"))) (autoload 'company-tng-frontend "company-tng" "When the user changes the selection at least once, this
frontend will display the candidate in the buffer as if it's
already there and any key outside of `company-active-map' will
confirm the selection and finish the completion.

(fn COMMAND)" nil nil) (define-obsolete-function-alias 'company-tng-configure-default 'company-tng-mode "0.9.14" "Applies the default configuration to enable company-tng.") (defvar company-tng-mode nil "Non-nil if Company-Tng mode is enabled.
See the `company-tng-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `company-tng-mode'.") (custom-autoload 'company-tng-mode "company-tng" nil) (autoload 'company-tng-mode "company-tng" "This minor mode enables `company-tng-frontend'.

If called interactively, enable Company-Tng mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-tng" '("company-tng-"))) (autoload 'company-yasnippet "company-yasnippet" "`company-mode' backend for `yasnippet'.

This backend should be used with care, because as long as there are
snippets defined for the current major mode, this backend will always
shadow backends that come after it.  Recommended usages:

* In a buffer-local value of `company-backends', grouped with a backend or
  several that provide actual text completions.

  (add-hook \\='js-mode-hook
            (lambda ()
              (set (make-local-variable \\='company-backends)
                   \\='((company-dabbrev-code company-yasnippet)))))

* After keyword `:with', grouped with other backends.

  (push \\='(company-semantic :with company-yasnippet) company-backends)

* Not in `company-backends', just bound to a key.

  (global-set-key (kbd \"C-c y\") \\='company-yasnippet)

(fn COMMAND &optional ARG &rest IGNORE)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-yasnippet" '("company-yasnippet-"))) (provide 'company-autoloads)) "dash-functional" ((dash-functional)) "frame-local" ((frame-local-autoloads frame-local) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "frame-local" '("frame-local-"))) (provide 'frame-local-autoloads)) "company-box" ((company-box company-box-doc company-box-autoloads company-box-icons) (autoload 'company-box-mode "company-box" "Company-box minor mode.

If called interactively, enable Company-Box mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-box" '("company-box-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-box-doc" '("company-box-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-box-icons" '("company-box-icons-"))) (provide 'company-box-autoloads)) "web-mode" ((web-mode-autoloads web-mode) (autoload 'web-mode "web-mode" "Major mode for editing web templates.

(fn)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "web-mode" '("web-mode-"))) (provide 'web-mode-autoloads)) "yasnippet" ((yasnippet-autoloads yasnippet) (autoload 'yas-minor-mode "yasnippet" "Toggle YASnippet mode.

If called interactively, enable Yas minor mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

When YASnippet mode is enabled, `yas-expand', normally bound to
the TAB key, expands snippets of code depending on the major
mode.

With no argument, this command toggles the mode.
positive prefix argument turns on the mode.
Negative prefix argument turns off the mode.

Key bindings:
\\{yas-minor-mode-map}

(fn &optional ARG)" t nil) (put 'yas-global-mode 'globalized-minor-mode t) (defvar yas-global-mode nil "Non-nil if Yas-Global mode is enabled.
See the `yas-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `yas-global-mode'.") (custom-autoload 'yas-global-mode "yasnippet" nil) (autoload 'yas-global-mode "yasnippet" "Toggle Yas minor mode in all buffers.
With prefix ARG, enable Yas-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Yas minor mode is enabled in all buffers where
`yas-minor-mode-on' would do it.
See `yas-minor-mode' for more information on Yas minor mode.

(fn &optional ARG)" t nil) (autoload 'snippet-mode "yasnippet" "A mode for editing yasnippets" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "yasnippet" '("help-snippet-def" "snippet-mode-map" "yas"))) (provide 'yasnippet-autoloads)) "markdown-mode" ((markdown-mode-autoloads markdown-mode) (autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files.

(fn)" t nil) (add-to-list 'auto-mode-alist '("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode)) (autoload 'gfm-mode "markdown-mode" "Major mode for editing GitHub Flavored Markdown files.

(fn)" t nil) (autoload 'markdown-view-mode "markdown-mode" "Major mode for viewing Markdown content.

(fn)" t nil) (autoload 'gfm-view-mode "markdown-mode" "Major mode for viewing GitHub Flavored Markdown content.

(fn)" t nil) (autoload 'markdown-live-preview-mode "markdown-mode" "Toggle native previewing on save for a specific markdown file.

If called interactively, enable Markdown-Live-Preview mode if ARG
is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "markdown-mode" '("defun-markdown-" "gfm-" "markdown"))) (provide 'markdown-mode-autoloads)) "parsebib" ((parsebib-autoloads parsebib) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "parsebib" '("parsebib-"))) (provide 'parsebib-autoloads)) "f" ((f f-autoloads) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "f" '("f-"))) (provide 'f-autoloads)) "biblio-core" ((biblio-core biblio-core-autoloads) (autoload 'biblio-lookup "biblio-core" "Perform a search using BACKEND, and QUERY.
Prompt for any missing or nil arguments.  BACKEND should be a
function obeying the interface described in the docstring of
`biblio-backends'.  Returns the buffer in which results will be
inserted.

(fn &optional BACKEND QUERY)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "biblio-core" '("biblio-"))) (provide 'biblio-core-autoloads)) "biblio" ((biblio-pkg biblio-crossref biblio-dissemin biblio-autoloads biblio biblio-doi biblio-ieee biblio-download biblio-dblp biblio-hal biblio-arxiv) (autoload 'biblio-arxiv-backend "biblio-arxiv" "A arXiv backend for biblio.el.
COMMAND, ARG, MORE: See `biblio-backends'.

(fn COMMAND &optional ARG &rest MORE)" nil nil) (add-hook 'biblio-init-hook #'biblio-arxiv-backend) (autoload 'biblio-arxiv-lookup "biblio-arxiv" "Start an arXiv search for QUERY, prompting if needed.

(fn &optional QUERY)" t nil) (defalias 'arxiv-lookup 'biblio-arxiv-lookup) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "biblio-arxiv" '("biblio-arxiv-"))) (autoload 'biblio-crossref-backend "biblio-crossref" "A CrossRef backend for biblio.el.
COMMAND, ARG, MORE: See `biblio-backends'.

(fn COMMAND &optional ARG &rest MORE)" nil nil) (add-hook 'biblio-init-hook #'biblio-crossref-backend) (autoload 'biblio-crossref-lookup "biblio-crossref" "Start a CrossRef search for QUERY, prompting if needed.

(fn &optional QUERY)" t nil) (defalias 'crossref-lookup 'biblio-crossref-lookup) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "biblio-crossref" '("biblio-crossref-"))) (autoload 'biblio-dblp-backend "biblio-dblp" "A DBLP backend for biblio.el.
COMMAND, ARG, MORE: See `biblio-backends'.

(fn COMMAND &optional ARG &rest MORE)" nil nil) (add-hook 'biblio-init-hook #'biblio-dblp-backend) (autoload 'biblio-dblp-lookup "biblio-dblp" "Start a DBLP search for QUERY, prompting if needed.

(fn &optional QUERY)" t nil) (defalias 'dblp-lookup 'biblio-dblp-lookup) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "biblio-dblp" '("biblio-dblp--"))) (autoload 'biblio-dissemin-lookup "biblio-dissemin" "Retrieve a record by DOI from Dissemin, and display it.
Interactively, or if CLEANUP is non-nil, pass DOI through
`biblio-cleanup-doi'.

(fn DOI &optional CLEANUP)" t nil) (defalias 'dissemin-lookup 'biblio-dissemin-lookup) (autoload 'biblio-dissemin--register-action "biblio-dissemin" "Add Dissemin to list of `biblio-selection-mode' actions." nil nil) (add-hook 'biblio-selection-mode-hook #'biblio-dissemin--register-action) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "biblio-dissemin" '("biblio-dissemin--"))) (autoload 'biblio-doi-insert-bibtex "biblio-doi" "Insert BibTeX entry matching DOI.

(fn DOI)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "biblio-doi" '("biblio-doi-" "doi-insert-bibtex"))) (autoload 'biblio-download--register-action "biblio-download" "Add download to list of `biblio-selection-mode' actions." nil nil) (add-hook 'biblio-selection-mode-hook #'biblio-download--register-action) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "biblio-download" '("biblio-download-"))) (autoload 'biblio-hal-backend "biblio-hal" "A HAL backend for biblio.el.
COMMAND, ARG, MORE: See `biblio-backends'.

(fn COMMAND &optional ARG &rest MORE)" nil nil) (add-hook 'biblio-init-hook #'biblio-hal-backend) (autoload 'biblio-hal-lookup "biblio-hal" "Start a HAL search for QUERY, prompting if needed.

(fn &optional QUERY)" t nil) (defalias 'hal-lookup 'biblio-hal-lookup) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "biblio-hal" '("biblio-hal--"))) (autoload 'biblio-ieee-backend "biblio-ieee" "A IEEE Xplore backend for biblio.el.
COMMAND, ARG, MORE: See `biblio-backends'.

(fn COMMAND &optional ARG &rest MORE)" nil nil) (add-hook 'biblio-init-hook #'biblio-ieee-backend) (autoload 'biblio-ieee-lookup "biblio-ieee" "Start a IEEE search for QUERY, prompting if needed.

(fn &optional QUERY)" t nil) (defalias 'ieee-lookup 'biblio-ieee-lookup) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "biblio-ieee" '("biblio-ieee--"))) (provide 'biblio-autoloads)) "bibtex-completion" ((bibtex-completion-autoloads bibtex-completion) (put 'bibtex-completion-notes-global-mode 'globalized-minor-mode t) (defvar bibtex-completion-notes-global-mode nil "Non-nil if Bibtex-Completion-Notes-Global mode is enabled.
See the `bibtex-completion-notes-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `bibtex-completion-notes-global-mode'.") (custom-autoload 'bibtex-completion-notes-global-mode "bibtex-completion" nil) (autoload 'bibtex-completion-notes-global-mode "bibtex-completion" "Toggle Bibtex-Completion-Notes mode in all buffers.
With prefix ARG, enable Bibtex-Completion-Notes-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Bibtex-Completion-Notes mode is enabled in all buffers where
`bibtex-completion-notes-mode' would do it.
See `bibtex-completion-notes-mode' for more information on Bibtex-Completion-Notes mode.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "bibtex-completion" '("bibtex-completion-"))) (provide 'bibtex-completion-autoloads)) "tablist" ((tablist tablist-autoloads tablist-filter) (autoload 'tablist-minor-mode "tablist" "Toggle Tablist minor mode on or off.

If called interactively, enable Tablist minor mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\\{tablist-minor-mode-map}

(fn &optional ARG)" t nil) (autoload 'tablist-mode "tablist" "

(fn)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "tablist" '("tablist-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "tablist-filter" '("tablist-filter-"))) (provide 'tablist-autoloads)) "pdf-tools" ((pdf-tools pdf-virtual pdf-annot pdf-sync pdf-view pdf-loader pdf-util pdf-misc pdf-cache pdf-dev pdf-history pdf-macs pdf-links pdf-info pdf-outline pdf-tools-autoloads pdf-isearch pdf-occur) (autoload 'pdf-annot-minor-mode "pdf-annot" "Support for PDF Annotations.

If called interactively, enable Pdf-Annot minor mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\\{pdf-annot-minor-mode-map}

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pdf-annot" '("pdf-annot-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pdf-cache" '("boundingbox" "define-pdf-cache-function" "page" "pdf-cache-" "textregions"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pdf-dev" '("pdf-dev-"))) (autoload 'pdf-history-minor-mode "pdf-history" "Keep a history of previously visited pages.

If called interactively, enable Pdf-History minor mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

This is a simple stack-based history.  Turning the page or
following a link pushes the left-behind page on the stack, which
may be navigated with the following keys.

\\{pdf-history-minor-mode-map}

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pdf-history" '("pdf-history-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pdf-info" '("pdf-info-"))) (autoload 'pdf-isearch-minor-mode "pdf-isearch" "Isearch mode for PDF buffer.

If called interactively, enable Pdf-Isearch minor mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

When this mode is enabled \\[isearch-forward], among other keys,
starts an incremental search in this PDF document.  Since this mode
uses external programs to highlight found matches via
image-processing, proceeding to the next match may be slow.

Therefore two isearch behaviours have been defined: Normal isearch and
batch mode.  The later one is a minor mode
(`pdf-isearch-batch-mode'), which when activated inhibits isearch
from stopping at and highlighting every single match, but rather
display them batch-wise.  Here a batch means a number of matches
currently visible in the selected window.

The kind of highlighting is determined by three faces
`pdf-isearch-match' (for the current match), `pdf-isearch-lazy'
(for all other matches) and `pdf-isearch-batch' (when in batch
mode), which see.

Colors may also be influenced by the minor-mode
`pdf-view-dark-minor-mode'.  If this is minor mode enabled, each face's
dark colors, are used (see e.g. `frame-background-mode'), instead
of the light ones.

\\{pdf-isearch-minor-mode-map}
While in `isearch-mode' the following keys are available. Note
that not every isearch command work as expected.

\\{pdf-isearch-active-mode-map}

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pdf-isearch" '("pdf-isearch-"))) (autoload 'pdf-links-minor-mode "pdf-links" "Handle links in PDF documents.\\<pdf-links-minor-mode-map>

If called interactively, enable Pdf-Links minor mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

If this mode is enabled, most links in the document may be
activated by clicking on them or by pressing \\[pdf-links-action-perform] and selecting
one of the displayed keys, or by using isearch limited to
links via \\[pdf-links-isearch-link].

\\{pdf-links-minor-mode-map}

(fn &optional ARG)" t nil) (autoload 'pdf-links-action-perform "pdf-links" "Follow LINK, depending on its type.

This may turn to another page, switch to another PDF buffer or
invoke `pdf-links-browse-uri-function'.

Interactively, link is read via `pdf-links-read-link-action'.
This function displays characters around the links in the current
page and starts reading characters (ignoring case).  After a
sufficient number of characters have been read, the corresponding
link's link is invoked.  Additionally, SPC may be used to
scroll the current page.

(fn LINK)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pdf-links" '("pdf-links-"))) (autoload 'pdf-loader-install "pdf-loader" "Prepare Emacs for using PDF Tools.

This function acts as a replacement for `pdf-tools-install' and
makes Emacs load and use PDF Tools as soon as a PDF file is
opened, but not sooner.

The arguments are passed verbatim to `pdf-tools-install', which
see.

(fn &optional NO-QUERY-P SKIP-DEPENDENCIES-P NO-ERROR-P FORCE-DEPENDENCIES-P)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pdf-loader" '("pdf-loader--"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pdf-macs" '("pdf-view-"))) (autoload 'pdf-misc-minor-mode "pdf-misc" "FIXME:  Not documented.

If called interactively, enable Pdf-Misc minor mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (autoload 'pdf-misc-size-indication-minor-mode "pdf-misc" "Provide a working size indication in the mode-line.

If called interactively, enable Pdf-Misc-Size-Indication minor
mode if ARG is positive, and disable it if ARG is zero or
negative.  If called from Lisp, also enable the mode if ARG is
omitted or nil, and toggle it if ARG is `toggle'; disable the
mode otherwise.

(fn &optional ARG)" t nil) (autoload 'pdf-misc-menu-bar-minor-mode "pdf-misc" "Display a PDF Tools menu in the menu-bar.

If called interactively, enable Pdf-Misc-Menu-Bar minor mode if
ARG is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (autoload 'pdf-misc-context-menu-minor-mode "pdf-misc" "Provide a right-click context menu in PDF buffers.

If called interactively, enable Pdf-Misc-Context-Menu minor mode
if ARG is positive, and disable it if ARG is zero or negative.
If called from Lisp, also enable the mode if ARG is omitted or
nil, and toggle it if ARG is `toggle'; disable the mode
otherwise.

\\{pdf-misc-context-menu-minor-mode-map}

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pdf-misc" '("pdf-misc-"))) (autoload 'pdf-occur "pdf-occur" "List lines matching STRING or PCRE.

Interactively search for a regexp. Unless a prefix arg was given,
in which case this functions performs a string search.

If `pdf-occur-prefer-string-search' is non-nil, the meaning of
the prefix-arg is inverted.

(fn STRING &optional REGEXP-P)" t nil) (autoload 'pdf-occur-multi-command "pdf-occur" "Perform `pdf-occur' on multiple buffer.

For a programmatic search of multiple documents see
`pdf-occur-search'." t nil) (defvar pdf-occur-global-minor-mode nil "Non-nil if Pdf-Occur-Global minor mode is enabled.
See the `pdf-occur-global-minor-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `pdf-occur-global-minor-mode'.") (custom-autoload 'pdf-occur-global-minor-mode "pdf-occur" nil) (autoload 'pdf-occur-global-minor-mode "pdf-occur" "Enable integration of Pdf Occur with other modes.

If called interactively, enable Pdf-Occur-Global minor mode if
ARG is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

This global minor mode enables (or disables)
`pdf-occur-ibuffer-minor-mode' and `pdf-occur-dired-minor-mode'
in all current and future ibuffer/dired buffer.

(fn &optional ARG)" t nil) (autoload 'pdf-occur-ibuffer-minor-mode "pdf-occur" "Hack into ibuffer's do-occur binding.

If called interactively, enable Pdf-Occur-Ibuffer minor mode if
ARG is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

This mode remaps `ibuffer-do-occur' to
`pdf-occur-ibuffer-do-occur', which will start the PDF Tools
version of `occur', if all marked buffer's are in `pdf-view-mode'
and otherwise fallback to `ibuffer-do-occur'.

(fn &optional ARG)" t nil) (autoload 'pdf-occur-dired-minor-mode "pdf-occur" "Hack into dired's `dired-do-search' binding.

If called interactively, enable Pdf-Occur-Dired minor mode if ARG
is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

This mode remaps `dired-do-search' to
`pdf-occur-dired-do-search', which will start the PDF Tools
version of `occur', if all marked buffer's are in `pdf-view-mode'
and otherwise fallback to `dired-do-search'.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pdf-occur" '("pdf-occur-"))) (autoload 'pdf-outline-minor-mode "pdf-outline" "Display an outline of a PDF document.

If called interactively, enable Pdf-Outline minor mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

This provides a PDF's outline on the menu bar via imenu.
Additionally the same outline may be viewed in a designated
buffer.

\\{pdf-outline-minor-mode-map}

(fn &optional ARG)" t nil) (autoload 'pdf-outline "pdf-outline" "Display an PDF outline of BUFFER.

BUFFER defaults to the current buffer.  Select the outline
buffer, unless NO-SELECT-WINDOW-P is non-nil.

(fn &optional BUFFER NO-SELECT-WINDOW-P)" t nil) (autoload 'pdf-outline-imenu-enable "pdf-outline" "Enable imenu in the current PDF buffer." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pdf-outline" '("pdf-outline"))) (autoload 'pdf-sync-minor-mode "pdf-sync" "Correlate a PDF position with the TeX file.
\\<pdf-sync-minor-mode-map>
This works via SyncTeX, which means the TeX sources need to have
been compiled with `--synctex=1'.  In AUCTeX this can be done by
setting `TeX-source-correlate-method' to 'synctex (before AUCTeX
is loaded) and enabling `TeX-source-correlate-mode'.

If called interactively, enable Pdf-Sync minor mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

Then \\[pdf-sync-backward-search-mouse] in the PDF buffer will open the
corresponding TeX location.

If AUCTeX is your preferred tex-mode, this library arranges to
bind `pdf-sync-forward-display-pdf-key' (the default is `C-c C-g')
to `pdf-sync-forward-search' in `TeX-source-correlate-map'.  This
function displays the PDF page corresponding to the current
position in the TeX buffer.  This function only works together
with AUCTeX.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pdf-sync" '("pdf-sync-"))) (defvar pdf-tools-handle-upgrades t "Whether PDF Tools should handle upgrading itself.") (custom-autoload 'pdf-tools-handle-upgrades "pdf-tools" t) (autoload 'pdf-tools-install "pdf-tools" "Install PDF-Tools in all current and future PDF buffers.

If the `pdf-info-epdfinfo-program' is not running or does not
appear to be working, attempt to rebuild it.  If this build
succeeded, continue with the activation of the package.
Otherwise fail silently, i.e. no error is signaled.

Build the program (if necessary) without asking first, if
NO-QUERY-P is non-nil.

Don't attempt to install system packages, if SKIP-DEPENDENCIES-P
is non-nil.

Do not signal an error in case the build failed, if NO-ERROR-P is
non-nil.

Attempt to install system packages (even if it is deemed
unnecessary), if FORCE-DEPENDENCIES-P is non-nil.

Note that SKIP-DEPENDENCIES-P and FORCE-DEPENDENCIES-P are
mutually exclusive.

Note further, that you can influence the installation directory
by setting `pdf-info-epdfinfo-program' to an appropriate
value (e.g. ~/bin/epdfinfo) before calling this function.

See `pdf-view-mode' and `pdf-tools-enabled-modes'.

(fn &optional NO-QUERY-P SKIP-DEPENDENCIES-P NO-ERROR-P FORCE-DEPENDENCIES-P)" t nil) (autoload 'pdf-tools-enable-minor-modes "pdf-tools" "Enable MODES in the current buffer.

MODES defaults to `pdf-tools-enabled-modes'.

(fn &optional MODES)" t nil) (autoload 'pdf-tools-help "pdf-tools" nil t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pdf-tools" '("pdf-tools-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pdf-util" '("display-buffer-split-below-and-attach" "pdf-util-"))) (autoload 'pdf-view-bookmark-jump-handler "pdf-view" "The bookmark handler-function interface for bookmark BMK.

See also `pdf-view-bookmark-make-record'.

(fn BMK)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pdf-view" '("pdf-view-"))) (autoload 'pdf-virtual-edit-mode "pdf-virtual" "Major mode when editing a virtual PDF buffer.

(fn)" t nil) (autoload 'pdf-virtual-view-mode "pdf-virtual" "Major mode in virtual PDF buffers.

(fn)" t nil) (defvar pdf-virtual-global-minor-mode nil "Non-nil if Pdf-Virtual-Global minor mode is enabled.
See the `pdf-virtual-global-minor-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `pdf-virtual-global-minor-mode'.") (custom-autoload 'pdf-virtual-global-minor-mode "pdf-virtual" nil) (autoload 'pdf-virtual-global-minor-mode "pdf-virtual" "Enable recognition and handling of VPDF files.

If called interactively, enable Pdf-Virtual-Global minor mode if
ARG is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (autoload 'pdf-virtual-buffer-create "pdf-virtual" "

(fn &optional FILENAMES BUFFER-NAME DISPLAY-P)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pdf-virtual" '("pdf-virtual-"))) (provide 'pdf-tools-autoloads)) "ivy-bibtex" ((ivy-bibtex-autoloads ivy-bibtex) (autoload 'ivy-bibtex "ivy-bibtex" "Search BibTeX entries using ivy.

With a prefix ARG the cache is invalidated and the bibliography
reread.

If LOCAL-BIB is non-nil, display that the BibTeX entries are read
from the local bibliography.  This is set internally by
`ivy-bibtex-with-local-bibliography'.

(fn &optional ARG LOCAL-BIB)" t nil) (autoload 'ivy-bibtex-with-local-bibliography "ivy-bibtex" "Search BibTeX entries with local bibliography.

With a prefix ARG the cache is invalidated and the bibliography
reread.

(fn &optional ARG)" t nil) (autoload 'ivy-bibtex-with-notes "ivy-bibtex" "Search BibTeX entries with notes.

With a prefix ARG the cache is invalidated and the bibliography
reread.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ivy-bibtex" '("ivy-bibtex-"))) (provide 'ivy-bibtex-autoloads)) "undo-tree" ((undo-tree undo-tree-autoloads) (autoload 'undo-tree-mode "undo-tree" "Toggle undo-tree mode.
With no argument, this command toggles the mode.
A positive prefix argument turns the mode on.
A negative prefix argument turns it off.

If called interactively, enable Undo-Tree mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

Undo-tree-mode replaces Emacs' standard undo feature with a more
powerful yet easier to use version, that treats the undo history
as what it is: a tree.

The following keys are available in `undo-tree-mode':

  \\{undo-tree-map}

Within the undo-tree visualizer, the following keys are available:

  \\{undo-tree-visualizer-mode-map}

(fn &optional ARG)" t nil) (put 'global-undo-tree-mode 'globalized-minor-mode t) (defvar global-undo-tree-mode nil "Non-nil if Global Undo-Tree mode is enabled.
See the `global-undo-tree-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-undo-tree-mode'.") (custom-autoload 'global-undo-tree-mode "undo-tree" nil) (autoload 'global-undo-tree-mode "undo-tree" "Toggle Undo-Tree mode in all buffers.
With prefix ARG, enable Global Undo-Tree mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Undo-Tree mode is enabled in all buffers where
`turn-on-undo-tree-mode' would do it.
See `undo-tree-mode' for more information on Undo-Tree mode.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "undo-tree" '("*undo-tree-id-counter*" "buffer-undo-tree" "turn-on-undo-tree-mode" "undo-"))) (provide 'undo-tree-autoloads)) "which-key" ((which-key which-key-autoloads) (defvar which-key-mode nil "Non-nil if Which-Key mode is enabled.
See the `which-key-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `which-key-mode'.") (custom-autoload 'which-key-mode "which-key" nil) (autoload 'which-key-mode "which-key" "Toggle which-key-mode.

If called interactively, enable Which-Key mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (autoload 'which-key-setup-side-window-right "which-key" "Apply suggested settings for side-window that opens on right." t nil) (autoload 'which-key-setup-side-window-right-bottom "which-key" "Apply suggested settings for side-window that opens on right
if there is space and the bottom otherwise." t nil) (autoload 'which-key-setup-side-window-bottom "which-key" "Apply suggested settings for side-window that opens on bottom." t nil) (autoload 'which-key-setup-minibuffer "which-key" "Apply suggested settings for minibuffer.
Do not use this setup if you use the paging commands. Instead use
`which-key-setup-side-window-bottom', which is nearly identical
but more functional." t nil) (autoload 'which-key-add-keymap-based-replacements "which-key" "Replace the description of KEY using REPLACEMENT in KEYMAP.
KEY should take a format suitable for use in `kbd'. REPLACEMENT
should be a cons cell of the form (STRING . COMMAND) for each
REPLACEMENT, where STRING is the replacement string and COMMAND
is a symbol corresponding to the intended command to be
replaced. COMMAND can be nil if the binding corresponds to a key
prefix. An example is

(which-key-add-keymap-based-replacements global-map
  \"C-x w\" '(\"Save as\" . write-file)).

For backwards compatibility, REPLACEMENT can also be a string,
but the above format is preferred, and the option to use a string
for REPLACEMENT will eventually be removed.

(fn KEYMAP KEY REPLACEMENT &rest MORE)" nil nil) (autoload 'which-key-add-key-based-replacements "which-key" "Replace the description of KEY-SEQUENCE with REPLACEMENT.
KEY-SEQUENCE is a string suitable for use in `kbd'. REPLACEMENT
may either be a string, as in

(which-key-add-key-based-replacements \"C-x 1\" \"maximize\")

a cons of two strings as in

(which-key-add-key-based-replacements \"C-x 8\"
                                        '(\"unicode\" . \"Unicode keys\"))

or a function that takes a (KEY . BINDING) cons and returns a
replacement.

In the second case, the second string is used to provide a longer
name for the keys under a prefix.

MORE allows you to specifcy additional KEY REPLACEMENT pairs.  All
replacements are added to `which-key-replacement-alist'.

(fn KEY-SEQUENCE REPLACEMENT &rest MORE)" nil nil) (autoload 'which-key-add-major-mode-key-based-replacements "which-key" "Functions like `which-key-add-key-based-replacements'.
The difference is that MODE specifies the `major-mode' that must
be active for KEY-SEQUENCE and REPLACEMENT (MORE contains
addition KEY-SEQUENCE REPLACEMENT pairs) to apply.

(fn MODE KEY-SEQUENCE REPLACEMENT &rest MORE)" nil nil) (autoload 'which-key-reload-key-sequence "which-key" "Simulate entering the key sequence KEY-SEQ.
KEY-SEQ should be a list of events as produced by
`listify-key-sequence'. If nil, KEY-SEQ defaults to
`which-key--current-key-list'. Any prefix arguments that were
used are reapplied to the new key sequence.

(fn &optional KEY-SEQ)" nil nil) (autoload 'which-key-show-standard-help "which-key" "Call the command in `which-key--prefix-help-cmd-backup'.
Usually this is `describe-prefix-bindings'.

(fn &optional _)" t nil) (autoload 'which-key-show-next-page-no-cycle "which-key" "Show next page of keys unless on the last page, in which case
call `which-key-show-standard-help'." t nil) (autoload 'which-key-show-previous-page-no-cycle "which-key" "Show previous page of keys unless on the first page, in which
case do nothing." t nil) (autoload 'which-key-show-next-page-cycle "which-key" "Show the next page of keys, cycling from end to beginning
after last page.

(fn &optional _)" t nil) (autoload 'which-key-show-previous-page-cycle "which-key" "Show the previous page of keys, cycling from beginning to end
after first page.

(fn &optional _)" t nil) (autoload 'which-key-show-top-level "which-key" "Show top-level bindings.

(fn &optional _)" t nil) (autoload 'which-key-show-major-mode "which-key" "Show top-level bindings in the map of the current major mode.

This function will also detect evil bindings made using
`evil-define-key' in this map. These bindings will depend on the
current evil state. 

(fn &optional ALL)" t nil) (autoload 'which-key-show-full-major-mode "which-key" "Show all bindings in the map of the current major mode.

This function will also detect evil bindings made using
`evil-define-key' in this map. These bindings will depend on the
current evil state. " t nil) (autoload 'which-key-dump-bindings "which-key" "Dump bindings from PREFIX into buffer named BUFFER-NAME.

PREFIX should be a string suitable for `kbd'.

(fn PREFIX BUFFER-NAME)" t nil) (autoload 'which-key-undo-key "which-key" "Undo last keypress and force which-key update.

(fn &optional _)" t nil) (autoload 'which-key-C-h-dispatch "which-key" "Dispatch C-h commands by looking up key in
`which-key-C-h-map'. This command is always accessible (from any
prefix) if `which-key-use-C-h-commands' is non nil." t nil) (autoload 'which-key-show-keymap "which-key" "Show the top-level bindings in KEYMAP using which-key.
KEYMAP is selected interactively from all available keymaps.

If NO-PAGING is non-nil, which-key will not intercept subsequent
keypresses for the paging functionality.

(fn KEYMAP &optional NO-PAGING)" t nil) (autoload 'which-key-show-full-keymap "which-key" "Show all bindings in KEYMAP using which-key.
KEYMAP is selected interactively from all available keymaps.

(fn KEYMAP)" t nil) (autoload 'which-key-show-minor-mode-keymap "which-key" "Show the top-level bindings in KEYMAP using which-key.
KEYMAP is selected interactively by mode in
`minor-mode-map-alist'.

(fn &optional ALL)" t nil) (autoload 'which-key-show-full-minor-mode-keymap "which-key" "Show all bindings in KEYMAP using which-key.
KEYMAP is selected interactively by mode in
`minor-mode-map-alist'." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "which-key" '("evil-state" "which-key-"))) (provide 'which-key-autoloads)) "emacs-async" ((dired-async smtpmail-async async async-bytecomp emacs-async-autoloads) (autoload 'async-start-process "async" "Start the executable PROGRAM asynchronously named NAME.  See `async-start'.
PROGRAM is passed PROGRAM-ARGS, calling FINISH-FUNC with the
process object when done.  If FINISH-FUNC is nil, the future
object will return the process object when the program is
finished.  Set DEFAULT-DIRECTORY to change PROGRAM's current
working directory.

(fn NAME PROGRAM FINISH-FUNC &rest PROGRAM-ARGS)" nil nil) (autoload 'async-start "async" "Execute START-FUNC (often a lambda) in a subordinate Emacs process.
When done, the return value is passed to FINISH-FUNC.  Example:

    (async-start
       ;; What to do in the child process
       (lambda ()
         (message \"This is a test\")
         (sleep-for 3)
         222)

       ;; What to do when it finishes
       (lambda (result)
         (message \"Async process done, result should be 222: %s\"
                  result)))

If FINISH-FUNC is nil or missing, a future is returned that can
be inspected using `async-get', blocking until the value is
ready.  Example:

    (let ((proc (async-start
                   ;; What to do in the child process
                   (lambda ()
                     (message \"This is a test\")
                     (sleep-for 3)
                     222))))

        (message \"I'm going to do some work here\") ;; ....

        (message \"Waiting on async process, result should be 222: %s\"
                 (async-get proc)))

If you don't want to use a callback, and you don't care about any
return value from the child process, pass the `ignore' symbol as
the second argument (if you don't, and never call `async-get', it
will leave *emacs* process buffers hanging around):

    (async-start
     (lambda ()
       (delete-file \"a remote file on a slow link\" nil))
     \\='ignore)

Special case:
If the output of START-FUNC is a string with properties
e.g. (buffer-string) RESULT will be transformed in a list where the
car is the string itself (without props) and the cdr the rest of
properties, this allows using in FINISH-FUNC the string without
properties and then apply the properties in cdr to this string (if
needed).
Properties handling special objects like markers are returned as
list to allow restoring them later.
See <https://github.com/jwiegley/emacs-async/issues/145> for more infos.

Note: Even when FINISH-FUNC is present, a future is still
returned except that it yields no value (since the value is
passed to FINISH-FUNC).  Call `async-get' on such a future always
returns nil.  It can still be useful, however, as an argument to
`async-ready' or `async-wait'.

(fn START-FUNC &optional FINISH-FUNC)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "async" '("async-"))) (autoload 'async-byte-recompile-directory "async-bytecomp" "Compile all *.el files in DIRECTORY asynchronously.
All *.elc files are systematically deleted before proceeding.

(fn DIRECTORY &optional QUIET)" nil nil) (defvar async-bytecomp-package-mode nil "Non-nil if Async-Bytecomp-Package mode is enabled.
See the `async-bytecomp-package-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `async-bytecomp-package-mode'.") (custom-autoload 'async-bytecomp-package-mode "async-bytecomp" nil) (autoload 'async-bytecomp-package-mode "async-bytecomp" "Byte compile asynchronously packages installed with package.el.
Async compilation of packages can be controlled by
`async-bytecomp-allowed-packages'.

If called interactively, enable Async-Bytecomp-Package mode if
ARG is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (autoload 'async-byte-compile-file "async-bytecomp" "Byte compile Lisp code FILE asynchronously.

Same as `byte-compile-file' but asynchronous.

(fn FILE)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "async-bytecomp" '("async-"))) (defvar dired-async-mode nil "Non-nil if Dired-Async mode is enabled.
See the `dired-async-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `dired-async-mode'.") (custom-autoload 'dired-async-mode "dired-async" nil) (autoload 'dired-async-mode "dired-async" "Do dired actions asynchronously.

If called interactively, enable Dired-Async mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (autoload 'dired-async-do-copy "dired-async" "Run \342\200\230dired-do-copy\342\200\231 asynchronously.

(fn &optional ARG)" t nil) (autoload 'dired-async-do-symlink "dired-async" "Run \342\200\230dired-do-symlink\342\200\231 asynchronously.

(fn &optional ARG)" t nil) (autoload 'dired-async-do-hardlink "dired-async" "Run \342\200\230dired-do-hardlink\342\200\231 asynchronously.

(fn &optional ARG)" t nil) (autoload 'dired-async-do-rename "dired-async" "Run \342\200\230dired-do-rename\342\200\231 asynchronously.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dired-async" '("dired-async-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smtpmail-async" '("async-smtpmail-"))) (provide 'emacs-async-autoloads)) "htmlize" ((htmlize-autoloads htmlize) (autoload 'htmlize-buffer "htmlize" "Convert BUFFER to HTML, preserving colors and decorations.

The generated HTML is available in a new buffer, which is returned.
When invoked interactively, the new buffer is selected in the current
window.  The title of the generated document will be set to the buffer's
file name or, if that's not available, to the buffer's name.

Note that htmlize doesn't fontify your buffers, it only uses the
decorations that are already present.  If you don't set up font-lock or
something else to fontify your buffers, the resulting HTML will be
plain.  Likewise, if you don't like the choice of colors, fix the mode
that created them, or simply alter the faces it uses.

(fn &optional BUFFER)" t nil) (autoload 'htmlize-region "htmlize" "Convert the region to HTML, preserving colors and decorations.
See `htmlize-buffer' for details.

(fn BEG END)" t nil) (autoload 'htmlize-file "htmlize" "Load FILE, fontify it, convert it to HTML, and save the result.

Contents of FILE are inserted into a temporary buffer, whose major mode
is set with `normal-mode' as appropriate for the file type.  The buffer
is subsequently fontified with `font-lock' and converted to HTML.  Note
that, unlike `htmlize-buffer', this function explicitly turns on
font-lock.  If a form of highlighting other than font-lock is desired,
please use `htmlize-buffer' directly on buffers so highlighted.

Buffers currently visiting FILE are unaffected by this function.  The
function does not change current buffer or move the point.

If TARGET is specified and names a directory, the resulting file will be
saved there instead of to FILE's directory.  If TARGET is specified and
does not name a directory, it will be used as output file name.

(fn FILE &optional TARGET)" t nil) (autoload 'htmlize-many-files "htmlize" "Convert FILES to HTML and save the corresponding HTML versions.

FILES should be a list of file names to convert.  This function calls
`htmlize-file' on each file; see that function for details.  When
invoked interactively, you are prompted for a list of files to convert,
terminated with RET.

If TARGET-DIRECTORY is specified, the HTML files will be saved to that
directory.  Normally, each HTML file is saved to the directory of the
corresponding source file.

(fn FILES &optional TARGET-DIRECTORY)" t nil) (autoload 'htmlize-many-files-dired "htmlize" "HTMLize dired-marked files.

(fn ARG &optional TARGET-DIRECTORY)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "htmlize" '("htmlize-"))) (provide 'htmlize-autoloads)) "async" ((dired-async async-autoloads smtpmail-async async async-bytecomp) (autoload 'async-start-process "async" "Start the executable PROGRAM asynchronously named NAME.  See `async-start'.
PROGRAM is passed PROGRAM-ARGS, calling FINISH-FUNC with the
process object when done.  If FINISH-FUNC is nil, the future
object will return the process object when the program is
finished.  Set DEFAULT-DIRECTORY to change PROGRAM's current
working directory.

(fn NAME PROGRAM FINISH-FUNC &rest PROGRAM-ARGS)" nil nil) (autoload 'async-start "async" "Execute START-FUNC (often a lambda) in a subordinate Emacs process.
When done, the return value is passed to FINISH-FUNC.  Example:

    (async-start
       ;; What to do in the child process
       (lambda ()
         (message \"This is a test\")
         (sleep-for 3)
         222)

       ;; What to do when it finishes
       (lambda (result)
         (message \"Async process done, result should be 222: %s\"
                  result)))

If FINISH-FUNC is nil or missing, a future is returned that can
be inspected using `async-get', blocking until the value is
ready.  Example:

    (let ((proc (async-start
                   ;; What to do in the child process
                   (lambda ()
                     (message \"This is a test\")
                     (sleep-for 3)
                     222))))

        (message \"I'm going to do some work here\") ;; ....

        (message \"Waiting on async process, result should be 222: %s\"
                 (async-get proc)))

If you don't want to use a callback, and you don't care about any
return value from the child process, pass the `ignore' symbol as
the second argument (if you don't, and never call `async-get', it
will leave *emacs* process buffers hanging around):

    (async-start
     (lambda ()
       (delete-file \"a remote file on a slow link\" nil))
     \\='ignore)

Special case:
If the output of START-FUNC is a string with properties
e.g. (buffer-string) RESULT will be transformed in a list where the
car is the string itself (without props) and the cdr the rest of
properties, this allows using in FINISH-FUNC the string without
properties and then apply the properties in cdr to this string (if
needed).
Properties handling special objects like markers are returned as
list to allow restoring them later.
See <https://github.com/jwiegley/emacs-async/issues/145> for more infos.

Note: Even when FINISH-FUNC is present, a future is still
returned except that it yields no value (since the value is
passed to FINISH-FUNC).  Call `async-get' on such a future always
returns nil.  It can still be useful, however, as an argument to
`async-ready' or `async-wait'.

(fn START-FUNC &optional FINISH-FUNC)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "async" '("async-"))) (autoload 'async-byte-recompile-directory "async-bytecomp" "Compile all *.el files in DIRECTORY asynchronously.
All *.elc files are systematically deleted before proceeding.

(fn DIRECTORY &optional QUIET)" nil nil) (defvar async-bytecomp-package-mode nil "Non-nil if Async-Bytecomp-Package mode is enabled.
See the `async-bytecomp-package-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `async-bytecomp-package-mode'.") (custom-autoload 'async-bytecomp-package-mode "async-bytecomp" nil) (autoload 'async-bytecomp-package-mode "async-bytecomp" "Byte compile asynchronously packages installed with package.el.
Async compilation of packages can be controlled by
`async-bytecomp-allowed-packages'.

If called interactively, enable Async-Bytecomp-Package mode if
ARG is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (autoload 'async-byte-compile-file "async-bytecomp" "Byte compile Lisp code FILE asynchronously.

Same as `byte-compile-file' but asynchronous.

(fn FILE)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "async-bytecomp" '("async-"))) (defvar dired-async-mode nil "Non-nil if Dired-Async mode is enabled.
See the `dired-async-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `dired-async-mode'.") (custom-autoload 'dired-async-mode "dired-async" nil) (autoload 'dired-async-mode "dired-async" "Do dired actions asynchronously.

If called interactively, enable Dired-Async mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (autoload 'dired-async-do-copy "dired-async" "Run \342\200\230dired-do-copy\342\200\231 asynchronously.

(fn &optional ARG)" t nil) (autoload 'dired-async-do-symlink "dired-async" "Run \342\200\230dired-do-symlink\342\200\231 asynchronously.

(fn &optional ARG)" t nil) (autoload 'dired-async-do-hardlink "dired-async" "Run \342\200\230dired-do-hardlink\342\200\231 asynchronously.

(fn &optional ARG)" t nil) (autoload 'dired-async-do-rename "dired-async" "Run \342\200\230dired-do-rename\342\200\231 asynchronously.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dired-async" '("dired-async-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smtpmail-async" '("async-smtpmail-"))) (provide 'async-autoloads)) "popup" ((popup popup-autoloads) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "popup" '("popup-"))) (provide 'popup-autoloads)) "helm-core" ((helm-core-autoloads helm-lib helm helm-multi-match helm-core-pkg helm-source) (autoload 'helm-configuration "helm" "Customize Helm." t nil) (autoload 'helm-define-multi-key "helm" "In KEYMAP, define key sequence KEY for function list FUNCTIONS.
Each function runs sequentially for each KEY press.
If DELAY is specified, switch back to initial function of FUNCTIONS list
after DELAY seconds.
The functions in FUNCTIONS list take no args.
E.g.
    (defun foo ()
      (interactive)
      (message \"Run foo\"))
    (defun bar ()
      (interactive)
      (message \"Run bar\"))
    (defun baz ()
      (interactive)
      (message \"Run baz\"))

(helm-define-multi-key global-map (kbd \"<f5> q\") '(foo bar baz) 2)

Each time \"<f5> q\" is pressed, the next function is executed.
Waiting more than 2 seconds between key presses switches back to
executing the first function on the next hit.

(fn KEYMAP KEY FUNCTIONS &optional DELAY)" nil nil) (autoload 'helm-multi-key-defun "helm" "Define NAME as a multi-key command running FUNS.
After DELAY seconds, the FUNS list is reinitialized.
See `helm-define-multi-key'.

(fn NAME DOCSTRING FUNS &optional DELAY)" nil t) (function-put 'helm-multi-key-defun 'lisp-indent-function '2) (autoload 'helm-define-key-with-subkeys "helm" "Define in MAP a KEY and SUBKEY to COMMAND.

This allows typing KEY to call COMMAND the first time and
type only SUBKEY on subsequent calls.

Arg MAP is the keymap to use, SUBKEY is the initial short
key binding to call COMMAND.

Arg OTHER-SUBKEYS is an alist specifying other short key bindings
to use once started, e.g.:

    (helm-define-key-with-subkeys global-map
       (kbd \"C-x v n\") ?n 'git-gutter:next-hunk
       '((?p . git-gutter:previous-hunk)))

In this example, `C-x v n' will run `git-gutter:next-hunk'
subsequent \"n\" will run this command again and subsequent \"p\"
will run `git-gutter:previous-hunk'.

If specified PROMPT can be displayed in minibuffer to describe
SUBKEY and OTHER-SUBKEYS.  Arg EXIT-FN specifies a function to run
on exit.

For any other key pressed, run their assigned command as defined
in MAP and then exit the loop running EXIT-FN, if specified.

If DELAY an integer is specified exit after DELAY seconds.

NOTE: SUBKEY and OTHER-SUBKEYS bindings support only char syntax
and vectors, so don't use strings to define them.

(fn MAP KEY SUBKEY COMMAND &optional OTHER-SUBKEYS PROMPT EXIT-FN DELAY)" nil nil) (function-put 'helm-define-key-with-subkeys 'lisp-indent-function '1) (autoload 'helm-debug-open-last-log "helm" "Open Helm log file or buffer of last Helm session." t nil) (autoload 'helm "helm" "Main function to execute helm sources.

PLIST is a list like

(:key1 val1 :key2 val2 ...)

 or

(&optional sources input prompt resume preselect
            buffer keymap default history allow-nest).

** Keywords

Keywords supported:

- :sources
- :input
- :prompt
- :resume
- :preselect
- :buffer
- :keymap
- :default
- :history
- :allow-nest

Extra LOCAL-VARS keywords are supported, see the \"** Other
keywords\" section below.

Basic keywords are the following:

*** :sources

One of the following:

- List of sources
- Symbol whose value is a list of sources
- Alist representing a Helm source.
  - In this case the source has no name and is referenced in
    `helm-sources' as a whole alist.

*** :input

Initial input of minibuffer (temporary value of `helm-pattern')

*** :prompt

Minibuffer prompt. Default value is `helm--prompt'.

*** :resume

If t, allow resumption of the previous session of this Helm
command, skipping initialization.

If 'noresume, this instance of `helm' cannot be resumed.

*** :preselect

Initially selected candidate (string or regexp).

*** :buffer

Buffer name for this Helm session. `helm-buffer' will take this value.

*** :keymap

[Obsolete]

Keymap used at the start of this Helm session.

It is overridden by keymaps specified in sources, and is kept
only for backward compatibility.

Keymaps should be specified in sources using the :keymap slot
instead. See `helm-source'.

This keymap is not restored by `helm-resume'.

*** :default

Default value inserted into the minibuffer with
\\<minibuffer-local-map>\\[next-history-element].

It can be a string or a list of strings, in this case
\\<minibuffer-local-map>\\[next-history-element] cycles through
the list items, starting with the first.

If nil, `thing-at-point' is used.

If `helm-maybe-use-default-as-input' is non-nil, display is
updated using this value if this value matches, otherwise it is
ignored. If :input is specified, it takes precedence on :default.

*** :history

Minibuffer input, by default, is pushed to `minibuffer-history'.

When an argument HISTORY is provided, input is pushed to
HISTORY. HISTORY should be a valid symbol.

*** :allow-nest

Allow running this Helm command in a running Helm session.

** Other keywords

Other keywords are interpreted as local variables of this Helm
session. The `helm-' prefix can be omitted. For example,

(helm :sources 'helm-source-buffers-list
       :buffer \"*helm buffers*\"
       :candidate-number-limit 10)

Starts a Helm session with the variable
`helm-candidate-number-limit' set to 10.

** Backward compatibility

For backward compatibility, positional parameters are
supported:

(helm sources input prompt resume preselect
       buffer keymap default history allow-nest)

However, the use of non-keyword args is deprecated.

(fn &key SOURCES INPUT PROMPT RESUME PRESELECT BUFFER KEYMAP DEFAULT HISTORY ALLOW-NEST OTHER-LOCAL-VARS)" nil nil) (autoload 'helm-cycle-resume "helm" "Cycle in `helm-buffers' list and resume when waiting more than 1.2s." t nil) (autoload 'helm-other-buffer "helm" "Simplified Helm interface with other `helm-buffer'.
Call `helm' only with SOURCES and BUFFER as args.

(fn SOURCES BUFFER)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm" '("helm-" "with-helm-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-lib" '("helm-" "with-helm-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-multi-match" '("helm-m"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-source" '("helm-"))) (provide 'helm-core-autoloads)) "helm" ((helm-elisp-package helm-misc helm-utils helm-tags helm-net helm-imenu helm-autoloads helm-fd helm-dabbrev helm-pkg helm-help helm-ring helm-shell helm-color helm-command helm-info helm-font helm-regexp helm-mode helm-find helm-config helm-types helm-adaptive helm-man helm-bookmark helm-for-files helm-grep \.dir-locals helm-external helm-files helm-elisp helm-easymenu helm-eval helm-buffers helm-comint helm-semantic helm-x-files helm-eshell helm-global-bindings helm-occur helm-locate helm-epa helm-sys helm-id-utils) (defvar helm-adaptive-mode nil "Non-nil if Helm-Adaptive mode is enabled.
See the `helm-adaptive-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `helm-adaptive-mode'.") (custom-autoload 'helm-adaptive-mode "helm-adaptive" nil) (autoload 'helm-adaptive-mode "helm-adaptive" "Toggle adaptive sorting in all sources.

If called interactively, enable Helm-Adaptive mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (autoload 'helm-reset-adaptive-history "helm-adaptive" "Delete all `helm-adaptive-history' and his file.
Useful when you have a old or corrupted
`helm-adaptive-history-file'." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-adaptive" '("helm-adapt"))) (autoload 'helm-bookmarks "helm-bookmark" "Preconfigured `helm' for bookmarks." t nil) (autoload 'helm-filtered-bookmarks "helm-bookmark" "Preconfigured `helm' for bookmarks (filtered by category).
Optional source `helm-source-bookmark-addressbook' is loaded only
if external addressbook-bookmark package is installed." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-bookmark" '("bmkext-jump-" "bookmark" "helm-"))) (autoload 'helm-buffers-list "helm-buffers" "Preconfigured `helm' to list buffers." t nil) (autoload 'helm-mini "helm-buffers" "Preconfigured `helm' displaying `helm-mini-default-sources'." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-buffers" '("helm-"))) (autoload 'helm-colors "helm-color" "Preconfigured `helm' for color." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-color" '("helm-"))) (autoload 'helm-comint-prompts "helm-comint" "Pre-configured `helm' to browse the prompts of the current comint buffer." t nil) (autoload 'helm-comint-prompts-all "helm-comint" "Pre-configured `helm' to browse the prompts of all comint sessions." t nil) (autoload 'helm-comint-input-ring "helm-comint" "Preconfigured `helm' that provide completion of `comint' history." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-comint" '("helm-"))) (autoload 'helm-M-x "helm-command" "Preconfigured `helm' for Emacs commands.
It is `helm' replacement of regular `M-x'
`execute-extended-command'.

Unlike regular `M-x' Emacs vanilla `execute-extended-command'
command, the prefix args if needed, can be passed AFTER starting
`helm-M-x'.  When a prefix arg is passed BEFORE starting
`helm-M-x', the first `C-u' while in `helm-M-x' session will
disable it.

You can get help on each command by persistent action.

(fn ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-command" '("helm-"))) (autoload 'helm-dabbrev "helm-dabbrev" "Preconfigured helm for dynamic abbreviations." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-dabbrev" '("helm-dabbrev-"))) (autoload 'helm-lisp-completion-at-point "helm-elisp" "Preconfigured Helm for Lisp symbol completion at point." t nil) (autoload 'helm-complete-file-name-at-point "helm-elisp" "Preconfigured Helm to complete file name at point.

(fn &optional FORCE)" t nil) (autoload 'helm-lisp-indent "helm-elisp" nil t nil) (autoload 'helm-lisp-completion-or-file-name-at-point "helm-elisp" "Preconfigured Helm to complete Lisp symbol or filename at point.
Filename completion happens if string start after or between a
double quote." t nil) (autoload 'helm-apropos "helm-elisp" "Preconfigured Helm to describe commands, functions, variables and faces.
In non interactives calls DEFAULT argument should be provided as
a string, i.e. the `symbol-name' of any existing symbol.

(fn DEFAULT)" t nil) (autoload 'helm-manage-advice "helm-elisp" "Preconfigured `helm' to disable/enable function advices." t nil) (autoload 'helm-locate-library "helm-elisp" "Preconfigured helm to locate elisp libraries." t nil) (autoload 'helm-timers "helm-elisp" "Preconfigured `helm' for timers." t nil) (autoload 'helm-complex-command-history "helm-elisp" "Preconfigured `helm' for complex command history." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-elisp" '("helm-" "with-helm-show-completion"))) (autoload 'helm-list-elisp-packages "helm-elisp-package" "Preconfigured `helm' for listing and handling Emacs packages.

(fn ARG)" t nil) (autoload 'helm-list-elisp-packages-no-fetch "helm-elisp-package" "Preconfigured Helm for Emacs packages.

Same as `helm-list-elisp-packages' but don't fetch packages on
remote.  Called with a prefix ARG always fetch packages on
remote.

(fn ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-elisp-package" '("helm-"))) (defvar helm-epa-mode nil "Non-nil if Helm-Epa mode is enabled.
See the `helm-epa-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `helm-epa-mode'.") (custom-autoload 'helm-epa-mode "helm-epa" nil) (autoload 'helm-epa-mode "helm-epa" "Enable helm completion on gpg keys in epa functions.

If called interactively, enable Helm-Epa mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (autoload 'helm-epa-list-keys "helm-epa" "List all gpg keys.
This is the helm interface for `epa-list-keys'." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-epa" '("helm-epa"))) (autoload 'helm-esh-pcomplete "helm-eshell" "Preconfigured `helm' to provide Helm completion in Eshell." t nil) (autoload 'helm-eshell-history "helm-eshell" "Preconfigured Helm for Eshell history." t nil) (autoload 'helm-eshell-prompts "helm-eshell" "Pre-configured `helm' to browse the prompts of the current Eshell." t nil) (autoload 'helm-eshell-prompts-all "helm-eshell" "Pre-configured `helm' to browse the prompts of all Eshell sessions." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-eshell" '("helm-e"))) (autoload 'helm-eval-expression "helm-eval" "Preconfigured `helm' for `helm-source-evaluation-result'.

(fn ARG)" t nil) (autoload 'helm-eval-expression-with-eldoc "helm-eval" "Preconfigured `helm' for `helm-source-evaluation-result' with `eldoc' support." t nil) (autoload 'helm-calcul-expression "helm-eval" "Preconfigured `helm' for `helm-source-calculation-result'." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-eval" '("helm-"))) (autoload 'helm-run-external-command "helm-external" "Preconfigured `helm' to run External PROGRAM asyncronously from Emacs.
If program is already running exit with error.
You can set your own list of commands with
`helm-external-commands-list'.

(fn PROGRAM)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-external" '("helm-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-fd" '("helm-fd-"))) (autoload 'helm-projects-history "helm-files" "

(fn ARG)" t nil) (autoload 'helm-browse-project "helm-files" "Preconfigured helm to browse projects.
Browse files and see status of project with its VCS.
Only HG and GIT are supported for now.
Fall back to `helm-browse-project-find-files' if current
directory is not under control of one of those VCS.
With a prefix ARG browse files recursively, with two prefix ARG
rebuild the cache.
If the current directory is found in the cache, start
`helm-browse-project-find-files' even with no prefix ARG.
NOTE: The prefix ARG have no effect on the VCS controlled
directories.

Needed dependencies for VCS:
<https://github.com/emacs-helm/helm-ls-git>
and
<https://github.com/emacs-helm/helm-ls-hg>.

(fn ARG)" t nil) (autoload 'helm-find-files "helm-files" "Preconfigured `helm' for helm implementation of `find-file'.
Called with a prefix arg show history if some.
Don't call it from programs, use `helm-find-files-1' instead.
This is the starting point for nearly all actions you can do on
files.

(fn ARG)" t nil) (autoload 'helm-delete-tramp-connection "helm-files" "Allow deleting tramp connection or marked tramp connections at once.

This replace `tramp-cleanup-connection' which is partially broken
in Emacs < to 25.1.50.1 (See Emacs bug http://debbugs.gnu.org/cgi/bugreport.cgi?bug=24432).

It allows additionally to delete more than one connection at
once." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-files" '("eshell-command-aliases-list" "helm-"))) (autoload 'helm-find "helm-find" "Preconfigured `helm' for the find shell command.

Recursively find files whose names are matched by all specified
globbing PATTERNs under the current directory using the external
program specified in `find-program' (usually \"find\").  Every
input PATTERN is silently wrapped into two stars: *PATTERN*.

With prefix argument, prompt for a directory to search.

When user option `helm-findutils-search-full-path' is non-nil,
match against complete paths, otherwise, against file names
without directory part.

The (possibly empty) list of globbing PATTERNs can be followed by
the separator \"*\" plus any number of additional arguments that
are passed to \"find\" literally.

(fn ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-find" '("helm-"))) (autoload 'helm-select-xfont "helm-font" "Preconfigured `helm' to select Xfont." t nil) (autoload 'helm-ucs "helm-font" "Preconfigured `helm' for `ucs-names'.

Called with a prefix arg force reloading cache.

(fn ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-font" '("helm-"))) (autoload 'helm-for-files "helm-for-files" "Preconfigured `helm' for opening files.
Run all sources defined in `helm-for-files-preferred-list'." t nil) (autoload 'helm-multi-files "helm-for-files" "Preconfigured helm like `helm-for-files' but running locate only on demand.

Allow toggling back and forth from locate to others sources with
`helm-multi-files-toggle-locate-binding' key.
This avoids launching locate needlessly when what you are
searching for is already found." t nil) (autoload 'helm-recentf "helm-for-files" "Preconfigured `helm' for `recentf'." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-for-files" '("helm-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-global-bindings" '("helm-"))) (autoload 'helm-goto-precedent-file "helm-grep" "Go to previous file in Helm grep/etags buffers." t nil) (autoload 'helm-goto-next-file "helm-grep" "Go to previous file in Helm grep/etags buffers." t nil) (autoload 'helm-do-grep-ag "helm-grep" "Preconfigured `helm' for grepping with AG in `default-directory'.
With prefix arg prompt for type if available with your AG
version.

(fn ARG)" t nil) (autoload 'helm-grep-do-git-grep "helm-grep" "Preconfigured `helm' for git-grepping `default-directory'.
With a prefix arg ARG git-grep the whole repository.

(fn ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-grep" '("helm-"))) (autoload 'helm-documentation "helm-help" "Preconfigured `helm' for Helm documentation.
With a prefix arg refresh the documentation.

Find here the documentation of all documented sources." t nil) (defvar helm-comp-read-mode-line "\\<helm-comp-read-map>C/\\[helm-cr-empty-string]:Empty \\<helm-map>\\[helm-help]:Help \\[helm-select-action]:Act \\[helm-maybe-exit-minibuffer]/f1/f2/f-n:NthAct \\[helm-toggle-suspend-update]:Tog.suspend \\[helm-customize-group]:Conf") (defvar helm-read-file-name-mode-line-string "\\<helm-read-file-map>\\[helm-help]:Help C/\\[helm-cr-empty-string]:Empty \\<helm-map>\\[helm-select-action]:Act \\[helm-maybe-exit-minibuffer]/f1/f2/f-n:NthAct \\[helm-toggle-suspend-update]:Tog.suspend \\[helm-customize-group]:Conf" "String displayed in mode-line in `helm-source-find-files'.") (defvar helm-top-mode-line "\\<helm-top-map>\\[helm-help]:Help \\<helm-map>\\[helm-select-action]:Act \\[helm-maybe-exit-minibuffer]/f1/f2/f-n:NthAct \\[helm-toggle-suspend-update]:Tog.suspend \\[helm-customize-group]:Conf") (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-help" '("helm-"))) (autoload 'helm-gid "helm-id-utils" "Preconfigured `helm' for `gid' command line of `ID-Utils'.
Need A database created with the command `mkid' above
`default-directory'.
Need id-utils as dependency which provide `mkid', `gid' etc..
See <https://www.gnu.org/software/idutils/>." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-id-utils" '("helm-gid-"))) (autoload 'helm-imenu "helm-imenu" "Preconfigured `helm' for `imenu'." t nil) (autoload 'helm-imenu-in-all-buffers "helm-imenu" "Preconfigured `helm' for fetching imenu entries in all buffers with similar mode as current.
A mode is similar as current if it is the same, it is derived
i.e. `derived-mode-p' or it have an association in
`helm-imenu-all-buffer-assoc'." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-imenu" '("helm-"))) (autoload 'helm-info "helm-info" "Preconfigured `helm' for searching Info files' indices.

With a prefix argument \\[universal-argument], set REFRESH to
non-nil.

Optional parameter REFRESH, when non-nil, re-evaluates
`helm-default-info-index-list'.  If the variable has been
customized, set it to its saved value.  If not, set it to its
standard value. See `custom-reevaluate-setting' for more.

REFRESH is useful when new Info files are installed.  If
`helm-default-info-index-list' has not been customized, the new
Info files are made available.

(fn &optional REFRESH)" t nil) (autoload 'helm-info-at-point "helm-info" "Preconfigured `helm' for searching info at point." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-info" '("helm-"))) (autoload 'helm-projects-find-files "helm-locate" "Find files with locate in `helm-locate-project-list'.
With a prefix arg refresh the database in each project.

(fn UPDATE)" t nil) (autoload 'helm-locate "helm-locate" "Preconfigured `helm' for Locate.
Note: you can add locate options after entering pattern.
See 'man locate' for valid options and also `helm-locate-command'.

You can specify a local database with prefix argument ARG.
With two prefix arg, refresh the current local db or create it if
it doesn't exists.

To create a user specific db, use
\"updatedb -l 0 -o db_path -U directory\".
Where db_path is a filename matched by
`helm-locate-db-file-regexp'.

(fn ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-locate" '("helm-"))) (autoload 'helm-man-woman "helm-man" "Preconfigured `helm' for Man and Woman pages.
With a prefix arg reinitialize the cache.

(fn ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-man" '("helm-"))) (autoload 'helm-world-time "helm-misc" "Preconfigured `helm' to show world time.
Default action change TZ environment variable locally to emacs." t nil) (autoload 'helm-insert-latex-math "helm-misc" "Preconfigured helm for latex math symbols completion." t nil) (autoload 'helm-ratpoison-commands "helm-misc" "Preconfigured `helm' to execute ratpoison commands." t nil) (autoload 'helm-stumpwm-commands "helm-misc" "Preconfigured helm for stumpwm commands." t nil) (autoload 'helm-minibuffer-history "helm-misc" "Preconfigured `helm' for `minibuffer-history'." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-misc" '("helm-"))) (autoload 'helm-comp-read "helm-mode" "Read a string in the minibuffer, with helm completion.

It is helm `completing-read' equivalent.

- PROMPT is the prompt name to use.

- COLLECTION can be a list, alist, vector, obarray or hash-table.
  For alists and hash-tables their car are use as real value of
  candidate unless ALISTP is non-nil.
  It can be also a function that receives three arguments:
  the values string, predicate and t. See `all-completions' for more details.

Keys description:

- TEST: A predicate called with one arg i.e candidate.

- INITIAL-INPUT: Same as input arg in `helm'.

- PRESELECT: See preselect arg of `helm'.

- DEFAULT: This option is used only for compatibility with regular
  Emacs `completing-read' (Same as DEFAULT arg of `completing-read').

- BUFFER: Name of helm-buffer.

- MUST-MATCH: Candidate selected must be one of COLLECTION.

- FUZZY: Enable fuzzy matching.

- REVERSE-HISTORY: When non--nil display history source after current
  source completion.

- REQUIRES-PATTERN: Same as helm attribute, default is 0.

- HISTORY: A list containing specific history, default is nil.
  When it is non--nil, all elements of HISTORY are displayed in
  a special source before COLLECTION.

- INPUT-HISTORY: A symbol. The minibuffer input history will be
  stored there, if nil or not provided, `minibuffer-history'
  will be used instead.

- CASE-FOLD: Same as `helm-case-fold-search'.

- DEL-INPUT: Boolean, when non--nil (default) remove the partial
  minibuffer input from HISTORY is present.

- PERSISTENT-ACTION: A function called with one arg i.e candidate.

- PERSISTENT-HELP: A string to document PERSISTENT-ACTION.

- MODE-LINE: A string or list to display in mode line.
  Default is `helm-comp-read-mode-line'.

- KEYMAP: A keymap to use in this `helm-comp-read'.
  (the keymap will be shared with history source)

- NAME: The name related to this local source.

- HEADER-NAME: A function to alter NAME, see `helm'.

- EXEC-WHEN-ONLY-ONE: Bound `helm-execute-action-at-once-if-one'
  to non--nil. (possibles values are t or nil).

- VOLATILE: Use volatile attribute.

- SORT: A predicate to give to `sort' e.g `string-lessp'
  Use this only on small data as it is inefficient.
  If you want to sort faster add a sort function to
  FC-TRANSFORMER.
  Note that FUZZY when enabled is already providing a sort function.

- FC-TRANSFORMER: A `filtered-candidate-transformer' function
  or a list of functions.

- HIST-FC-TRANSFORMER: A `filtered-candidate-transformer'
  function for the history source.

- MARKED-CANDIDATES: If non--nil return candidate or marked candidates as a list.

- NOMARK: When non--nil don't allow marking candidates.

- ALISTP:
  When non-nil (default) pass the value of (DISPLAY . REAL)
  candidate in COLLECTION to action when COLLECTION is an alist or a
  hash-table, otherwise DISPLAY is always returned as result on exit,
  which is the default when using `completing-read'.
  See `helm-comp-read-get-candidates'.

- CANDIDATES-IN-BUFFER: when non--nil use a source build with
  `helm-source-in-buffer' which is much faster.
  Argument VOLATILE have no effect when CANDIDATES-IN-BUFFER is non--nil.

- MATCH-PART: Allow matching only one part of candidate.
  See match-part documentation in `helm-source'.

- MATCH-DYNAMIC: See match-dynamic in `helm-source-sync'
  It has no effect when used with CANDIDATES-IN-BUFFER.

- ALLOW-NEST: Allow nesting this `helm-comp-read' in a helm session.
  See `helm'.

- MULTILINE: See multiline in `helm-source'.

- COERCE: See coerce in `helm-source'.

- GROUP: See group in `helm-source'.

Any prefix args passed during `helm-comp-read' invocation will be recorded
in `helm-current-prefix-arg', otherwise if prefix args were given before
`helm-comp-read' invocation, the value of `current-prefix-arg' will be used.
That means you can pass prefix args before or after calling a command
that use `helm-comp-read'.  See `helm-M-x' for example.

(fn PROMPT COLLECTION &key TEST INITIAL-INPUT DEFAULT PRESELECT (BUFFER \"*Helm Completions*\") MUST-MATCH FUZZY REVERSE-HISTORY (REQUIRES-PATTERN 0) HISTORY INPUT-HISTORY (CASE-FOLD helm-comp-read-case-fold-search) (DEL-INPUT t) (PERSISTENT-ACTION nil) (PERSISTENT-HELP \"DoNothing\") (MODE-LINE helm-comp-read-mode-line) HELP-MESSAGE (KEYMAP helm-comp-read-map) (NAME \"Helm Completions\") HEADER-NAME CANDIDATES-IN-BUFFER MATCH-PART MATCH-DYNAMIC EXEC-WHEN-ONLY-ONE QUIT-WHEN-NO-CAND (VOLATILE t) SORT FC-TRANSFORMER HIST-FC-TRANSFORMER (MARKED-CANDIDATES helm-comp-read-use-marked) NOMARK (ALISTP t) (CANDIDATE-NUMBER-LIMIT helm-candidate-number-limit) MULTILINE ALLOW-NEST COERCE (GROUP \\='helm))" nil nil) (autoload 'helm-read-file-name "helm-mode" "Read a file name with helm completion.
It is helm `read-file-name' emulation.

Argument PROMPT is the default prompt to use.

Keys description:

- NAME: Source name, default to \"Read File Name\".

- INITIAL-INPUT: Where to start reading file name, default to `default-directory'.

- BUFFER: `helm-buffer' name, defaults to \"*Helm Completions*\".

- TEST: A predicate called with one arg 'candidate'.

- NORET: Allow disabling helm-ff-RET (have no effect if helm-ff-RET
                                      isn't bound to RET).

- CASE-FOLD: Same as `helm-case-fold-search'.

- PRESELECT: helm preselection.

- HISTORY: Display HISTORY in a special source.

- MUST-MATCH: Can be 'confirm, nil, or t.

- FUZZY: Enable fuzzy matching when non-nil (Enabled by default).

- MARKED-CANDIDATES: When non--nil return a list of marked candidates.

- NOMARK: When non--nil don't allow marking candidates.

- ALISTP: Don't use `all-completions' in history (take effect only on history).

- PERSISTENT-ACTION-IF: a persistent if action function.

- PERSISTENT-HELP: persistent help message.

- MODE-LINE: A mode line message, default is `helm-read-file-name-mode-line-string'.

(fn PROMPT &key (NAME \"Read File Name\") (INITIAL-INPUT default-directory) (BUFFER \"*Helm file completions*\") TEST NORET (CASE-FOLD helm-file-name-case-fold-search) PRESELECT HISTORY MUST-MATCH (FUZZY t) DEFAULT MARKED-CANDIDATES (CANDIDATE-NUMBER-LIMIT helm-ff-candidate-number-limit) NOMARK (ALISTP t) (PERSISTENT-ACTION-IF \\='helm-find-files-persistent-action-if) (PERSISTENT-HELP \"Hit1 Expand Candidate, Hit2 or (C-u) Find file\") (MODE-LINE helm-read-file-name-mode-line-string))" nil nil) (defvar helm-mode nil "Non-nil if Helm mode is enabled.
See the `helm-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `helm-mode'.") (custom-autoload 'helm-mode "helm-mode" nil) (autoload 'helm-mode "helm-mode" "Toggle generic helm completion.

All functions in Emacs that use `completing-read',
`read-file-name', `completion-in-region' and friends will use helm
interface when this mode is turned on.

However you can modify this behavior for functions of your choice
with `helm-completing-read-handlers-alist'.

Called with a positive arg, turn on unconditionally, with a
negative arg turn off.
You can toggle it with M-x `helm-mode'.

About `ido-mode':
DO NOT enable `ido-everywhere' when using `helm-mode'.  Instead of
using `ido-mode', add the commands where you want to use ido to
`helm-completing-read-handlers-alist' with `ido' as value.

Note: This mode is incompatible with Emacs23.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-mode" '("helm-"))) (autoload 'helm-browse-url-firefox "helm-net" "Same as `browse-url-firefox' but detach from Emacs.

So when you quit Emacs you can keep your Firefox session open and
not be prompted to kill the Firefox process.

NOTE: Probably not supported on some systems (e.g., Windows).

(fn URL &optional IGNORE)" t nil) (autoload 'helm-browse-url-opera "helm-net" "Browse URL with Opera browser and detach from Emacs.

So when you quit Emacs you can keep your Opera session open and
not be prompted to kill the Opera process.

NOTE: Probably not supported on some systems (e.g., Windows).

(fn URL &optional IGNORE)" t nil) (autoload 'helm-browse-url-chromium "helm-net" "Browse URL with Google Chrome browser.

(fn URL &optional IGNORE)" t nil) (autoload 'helm-browse-url-uzbl "helm-net" "Browse URL with uzbl browser.

(fn URL &optional IGNORE)" t nil) (autoload 'helm-browse-url-conkeror "helm-net" "Browse URL with conkeror browser.

(fn URL &optional IGNORE)" t nil) (autoload 'helm-browse-url-nyxt "helm-net" "Browse URL with nyxt browser.

(fn URL &optional IGNORE)" t nil) (autoload 'helm-surfraw "helm-net" "Preconfigured `helm' to search PATTERN with search ENGINE.

(fn PATTERN ENGINE)" t nil) (autoload 'helm-google-suggest "helm-net" "Preconfigured `helm' for Google search with Google suggest." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-net" '("helm-"))) (autoload 'helm-occur "helm-occur" "Preconfigured helm for searching lines matching pattern in `current-buffer'.

When `helm-source-occur' is member of
`helm-sources-using-default-as-input' which is the default,
symbol at point is searched at startup.

When a region is marked search only in this region by narrowing.

To search in multiples buffers start from one of the commands listing
buffers (i.e. a helm command using `helm-source-buffers-list' like
`helm-mini') and use the multi occur buffers action.

This is the helm implementation that collect lines matching pattern
like vanilla Emacs `occur' but have nothing to do with it, the search
engine beeing completely different and also much faster." t nil) (autoload 'helm-occur-visible-buffers "helm-occur" "Run helm-occur on all visible buffers in frame." t nil) (autoload 'helm-occur-from-isearch "helm-occur" "Invoke `helm-occur' from isearch.

To use this bind it to a key in `isearch-mode-map'." t nil) (autoload 'helm-multi-occur-from-isearch "helm-occur" "Invoke `helm-multi-occur' from isearch.

With a prefix arg, reverse the behavior of
`helm-moccur-always-search-in-current'.
The prefix arg can be set before calling
`helm-multi-occur-from-isearch' or during the buffer selection.

To use this bind it to a key in `isearch-mode-map'." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-occur" '("helm-"))) (autoload 'helm-regexp "helm-regexp" "Preconfigured helm to build regexps.
`query-replace-regexp' can be run from there against found regexp." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-regexp" '("helm-"))) (autoload 'helm-mark-ring "helm-ring" "Preconfigured `helm' for `helm-source-mark-ring'." t nil) (autoload 'helm-global-mark-ring "helm-ring" "Preconfigured `helm' for `helm-source-global-mark-ring'." t nil) (autoload 'helm-all-mark-rings "helm-ring" "Preconfigured `helm' for `helm-source-global-mark-ring' and `helm-source-mark-ring'." t nil) (autoload 'helm-register "helm-ring" "Preconfigured `helm' for Emacs registers." t nil) (autoload 'helm-show-kill-ring "helm-ring" "Preconfigured `helm' for `kill-ring'.
It is drop-in replacement of `yank-pop'.

First call open the kill-ring browser, next calls move to next line." t nil) (autoload 'helm-execute-kmacro "helm-ring" "Preconfigured helm for keyboard macros.
Define your macros with `f3' and `f4'.
See (info \"(emacs) Keyboard Macros\") for detailed infos.
This command is useful when used with persistent action." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-ring" '("helm-"))) (autoload 'helm-semantic "helm-semantic" "Preconfigured `helm' for `semantic'.
If ARG is supplied, pre-select symbol at point instead of current.

(fn ARG)" t nil) (autoload 'helm-semantic-or-imenu "helm-semantic" "Preconfigured helm for `semantic' or `imenu'.
If ARG is supplied, pre-select symbol at point instead of current
semantic tag in scope.

If `semantic-mode' is active in the current buffer, then use
semantic for generating tags, otherwise fall back to `imenu'.
Fill in the symbol at point by default.

(fn ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-semantic" '("helm-s"))) (defalias 'helm-shell-prompts 'helm-comint-prompts) (defalias 'helm-shell-prompts-all 'helm-comint-prompts-all) (defvar helm-top-poll-mode nil "Non-nil if Helm-Top-Poll mode is enabled.
See the `helm-top-poll-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `helm-top-poll-mode'.") (custom-autoload 'helm-top-poll-mode "helm-sys" nil) (autoload 'helm-top-poll-mode "helm-sys" "Refresh automatically helm top buffer once enabled.

If called interactively, enable Helm-Top-Poll mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (autoload 'helm-top "helm-sys" "Preconfigured `helm' for top command." t nil) (autoload 'helm-list-emacs-process "helm-sys" "Preconfigured `helm' for Emacs process." t nil) (autoload 'helm-xrandr-set "helm-sys" "Preconfigured helm for xrandr." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-sys" '("helm-"))) (autoload 'helm-etags-select "helm-tags" "Preconfigured helm for etags.
If called with a prefix argument REINIT
or if any of the tag files have been modified, reinitialize cache.

This function aggregates three sources of tag files:

  1) An automatically located file in the parent directories,
     by `helm-etags-get-tag-file'.
  2) `tags-file-name', which is commonly set by `find-tag' command.
  3) `tags-table-list' which is commonly set by `visit-tags-table' command.

(fn REINIT)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-tags" '("helm-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-types" '("helm-"))) (defvar helm-popup-tip-mode nil "Non-nil if Helm-Popup-Tip mode is enabled.
See the `helm-popup-tip-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `helm-popup-tip-mode'.") (custom-autoload 'helm-popup-tip-mode "helm-utils" nil) (autoload 'helm-popup-tip-mode "helm-utils" "Show help-echo informations in a popup tip at end of line.

If called interactively, enable Helm-Popup-Tip mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-utils" '("helm-" "with-helm-display-marked-candidates"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-x-files" '("helm-"))) (provide 'helm-autoloads)) "helm-bibtex" ((helm-bibtex-autoloads helm-bibtex) (autoload 'helm-bibtex "helm-bibtex" "Search BibTeX entries.

With a prefix ARG, the cache is invalidated and the bibliography
reread.

If LOCAL-BIB is non-nil, display that the BibTeX entries are read
from the local bibliography.  This is set internally by
`helm-bibtex-with-local-bibliography'.

If INPUT is non-nil and a string, that value is going to be used
as a predefined search term.  Can be used to define functions for
frequent searches (e.g. your own publications).

(fn &optional ARG LOCAL-BIB INPUT)" t nil) (autoload 'helm-bibtex-with-local-bibliography "helm-bibtex" "Search BibTeX entries with local bibliography.

If none is found the global bibliography is used instead.  With a
prefix ARG the cache is invalidated and the bibliography
reloaded.

(fn &optional ARG)" t nil) (autoload 'helm-bibtex-with-notes "helm-bibtex" "Search BibTeX entries with notes.

With a prefix ARG the cache is invalidated and the bibliography
reread.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-bibtex" '("helm-"))) (provide 'helm-bibtex-autoloads)) "key-chord" ((key-chord-autoloads key-chord) (defvar key-chord-mode nil "Non-nil if Key-Chord mode is enabled.
See the `key-chord-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `key-chord-mode'.") (custom-autoload 'key-chord-mode "key-chord" nil) (autoload 'key-chord-mode "key-chord" "Map pairs of simultaneously pressed keys to commands.

If called interactively, enable Key-Chord mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

See functions `key-chord-define-global', `key-chord-define-local',
and `key-chord-define' and variables `key-chord-two-keys-delay'
and `key-chord-one-key-delay'.

(fn &optional ARG)" t nil) (autoload 'key-chord-define-global "key-chord" "Define a key-chord of the two keys in KEYS starting a COMMAND.

KEYS can be a string or a vector of two elements. Currently only
elements that corresponds to ascii codes in the range 32 to 126
can be used.

COMMAND can be an interactive function, a string, or nil.
If COMMAND is nil, the key-chord is removed.

Note that KEYS defined locally in the current buffer will have
precedence.

(fn KEYS COMMAND)" t nil) (autoload 'key-chord-define-local "key-chord" "Locally define a key-chord of the two keys in KEYS starting a COMMAND.

KEYS can be a string or a vector of two elements. Currently only
elements that corresponds to ascii codes in the range 32 to 126
can be used.

COMMAND can be an interactive function, a string, or nil.
If COMMAND is nil, the key-chord is removed.

The binding goes in the current buffer's local map, which in most
cases is shared with all other buffers in the same major mode.

(fn KEYS COMMAND)" t nil) (autoload 'key-chord-define "key-chord" "Define in KEYMAP, a key-chord of the two keys in KEYS starting a COMMAND.

KEYS can be a string or a vector of two elements. Currently only
elements that corresponds to ascii codes in the range 32 to 126
can be used.

COMMAND can be an interactive function, a string, or nil.
If COMMAND is nil, the key-chord is removed.

(fn KEYMAP KEYS COMMAND)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "key-chord" '("key-chord-"))) (provide 'key-chord-autoloads)) "org-ref" ((nist-webbook org-ref-reftex org-ref-isbn org-ref-autoloads doi-utils org-ref-ivy-cite org-ref-scifinder org-ref-helm-cite org-ref-bibtex org-ref org-ref-latex org-ref-url-utils org-ref-scopus org-ref-helm org-ref-helm-bibtex x2bib org-ref-pgk org-ref-pubmed org-ref-citeproc org-ref-ivy org-ref-worldcat org-ref-core org-ref-utils org-ref-sci-id org-ref-glossary org-ref-arxiv org-ref-pdf org-ref-wos) (autoload 'doi-utils-get-bibtex-entry-pdf "doi-utils" "Download pdf for entry at point if the pdf does not already exist locally.
The entry must have a doi. The pdf will be saved
to `org-ref-pdf-directory', by the name %s.pdf where %s is the
bibtex label.  Files will not be overwritten.  The pdf will be
checked to make sure it is a pdf, and not some html failure
page. You must have permission to access the pdf. We open the pdf
at the end if `doi-utils-open-pdf-after-download' is non-nil.

With one prefix ARG, directly get the pdf from a file (through
`read-file-name') instead of looking up a DOI. With a double
prefix ARG, directly get the pdf from an open buffer (through
`read-buffer-to-switch') instead. These two alternative methods
work even if the entry has no DOI, and the pdf file is not
checked.

(fn &optional ARG)" t nil) (autoload 'doi-utils-add-bibtex-entry-from-doi "doi-utils" "Add DOI entry to end of a file in the current directory.
Pick the file ending with .bib or in
`org-ref-default-bibliography'.  If you have an active region that
starts like a DOI, that will be the initial prompt.  If no region
is selected and the first entry of the \342\200\230kill-ring\342\200\231 starts like a
DOI, then that is the intial prompt.  Otherwise, you have to type
or paste in a DOI.
Argument BIBFILE the bibliography to use.

(fn DOI &optional BIBFILE)" t nil) (autoload 'doi-utils-doi-to-org-bibtex "doi-utils" "Convert a DOI to an \342\200\230org-bibtex\342\200\231 form and insert it at point.

(fn DOI)" t nil) (autoload 'bibtex-set-field "doi-utils" "Set FIELD to VALUE in bibtex file.  create field if it does not exist.
Optional argument NODELIM see `bibtex-make-field'.

(fn FIELD VALUE &optional NODELIM)" t nil) (autoload 'doi-utils-update-bibtex-entry-from-doi "doi-utils" "Update fields in a bibtex entry from the DOI.
Every field will be updated, so previous change will be lost.

(fn DOI)" t nil) (autoload 'doi-utils-update-field "doi-utils" "Update the field at point in the bibtex entry.
Data is retrieved from the doi in the entry." t nil) (autoload 'doi-utils-wos "doi-utils" "Open Web of Science entry for DOI.

(fn DOI)" t nil) (autoload 'doi-utils-wos-citing "doi-utils" "Open Web of Science citing articles entry for DOI.
May be empty if none are found.

(fn DOI)" t nil) (autoload 'doi-utils-wos-related "doi-utils" "Open Web of Science related articles page for DOI.

(fn DOI)" t nil) (autoload 'doi-utils-ads "doi-utils" "Open ADS entry for DOI

(fn DOI)" t nil) (autoload 'doi-utils-open "doi-utils" "Open DOI in browser.

(fn DOI)" t nil) (autoload 'doi-utils-open-bibtex "doi-utils" "Search through variable `reftex-default-bibliography' for DOI.

(fn DOI)" t nil) (autoload 'doi-utils-crossref "doi-utils" "Search DOI in CrossRef.

(fn DOI)" t nil) (autoload 'doi-utils-google-scholar "doi-utils" "Google scholar the DOI.

(fn DOI)" t nil) (autoload 'doi-utils-pubmed "doi-utils" "Search Pubmed for the DOI.

(fn DOI)" t nil) (autoload 'doi-link-menu "doi-utils" "Generate the link menu message, get choice and execute it.
Options are stored in `doi-link-menu-funcs'.
Argument LINK-STRING Passed in on link click.

(fn LINK-STRING)" t nil) (autoload 'doi-utils-crossref-citation-query "doi-utils" "Query Crossref with the title of the bibtex entry at point.
Get a list of possible matches.  This opens a helm buffer to
select an entry.  The default action inserts a doi and url field
in the bibtex entry at point.  The second action opens the doi
url.  If there is already a doi field, the function raises an
error." t nil) (autoload 'doi-utils-debug "doi-utils" "Generate an org-buffer showing data about DOI.

(fn DOI)" t nil) (autoload 'doi-utils-add-entry-from-crossref-query "doi-utils" "Search Crossref with QUERY and use helm to select an entry to add to BIBTEX-FILE.

(fn QUERY BIBTEX-FILE)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doi-utils" '("*doi-utils-" "agu-pdf-url" "aip-pdf-url" "aps-pdf-url" "asme-biomechanical-pdf-url" "copernicus-" "crossref-add-bibtex-entry" "doi-" "ecs" "generic-full-pdf-url" "ieee" "iop-pdf-url" "jneurosci-pdf-url" "jstor-pdf-url" "linkinghub-elsevier-pdf-url" "nature-pdf-url" "osa-pdf-url" "pnas-pdf-url" "rsc-pdf-url" "sage-pdf-url" "science-" "siam-pdf-url" "springer-" "tandfonline-pdf-url" "wiley-pdf-url"))) (autoload 'nist-webbook-formula "nist-webbook" "Search NIST webbook for FORMULA.

(fn FORMULA)" t nil) (autoload 'nist-webbook-name "nist-webbook" "Search NIST webbook for NAME.

(fn NAME)" t nil) (autoload 'arxiv-add-bibtex-entry "org-ref-arxiv" "Add bibtex entry for ARXIV-NUMBER to BIBFILE.

(fn ARXIV-NUMBER BIBFILE)" t nil) (autoload 'arxiv-get-pdf "org-ref-arxiv" "Retrieve a pdf for ARXIV-NUMBER and save it to PDF.

(fn ARXIV-NUMBER PDF)" t nil) (autoload 'arxiv-get-pdf-add-bibtex-entry "org-ref-arxiv" "Add bibtex entry for ARXIV-NUMBER to BIBFILE.
Remove troublesome chars from the bibtex key, retrieve a pdf
for ARXIV-NUMBER and save it to PDFDIR with the same name of the
key.

(fn ARXIV-NUMBER BIBFILE PDFDIR)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-ref-arxiv" '("arxiv-"))) (autoload 'org-ref-bibtex-generate-longtitles "org-ref-bibtex" "Generate longtitles.bib which are @string definitions.
The full journal names are in `org-ref-bibtex-journal-abbreviations'." t nil) (autoload 'org-ref-bibtex-generate-shorttitles "org-ref-bibtex" "Generate shorttitles.bib which are @string definitions.
The abbreviated journal names in `org-ref-bibtex-journal-abbreviations'." t nil) (autoload 'org-ref-stringify-journal-name "org-ref-bibtex" "Replace journal name in a bibtex entry with a string.
The strings are defined in
`org-ref-bibtex-journal-abbreviations'.  The optional arguments KEY,
START and END allow you to use this with `bibtex-map-entries'

(fn &optional KEY START END)" t nil) (autoload 'org-ref-helm-set-journal-string "org-ref-bibtex" "Helm interface to set a journal string in a bibtex entry.
Entries come from `org-ref-bibtex-journal-abbreviations'." t nil) (autoload 'org-ref-set-journal-string "org-ref-bibtex" "Set a bibtex journal name to the string that represents FULL-JOURNAL-NAME.
This is defined in `org-ref-bibtex-journal-abbreviations'.

(fn FULL-JOURNAL-NAME)" t nil) (autoload 'org-ref-replace-nonascii "org-ref-bibtex" "Hook function to replace non-ascii characters in a bibtex entry." t nil) (autoload 'org-ref-title-case "org-ref-bibtex" "Convert a bibtex entry title and booktitle to title-case.
Convert only if the entry type is a member of the list
`org-ref-title-case-types'. The arguments KEY, START and END are
optional, and are only there so you can use this function with
`bibtex-map-entries' to change all the title entries in articles and
books.

(fn &optional KEY START END)" t nil) (autoload 'org-ref-title-case-article "org-ref-bibtex" "Convert a bibtex entry article or book title to title-case.
The arguments KEY, START and END are optional, and are only there
so you can use this function with `bibtex-map-entries' to change
all the title entries in articles and books.

(fn &optional KEY START END)" t nil) (autoload 'org-ref-sentence-case-article "org-ref-bibtex" "Convert a bibtex entry article title to sentence-case.
The arguments KEY, START and END are optional, and are only there
so you can use this function with `bibtex-map-entries' to change
all the title entries in articles.

(fn &optional KEY START END)" t nil) (autoload 'org-ref-bibtex-next-entry "org-ref-bibtex" "Jump to the beginning of the next bibtex entry.
N is a prefix argument.  If it is numeric, jump that many entries
forward.  Negative numbers do nothing.

(fn &optional N)" t nil) (autoload 'org-ref-bibtex-previous-entry "org-ref-bibtex" "Jump to beginning of the previous bibtex entry.
N is a prefix argument.  If it is numeric, jump that many entries back.

(fn &optional N)" t nil) (autoload 'org-ref-bibtex-entry-doi "org-ref-bibtex" "Get doi from entry at point." t nil) (autoload 'org-ref-bibtex-format-url-if-doi "org-ref-bibtex" "Hook function to format url to follow the current DOI conventions." t nil) (autoload 'org-ref-bibtex-wos "org-ref-bibtex" "Open bibtex entry in Web Of Science if there is a DOI." t nil) (autoload 'org-ref-bibtex-wos-citing "org-ref-bibtex" "Open citing articles for bibtex entry in Web Of Science if
there is a DOI." t nil) (autoload 'org-ref-bibtex-wos-related "org-ref-bibtex" "Open related articles for bibtex entry in Web Of Science if
there is a DOI." t nil) (autoload 'org-ref-bibtex-crossref "org-ref-bibtex" "Open the bibtex entry in Crossref by its doi." t nil) (autoload 'org-ref-bibtex-google-scholar "org-ref-bibtex" "Open the bibtex entry at point in google-scholar by its doi." t nil) (autoload 'org-ref-bibtex-pubmed "org-ref-bibtex" "Open the bibtex entry at point in Pubmed by its doi." t nil) (autoload 'org-ref-bibtex-pdf "org-ref-bibtex" "Open the pdf for the bibtex entry at point.
Thin wrapper to get `org-ref-bibtex' to open pdf, because it
calls functions with a DOI argument.

(fn &optional _)" t nil) (autoload 'org-ref-bibtex-assoc-pdf-with-entry "org-ref-bibtex" "Prompt for pdf associated with entry at point and rename it.
Check whether a pdf already exists in `org-ref-pdf-directory' with the
name '[bibtexkey].pdf'. If the file does not exist, rename it to
'[bibtexkey].pdf' using
`org-ref-bibtex-assoc-pdf-with-entry-move-function' and place it in
`org-ref-pdf-directory'. Optional PREFIX argument toggles between
`rename-file' and `copy-file'.

(fn &optional PREFIX)" t nil) (autoload 'org-ref-bibtex "org-ref-bibtex" "Menu command to run in a bibtex entry.
Functions from `org-ref-bibtex-menu-funcs'.  They all rely on the
entry having a doi." t nil) (autoload 'org-ref-email-bibtex-entry "org-ref-bibtex" "Email current bibtex entry at point and pdf if it exists." t nil) (autoload 'org-ref-set-bibtex-keywords "org-ref-bibtex" "Add KEYWORDS to a bibtex entry.
If KEYWORDS is a list, it is converted to a comma-separated
string.  The KEYWORDS are added to the beginning of the
field.  Otherwise KEYWORDS should be a string of comma-separate
keywords.  Optional argument ARG prefix arg to replace keywords.

(fn KEYWORDS &optional ARG)" t nil) (autoload 'org-ref-extract-bibtex-blocks "org-ref-bibtex" "Extract all bibtex blocks in buffer to BIBFILE.
If BIBFILE exists, append, unless you use a prefix arg (C-u), which
will clobber the file.

(fn BIBFILE)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-ref-bibtex" '("org-ref-" "orhc-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-ref-citeproc" '("*orcp-" "baseline" "bibliography-style" "bold" "citation-style" "firstname" "italics" "lastname" "orcp-" "sentence-beginning-p" "superscript"))) (autoload 'org-ref-show-link-messages "org-ref-core" "Turn on link messages.
You will see a message in the minibuffer when on a cite, ref or
label link." t nil) (autoload 'org-ref-cancel-link-messages "org-ref-core" "Stop showing messages in minibuffer when on a link." t nil) (autoload 'org-ref-change-completion "org-ref-core" "Change the completion backend.
Options are \"org-ref-helm-bibtex\", \"org-ref-helm-cite\",
\"org-ref-ivy-cite\" and \"org-ref-reftex\"." t nil) (autoload 'org-ref-mouse-message "org-ref-core" "Display message for link under mouse cursor." t nil) (autoload 'org-ref-mouse-messages-on "org-ref-core" "Turn on mouse messages." t nil) (autoload 'org-ref-mouse-messages-off "org-ref-core" "Turn off mouse messages." t nil) (autoload 'org-ref-insert-bibliography-link "org-ref-core" "Insert a bibliography with completion." t nil) (autoload 'org-ref-list-of-figures "org-ref-core" "Generate buffer with list of figures in them.
ARG does nothing.
Ignore figures in COMMENTED sections.

(fn &optional ARG)" t nil) (autoload 'org-ref-list-of-tables "org-ref-core" "Generate a buffer with a list of tables.
ARG does nothing.

(fn &optional ARG)" t nil) (autoload 'org-ref-insert-ref-link "org-ref-core" "Completion function for a ref link." t nil) (autoload 'org-pageref-insert-ref-link "org-ref-core" "Insert a pageref link with completion." t nil) (autoload 'org-ref-define-citation-link "org-ref-core" "Add a citation link of TYPE for `org-ref'.
With optional KEY, set the reftex binding.  For example:
(org-ref-define-citation-link \"citez\" ?z) will create a new
citez link, with reftex key of z, and the completion function.

(fn TYPE &optional KEY)" t nil) (autoload 'org-ref-insert-cite-with-completion "org-ref-core" "Insert a cite link of TYPE with completion.

(fn TYPE)" t nil) (autoload 'org-ref-store-bibtex-entry-link "org-ref-core" "Save a citation link to the current bibtex entry.
Save in the default link type." t nil) (autoload 'org-ref-index "org-ref-core" "Open an *index* buffer with links to index entries.
PATH is required for the org-link, but it does nothing here.

(fn &optional PATH)" t nil) (autoload 'org-ref-open-bibtex-pdf "org-ref-core" "Open pdf for a bibtex entry, if it exists.
assumes point is in
the entry of interest in the bibfile.  but does not check that." t nil) (autoload 'org-ref-open-bibtex-notes "org-ref-core" "From a bibtex entry, open the notes if they exist." t nil) (autoload 'org-ref-open-in-browser "org-ref-core" "Open the bibtex entry at point in a browser using the url field or doi field." t nil) (autoload 'org-ref-build-full-bibliography "org-ref-core" "Build pdf of all bibtex entries, and open it." t nil) (autoload 'org-ref-extract-bibtex-entries "org-ref-core" "Extract the bibtex entries in the current buffer into a bibtex src block." t nil) (autoload 'org-ref-extract-bibtex-to-file "org-ref-core" "Extract all bibtex entries for citations buffer to BIBFILE.
If BIBFILE exists, append, unless you use a prefix arg (C-u),
which will CLOBBER the file.

(fn BIBFILE &optional CLOBBER)" t nil) (autoload 'org-ref-find-bad-citations "org-ref-core" "Create a list of citation keys that do not have a matching bibtex entry.
List is displayed in an `org-mode' buffer using the known bibtex
file.  Makes a new buffer with clickable links." t nil) (autoload 'org-ref-find-non-ascii-characters "org-ref-core" "Find non-ascii characters in the buffer.  Useful for cleaning up bibtex files." t nil) (autoload 'org-ref-sort-bibtex-entry "org-ref-core" "Sort fields of entry in standard order." t nil) (autoload 'org-ref-downcase-bibtex-entry "org-ref-core" "Downcase the entry type and fields." t nil) (autoload 'org-ref-clean-bibtex-entry "org-ref-core" "Clean and replace the key in a bibtex entry.
See functions in `org-ref-clean-bibtex-entry-hook'." t nil) (autoload 'org-ref-sort-citation-link "org-ref-core" "Replace link at point with sorted link by year." t nil) (autoload 'org-ref-swap-citation-link "org-ref-core" "Move citation at point in DIRECTION +1 is to the right, -1 to the left.

(fn DIRECTION)" t nil) (autoload 'org-ref-next-key "org-ref-core" "Move cursor to the next cite key when on a cite link.
Otherwise run `right-word'. If the cursor moves off the link,
move to the beginning of the next cite link after this one." t nil) (autoload 'org-ref-previous-key "org-ref-core" "Move cursor to the previous cite key when on a cite link.
Otherwise run `left-word'. If the cursor moves off the link,
move to the beginning of the previous cite link after this one." t nil) (autoload 'org-ref-link-message "org-ref-core" "Print a minibuffer message about the link that point is on." t nil) (autoload 'org-ref-insert-link "org-ref-core" "Insert an org-ref link.
If no prefix ARG insert a cite.
If one prefix ARG insert a ref.
If two prefix ARGs insert a label.

This is a generic function. Specific completion engines might
provide their own version.

(fn ARG)" t nil) (autoload 'org-ref-help "org-ref-core" "Open the `org-ref' manual." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-ref-core" '("orc" "org-" "ori" "orn" "oro" "orp" "orsl" "oru"))) (autoload 'org-ref-add-glossary-entry "org-ref-glossary" "Insert a new glossary entry.
LABEL is how you refer to it with links.
NAME is the name of the entry to be defined.
DESCRIPTION is the definition of the entry.
Entry gets added after the last #+latex_header line.

(fn LABEL NAME DESCRIPTION)" t nil) (autoload 'org-ref-add-acronym-entry "org-ref-glossary" "Add an acronym entry with LABEL.
  ABBRV is the abbreviated form.
  FULL is the expanded acronym.

(fn LABEL ABBRV FULL)" t nil) (autoload 'org-ref-insert-glossary-link "org-ref-glossary" "Helm command to insert glossary and acronym entries as links." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-ref-glossary" '("or-" "org-ref-"))) (autoload 'org-ref-helm-insert-label-link "org-ref-helm" "Insert label link at point.
Helm will display existing labels in the current buffer to avoid
duplication. If you use a prefix arg insert a radio target
instead of a label." t nil) (autoload 'org-ref-helm-insert-ref-link "org-ref-helm" "Helm menu to insert ref links to labels in the document.
If you are on link, replace with newly selected label.  Use
\\[universal-argument] to insert a different kind of ref link.
Use a double \\[universal-argument] \\[universal-argument] to insert a
[[#custom-id]] link" t nil) (autoload 'org-ref-helm "org-ref-helm" "Opens a helm interface to actions for `org-ref'.
Shows bad citations, ref links and labels.
This widens the file so that all links go to the right place." t nil) (autoload 'helm-tag-bibtex-entry "org-ref-helm" "Helm interface to add keywords to a bibtex entry.
Run this with the point in a bibtex entry." t nil) (autoload 'org-ref-bibtex-completion-completion "org-ref-helm-bibtex" "Use helm and \342\200\230helm-bibtex\342\200\231 for completion." t nil) (autoload 'org-ref-helm-load-completions-async "org-ref-helm-bibtex" "Load the bibtex files into helm sources asynchronously.
For large bibtex files, the initial call to \342\200\230org-ref-helm-insert-cite-link\342\200\231
can take a long time to load the completion sources.  This function loads
the completion sources in the background so the initial call to \342\200\230org-ref-helm-insert-cite-link\342\200\231 is much faster." t nil) (autoload 'org-ref-helm-insert-cite-link "org-ref-helm-bibtex" "Insert a citation link with `helm-bibtex'.
With one prefix ARG, insert a ref link.
With two prefix ARGs, insert a label link.

(fn &optional ARG)" t nil) (autoload 'org-ref-cite-click-helm "org-ref-helm-bibtex" "Open helm for actions on a cite link.
subtle points.

1. get name and candidates before entering helm because we need
the org-buffer.

2. switch back to the org buffer before evaluating the
action.  most of them need the point and buffer.

KEY is returned for the selected item(s) in helm.

(fn KEY)" t nil) (autoload 'org-ref-browser "org-ref-helm-bibtex" "Quickly browse label links in helm.
With a prefix ARG, browse citation links.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-ref-helm-bibtex" '("bibtex-completion-copy-candidate" "org-ref-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-ref-helm-cite" '("org-ref-helm-" "orhc"))) (autoload 'org-ref-isbn-clean-bibtex-entry "org-ref-isbn" "Clean a bibtex entry inserted via `isbn-to-bibtex'.
See functions in `org-ref-isbn-clean-bibtex-entry-hook'." t nil) (autoload 'isbn-to-bibtex-lead "org-ref-isbn" "Search lead.to for ISBN bibtex entry.
You have to copy the entry if it is on the page to your bibtex
file.

(fn ISBN)" t nil) (autoload 'isbn-to-bibtex "org-ref-isbn" "Get bibtex entry for ISBN and insert it into BIBFILE.
Nothing happens if an entry with the generated key already exists
in the file. Data comes from worldcat.

(fn ISBN BIBFILE)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-ref-isbn" '("org-ref-isbn-" "oricb-"))) (autoload 'org-ref-ivy-cite-completion "org-ref-ivy-cite" "Use ivy for completion." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-ref-ivy-cite" '("or-" "org-ref-"))) (autoload 'org-ref-latex-debug "org-ref-latex" nil t nil) (autoload 'org-ref-latex-click "org-ref-latex" "Jump to entry clicked on." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-ref-latex" '("org-ref-"))) (autoload 'org-ref-pdf-to-bibtex "org-ref-pdf" "Add pdf of current buffer to bib file and save pdf to
`org-ref-default-bibliography'. The pdf should be open in Emacs
using the `pdf-tools' package." t nil) (autoload 'org-ref-pdf-dnd-protocol "org-ref-pdf" "Drag-n-drop protocol.
PDF will be a string like file:path.
ACTION is what to do. It is required for `dnd-protocol-alist'.
This function should only apply when in a bibtex file.

(fn URI ACTION)" nil nil) (autoload 'org-ref-pdf-dir-to-bibtex "org-ref-pdf" "Create BIBFILE from pdf files in DIRECTORY.

(fn BIBFILE DIRECTORY)" t nil) (autoload 'org-ref-pdf-debug-pdf "org-ref-pdf" "Try to debug getting a doi from a pdf.
Opens a buffer with the pdf converted to text, and `occur' on the
variable `org-ref-pdf-doi-regex'.

(fn PDF-FILE)" t nil) (autoload 'org-ref-pdf-crossref-lookup "org-ref-pdf" "Lookup highlighted text in PDFView in CrossRef." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-ref-pdf" '("org-ref-" "pdftotext-executable"))) (autoload 'pubmed-insert-bibtex-from-pmid "org-ref-pubmed" "Insert a bibtex entry at point derived from PMID.
You must clean the entry after insertion.

(fn PMID)" t nil) (autoload 'pubmed "org-ref-pubmed" "Open http://www.ncbi.nlm.nih.gov/pubmed in a browser." t nil) (autoload 'pubmed-advanced "org-ref-pubmed" "Open http://www.ncbi.nlm.nih.gov/pubmed/advanced in a browser." t nil) (autoload 'pubmed-simple-search "org-ref-pubmed" "Open QUERY in Pubmed in a browser.

(fn QUERY)" t nil) (autoload 'pubmed-clinical "org-ref-pubmed" "Open http://www.ncbi.nlm.nih.gov/pubmed/clinical in a browser." t nil) (autoload 'pubmed-clinical-search "org-ref-pubmed" "Open QUERY in pubmed-clinical.

(fn QUERY)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-ref-pubmed" '("pubmed-"))) (autoload 'org-ref-reftex-completion "org-ref-reftex" "Use reftex and org-mode for completion." t nil) (autoload 'org-ref-open-notes-from-reftex "org-ref-reftex" "Call reftex, and open notes for selected entry." t nil) (autoload 'org-ref-cite-onclick-minibuffer-menu "org-ref-reftex" "Action when a cite link is clicked on.
Provides a menu of context sensitive actions.  If the bibtex entry
has a pdf, you get an option to open it.  If there is a doi, you
get a lot of options.  LINK-STRING is used by the link function.

(fn &optional LINK-STRING)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-ref-reftex" '("org-" "ornr"))) (autoload 'scifinder "org-ref-scifinder" "Open https://scifinder.cas.org/scifinder/view/scifinder/scifinderExplore.jsf in a browser." t nil) (autoload 'scopus-related-by-keyword-url "org-ref-scopus" "Return a Scopus url to articles related by keyword for DOI.

(fn DOI)" t nil) (autoload 'scopus-related-by-author-url "org-ref-scopus" "Return a Scopus url to articles related by author for DOI.

(fn DOI)" t nil) (autoload 'scopus-related-by-references-url "org-ref-scopus" "Return a Scopus url to articles related by references for DOI.

(fn DOI)" t nil) (autoload 'scopus-open-eid "org-ref-scopus" "Open article with EID in browser.

(fn EID)" t nil) (autoload 'scopus-basic-search "org-ref-scopus" "Open QUERY as a basic title-abstract-keyword search at scopus.com.

(fn QUERY)" t nil) (autoload 'scopus-advanced-search "org-ref-scopus" "Open QUERY as an advanced search at scopus.com.

(fn QUERY)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-ref-scopus" '("*hydra-eid*" "*scopus-api-key*" "scopus"))) (autoload 'org-ref-url-debug-url "org-ref-url-utils" "Open a buffer to URL with all doi patterns highlighted.

(fn URL)" t nil) (autoload 'org-ref-url-dnd-debug "org-ref-url-utils" "Drag-n-drop function to debug a url.

(fn EVENT)" t nil) (autoload 'org-ref-url-dnd-all "org-ref-url-utils" "Drag-n-drop function to get all DOI bibtex entries for a url.
You probably do not want to do this since the DOI patterns are
not perfect, and some hits are not actually DOIs.

(fn EVENT)" t nil) (autoload 'org-ref-url-dnd-first "org-ref-url-utils" "Drag-n-drop function to download the first DOI in a url.

(fn EVENT)" t nil) (autoload 'org-ref-url-html-to-bibtex "org-ref-url-utils" "Convert URL to a bibtex or biblatex entry in BIBFILE.
If URL is the first in the kill ring, use it. Otherwise, prompt for
one in the minibuffer.

(fn BIBFILE &optional URL)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-ref-url-utils" '("org-ref-"))) (autoload 'org-ref-version "org-ref-utils" "Provide a version string for org-ref.
Copies the string to the clipboard." t nil) (autoload 'org-ref-debug "org-ref-utils" "Print some debug information to a buffer." t nil) (autoload 'org-ref-open-pdf-at-point "org-ref-utils" "Open the pdf for bibtex key under point if it exists." t nil) (autoload 'org-ref-open-url-at-point "org-ref-utils" "Open the url for bibtex key under point." t nil) (autoload 'org-ref-open-notes-at-point "org-ref-utils" "Open the notes for bibtex key under point in a cite link in a buffer.
Can also be called with THEKEY in a program.

(fn &optional THEKEY)" t nil) (autoload 'org-ref-citation-at-point "org-ref-utils" "Give message of current citation at point." t nil) (autoload 'org-ref-open-citation-at-point "org-ref-utils" "Open bibtex file to key at point." t nil) (autoload 'org-ref-copy-entry-as-summary "org-ref-utils" "Copy the bibtex entry for the citation at point as a summary." t nil) (autoload 'org-ref-copy-cite-as-summary "org-ref-utils" "Copy a summary for the citation at point to the clipboard." t nil) (autoload 'org-ref-copy-entry-at-point-to-file "org-ref-utils" "Copy the bibtex entry for the citation at point to NEW-FILE.
Prompt for NEW-FILE includes bib files in
`org-ref-default-bibliography', and bib files in current working
directory.  You can also specify a new file." t nil) (autoload 'org-ref-ads-at-point "org-ref-utils" "Open the doi in ADS for bibtex key under point." t nil) (autoload 'org-ref-wos-at-point "org-ref-utils" "Open the doi in wos for bibtex key under point." t nil) (autoload 'org-ref-wos-citing-at-point "org-ref-utils" "Open the doi in wos citing articles for bibtex key under point." t nil) (autoload 'org-ref-wos-related-at-point "org-ref-utils" "Open the doi in wos related articles for bibtex key under point." t nil) (autoload 'org-ref-google-scholar-at-point "org-ref-utils" "Search google scholar for bibtex key under point using the title." t nil) (autoload 'org-ref-pubmed-at-point "org-ref-utils" "Open the doi in pubmed for bibtex key under point." t nil) (autoload 'org-ref-crossref-at-point "org-ref-utils" "Open the doi in crossref for bibtex key under point." t nil) (autoload 'org-ref-bibliography "org-ref-utils" "Create a new buffer with a bibliography.
If SORT is non-nil it is alphabetically sorted by key
This is mostly for convenience to see what has been cited.
Entries are formatted according to the bibtex entry type in
`org-ref-bibliography-entry-format', and the actual entries are
generated by `org-ref-reftex-format-citation'.

(fn &optional SORT)" t nil) (autoload 'org-ref-link-set-parameters "org-ref-utils" "Set link TYPE properties to PARAMETERS.

(fn TYPE &rest PARAMETERS)" nil t) (function-put 'org-ref-link-set-parameters 'lisp-indent-function '1) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-ref-utils" '("ords" "org-ref"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-ref-worldcat" '("worldcat-query-all"))) (autoload 'wos-search "org-ref-wos" "Open the word at point or selection in Web of Science as a topic query." t nil) (autoload 'wos "org-ref-wos" "Open Web of Science search page in a browser." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-ref-wos" '("*wos-" "wos-"))) (autoload 'ris2bib "x2bib" "Convert RISFILE to bibtex and insert at point.
Without a prefix arg, stderr is diverted.
If VERBOSE is non-nil show command output.
If the region is active, assume it is a ris entry
and convert it to bib format in place.

(fn RISFILE &optional VERBOSE)" t nil) (autoload 'medxml2bib "x2bib" "Convert MEDFILE (in Pubmed xml) to bibtex and insert at point.
Without a prefix arg, stderr is diverted.
Display output if VERBOSE is non-nil.

(fn MEDFILE &optional VERBOSE)" t nil) (autoload 'clean-entries "x2bib" "Map over bibtex entries and clean them." t nil) (provide 'org-ref-autoloads)) "kv" ((kv-autoloads kv) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "kv" '("dotass" "keyword->symbol" "map-bind"))) (provide 'kv-autoloads)) "esxml" ((esxml-autoloads esxml-query esxml-pkg esxml) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "esxml" '("attr" "esxml-" "pp-esxml-to-xml" "string-trim-whitespace" "sxml-to-" "xml-to-esxml"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "esxml-query" '("esxml-"))) (provide 'esxml-autoloads)) "deft" ((deft deft-autoloads) (autoload 'deft-find-file "deft" "Find FILE interactively using the minibuffer.
FILE must exist and be a relative or absolute path, with extension.
If FILE is not inside `deft-directory', fall back to using `find-file'.

(fn FILE)" t nil) (autoload 'deft-new-file "deft" "Create a new file quickly.
Use either an automatically generated filename or the filter string if non-nil
and `deft-use-filter-string-for-filename' is set.  If the filter string is
non-nil and title is not from filename, use it as the title." t nil) (autoload 'deft "deft" "Switch to *Deft* buffer and load files." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "deft" '("deft-" "org-deft-store-link"))) (provide 'deft-autoloads)) "neotree" ((neotree neotree-autoloads) (autoload 'neotree-find "neotree" "Quick select node which specified PATH in NeoTree.
If path is nil and no buffer file name, then use DEFAULT-PATH,

(fn &optional PATH DEFAULT-PATH)" t nil) (autoload 'neotree-projectile-action "neotree" "Integration with `Projectile'.

Usage:
    (setq projectile-switch-project-action 'neotree-projectile-action).

When running `projectile-switch-project' (C-c p p), `neotree' will change root
automatically." t nil) (autoload 'neotree-toggle "neotree" "Toggle show the NeoTree window." t nil) (autoload 'neotree-show "neotree" "Show the NeoTree window." t nil) (autoload 'neotree-hide "neotree" "Close the NeoTree window." t nil) (autoload 'neotree-dir "neotree" "Show the NeoTree window, and change root to PATH.

(fn PATH)" t nil) (defalias 'neotree 'neotree-show "Show the NeoTree window.") (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "neotree" '("neo" "off-p"))) (provide 'neotree-autoloads)) "emmet-mode" ((emmet-mode emmet-mode-autoloads) (autoload 'emmet-expand-line "emmet-mode" "Replace the current line's emmet expression with the corresponding expansion.
If prefix ARG is given or region is visible call `emmet-preview' to start an
interactive preview.

Otherwise expand line directly.

For more information see `emmet-mode'.

(fn ARG)" t nil) (autoload 'emmet-mode "emmet-mode" "Minor mode for writing HTML and CSS markup.
With emmet for HTML and CSS you can write a line like

If called interactively, enable Emmet mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

  ul#name>li.item*2

and have it expanded to

  <ul id=\"name\">
    <li class=\"item\"></li>
    <li class=\"item\"></li>
  </ul>

This minor mode defines keys for quick access:

\\{emmet-mode-keymap}

Home page URL `http://www.emacswiki.org/emacs/Emmet'.

See also `emmet-expand-line'.

(fn &optional ARG)" t nil) (autoload 'emmet-expand-yas "emmet-mode" nil t nil) (autoload 'emmet-preview "emmet-mode" "Expand emmet between BEG and END interactively.
This will show a preview of the expanded emmet code and you can
accept it or skip it.

(fn BEG END)" t nil) (autoload 'emmet-wrap-with-markup "emmet-mode" "Wrap region with markup.

(fn WRAP-WITH)" t nil) (autoload 'emmet-next-edit-point "emmet-mode" "

(fn COUNT)" t nil) (autoload 'emmet-prev-edit-point "emmet-mode" "

(fn COUNT)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emmet-mode" '("emmet-"))) (provide 'emmet-mode-autoloads)) "ht" ((ht ht-autoloads) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ht" 'nil)) (provide 'ht-autoloads)) "spinner" ((spinner-autoloads spinner) (autoload 'spinner-create "spinner" "Create a spinner of the given TYPE.
The possible TYPEs are described in `spinner--type-to-frames'.

FPS, if given, is the number of desired frames per second.
Default is `spinner-frames-per-second'.

If BUFFER-LOCAL is non-nil, the spinner will be automatically
deactivated if the buffer is killed.  If BUFFER-LOCAL is a
buffer, use that instead of current buffer.

When started, in order to function properly, the spinner runs a
timer which periodically calls `force-mode-line-update' in the
current buffer.  If BUFFER-LOCAL was set at creation time, then
`force-mode-line-update' is called in that buffer instead.  When
the spinner is stopped, the timer is deactivated.

DELAY, if given, is the number of seconds to wait after starting
the spinner before actually displaying it. It is safe to cancel
the spinner before this time, in which case it won't display at
all.

(fn &optional TYPE BUFFER-LOCAL FPS DELAY)" nil nil) (autoload 'spinner-start "spinner" "Start a mode-line spinner of given TYPE-OR-OBJECT.
If TYPE-OR-OBJECT is an object created with `make-spinner',
simply activate it.  This method is designed for minor modes, so
they can use the spinner as part of their lighter by doing:
    '(:eval (spinner-print THE-SPINNER))
To stop this spinner, call `spinner-stop' on it.

If TYPE-OR-OBJECT is anything else, a buffer-local spinner is
created with this type, and it is displayed in the
`mode-line-process' of the buffer it was created it.  Both
TYPE-OR-OBJECT and FPS are passed to `make-spinner' (which see).
To stop this spinner, call `spinner-stop' in the same buffer.

Either way, the return value is a function which can be called
anywhere to stop this spinner.  You can also call `spinner-stop'
in the same buffer where the spinner was created.

FPS, if given, is the number of desired frames per second.
Default is `spinner-frames-per-second'.

DELAY, if given, is the number of seconds to wait until actually
displaying the spinner. It is safe to cancel the spinner before
this time, in which case it won't display at all.

(fn &optional TYPE-OR-OBJECT FPS DELAY)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "spinner" '("spinner-"))) (provide 'spinner-autoloads)) "lsp-mode" ((lsp-graphql lsp-groovy lsp-php lsp-protocol lsp-crystal lsp-angular lsp-dired lsp-fsharp lsp-dhall lsp-perl lsp-lua lsp-terraform lsp-vala lsp-zig lsp-go lsp-v lsp-purescript lsp-ada lsp-xml lsp-eslint lsp-nix lsp-clojure lsp-lens lsp-nim lsp-erlang lsp-tex lsp-modeline lsp-d lsp-css lsp-gdscript lsp-vimscript lsp-verilog lsp-elm lsp-rust lsp-pyls lsp-html lsp-iedit lsp-kotlin lsp-beancount lsp-r lsp-haxe lsp-sqls lsp-ocaml lsp-csharp lsp-vetur lsp-pylsp lsp-icons lsp lsp-cmake lsp-json lsp-dockerfile lsp-fortran lsp-completion lsp-solargraph lsp-rf lsp-steep lsp-ido lsp-elixir lsp-diagnostics lsp-bash lsp-headerline lsp-markdown lsp-actionscript lsp-javascript lsp-clangd lsp-yaml lsp-racket lsp-vhdl lsp-mode-autoloads lsp-mode lsp-pwsh lsp-hack lsp-sorbet lsp-prolog lsp-svelte lsp-semantic-tokens) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-actionscript" '("lsp-actionscript-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-ada" '("lsp-ada-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-angular" '("lsp-client"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-bash" '("lsp-bash-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-beancount" '("lsp-beancount-"))) (autoload 'lsp-cpp-flycheck-clang-tidy-error-explainer "lsp-clangd" "Explain a clang-tidy ERROR by scraping documentation from llvm.org.

(fn ERROR)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-clangd" '("lsp-c"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-clojure" '("lsp-clojure-"))) (define-obsolete-variable-alias 'lsp-prefer-capf 'lsp-completion-provider "lsp-mode 7.0.1") (define-obsolete-variable-alias 'lsp-enable-completion-at-point 'lsp-completion-enable "lsp-mode 7.0.1") (autoload 'lsp-completion-at-point "lsp-completion" "Get lsp completions." nil nil) (autoload 'lsp-completion--enable "lsp-completion" "Enable LSP completion support." nil nil) (autoload 'lsp-completion-mode "lsp-completion" "Toggle LSP completion support.

If called interactively, enable Lsp-Completion mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (add-hook 'lsp-configure-hook (lambda nil (when (and lsp-auto-configure lsp-completion-enable) (lsp-completion--enable)))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-completion" '("lsp-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-crystal" '("lsp-clients-crystal-executable"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-csharp" '("lsp-csharp-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-css" '("lsp-css-"))) (define-obsolete-variable-alias 'lsp-diagnostic-package 'lsp-diagnostics-provider "lsp-mode 7.0.1") (define-obsolete-variable-alias 'lsp-flycheck-default-level 'lsp-diagnostics-flycheck-default-level "lsp-mode 7.0.1") (autoload 'lsp-diagnostics-lsp-checker-if-needed "lsp-diagnostics" nil nil nil) (autoload 'lsp-diagnostics--enable "lsp-diagnostics" "Enable LSP checker support." nil nil) (autoload 'lsp-diagnostics-mode "lsp-diagnostics" "Toggle LSP diagnostics integration.

If called interactively, enable Lsp-Diagnostics mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (add-hook 'lsp-configure-hook (lambda nil (when lsp-auto-configure (lsp-diagnostics--enable)))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-diagnostics" '("lsp-diagnostics-"))) (defvar lsp-dired-mode nil "Non-nil if Lsp-Dired mode is enabled.
See the `lsp-dired-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `lsp-dired-mode'.") (custom-autoload 'lsp-dired-mode "lsp-dired" nil) (autoload 'lsp-dired-mode "lsp-dired" "Display `lsp-mode' icons for each file in a dired buffer.

If called interactively, enable Lsp-Dired mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-dired" '("lsp-dired-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-dockerfile" '("lsp-dockerfile-language-server-command"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-elixir" '("lsp-elixir-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-elm" '("lsp-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-erlang" '("lsp-erlang-server-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-eslint" '("lsp-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-fortran" '("lsp-clients-"))) (autoload 'lsp-fsharp--workspace-load "lsp-fsharp" "Load all of the provided PROJECTS.

(fn PROJECTS)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-fsharp" '("lsp-fsharp-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-gdscript" '("lsp-gdscript-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-go" '("lsp-go-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-graphql" '("lsp-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-groovy" '("lsp-groovy-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-hack" '("lsp-clients-hack-command"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-haxe" '("lsp-"))) (autoload 'lsp-headerline-breadcrumb-mode "lsp-headerline" "Toggle breadcrumb on headerline.

If called interactively, enable Lsp-Headerline-Breadcrumb mode if
ARG is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (autoload 'lsp-breadcrumb-go-to-symbol "lsp-headerline" "Go to the symbol on breadcrumb at SYMBOL-POSITION.

(fn SYMBOL-POSITION)" t nil) (autoload 'lsp-breadcrumb-narrow-to-symbol "lsp-headerline" "Narrow to the symbol range on breadcrumb at SYMBOL-POSITION.

(fn SYMBOL-POSITION)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-headerline" '("lsp-headerline-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-html" '("lsp-html-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-icons" '("lsp-"))) (autoload 'lsp-ido-workspace-symbol "lsp-ido" "`ido' for lsp workspace/symbol.
When called with prefix ARG the default selection will be symbol at point.

(fn ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-ido" '("lsp-ido-"))) (autoload 'lsp-iedit-highlights "lsp-iedit" "Start an `iedit' operation on the documentHighlights at point.
This can be used as a primitive `lsp-rename' replacement if the
language server doesn't support renaming.

See also `lsp-enable-symbol-highlighting'." t nil) (autoload 'lsp-evil-multiedit-highlights "lsp-iedit" "Start an `evil-multiedit' operation on the documentHighlights at point.
This can be used as a primitive `lsp-rename' replacement if the
language server doesn't support renaming.

See also `lsp-enable-symbol-highlighting'." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-iedit" '("lsp-iedit--on-ranges"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-javascript" '("lsp-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-json" '("lsp-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-kotlin" '("lsp-"))) (autoload 'lsp-lens--enable "lsp-lens" "Enable lens mode." nil nil) (autoload 'lsp-lens-show "lsp-lens" "Display lenses in the buffer." t nil) (autoload 'lsp-lens-hide "lsp-lens" "Delete all lenses." t nil) (autoload 'lsp-lens-mode "lsp-lens" "Toggle code-lens overlays.

If called interactively, enable Lsp-Lens mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (autoload 'lsp-avy-lens "lsp-lens" "Click lsp lens using `avy' package." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-lens" '("lsp-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-lua" '("lsp-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-markdown" '("lsp-markdown-"))) (put 'lsp-enable-file-watchers 'safe-local-variable #'booleanp) (put 'lsp-file-watch-threshold 'safe-local-variable (lambda (i) (or (numberp i) (not i)))) (autoload 'lsp-load-vscode-workspace "lsp-mode" "Load vscode workspace from FILE

(fn FILE)" t nil) (autoload 'lsp-save-vscode-workspace "lsp-mode" "Save vscode workspace to FILE

(fn FILE)" t nil) (autoload 'lsp-install-server "lsp-mode" "Interactively install server.
When prefix UPDATE? is t force installation even if the server is present.

(fn UPDATE\\=\\? &optional SERVER-ID)" t nil) (autoload 'lsp-ensure-server "lsp-mode" "Ensure server SERVER-ID

(fn SERVER-ID)" nil nil) (autoload 'lsp "lsp-mode" "Entry point for the server startup.
When ARG is t the lsp mode will start new language server even if
there is language server which can handle current language. When
ARG is nil current file will be opened in multi folder language
server if there is such. When `lsp' is called with prefix
argument ask the user to select which language server to start.

(fn &optional ARG)" t nil) (autoload 'lsp-deferred "lsp-mode" "Entry point that defers server startup until buffer is visible.
`lsp-deferred' will wait until the buffer is visible before invoking `lsp'.
This avoids overloading the server with many files when starting Emacs." nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-mode" '("lsp-" "make-lsp-client" "when-lsp-workspace" "with-lsp-workspace"))) (define-obsolete-variable-alias 'lsp-diagnostics-modeline-scope 'lsp-modeline-diagnostics-scope "lsp-mode 7.0.1") (autoload 'lsp-modeline-code-actions-mode "lsp-modeline" "Toggle code actions on modeline.

If called interactively, enable Lsp-Modeline-Code-Actions mode if
ARG is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (define-obsolete-function-alias 'lsp-diagnostics-modeline-mode 'lsp-modeline-diagnostics-mode "lsp-mode 7.0.1") (autoload 'lsp-modeline-diagnostics-mode "lsp-modeline" "Toggle diagnostics modeline.

If called interactively, enable Lsp-Modeline-Diagnostics mode if
ARG is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (autoload 'lsp-modeline-workspace-status-mode "lsp-modeline" "Toggle workspace status on modeline.

If called interactively, enable Lsp-Modeline-Workspace-Status
mode if ARG is positive, and disable it if ARG is zero or
negative.  If called from Lisp, also enable the mode if ARG is
omitted or nil, and toggle it if ARG is `toggle'; disable the
mode otherwise.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-modeline" '("lsp-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-nix" '("lsp-nix-server-path"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-ocaml" '("lsp-ocaml-l"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-perl" '("lsp-perl-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-php" '("lsp-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-prolog" '("lsp-prolog-server-command"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-protocol" '("dash-expand:&RangeToPoint" "lsp"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-purescript" '("lsp-purescript-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-pwsh" '("lsp-pwsh-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-pyls" '("lsp-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-pylsp" '("lsp-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-r" '("lsp-clients-r-server-command"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-racket" '("lsp-racket-lang"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-rf" '("expand-start-command" "lsp-rf-language-server-" "parse-rf-language-server-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-rust" '("lsp-"))) (autoload 'lsp--semantic-tokens-initialize-buffer "lsp-semantic-tokens" "Initialize the buffer for semantic tokens.
IS-RANGE-PROVIDER is non-nil when server supports range requests." nil nil) (autoload 'lsp--semantic-tokens-initialize-workspace "lsp-semantic-tokens" "Initialize semantic tokens for WORKSPACE.

(fn WORKSPACE)" nil nil) (autoload 'lsp-semantic-tokens--warn-about-deprecated-setting "lsp-semantic-tokens" "Warn about deprecated semantic highlighting variable." nil nil) (autoload 'lsp-semantic-tokens--enable "lsp-semantic-tokens" "Enable semantic tokens mode." nil nil) (autoload 'lsp-semantic-tokens-mode "lsp-semantic-tokens" "Toggle semantic-tokens support.

If called interactively, enable Lsp-Semantic-Tokens mode if ARG
is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-semantic-tokens" '("lsp-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-solargraph" '("lsp-solargraph-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-sorbet" '("lsp-sorbet-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-sqls" '("lsp-sql"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-steep" '("lsp-steep-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-svelte" '("lsp-svelte-plugin-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-terraform" '("lsp-terraform-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-tex" '("lsp-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-v" '("lsp-v-vls-executable"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-vala" '("lsp-clients-vala-ls-executable"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-verilog" '("lsp-clients-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-vetur" '("lsp-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-vhdl" '("ghdl-ls-bin-name" "hdl-checker-bin-name" "lsp-vhdl-" "vhdl-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-vimscript" '("lsp-clients-vim-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-xml" '("lsp-xml-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-yaml" '("lsp-yaml-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-zig" '("lsp-zig-zls-executable"))) (provide 'lsp-mode-autoloads)) "lsp-ui" ((lsp-ui-imenu lsp-ui-util lsp-ui-autoloads lsp-ui-flycheck lsp-ui lsp-ui-doc lsp-ui-sideline lsp-ui-peek) (autoload 'lsp-ui-mode "lsp-ui" "Toggle language server UI mode on or off.
\342\200\230lsp-ui-mode\342\200\231 is a minor mode that contains a series of useful UI
integrations for \342\200\230lsp-mode\342\200\231.  With a prefix argument ARG, enable
language server UI mode if ARG is positive, and disable it
otherwise.  If called from Lisp, enable the mode if ARG is
omitted or nil, and toggle it if ARG is \342\200\230toggle\342\200\231.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-ui" '("lsp-ui-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-ui-doc" '("lsp-ui-doc-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-ui-flycheck" '("lsp-ui-flycheck-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-ui-imenu" '("lsp-ui-imenu"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-ui-peek" '("lsp-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-ui-sideline" '("lsp-ui-sideline"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-ui-util" '("lsp-ui-util-"))) (provide 'lsp-ui-autoloads)) "lsp-ivy" ((lsp-ivy-autoloads lsp-ivy) (autoload 'lsp-ivy-workspace-symbol "lsp-ivy" "`ivy' for lsp workspace/symbol.
When called with prefix ARG the default selection will be symbol at point.

(fn ARG)" t nil) (autoload 'lsp-ivy-global-workspace-symbol "lsp-ivy" "`ivy' for lsp workspace/symbol for all of the current workspaces.
When called with prefix ARG the default selection will be symbol at point.

(fn ARG)" t nil) (autoload 'lsp-ivy-workspace-folders-remove "lsp-ivy" "Remove a project-root from the list of workspace folders." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-ivy" '("lsp-ivy-"))) (provide 'lsp-ivy-autoloads)) "avy" ((avy-autoloads avy) (autoload 'avy-process "avy" "Select one of CANDIDATES using `avy-read'.
Use OVERLAY-FN to visualize the decision overlay.
CLEANUP-FN should take no arguments and remove the effects of
multiple OVERLAY-FN invocations.

(fn CANDIDATES &optional OVERLAY-FN CLEANUP-FN)" nil nil) (autoload 'avy-goto-char "avy" "Jump to the currently visible CHAR.
The window scope is determined by `avy-all-windows' (ARG negates it).

(fn CHAR &optional ARG)" t nil) (autoload 'avy-goto-char-in-line "avy" "Jump to the currently visible CHAR in the current line.

(fn CHAR)" t nil) (autoload 'avy-goto-char-2 "avy" "Jump to the currently visible CHAR1 followed by CHAR2.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched.

(fn CHAR1 CHAR2 &optional ARG BEG END)" t nil) (autoload 'avy-goto-char-2-above "avy" "Jump to the currently visible CHAR1 followed by CHAR2.
This is a scoped version of `avy-goto-char-2', where the scope is
the visible part of the current buffer up to point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR1 CHAR2 &optional ARG)" t nil) (autoload 'avy-goto-char-2-below "avy" "Jump to the currently visible CHAR1 followed by CHAR2.
This is a scoped version of `avy-goto-char-2', where the scope is
the visible part of the current buffer following point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR1 CHAR2 &optional ARG)" t nil) (autoload 'avy-isearch "avy" "Jump to one of the current isearch candidates." t nil) (autoload 'avy-goto-word-0 "avy" "Jump to a word start.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched.

(fn ARG &optional BEG END)" t nil) (autoload 'avy-goto-whitespace-end "avy" "Jump to the end of a whitespace sequence.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched.

(fn ARG &optional BEG END)" t nil) (autoload 'avy-goto-word-1 "avy" "Jump to the currently visible CHAR at a word start.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched.
When SYMBOL is non-nil, jump to symbol start instead of word start.

(fn CHAR &optional ARG BEG END SYMBOL)" t nil) (autoload 'avy-goto-word-1-above "avy" "Jump to the currently visible CHAR at a word start.
This is a scoped version of `avy-goto-word-1', where the scope is
the visible part of the current buffer up to point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR &optional ARG)" t nil) (autoload 'avy-goto-word-1-below "avy" "Jump to the currently visible CHAR at a word start.
This is a scoped version of `avy-goto-word-1', where the scope is
the visible part of the current buffer following point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR &optional ARG)" t nil) (autoload 'avy-goto-symbol-1 "avy" "Jump to the currently visible CHAR at a symbol start.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR &optional ARG)" t nil) (autoload 'avy-goto-symbol-1-above "avy" "Jump to the currently visible CHAR at a symbol start.
This is a scoped version of `avy-goto-symbol-1', where the scope is
the visible part of the current buffer up to point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR &optional ARG)" t nil) (autoload 'avy-goto-symbol-1-below "avy" "Jump to the currently visible CHAR at a symbol start.
This is a scoped version of `avy-goto-symbol-1', where the scope is
the visible part of the current buffer following point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR &optional ARG)" t nil) (autoload 'avy-goto-subword-0 "avy" "Jump to a word or subword start.
The window scope is determined by `avy-all-windows' (ARG negates it).

When PREDICATE is non-nil it's a function of zero parameters that
should return true.

BEG and END narrow the scope where candidates are searched.

(fn &optional ARG PREDICATE BEG END)" t nil) (autoload 'avy-goto-subword-1 "avy" "Jump to the currently visible CHAR at a subword start.
The window scope is determined by `avy-all-windows' (ARG negates it).
The case of CHAR is ignored.

(fn CHAR &optional ARG)" t nil) (autoload 'avy-goto-word-or-subword-1 "avy" "Forward to `avy-goto-subword-1' or `avy-goto-word-1'.
Which one depends on variable `subword-mode'." t nil) (autoload 'avy-goto-line "avy" "Jump to a line start in current buffer.

When ARG is 1, jump to lines currently visible, with the option
to cancel to `goto-line' by entering a number.

When ARG is 4, negate the window scope determined by
`avy-all-windows'.

Otherwise, forward to `goto-line' with ARG.

(fn &optional ARG)" t nil) (autoload 'avy-goto-line-above "avy" "Goto visible line above the cursor.
OFFSET changes the distance between the closest key to the cursor and
the cursor
When BOTTOM-UP is non-nil, display avy candidates from top to bottom

(fn &optional OFFSET BOTTOM-UP)" t nil) (autoload 'avy-goto-line-below "avy" "Goto visible line below the cursor.
OFFSET changes the distance between the closest key to the cursor and
the cursor
When BOTTOM-UP is non-nil, display avy candidates from top to bottom

(fn &optional OFFSET BOTTOM-UP)" t nil) (autoload 'avy-goto-end-of-line "avy" "Call `avy-goto-line' and move to the end of the line.

(fn &optional ARG)" t nil) (autoload 'avy-copy-line "avy" "Copy a selected line above the current line.
ARG lines can be used.

(fn ARG)" t nil) (autoload 'avy-move-line "avy" "Move a selected line above the current line.
ARG lines can be used.

(fn ARG)" t nil) (autoload 'avy-copy-region "avy" "Select two lines and copy the text between them to point.

The window scope is determined by `avy-all-windows' or
`avy-all-windows-alt' when ARG is non-nil.

(fn ARG)" t nil) (autoload 'avy-move-region "avy" "Select two lines and move the text between them above the current line." t nil) (autoload 'avy-kill-region "avy" "Select two lines and kill the region between them.

The window scope is determined by `avy-all-windows' or
`avy-all-windows-alt' when ARG is non-nil.

(fn ARG)" t nil) (autoload 'avy-kill-ring-save-region "avy" "Select two lines and save the region between them to the kill ring.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn ARG)" t nil) (autoload 'avy-kill-whole-line "avy" "Select line and kill the whole selected line.

With a numerical prefix ARG, kill ARG line(s) starting from the
selected line.  If ARG is negative, kill backward.

If ARG is zero, kill the selected line but exclude the trailing
newline.

\\[universal-argument] 3 \\[avy-kil-whole-line] kill three lines
starting from the selected line.  \\[universal-argument] -3

\\[avy-kill-whole-line] kill three lines backward including the
selected line.

(fn ARG)" t nil) (autoload 'avy-kill-ring-save-whole-line "avy" "Select line and save the whole selected line as if killed, but don\342\200\231t kill it.

This command is similar to `avy-kill-whole-line', except that it
saves the line(s) as if killed, but does not kill it(them).

With a numerical prefix ARG, kill ARG line(s) starting from the
selected line.  If ARG is negative, kill backward.

If ARG is zero, kill the selected line but exclude the trailing
newline.

(fn ARG)" t nil) (autoload 'avy-setup-default "avy" "Setup the default shortcuts." nil nil) (autoload 'avy-goto-char-timer "avy" "Read one or many consecutive chars and jump to the first one.
The window scope is determined by `avy-all-windows' (ARG negates it).

(fn &optional ARG)" t nil) (autoload 'avy-transpose-lines-in-region "avy" "Transpose lines in the active region." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "avy" '("avy-"))) (provide 'avy-autoloads)) "ace-window" ((ace-window ace-window-autoloads) (autoload 'ace-select-window "ace-window" "Ace select window." t nil) (autoload 'ace-delete-window "ace-window" "Ace delete window." t nil) (autoload 'ace-swap-window "ace-window" "Ace swap window." t nil) (autoload 'ace-delete-other-windows "ace-window" "Ace delete other windows." t nil) (autoload 'ace-display-buffer "ace-window" "Make `display-buffer' and `pop-to-buffer' select using `ace-window'.
See sample config for `display-buffer-base-action' and `display-buffer-alist':
https://github.com/abo-abo/ace-window/wiki/display-buffer.

(fn BUFFER ALIST)" nil nil) (autoload 'ace-window "ace-window" "Select a window.
Perform an action based on ARG described below.

By default, behaves like extended `other-window'.
See `aw-scope' which extends it to work with frames.

Prefixed with one \\[universal-argument], does a swap between the
selected window and the current window, so that the selected
buffer moves to current window (and current buffer moves to
selected window).

Prefixed with two \\[universal-argument]'s, deletes the selected
window.

(fn ARG)" t nil) (defvar ace-window-display-mode nil "Non-nil if Ace-Window-Display mode is enabled.
See the `ace-window-display-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `ace-window-display-mode'.") (custom-autoload 'ace-window-display-mode "ace-window" nil) (autoload 'ace-window-display-mode "ace-window" "Minor mode for showing the ace window key in the mode line.

If called interactively, enable Ace-Window-Display mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ace-window" '("ace-window-mode" "aw-"))) (provide 'ace-window-autoloads)) "pfuture" ((pfuture-autoloads pfuture) (autoload 'pfuture-new "pfuture" "Create a new future process for command CMD.
Any arguments after the command are interpreted as arguments to the command.
This will return a process object with additional 'stderr and 'stdout
properties, which can be read via (process-get process 'stdout) and
(process-get process 'stderr) or alternatively with
(pfuture-result process) or (pfuture-stderr process).

Note that CMD must be a *sequence* of strings, meaning
this is wrong: (pfuture-new \"git status\")
this is right: (pfuture-new \"git\" \"status\")

(fn &rest CMD)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pfuture" '("pfuture-"))) (provide 'pfuture-autoloads)) "posframe" ((posframe-autoloads posframe-benchmark posframe) (autoload 'posframe-workable-p "posframe" "Test posframe workable status." nil nil) (autoload 'posframe-show "posframe" "Pop up a posframe and show STRING at POSITION.

(1) POSITION

POSITION can be:
1. An integer, meaning point position.
2. A cons of two integers, meaning absolute X and Y coordinates.
3. Other type, in which case the corresponding POSHANDLER should be
   provided.

(2) POSHANDLER

POSHANDLER is a function of one argument returning an actual
position.  Its argument is a plist of the following form:

  (:position xxx
   :poshandler xxx
   :font-height xxx
   :font-width xxx
   :posframe xxx
   :posframe-width xxx
   :posframe-height xxx
   :posframe-buffer xxx
   :parent-frame xxx
   :parent-window-left xxx
   :parent-window-top xxx
   :parent-frame-width xxx
   :parent-frame-height xxx
   :parent-window xxx
   :parent-window-width  xxx
   :parent-window-height xxx
   :minibuffer-height xxx
   :mode-line-height  xxx
   :header-line-height xxx
   :tab-line-height xxx
   :x-pixel-offset xxx
   :y-pixel-offset xxx)

By default, poshandler is auto-selected based on the type of POSITION,
but the selection can be overridden using the POSHANDLER argument.
The builtin poshandler functions are listed below:

1.  `posframe-poshandler-frame-center'
2.  `posframe-poshandler-frame-top-center'
3.  `posframe-poshandler-frame-top-left-corner'
4.  `posframe-poshandler-frame-top-right-corner'
5.  `posframe-poshandler-frame-bottom-center'
6.  `posframe-poshandler-frame-bottom-left-corner'
7.  `posframe-poshandler-frame-bottom-right-corner'
8.  `posframe-poshandler-window-center'
9.  `posframe-poshandler-window-top-center'
10. `posframe-poshandler-window-top-left-corner'
11. `posframe-poshandler-window-top-right-corner'
12. `posframe-poshandler-window-bottom-center'
13. `posframe-poshandler-window-bottom-left-corner'
14. `posframe-poshandler-window-bottom-right-corner'
15. `posframe-poshandler-point-top-left-corner'
16. `posframe-poshandler-point-bottom-left-corner'
17. `posframe-poshandler-point-bottom-left-corner-upward'
18. `posframe-poshandler-point-window-center'

by the way, poshandler can be used by other packages easily
(for example: mini-frame) with the help of function
`posframe-poshandler-argbuilder'. like:

   (let* ((info (posframe-poshandler-argbuilder child-frame))
          (posn (posframe-poshandler-window-center info)))
     `((left . ,(car posn))
       (top . ,(cdr posn))))

(3) POSHANDLER-EXTRA-INFO

POSHANDLER-EXTRA-INFO is a plist, which will prepend to the
argument of poshandler function: 'info', it will *OVERRIDE* the
exist key in 'info'.

(4) BUFFER-OR-NAME

This posframe's buffer is BUFFER-OR-NAME, which can be a buffer
or a name of a (possibly nonexistent) buffer.

buffer name can prefix with space, for example ' *mybuffer*', so
the buffer name will hide for ibuffer and list-buffers.

(5) NO-PROPERTIES

If NO-PROPERTIES is non-nil, The STRING's properties will
be removed before being shown in posframe.

(6) WIDTH, MIN-WIDTH, HEIGHT and MIN-HEIGHT

WIDTH, MIN-WIDTH, HEIGHT and MIN-HEIGHT, specify bounds on the
new total size of posframe.  MIN-HEIGHT and MIN-WIDTH default to
the values of \342\200\230window-min-height\342\200\231 and \342\200\230window-min-width\342\200\231
respectively.  These arguments are specified in the canonical
character width and height of posframe.

(7) LEFT-FRINGE and RIGHT-FRINGE

If LEFT-FRINGE or RIGHT-FRINGE is a number, left fringe or
right fringe with be shown with the specified width.

(8) BORDER-WIDTH, BORDER-COLOR, INTERNAL-BORDER-WIDTH and INTERNAL-BORDER-COLOR

By default, posframe shows no borders, but users can specify
borders by setting BORDER-WIDTH to a positive number.  Border
color can be specified by BORDER-COLOR.

INTERNAL-BORDER-WIDTH and INTERNAL-BORDER-COLOR are same as
BORDER-WIDTH and BORDER-COLOR, but do not suggest to use for the
reason:

   Add distinct controls for child frames' borders (Bug#45620)
   http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=ff7b1a133bfa7f2614650f8551824ffaef13fadc

(9) FONT, FOREGROUND-COLOR and BACKGROUND-COLOR

Posframe's font as well as foreground and background colors are
derived from the current frame by default, but can be overridden
using the FONT, FOREGROUND-COLOR and BACKGROUND-COLOR arguments,
respectively.

(10) RESPECT-HEADER-LINE and RESPECT-MODE-LINE

By default, posframe will display no header-line, mode-line and
tab-line.  In case a header-line, mode-line or tab-line is
desired, users can set RESPECT-HEADER-LINE and RESPECT-MODE-LINE
to t.

(11) INITIALIZE

INITIALIZE is a function with no argument.  It will run when
posframe buffer is first selected with `with-current-buffer'
in `posframe-show', and only run once (for performance reasons).

(12) LINES-TRUNCATE

If LINES-TRUNCATE is non-nil, then lines will truncate in the
posframe instead of wrap.

(13) OVERRIDE-PARAMETERS

OVERRIDE-PARAMETERS is very powful, *all* the frame parameters
used by posframe's frame can be overridden by it.

(14) TIMEOUT

TIMEOUT can specify the number of seconds after which the posframe
will auto-hide.

(15) REFRESH

If REFRESH is a number, posframe's frame-size will be re-adjusted
every REFRESH seconds.

(16) ACCEPT-FOCUS

When ACCEPT-FOCUS is non-nil, posframe will accept focus.
be careful, you may face some bugs when set it to non-nil.

(17) HIDEHANDLER

HIDEHANDLER is a function, when it return t, posframe will be
hide, this function has a plist argument:

  (:posframe-buffer xxx
   :posframe-parent-buffer xxx)

The builtin hidehandler functions are listed below:

1. `posframe-hidehandler-when-buffer-switch'

(18) REFPOSHANDLER

REFPOSHANDLER is a function, a reference position (most is
top-left of current frame) will be returned when call this
function.

when it is nil or it return nil, child-frame feature will be used
and reference position will be deal with in emacs.

The user case I know at the moment is let ivy-posframe work well
in EXWM environment (let posframe show on the other appliction
window).

         DO NOT USE UNLESS NECESSARY!!!

An example parent frame poshandler function is:

1. `posframe-refposhandler-xwininfo'

(19) Others

You can use `posframe-delete-all' to delete all posframes.

(fn BUFFER-OR-NAME &key STRING POSITION POSHANDLER POSHANDLER-EXTRA-INFO WIDTH HEIGHT MIN-WIDTH MIN-HEIGHT X-PIXEL-OFFSET Y-PIXEL-OFFSET LEFT-FRINGE RIGHT-FRINGE BORDER-WIDTH BORDER-COLOR INTERNAL-BORDER-WIDTH INTERNAL-BORDER-COLOR FONT FOREGROUND-COLOR BACKGROUND-COLOR RESPECT-HEADER-LINE RESPECT-MODE-LINE INITIALIZE NO-PROPERTIES KEEP-RATIO LINES-TRUNCATE OVERRIDE-PARAMETERS TIMEOUT REFRESH ACCEPT-FOCUS HIDEHANDLER REFPOSHANDLER &allow-other-keys)" nil nil) (autoload 'posframe-hide-all "posframe" "Hide all posframe frames." t nil) (autoload 'posframe-delete-all "posframe" "Delete all posframe frames and buffers." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "posframe" '("posframe-"))) (autoload 'posframe-benchmark "posframe-benchmark" "Benchmark tool for posframe." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "posframe-benchmark" '("posframe-benchmark-alist"))) (provide 'posframe-autoloads)) "cfrs" ((cfrs-autoloads cfrs) (autoload 'cfrs-read "cfrs" "Read a string using a pos-frame with given PROMPT and INITIAL-INPUT.

(fn PROMPT &optional INITIAL-INPUT)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "cfrs" '("cfrs-"))) (provide 'cfrs-autoloads)) "treemacs" ((treemacs-autoloads treemacs-themes treemacs-mode treemacs-logging treemacs-customization treemacs-follow-mode treemacs-workspaces treemacs treemacs-scope treemacs-interface treemacs-tags treemacs-filewatch-mode treemacs-compatibility treemacs-hydras treemacs-header-line treemacs-visuals treemacs-dom treemacs-core-utils treemacs-diagnostics treemacs-faces treemacs-icons treemacs-tag-follow-mode treemacs-bookmarks treemacs-rendering treemacs-project-follow-mode treemacs-async treemacs-extensions treemacs-fringe-indicator treemacs-macros treemacs-mouse-interface treemacs-persistence) (autoload 'treemacs-version "treemacs" "Return the `treemacs-version'." t nil) (autoload 'treemacs "treemacs" "Initialise or toggle treemacs.
* If the treemacs window is visible hide it.
* If a treemacs buffer exists, but is not visible show it.
* If no treemacs buffer exists for the current frame create and show it.
* If the workspace is empty additionally ask for the root path of the first
  project to add." t nil) (autoload 'treemacs-find-file "treemacs" "Find and focus the current file in the treemacs window.
If the current buffer has visits no file or with a prefix ARG ask for the
file instead.
Will show/create a treemacs buffers if it is not visible/does not exist.
For the most part only useful when `treemacs-follow-mode' is not active.

(fn &optional ARG)" t nil) (autoload 'treemacs-find-tag "treemacs" "Find and move point to the tag at point in the treemacs view.
Most likely to be useful when `treemacs-tag-follow-mode' is not active.

Will ask to change the treemacs root if the file to find is not under the
root.  If no treemacs buffer exists it will be created with the current file's
containing directory as root.  Will do nothing if the current buffer is not
visiting a file or Emacs cannot find any tags for the current file." t nil) (autoload 'treemacs-select-window "treemacs" "Select the treemacs window if it is visible.
Bring it to the foreground if it is not visible.
Initialise a new treemacs buffer as calling `treemacs' would if there is no
treemacs buffer for this frame.
Jump back to the previously used window if point is already in treemacs." t nil) (autoload 'treemacs-show-changelog "treemacs" "Show the changelog of treemacs." t nil) (autoload 'treemacs-edit-workspaces "treemacs" "Edit your treemacs workspaces and projects as an `org-mode' file." t nil) (autoload 'treemacs-display-current-project-exclusively "treemacs" "Display the current project, and *only* the current project.
Like `treemacs-add-and-display-current-project' this will add the current
project to treemacs based on either projectile, the built-in project.el, or the
current working directory.

However the 'exclusive' part means that it will make the current project the
only project, all other projects *will be removed* from the current workspace." t nil) (autoload 'treemacs-add-and-display-current-project "treemacs" "Open treemacs and add the current project root to the workspace.
The project is determined first by projectile (if treemacs-projectile is
installed), then by project.el, then by the current working directory.

If the project is already registered with treemacs just move point to its root.
An error message is displayed if the current buffer is not part of any project." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs" '("treemacs-version"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-async" '("treemacs-"))) (autoload 'treemacs-bookmark "treemacs-bookmarks" "Find a bookmark in treemacs.
Only bookmarks marking either a file or a directory are offered for selection.
Treemacs will try to find and focus the given bookmark's location, in a similar
fashion to `treemacs-find-file'.

With a prefix argument ARG treemacs will also open the bookmarked location.

(fn &optional ARG)" t nil) (autoload 'treemacs--bookmark-handler "treemacs-bookmarks" "Open Treemacs into a bookmark RECORD.

(fn RECORD)" nil nil) (autoload 'treemacs-add-bookmark "treemacs-bookmarks" "Add the current node to Emacs' list of bookmarks.
For file and directory nodes their absolute path is saved.  Tag nodes
additionally also save the tag's position.  A tag can only be bookmarked if the
treemacs node is pointing to a valid buffer position." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-bookmarks" '("treemacs--"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-compatibility" '("treemacs-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-core-utils" '("treemacs-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-customization" '("treemacs-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-diagnostics" '("treemacs-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-dom" '("treemacs-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-extensions" '("treemacs-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-filewatch-mode" '("treemacs-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-follow-mode" '("treemacs-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-fringe-indicator" '("treemacs-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-header-line" '("treemacs-header-buttons-format"))) (autoload 'treemacs-common-helpful-hydra "treemacs-hydras" "Summon a helpful hydra to show you the treemacs keymap.

This hydra will show the most commonly used keybinds for treemacs.  For the more
advanced (probably rarely used keybinds) see `treemacs-advanced-helpful-hydra'.

The keybinds shown in this hydra are not static, but reflect the actual
keybindings currently in use (including evil mode).  If the hydra is unable to
find the key a command is bound to it will show a blank instead." t nil) (autoload 'treemacs-advanced-helpful-hydra "treemacs-hydras" "Summon a helpful hydra to show you the treemacs keymap.

This hydra will show the more advanced (rarely used) keybinds for treemacs.  For
the more commonly used keybinds see `treemacs-common-helpful-hydra'.

The keybinds shown in this hydra are not static, but reflect the actual
keybindings currently in use (including evil mode).  If the hydra is unable to
find the key a command is bound to it will show a blank instead." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-hydras" '("treemacs-helpful-hydra"))) (autoload 'treemacs-resize-icons "treemacs-icons" "Resize the current theme's icons to the given SIZE.

If SIZE is 'nil' the icons are not resized and will retain their default size of
22 pixels.

There is only one size, the icons are square and the aspect ratio will be
preserved when resizing them therefore width and height are the same.

Resizing the icons only works if Emacs was built with ImageMagick support, or if
using Emacs >= 27.1,which has native image resizing support.  If this is not the
case this function will not have any effect.

Custom icons are not taken into account, only the size of treemacs' own icons
png are changed.

(fn SIZE)" t nil) (autoload 'treemacs-define-custom-icon "treemacs-icons" "Define a custom ICON for the current theme to use for FILE-EXTENSIONS.

Note that treemacs has a very loose definition of what constitutes a file
extension - it's either everything past the last period, or just the file's full
name if there is no period.  This makes it possible to match file names like
'.gitignore' and 'Makefile'.

Additionally FILE-EXTENSIONS are also not case sensitive and will be stored in a
down-cased state.

(fn ICON &rest FILE-EXTENSIONS)" nil nil) (autoload 'treemacs-define-custom-image-icon "treemacs-icons" "Same as `treemacs-define-custom-icon' but for image icons instead of strings.
FILE is the path to an icon image (and not the actual icon string).
FILE-EXTENSIONS are all the (not case-sensitive) file extensions the icon
should be used for.

(fn FILE &rest FILE-EXTENSIONS)" nil nil) (autoload 'treemacs-map-icons-with-auto-mode-alist "treemacs-icons" "Remaps icons for EXTENSIONS according to `auto-mode-alist'.
EXTENSIONS should be a list of file extensions such that they match the regex
stored in `auto-mode-alist', for example '(\".cc\").
MODE-ICON-ALIST is an alist that maps which mode from `auto-mode-alist' should
be assigned which treemacs icon, for example
'((c-mode . treemacs-icon-c)
  (c++-mode . treemacs-icon-cpp))

(fn EXTENSIONS MODE-ICON-ALIST)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-icons" '("treemacs-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-interface" '("treemacs-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-logging" '("treemacs-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-macros" '("treemacs-"))) (autoload 'treemacs-mode "treemacs-mode" "A major mode for displaying the file system in a tree layout.

(fn)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-mode" '("treemacs-"))) (autoload 'treemacs-leftclick-action "treemacs-mouse-interface" "Move focus to the clicked line.
Must be bound to a mouse click, or EVENT will not be supplied.

(fn EVENT)" t nil) (autoload 'treemacs-doubleclick-action "treemacs-mouse-interface" "Run the appropriate double-click action for the current node.
In the default configuration this means to do the same as `treemacs-RET-action'.

This function's exact configuration is stored in
`treemacs-doubleclick-actions-config'.

Must be bound to a mouse click, or EVENT will not be supplied.

(fn EVENT)" t nil) (autoload 'treemacs-single-click-expand-action "treemacs-mouse-interface" "A modified single-leftclick action that expands the clicked nodes.
Can be bound to <mouse1> if you prefer to expand nodes with a single click
instead of a double click.  Either way it must be bound to a mouse click, or
EVENT will not be supplied.

Clicking on icons will expand a file's tags, just like
`treemacs-leftclick-action'.

(fn EVENT)" t nil) (autoload 'treemacs-dragleftclick-action "treemacs-mouse-interface" "Drag a file/dir node to be opened in a window.
Must be bound to a mouse click, or EVENT will not be supplied.

(fn EVENT)" t nil) (autoload 'treemacs-define-doubleclick-action "treemacs-mouse-interface" "Define the behaviour of `treemacs-doubleclick-action'.
Determines that a button with a given STATE should lead to the execution of
ACTION.

The list of possible states can be found in `treemacs-valid-button-states'.
ACTION should be one of the `treemacs-visit-node-*' commands.

(fn STATE ACTION)" nil nil) (autoload 'treemacs-node-buffer-and-position "treemacs-mouse-interface" "Return source buffer or list of buffer and position for the current node.
This information can be used for future display.  Stay in the selected window
and ignore any prefix argument.

(fn &optional _)" t nil) (autoload 'treemacs-rightclick-menu "treemacs-mouse-interface" "Show a contextual right click menu based on click EVENT.

(fn EVENT)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-mouse-interface" '("treemacs--"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-persistence" '("treemacs-"))) (defvar treemacs-project-follow-mode nil "Non-nil if Treemacs-Project-Follow mode is enabled.
See the `treemacs-project-follow-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `treemacs-project-follow-mode'.") (custom-autoload 'treemacs-project-follow-mode "treemacs-project-follow-mode" nil) (autoload 'treemacs-project-follow-mode "treemacs-project-follow-mode" "Toggle `treemacs-only-current-project-mode'.

If called interactively, enable Treemacs-Project-Follow mode if
ARG is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

This is a minor mode meant for those who do not care about treemacs' workspace
features, or its preference to work with multiple projects simultaneously.  When
enabled it will function as an automated version of
`treemacs-display-current-project-exclusively', making sure that, after a small
idle delay, the current project, and *only* the current project, is displayed in
treemacs.

The project detection is based on the current buffer, and will try to determine
the project using the following methods, in the order they are listed:

- the current projectile.el project, if `treemacs-projectile' is installed
- the current project.el project
- the current `default-directory'

The update will only happen when treemacs is in the foreground, meaning a
treemacs window must exist in the current scope.

This mode requires at least Emacs version 27 since it relies on
`window-buffer-change-functions' and `window-selection-change-functions'.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-project-follow-mode" '("treemacs--"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-rendering" '("treemacs-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-scope" '("treemacs-"))) (autoload 'treemacs--flatten&sort-imenu-index "treemacs-tag-follow-mode" "Flatten current file's imenu index and sort it by tag position.
The tags are sorted into the order in which they appear, regardless of section
or nesting depth." nil nil) (defvar treemacs-tag-follow-mode nil "Non-nil if Treemacs-Tag-Follow mode is enabled.
See the `treemacs-tag-follow-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `treemacs-tag-follow-mode'.") (custom-autoload 'treemacs-tag-follow-mode "treemacs-tag-follow-mode" nil) (autoload 'treemacs-tag-follow-mode "treemacs-tag-follow-mode" "Toggle `treemacs-tag-follow-mode'.

If called interactively, enable Treemacs-Tag-Follow mode if ARG
is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

This acts as more fine-grained alternative to `treemacs-follow-mode' and will
thus disable `treemacs-follow-mode' on activation.  When enabled treemacs will
focus not only the file of the current buffer, but also the tag at point.

The follow action is attached to Emacs' idle timer and will run
`treemacs-tag-follow-delay' seconds of idle time.  The delay value is not an
integer, meaning it accepts floating point values like 1.5.

Every time a tag is followed a re--scan of the imenu index is forced by
temporarily setting `imenu-auto-rescan' to t (though a cache is applied as long
as the buffer is unmodified).  This is necessary to assure that creation or
deletion of tags does not lead to errors and guarantees an always up-to-date tag
view.

Note that in order to move to a tag in treemacs the treemacs buffer's window
needs to be temporarily selected, which will reset blink-cursor-mode's timer if
it is enabled.  This will result in the cursor blinking seemingly pausing for a
short time and giving the appearance of the tag follow action lasting much
longer than it really does.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-tag-follow-mode" '("treemacs--"))) (autoload 'treemacs--expand-file-node "treemacs-tags" "Open tag items for file BTN.
Recursively open all tags below BTN when RECURSIVE is non-nil.

(fn BTN &optional RECURSIVE)" nil nil) (autoload 'treemacs--collapse-file-node "treemacs-tags" "Close node given by BTN.
Remove all open tag entries under BTN when RECURSIVE.

(fn BTN &optional RECURSIVE)" nil nil) (autoload 'treemacs--visit-or-expand/collapse-tag-node "treemacs-tags" "Visit tag section BTN if possible, expand or collapse it otherwise.
Pass prefix ARG on to either visit or toggle action.

FIND-WINDOW is a special provision depending on this function's invocation
context and decides whether to find the window to display in (if the tag is
visited instead of the node being expanded).

On the one hand it can be called based on `treemacs-RET-actions-config' (or
TAB).  The functions in these configs are expected to find the windows they need
to display in themselves, so FIND-WINDOW must be t. On the other hand this
function is also called from the top level vist-node functions like
`treemacs-visit-node-vertical-split' which delegates to the
`treemacs--execute-button-action' macro which includes the determination of
the display window.

(fn BTN ARG FIND-WINDOW)" nil nil) (autoload 'treemacs--expand-tag-node "treemacs-tags" "Open tags node items for BTN.
Open all tag section under BTN when call is RECURSIVE.

(fn BTN &optional RECURSIVE)" nil nil) (autoload 'treemacs--collapse-tag-node "treemacs-tags" "Close tags node at BTN.
Remove all open tag entries under BTN when RECURSIVE.

(fn BTN &optional RECURSIVE)" nil nil) (autoload 'treemacs--goto-tag "treemacs-tags" "Go to the tag at BTN.

(fn BTN)" nil nil) (autoload 'treemacs--create-imenu-index-function "treemacs-tags" "The `imenu-create-index-function' for treemacs buffers." nil nil) (function-put 'treemacs--create-imenu-index-function 'side-effect-free 't) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-tags" '("treemacs--"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-themes" '("treemacs-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-visuals" '("treemacs-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-workspaces" '("treemacs-"))) (provide 'treemacs-autoloads)) "lsp-treemacs" ((lsp-treemacs lsp-treemacs-themes lsp-treemacs-autoloads) (autoload 'lsp-treemacs-symbols "lsp-treemacs" "Show symbols view." t nil) (autoload 'lsp-treemacs-java-deps-list "lsp-treemacs" "Display java dependencies." t nil) (autoload 'lsp-treemacs-java-deps-follow "lsp-treemacs" nil t nil) (defvar lsp-treemacs-sync-mode nil "Non-nil if Lsp-Treemacs-Sync mode is enabled.
See the `lsp-treemacs-sync-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `lsp-treemacs-sync-mode'.") (custom-autoload 'lsp-treemacs-sync-mode "lsp-treemacs" nil) (autoload 'lsp-treemacs-sync-mode "lsp-treemacs" "Global minor mode for synchronizing lsp-mode workspace folders and treemacs projects.

If called interactively, enable Lsp-Treemacs-Sync mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (autoload 'lsp-treemacs-references "lsp-treemacs" "Show the references for the symbol at point.
With a prefix argument, select the new window and expand the tree of references automatically.

(fn ARG)" t nil) (autoload 'lsp-treemacs-implementations "lsp-treemacs" "Show the implementations for the symbol at point.
With a prefix argument, select the new window expand the tree of implementations automatically.

(fn ARG)" t nil) (autoload 'lsp-treemacs-call-hierarchy "lsp-treemacs" "Show the incoming call hierarchy for the symbol at point.
With a prefix argument, show the outgoing call hierarchy.

(fn OUTGOING)" t nil) (autoload 'lsp-treemacs-type-hierarchy "lsp-treemacs" "Show the type hierarchy for the symbol at point.
With prefix 0 show sub-types.
With prefix 1 show super-types.
With prefix 2 show both.

(fn DIRECTION)" t nil) (autoload 'lsp-treemacs-errors-list "lsp-treemacs" nil t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-treemacs" '("lsp-tree"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-treemacs-themes" '("lsp-treemacs-theme"))) (provide 'lsp-treemacs-autoloads)) "bui" ((bui-core bui-button bui-history bui-list bui-utils bui-info bui-entry bui-autoloads bui) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "bui" '("bui-define-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "bui-button" '("bui"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "bui-core" '("bui-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "bui-entry" '("bui-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "bui-history" '("bui-history"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "bui-info" '("bui-info-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "bui-list" '("bui-list-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "bui-utils" '("bui-"))) (provide 'bui-autoloads)) "dap-mode" ((dap-mode dap-variables dap-cpptools dap-ruby dap-codelldb dap-php dap-go dap-gdb-lldb dap-erlang dap-launch dap-swi-prolog dap-firefox dap-mouse dap-chrome dap-hydra dap-netcore dap-overlays dap-edge dapui dap-ui dap-python dap-utils dap-node dap-elixir dap-pwsh dap-lldb dap-mode-autoloads) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dap-chrome" '("dap-chrome-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dap-codelldb" '("dap-codelldb-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dap-cpptools" '("dap-cpptools-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dap-edge" '("dap-edge-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dap-elixir" '("dap-elixir--populate-start-file-args"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dap-erlang" '("dap-erlang--populate-start-file-args"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dap-firefox" '("dap-firefox-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dap-gdb-lldb" '("dap-gdb-lldb-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dap-go" '("dap-go-"))) (autoload 'dap-hydra "dap-hydra" "Run `dap-hydra/body'." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dap-hydra" '("dap-hydra"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dap-launch" '("dap-launch-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dap-lldb" '("dap-lldb-"))) (autoload 'dap-debug "dap-mode" "Run debug configuration DEBUG-ARGS.

If DEBUG-ARGS is not specified the configuration is generated
after selecting configuration template.

:dap-compilation specifies a shell command to be run using
`compilation-start' before starting the debug session. It could
be used to compile the project, spin up docker, ....

(fn DEBUG-ARGS)" t nil) (defvar dap-mode nil "Non-nil if Dap mode is enabled.
See the `dap-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `dap-mode'.") (custom-autoload 'dap-mode "dap-mode" nil) (autoload 'dap-mode "dap-mode" "Global minor mode for DAP mode.

If called interactively, enable Dap mode if ARG is positive, and
disable it if ARG is zero or negative.  If called from Lisp, also
enable the mode if ARG is omitted or nil, and toggle it if ARG is
`toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (defvar dap-auto-configure-mode nil "Non-nil if Dap-Auto-Configure mode is enabled.
See the `dap-auto-configure-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `dap-auto-configure-mode'.") (custom-autoload 'dap-auto-configure-mode "dap-mode" nil) (autoload 'dap-auto-configure-mode "dap-mode" "Auto configure dap minor mode.

If called interactively, enable Dap-Auto-Configure mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dap-mode" '("dap-"))) (defvar dap-tooltip-mode nil "Non-nil if Dap-Tooltip mode is enabled.
See the `dap-tooltip-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `dap-tooltip-mode'.") (custom-autoload 'dap-tooltip-mode "dap-mouse" nil) (autoload 'dap-tooltip-mode "dap-mouse" "Toggle the display of GUD tooltips.

If called interactively, enable Dap-Tooltip mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dap-mouse" '("dap-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dap-netcore" '("dap-netcore-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dap-node" '("dap-node-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dap-overlays" '("dap-overlays-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dap-php" '("dap-php-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dap-pwsh" '("dap-pwsh-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dap-python" '("dap-python-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dap-ruby" '("dap-ruby-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dap-swi-prolog" '("dap-swi-prolog-"))) (defvar dap-ui-mode nil "Non-nil if Dap-Ui mode is enabled.
See the `dap-ui-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `dap-ui-mode'.") (custom-autoload 'dap-ui-mode "dap-ui" nil) (autoload 'dap-ui-mode "dap-ui" "Displaying DAP visuals.

If called interactively, enable Dap-Ui mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (autoload 'dap-ui-breakpoints-list "dap-ui" "List breakpoints." t nil) (defvar dap-ui-controls-mode nil "Non-nil if Dap-Ui-Controls mode is enabled.
See the `dap-ui-controls-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `dap-ui-controls-mode'.") (custom-autoload 'dap-ui-controls-mode "dap-ui" nil) (autoload 'dap-ui-controls-mode "dap-ui" "Displaying DAP visuals.

If called interactively, enable Dap-Ui-Controls mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (autoload 'dap-ui-sessions "dap-ui" "Show currently active sessions." t nil) (autoload 'dap-ui-locals "dap-ui" nil t nil) (autoload 'dap-ui-show-many-windows "dap-ui" "Show auto configured feature windows." t nil) (autoload 'dap-ui-hide-many-windows "dap-ui" "Hide all debug windows when sessions are dead." t nil) (autoload 'dap-ui-repl "dap-ui" "Start an adapter-specific REPL.
This could be used to evaluate JavaScript in a browser, to
evaluate python in the context of the debugee, ...." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dap-ui" '("dap-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dap-utils" '("dap-utils-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dap-variables" '("dap-variables-"))) (autoload 'dapui-loaded-sources "dapui" nil t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dapui" '("dapui-"))) (provide 'dap-mode-autoloads)) "company-lsp" ((company-lsp company-lsp-autoloads) (autoload 'company-lsp "company-lsp" "Define a company backend for lsp-mode.

See the documentation of `company-backends' for COMMAND and ARG.

(fn COMMAND &optional ARG &rest _)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-lsp" '("company-lsp-"))) (provide 'company-lsp-autoloads)) "exec-path-from-shell" ((exec-path-from-shell-autoloads exec-path-from-shell) (autoload 'exec-path-from-shell-copy-envs "exec-path-from-shell" "Set the environment variables with NAMES from the user's shell.

As a special case, if the variable is $PATH, then the variables
`exec-path' and `eshell-path-env' are also set appropriately.
The result is an alist, as described by
`exec-path-from-shell-getenvs'.

(fn NAMES)" nil nil) (autoload 'exec-path-from-shell-copy-env "exec-path-from-shell" "Set the environment variable $NAME from the user's shell.

As a special case, if the variable is $PATH, then the variables
`exec-path' and `eshell-path-env' are also set appropriately.
Return the value of the environment variable.

(fn NAME)" t nil) (autoload 'exec-path-from-shell-initialize "exec-path-from-shell" "Initialize environment from the user's shell.

The values of all the environment variables named in
`exec-path-from-shell-variables' are set from the corresponding
values used in the user's shell." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "exec-path-from-shell" '("exec-path-from-shell-"))) (provide 'exec-path-from-shell-autoloads)) "yaml-mode" ((yaml-mode yaml-mode-autoloads) (let ((loads (get 'yaml 'custom-loads))) (if (member '"yaml-mode" loads) nil (put 'yaml 'custom-loads (cons '"yaml-mode" loads)))) (autoload 'yaml-mode "yaml-mode" "Simple mode to edit YAML.

\\{yaml-mode-map}

(fn)" t nil) (add-to-list 'auto-mode-alist '("\\.\\(e?ya?\\|ra\\)ml\\'" . yaml-mode)) (add-to-list 'magic-mode-alist '("^%YAML\\s-+[0-9]+\\.[0-9]+\\(\\s-+#\\|\\s-*$\\)" . yaml-mode)) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "yaml-mode" '("yaml-"))) (provide 'yaml-mode-autoloads)) "smartparens" ((smartparens-javascript smartparens-scala smartparens-autoloads smartparens-ml smartparens-haskell smartparens-crystal smartparens-ruby smartparens smartparens-elixir smartparens-ess smartparens-html smartparens-text sp-sublimetext-like smartparens-org smartparens-racket smartparens-latex smartparens-python smartparens-markdown smartparens-config smartparens-pkg smartparens-c smartparens-rst smartparens-clojure smartparens-lua smartparens-rust) (autoload 'sp-cheat-sheet "smartparens" "Generate a cheat sheet of all the smartparens interactive functions.

Without a prefix argument, print only the short documentation and examples.

With non-nil prefix argument ARG, show the full documentation for each function.

You can follow the links to the function or variable help page.
To get back to the full list, use \\[help-go-back].

You can use `beginning-of-defun' and `end-of-defun' to jump to
the previous/next entry.

Examples are fontified using the `font-lock-string-face' for
better orientation.

(fn &optional ARG)" t nil) (defvar smartparens-mode-map (make-sparse-keymap) "Keymap used for `smartparens-mode'.") (autoload 'sp-use-paredit-bindings "smartparens" "Initiate `smartparens-mode-map' with `sp-paredit-bindings'." t nil) (autoload 'sp-use-smartparens-bindings "smartparens" "Initiate `smartparens-mode-map' with `sp-smartparens-bindings'." t nil) (autoload 'smartparens-mode "smartparens" "Toggle smartparens mode.

If called interactively, enable Smartparens mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

You can enable pre-set bindings by customizing
`sp-base-key-bindings' variable.  The current content of
`smartparens-mode-map' is:

 \\{smartparens-mode-map}

(fn &optional ARG)" t nil) (autoload 'smartparens-strict-mode "smartparens" "Toggle the strict smartparens mode.

If called interactively, enable Smartparens-Strict mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

When strict mode is active, `delete-char', `kill-word' and their
backward variants will skip over the pair delimiters in order to
keep the structure always valid (the same way as `paredit-mode'
does).  This is accomplished by remapping them to
`sp-delete-char' and `sp-kill-word'.  There is also function
`sp-kill-symbol' that deletes symbols instead of words, otherwise
working exactly the same (it is not bound to any key by default).

When strict mode is active, this is indicated with \"/s\"
after the smartparens indicator in the mode list.

(fn &optional ARG)" t nil) (put 'smartparens-global-strict-mode 'globalized-minor-mode t) (defvar smartparens-global-strict-mode nil "Non-nil if Smartparens-Global-Strict mode is enabled.
See the `smartparens-global-strict-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `smartparens-global-strict-mode'.") (custom-autoload 'smartparens-global-strict-mode "smartparens" nil) (autoload 'smartparens-global-strict-mode "smartparens" "Toggle Smartparens-Strict mode in all buffers.
With prefix ARG, enable Smartparens-Global-Strict mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Smartparens-Strict mode is enabled in all buffers where
`turn-on-smartparens-strict-mode' would do it.
See `smartparens-strict-mode' for more information on Smartparens-Strict mode.

(fn &optional ARG)" t nil) (autoload 'turn-on-smartparens-strict-mode "smartparens" "Turn on `smartparens-strict-mode'." t nil) (autoload 'turn-off-smartparens-strict-mode "smartparens" "Turn off `smartparens-strict-mode'." t nil) (put 'smartparens-global-mode 'globalized-minor-mode t) (defvar smartparens-global-mode nil "Non-nil if Smartparens-Global mode is enabled.
See the `smartparens-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `smartparens-global-mode'.") (custom-autoload 'smartparens-global-mode "smartparens" nil) (autoload 'smartparens-global-mode "smartparens" "Toggle Smartparens mode in all buffers.
With prefix ARG, enable Smartparens-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Smartparens mode is enabled in all buffers where
`turn-on-smartparens-mode' would do it.
See `smartparens-mode' for more information on Smartparens mode.

(fn &optional ARG)" t nil) (autoload 'turn-on-smartparens-mode "smartparens" "Turn on `smartparens-mode'.

This function is used to turn on `smartparens-global-mode'.

By default `smartparens-global-mode' ignores buffers with
`mode-class' set to special, but only if they are also not comint
buffers.

Additionally, buffers on `sp-ignore-modes-list' are ignored.

You can still turn on smartparens in these mode manually (or
in mode's startup-hook etc.) by calling `smartparens-mode'." t nil) (autoload 'turn-off-smartparens-mode "smartparens" "Turn off `smartparens-mode'." t nil) (autoload 'show-smartparens-mode "smartparens" "Toggle visualization of matching pairs.  When enabled, any
matching pair is highlighted after `sp-show-pair-delay' seconds
of Emacs idle time if the point is immediately in front or after
a pair.  This mode works similarly to `show-paren-mode', but
support custom pairs.

If called interactively, enable Show-Smartparens mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (put 'show-smartparens-global-mode 'globalized-minor-mode t) (defvar show-smartparens-global-mode nil "Non-nil if Show-Smartparens-Global mode is enabled.
See the `show-smartparens-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `show-smartparens-global-mode'.") (custom-autoload 'show-smartparens-global-mode "smartparens" nil) (autoload 'show-smartparens-global-mode "smartparens" "Toggle Show-Smartparens mode in all buffers.
With prefix ARG, enable Show-Smartparens-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Show-Smartparens mode is enabled in all buffers where
`turn-on-show-smartparens-mode' would do it.
See `show-smartparens-mode' for more information on Show-Smartparens mode.

(fn &optional ARG)" t nil) (autoload 'turn-on-show-smartparens-mode "smartparens" "Turn on `show-smartparens-mode'." t nil) (autoload 'turn-off-show-smartparens-mode "smartparens" "Turn off `show-smartparens-mode'." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens" '("smartparens-" "sp-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-clojure" '("sp-clojure-prefix"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-config" '("sp-lisp-invalid-hyperlink-p"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-crystal" '("sp-crystal-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-elixir" '("sp-elixir-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-ess" '("sp-ess-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-haskell" '("sp-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-html" '("sp-html-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-latex" '("sp-latex-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-lua" '("sp-lua-post-keyword-insert"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-markdown" '("sp-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-org" '("sp-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-python" '("sp-python-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-rst" '("sp-rst-point-after-backtick"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-ruby" '("sp-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-rust" '("sp-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-scala" '("sp-scala-wrap-with-indented-newlines"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-text" '("sp-text-mode-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sp-sublimetext-like" '("sp-point-not-before-word"))) (provide 'smartparens-autoloads)) "lsp-jedi" ((lsp-jedi-autoloads lsp-jedi) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-jedi" '("lsp-jedi-"))) (provide 'lsp-jedi-autoloads)) "py-autopep8" ((py-autopep8-autoloads py-autopep8) (autoload 'py-autopep8 "py-autopep8" "Deprecated! Use py-autopep8-buffer instead." t nil) (autoload 'py-autopep8-buffer "py-autopep8" "Uses the \"autopep8\" tool to reformat the current buffer." t nil) (autoload 'py-autopep8-enable-on-save "py-autopep8" "Pre-save hook to be used before running autopep8." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "py-autopep8" '("py-autopep8-"))) (provide 'py-autopep8-autoloads)) "deferred" ((deferred-autoloads deferred) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "deferred" '("deferred:"))) (provide 'deferred-autoloads)) "concurrent" ((concurrent-autoloads concurrent) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "concurrent" '("cc:"))) (provide 'concurrent-autoloads)) "ctable" ((ctable ctable-autoloads) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ctable" '("ctbl:"))) (provide 'ctable-autoloads)) "epc" ((epc-autoloads epc epcs) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "epc" '("epc:"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "epcs" '("epcs:"))) (provide 'epc-autoloads)) "python-environment" ((python-environment-autoloads test-python-environment python-environment) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "python-environment" '("python-environment-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "test-python-environment" '("pye-"))) (provide 'python-environment-autoloads)) "jedi-core" ((jedi-core-autoloads jedi-core) (autoload 'jedi:start-dedicated-server "jedi-core" "Start Jedi server dedicated to this buffer.
This is useful, for example, when you want to use different
`sys.path' for some buffer.  When invoked as an interactive
command, it asks you how to start the Jedi server.  You can edit
the command in minibuffer to specify the way Jedi server run.

If you want to setup how Jedi server is started programmatically
per-buffer/per-project basis, make `jedi:server-command' and
`jedi:server-args' buffer local and set it in `python-mode-hook'.
See also: `jedi:server-args'.

(fn COMMAND)" t nil) (autoload 'helm-jedi-related-names "jedi-core" "Find related names of the object at point using `helm' interface." t nil) (autoload 'anything-jedi-related-names "jedi-core" "Find related names of the object at point using `anything' interface." t nil) (autoload 'jedi:setup "jedi-core" "Fully setup jedi.el for current buffer.
It setups `ac-sources' or `company-backends' and turns
`jedi-mode' on.

This function is intended to be called from `python-mode-hook',
like this::

       (add-hook 'python-mode-hook 'jedi:setup)

You can also call this function as a command, to quickly test
what jedi can do." t nil) (autoload 'jedi:install-server "jedi-core" "This command installs Jedi server script jediepcserver.py in a
Python environment dedicated to Emacs.  By default, the
environment is at ``~/.emacs.d/.python-environments/default/``.
This environment is automatically created by ``virtualenv`` if it
does not exist.

Run this command (i.e., type ``M-x jedi:install-server RET``)
whenever Jedi.el shows a message to do so.  It is a good idea to
run this every time after you update Jedi.el to sync version of
Python modules used by Jedi.el and Jedi.el itself.

You can modify the location of the environment by changing
`jedi:environment-root' and/or `python-environment-directory'.  More
specifically, Jedi.el will install Python modules under the directory
``PYTHON-ENVIRONMENT-DIRECTORY/JEDI:ENVIRONMENT-ROOT``.  Note that you
need command line program ``virtualenv``.  If you have the command in
an unusual location, use `python-environment-virtualenv' to specify the
location.

.. NOTE:: jediepcserver.py is installed in a virtual environment but it
   does not mean Jedi.el cannot recognize the modules in virtual
   environment you are using for your Python development.  Jedi
   EPC server recognize the virtualenv it is in (i.e., the
   environment variable ``VIRTUAL_ENV`` in your Emacs) and then
   add modules in that environment to its ``sys.path``.  You can
   also add ``--virtual-env PATH/TO/ENV`` to `jedi:server-args'
   to include modules of virtual environment even you launch
   Emacs outside of the virtual environment.

.. NOTE:: It is highly recommended to use this command to install
   Python modules for Jedi.el.  You still can install Python
   modules used by Jedi.el manually.  However, you are then
   responsible for keeping Jedi.el and Python modules compatible.

See also:

- https://github.com/tkf/emacs-jedi/pull/72
- https://github.com/tkf/emacs-jedi/issues/140#issuecomment-37358527" t nil) (autoload 'jedi:reinstall-server "jedi-core" "Reinstall Jedi server script jediepcserver.py." t nil) (autoload 'jedi:install-server-block "jedi-core" "Blocking version `jedi:install-server'." nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jedi-core" '("jedi"))) (provide 'jedi-core-autoloads)) "company-jedi" ((company-jedi-autoloads company-jedi) (autoload 'company-jedi "company-jedi" "A `command:company-mode' completion back-end for jedi-core.
Provide completion info according to COMMAND and ARG.

(fn COMMAND &optional ARG &rest IGNORED)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-jedi" '("company-jedi-"))) (provide 'company-jedi-autoloads)) "graphviz-dot-mode" ((company-graphviz-dot graphviz-dot-mode-autoloads graphviz-dot-mode) (autoload 'company-graphviz-dot-backend "company-graphviz-dot" "Company backend for `graphviz-dot-mode'.
In the signature, COMMAND, ARG and IGNORED are mandated by `company-mode'.

(fn COMMAND &optional ARG &rest IGNORED)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-graphviz-dot" '("company-g"))) (autoload 'graphviz-dot-mode "graphviz-dot-mode" "Major mode for the dot language. \\<graphviz-dot-mode-map>
TAB indents for graph lines.

\\[graphviz-dot-indent-graph]	- Indentation function.
\\[graphviz-dot-preview]	- Previews graph in a buffer.
\\[graphviz-dot-view]	- Views graph in an external viewer.
\\[graphviz-dot-indent-line]	- Indents current line of code.

Variables specific to this mode:

  `graphviz-dot-dot-program'                   (default `dot')
       Program used to compile the graphs.
  `graphviz-dot-preview-extension'             (default `png')
       File type to use for output.
  `graphviz-dot-view-command'                  (default `dotty %s')
       Command to run when `graphviz-dot-view' is executed.
  `graphviz-dot-view-edit-command'             (default nil)
       If the user should be asked to edit the view command.
  `graphviz-dot-save-before-view'              (default t)
       Automatically save current buffer berore `graphviz-dot-view'.

(fn)" t nil) (autoload 'graphviz-dot-preview "graphviz-dot-mode" "Compile the graph and preview it in an other buffer." t nil) (autoload 'graphviz-turn-on-live-preview "graphviz-dot-mode" "Turn on live preview.
This will update the preview on every save." t nil) (autoload 'graphviz-turn-off-live-preview "graphviz-dot-mode" "Turn off live preview.
Saving the file will no longer also update the preview." t nil) (add-to-list 'auto-mode-alist '("\\.dot\\'" . graphviz-dot-mode)) (add-to-list 'auto-mode-alist '("\\.gv\\'" . graphviz-dot-mode)) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "graphviz-dot-mode" '("dot-menu" "graphviz-"))) (provide 'graphviz-dot-mode-autoloads)) "emacsql" ((emacsql emacsql-compiler emacsql-autoloads) (autoload 'emacsql-show-last-sql "emacsql" "Display the compiled SQL of the s-expression SQL expression before point.
A prefix argument causes the SQL to be printed into the current buffer.

(fn &optional PREFIX)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emacsql" '("emacsql-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emacsql-compiler" '("emacsql-"))) (provide 'emacsql-autoloads)) "emacsql-sqlite" ((emacsql-sqlite-autoloads emacsql-sqlite) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emacsql-sqlite" '("emacsql-sqlite-"))) (provide 'emacsql-sqlite-autoloads)) "org-roam" ((org-roam-node org-roam-graph org-roam-overlay org-roam-utils org-roam org-roam-compat org-roam-protocol org-roam-capture org-roam-dailies org-roam-mode org-roam-autoloads org-roam-db org-roam-migrate) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam" '("org-roam-"))) (autoload 'org-roam-capture- "org-roam-capture" "Main entry point of `org-roam-capture' module.
GOTO and KEYS correspond to `org-capture' arguments.
INFO is a plist for filling up Org-roam's capture templates.
NODE is an `org-roam-node' construct containing information about the node.
PROPS is a plist containing additional Org-roam properties for each template.
TEMPLATES is a list of org-roam templates.

(fn &key GOTO KEYS NODE INFO PROPS TEMPLATES)" nil nil) (autoload 'org-roam-capture "org-roam-capture" "Launches an `org-capture' process for a new or existing node.
This uses the templates defined at `org-roam-capture-templates'.
Arguments GOTO and KEYS see `org-capture'.
FILTER-FN is a function to filter out nodes: it takes an `org-roam-node',
and when nil is returned the node will be filtered out.
The TEMPLATES, if provided, override the list of capture templates (see
`org-roam-capture-'.)
The INFO, if provided, is passed along to the underlying `org-roam-capture-'.

(fn &optional GOTO KEYS &key FILTER-FN TEMPLATES INFO)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-capture" '("org-roam-capture-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-compat" '("org-roam--"))) (autoload 'org-roam-dailies-capture-today "org-roam-dailies" "Create an entry in the daily-note for today.
When GOTO is non-nil, go the note without creating an entry.

(fn &optional GOTO)" t nil) (autoload 'org-roam-dailies-goto-today "org-roam-dailies" "Find the daily-note for today, creating it if necessary." t nil) (autoload 'org-roam-dailies-capture-tomorrow "org-roam-dailies" "Create an entry in the daily-note for tomorrow.

With numeric argument N, use the daily-note N days in the future.

With a `C-u' prefix or when GOTO is non-nil, go the note without
creating an entry.

(fn N &optional GOTO)" t nil) (autoload 'org-roam-dailies-goto-tomorrow "org-roam-dailies" "Find the daily-note for tomorrow, creating it if necessary.

With numeric argument N, use the daily-note N days in the
future.

(fn N)" t nil) (autoload 'org-roam-dailies-capture-yesterday "org-roam-dailies" "Create an entry in the daily-note for yesteday.

With numeric argument N, use the daily-note N days in the past.

When GOTO is non-nil, go the note without creating an entry.

(fn N &optional GOTO)" t nil) (autoload 'org-roam-dailies-goto-yesterday "org-roam-dailies" "Find the daily-note for yesterday, creating it if necessary.

With numeric argument N, use the daily-note N days in the
future.

(fn N)" t nil) (autoload 'org-roam-dailies-capture-date "org-roam-dailies" "Create an entry in the daily-note for a date using the calendar.
Prefer past dates, unless PREFER-FUTURE is non-nil.
With a `C-u' prefix or when GOTO is non-nil, go the note without
creating an entry.

(fn &optional GOTO PREFER-FUTURE)" t nil) (autoload 'org-roam-dailies-goto-date "org-roam-dailies" "Find the daily-note for a date using the calendar, creating it if necessary.
Prefer past dates, unless PREFER-FUTURE is non-nil.

(fn &optional PREFER-FUTURE)" t nil) (autoload 'org-roam-dailies-find-directory "org-roam-dailies" "Find and open `org-roam-dailies-directory'." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-dailies" '("org-roam-dailies-"))) (autoload 'org-roam-db-sync "org-roam-db" "Synchronize the cache state with the current Org files on-disk.
If FORCE, force a rebuild of the cache from scratch.

(fn &optional FORCE)" t nil) (defvar org-roam-db-autosync-mode nil "Non-nil if Org-Roam-Db-Autosync mode is enabled.
See the `org-roam-db-autosync-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `org-roam-db-autosync-mode'.") (custom-autoload 'org-roam-db-autosync-mode "org-roam-db" nil) (autoload 'org-roam-db-autosync-mode "org-roam-db" "Global minor mode to keep your Org-roam session automatically synchronized.
Through the session this will continue to setup your
buffers (that are Org-roam file visiting), keep track of the
related changes, maintain cache consistency and incrementally
update the currently active database.

If called interactively, enable Org-Roam-Db-Autosync mode if ARG
is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

If you need to manually trigger resync of the currently active
database, see `org-roam-db-sync' command.

(fn &optional ARG)" t nil) (autoload 'org-roam-db-autosync-enable "org-roam-db" "Activate `org-roam-db-autosync-mode'." nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-db" '("emacsql-constraint" "org-roam-"))) (autoload 'org-roam-graph "org-roam-graph" "Build and possibly display a graph for NODE.
ARG may be any of the following values:
  - nil       show the graph.
  - `\\[universal-argument]'     show the graph for NODE.
  - `\\[universal-argument]' N   show the graph for NODE limiting nodes to N steps.

(fn &optional ARG NODE)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-graph" '("org-roam-graph-"))) (autoload 'org-roam-migrate-wizard "org-roam-migrate" "Migrate all notes from to be compatible with Org-roam v2.
1. Convert all notes from v1 format to v2.
2. Rebuild the cache.
3. Replace all file links with ID links." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-migrate" '("org-roam-"))) (autoload 'org-roam-buffer-display-dedicated "org-roam-mode" "Launch NODE dedicated Org-roam buffer.
Unlike the persistent `org-roam-buffer', the contents of this
buffer won't be automatically changed and will be held in place.

In interactive calls prompt to select NODE, unless called with
`universal-argument', in which case NODE will be set to
`org-roam-node-at-point'.

(fn NODE)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-mode" '("org-roam-"))) (autoload 'org-roam-node-find "org-roam-node" "Find and open an Org-roam node by its title or alias.
INITIAL-INPUT is the initial input for the prompt.
FILTER-FN is a function to filter out nodes: it takes an `org-roam-node',
and when nil is returned the node will be filtered out.
If OTHER-WINDOW, visit the NODE in another window.
The TEMPLATES, if provided, override the list of capture templates (see
`org-roam-capture-'.)

(fn &optional OTHER-WINDOW INITIAL-INPUT FILTER-FN &key TEMPLATES)" t nil) (autoload 'org-roam-node-random "org-roam-node" "Find and open a random Org-roam node.
With prefix argument OTHER-WINDOW, visit the node in another
window instead.

(fn &optional OTHER-WINDOW)" t nil) (autoload 'org-roam-node-insert "org-roam-node" "Find an Org-roam node and insert (where the point is) an \"id:\" link to it.
FILTER-FN is a function to filter out nodes: it takes an `org-roam-node',
and when nil is returned the node will be filtered out.
The TEMPLATES, if provided, override the list of capture templates (see
`org-roam-capture-'.)
The INFO, if provided, is passed to the underlying `org-roam-capture-'.

(fn &optional FILTER-FN &key TEMPLATES INFO)" t nil) (autoload 'org-roam-refile "org-roam-node" "Refile node at point to an Org-roam node.
If region is active, then use it instead of the node at point." t nil) (autoload 'org-roam-extract-subtree "org-roam-node" "Convert current subtree at point to a node, and extract it into a new file." t nil) (autoload 'org-roam-update-org-id-locations "org-roam-node" "Scan Org-roam files to update `org-id' related state.
This is like `org-id-update-id-locations', but will automatically
use the currently bound `org-directory' and `org-roam-directory'
along with DIRECTORIES (if any), where the lookup for files in
these directories will be always recursive.

Note: Org-roam doesn't have hard dependency on
`org-id-locations-file' to lookup IDs for nodes that are stored
in the database, but it still tries to properly integrates with
`org-id'. This allows the user to cross-reference IDs outside of
the current `org-roam-directory', and also link with \"id:\"
links to headings/files within the current `org-roam-directory'
that are excluded from identification in Org-roam as
`org-roam-node's, e.g. with \"ROAM_EXCLUDE\" property.

(fn &rest DIRECTORIES)" t nil) (autoload 'org-roam-ref-find "org-roam-node" "Find and open an Org-roam node that's dedicated to a specific ref.
INITIAL-INPUT is the initial input to the prompt.
FILTER-FN is a function to filter out nodes: it takes an `org-roam-node',
and when nil is returned the node will be filtered out.

(fn &optional INITIAL-INPUT FILTER-FN)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-node" '("org-roam-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-overlay" '("org-roam-overlay-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-protocol" '("org-roam-"))) (autoload 'org-roam-version "org-roam-utils" "Return `org-roam' version.
Interactively, or when MESSAGE is non-nil, show in the echo area.

(fn &optional MESSAGE)" t nil) (autoload 'org-roam-diagnostics "org-roam-utils" "Collect and print info for `org-roam' issues." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-utils" '("org-roam-"))) (provide 'org-roam-autoloads)) "org-noter" ((org-noter org-noter-autoloads) (autoload 'org-noter "org-noter" "Start `org-noter' session.

There are two modes of operation. You may create the session from:
- The Org notes file
- The document to be annotated (PDF, EPUB, ...)

- Creating the session from notes file -----------------------------------------
This will open a session for taking your notes, with indirect
buffers to the document and the notes side by side. Your current
window configuration won't be changed, because this opens in a
new frame.

You only need to run this command inside a heading (which will
hold the notes for this document). If no document path property is found,
this command will ask you for the target file.

With a prefix universal argument ARG, only check for the property
in the current heading, don't inherit from parents.

With 2 prefix universal arguments ARG, ask for a new document,
even if the current heading annotates one.

With a prefix number ARG:
- Greater than 0: Open the document like `find-file'
-     Equal to 0: Create session with `org-noter-always-create-frame' toggled
-    Less than 0: Open the folder containing the document

- Creating the session from the document ---------------------------------------
This will try to find a notes file in any of the parent folders.
The names it will search for are defined in `org-noter-default-notes-file-names'.
It will also try to find a notes file with the same name as the
document, giving it the maximum priority.

When it doesn't find anything, it will interactively ask you what
you want it to do. The target notes file must be in a parent
folder (direct or otherwise) of the document.

You may pass a prefix ARG in order to make it let you choose the
notes file, even if it finds one.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-noter" '("org-noter-"))) (provide 'org-noter-autoloads)) "ox-epub" ((ox-epub ox-epub-autoloads) (autoload 'org-epub-export-to-epub "ox-epub" "Export the current buffer to an EPUB file.

ASYNC defines wether this process should run in the background,
SUBTREEP supports narrowing of the document, VISIBLE-ONLY allows
you to export only visible parts of the document, EXT-PLIST is
the property list for the export process.

(fn &optional ASYNC SUBTREEP VISIBLE-ONLY EXT-PLIST)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-epub" '("org-epub-"))) (provide 'ox-epub-autoloads)) "org-download" ((org-download-autoloads org-download) (autoload 'org-download-enable "org-download" "Enable org-download." nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-download" '("org-download-"))) (provide 'org-download-autoloads)) "md-roam" ((md-roam md-roam-autoloads) (defvar md-roam-mode nil "Non-nil if Md-Roam mode is enabled.
See the `md-roam-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `md-roam-mode'.") (custom-autoload 'md-roam-mode "md-roam" nil) (autoload 'md-roam-mode "md-roam" "Md-roam mode needs to be turned before `org-roam-db-sync'.
It needs to be turned on before `org-roam-db-autosync-mode'.

If called interactively, enable Md-Roam mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "md-roam" '("md-"))) (provide 'md-roam-autoloads)) "pretty-hydra" ((pretty-hydra pretty-hydra-autoloads) (autoload 'pretty-hydra-define "pretty-hydra" "Define a pretty hydra with given NAME, BODY options and HEADS-PLIST.
The generated hydra has a nice-looking docstring which is a table
with columns of command keys and hints.

NAME should be a symbol and is passed to `defhydra' as is.

BODY is the same as that in `defhydra', withe the following
pretty hydra specific ones:

  - `:separator' a single char used to generate the separator
    line.

  - `:title' a string that's added to the beginning of the
    docstring as a title of the hydra.

  - `:formatter' a function that takes the generated docstring
    and return a decorated one.  It can be used to further
    customize the hydra docstring.

  - `:quit-key' a key of list of keys for quitting the hydra.
    When specified, invisible head(s) are created with the
    specified keys for quitting the hydra.

HEADS-PLIST is a plist of columns of hydra heads.  The keys of
the plist should be column names.  The values should be lists of
hydra heads.  Each head has exactly the same syntax as that of
`defhydra', except hint is required for the head to appear in the
docstring.  The following additional options are supported:

  - `:width' the max width of a dynamic hint, used to calculate
    the final width of the entire column.  It is ignored when the
    hint is a string.

  - `:toggle' when specified, it makes the head a toggle and adds
    an indicator to the end of the hint for the status of the
    toggle.  The value of this option can be a symbol, an s-exp
    or t.  The toggle status is read from the given variable, by
    evaluating the given expression or checking the `cmd' as if
    it's a variable.  The latter is especially useful for minior
    modes, e.g.

       (\"n\" `linum-mode' \"line number\" :toggle t)

(fn NAME BODY HEADS-PLIST)" nil t) (function-put 'pretty-hydra-define 'lisp-indent-function 'defun) (autoload 'pretty-hydra-define+ "pretty-hydra" "Redefine an existing pretty-hydra by adding new HEADS-PLIST.
If heads are added to a column already in NAME, the heads are
appended to that column.  Existing BODY is replaced with the new
one if specified.  Arguments are the same as `pretty-hydra-define'.

(fn NAME BODY HEADS-PLIST)" nil t) (function-put 'pretty-hydra-define+ 'lisp-indent-function 'defun) (autoload 'pretty-hydra-toggle "pretty-hydra" "Create a dynamic hint that look like a radio button with given NAME.
Radio is considered on when STATUS is non-nil, otherwise off.

(fn NAME STATUS)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pretty-hydra" '("pretty-hydra-"))) (provide 'pretty-hydra-autoloads)) "major-mode-hydra" ((major-mode-hydra major-mode-hydra-autoloads) (autoload 'major-mode-hydra-define "major-mode-hydra" "Generate a major mode hydra for given MODE with given BODY and HEADS-PLIST.
Overwrite existing hydra if there is one.

MODE can also be a list of modes in which case the same hydras
are created for all these modes.  Useful in multiple closely
related major modes.

Refer to `pretty-hydra-define' for documentation about BODY and HEADS-PLIST.

(fn MODE BODY HEADS-PLIST)" nil t) (function-put 'major-mode-hydra-define 'lisp-indent-function 'defun) (autoload 'major-mode-hydra-define+ "major-mode-hydra" "Generate a major mode hydra for given MODE with given BODY and HEADS-PLIST.
Add new heads if there is already an existing one.

MODE can also be a list of modes in which case the same hydras
are created for all these modes.  Useful in multiple closely
related major modes.

Refer to `pretty-hydra-define' for documentation about BODY and HEADS-PLIST.

(fn MODE BODY HEADS-PLIST)" nil t) (function-put 'major-mode-hydra-define+ 'lisp-indent-function 'defun) (autoload 'major-mode-hydra-bind "major-mode-hydra" "Add BINDINGS (heads) for a MODE under the COLUMN.

MODE is the major mode name (symbol).  There is no need to quote it.

COLUMN is a string to put the hydra heads under.

BINDINGS is a list of hydra heads to be added.  Each head has
exactly the same structure as that in `pretty-hydra-define' or
`defhydra', except `:exit' is set to t by default.

(fn MODE COLUMN &rest BINDINGS)" nil t) (function-put 'major-mode-hydra-bind 'lisp-indent-function '2) (make-obsolete 'major-mode-hydra-bind 'major-mode-hydra-define+ '"July 2019") (autoload 'major-mode-hydra "major-mode-hydra" "Show the hydra for the current major mode." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "major-mode-hydra" '("major-mode-hydra-"))) (provide 'major-mode-hydra-autoloads))))

#s(hash-table size 65 test eq rehash-size 1.5 rehash-threshold 0.8125 data (org-elpa #s(hash-table size 145 test equal rehash-size 1.5 rehash-threshold 0.8125 data (version 11 "melpa" nil "gnu-elpa-mirror" nil "el-get" nil "emacsmirror-mirror" nil "straight" nil "use-package" nil "bind-key" nil "s" nil "hydra" nil "cl-lib" nil "lv" nil "swiper" nil "ivy" nil "counsel" nil "evil-escape" nil "evil" nil "goto-chg" nil "evil-leader" nil "projectile" nil "battery" nil "all-the-icons" nil "doom-themes" nil "cycle-themes" nil "smooth-scrolling" nil "calendar" nil "magit" nil "dash" nil "git-commit" nil "transient" nil "with-editor" nil "magit-section" nil "git-timemachine" nil "org-bullets" nil "org" (org :type git :repo "https://git.savannah.gnu.org/git/emacs/org-mode.git" :local-repo "org" :depth full :pre-build (straight-recipes-org-elpa--build) :build (:not autoloads) :files (:defaults "lisp/*.el" ("etc/styles/" "etc/styles/*"))) "ispell" nil "flyspell" nil "smart-quotes" nil "guess-language" nil "writeroom-mode" nil "visual-fill-column" nil "flycheck" nil "pkg-info" nil "epl" nil "let-alist" nil "seq" nil "company" nil "company-box" nil "dash-functional" nil "frame-local" nil "web-mode" nil "yasnippet" nil "markdown-mode" nil "bibtex-completion" nil "parsebib" nil "f" nil "biblio" nil "biblio-core" nil "pdf-tools" nil "tablist" nil "ivy-bibtex" nil "undo-tree" nil "which-key" nil "focus" nil "emacs-async" nil "org-ref" nil "htmlize" nil "helm" nil "async" nil "popup" nil "helm-core" nil "helm-bibtex" nil "key-chord" nil "esxml" nil "kv" nil "deft" nil "neotree" nil "emmet-mode" nil "lsp-mode" nil "ht" nil "spinner" nil "lsp-ui" nil "lsp-ivy" nil "lsp-treemacs" nil "treemacs" nil "ace-window" nil "avy" nil "pfuture" nil "cfrs" nil "posframe" nil "dap-mode" nil "bui" nil "company-lsp" nil "exec-path-from-shell" nil "yaml-mode" nil "smartparens" nil "lsp-jedi" nil "py-autopep8" nil "company-jedi" nil "jedi-core" nil "epc" nil "concurrent" nil "deferred" nil "ctable" nil "python-environment" nil "graphviz-dot-mode" nil "company-graphviz-dot" nil "org-roam" nil "emacsql" nil "emacsql-sqlite" nil "org-noter" nil "ox-epub" nil "org-download" nil "md-roam" nil "major-mode-hydra" nil "pretty-hydra" nil)) melpa #s(hash-table size 145 test equal rehash-size 1.5 rehash-threshold 0.8125 data (version 2 "gnu-elpa-mirror" nil "el-get" (el-get :type git :flavor melpa :files ("*.el" ("recipes" "recipes/el-get.rcp") "methods" "el-get-pkg.el") :host github :repo "dimitri/el-get") "emacsmirror-mirror" nil "straight" nil "use-package" (use-package :type git :flavor melpa :files (:defaults (:exclude "bind-key.el" "bind-chord.el" "use-package-chords.el" "use-package-ensure-system-package.el") "use-package-pkg.el") :host github :repo "jwiegley/use-package") "bind-key" (bind-key :type git :flavor melpa :files ("bind-key.el" "bind-key-pkg.el") :host github :repo "jwiegley/use-package") "s" (s :type git :flavor melpa :files ("s.el" "s-pkg.el") :host github :repo "magnars/s.el") "hydra" (hydra :type git :flavor melpa :files (:defaults (:exclude "lv.el") "hydra-pkg.el") :host github :repo "abo-abo/hydra") "cl-lib" nil "lv" (lv :type git :flavor melpa :files ("lv.el" "lv-pkg.el") :host github :repo "abo-abo/hydra") "swiper" (swiper :type git :flavor melpa :files ("swiper.el" "swiper-pkg.el") :host github :repo "abo-abo/swiper") "ivy" (ivy :type git :flavor melpa :files (:defaults (:exclude "swiper.el" "counsel.el" "ivy-hydra.el" "ivy-avy.el") "doc/ivy-help.org" "ivy-pkg.el") :host github :repo "abo-abo/swiper") "counsel" (counsel :type git :flavor melpa :files ("counsel.el" "counsel-pkg.el") :host github :repo "abo-abo/swiper") "evil-escape" (evil-escape :type git :flavor melpa :host github :repo "syl20bnr/evil-escape") "evil" (evil :type git :flavor melpa :files (:defaults "doc/build/texinfo/evil.texi" (:exclude "evil-test-helpers.el") "evil-pkg.el") :host github :repo "emacs-evil/evil") "goto-chg" (goto-chg :type git :flavor melpa :host github :repo "emacs-evil/goto-chg") "evil-leader" (evil-leader :type git :flavor melpa :host github :repo "cofi/evil-leader") "projectile" (projectile :type git :flavor melpa :files ("projectile.el" "projectile-pkg.el") :host github :repo "bbatsov/projectile") "battery" nil "all-the-icons" (all-the-icons :type git :flavor melpa :files (:defaults "data" "all-the-icons-pkg.el") :host github :repo "domtronn/all-the-icons.el") "doom-themes" (doom-themes :type git :flavor melpa :files (:defaults "themes/*.el" "doom-themes-pkg.el") :host github :repo "hlissner/emacs-doom-themes") "cycle-themes" (cycle-themes :type git :flavor melpa :host github :repo "toroidal-code/cycle-themes.el") "smooth-scrolling" (smooth-scrolling :type git :flavor melpa :host github :repo "aspiers/smooth-scrolling") "calendar" nil "magit" (magit :type git :flavor melpa :files ("lisp/magit" "lisp/magit*.el" "lisp/git-rebase.el" "Documentation/magit.texi" "Documentation/AUTHORS.md" "LICENSE" (:exclude "lisp/magit-libgit.el" "lisp/magit-section.el") "magit-pkg.el") :host github :repo "magit/magit") "dash" (dash :type git :flavor melpa :files ("dash.el" "dash.texi" "dash-pkg.el") :host github :repo "magnars/dash.el") "git-commit" (git-commit :type git :flavor melpa :files ("lisp/git-commit.el" "LICENSE" "git-commit-pkg.el") :host github :repo "magit/magit") "transient" (transient :type git :flavor melpa :files ("lisp/*.el" "docs/transient.texi" "transient-pkg.el") :host github :repo "magit/transient") "with-editor" (with-editor :type git :flavor melpa :host github :repo "magit/with-editor") "magit-section" (magit-section :type git :flavor melpa :files ("lisp/magit-section.el" "Documentation/magit-section.texi" "magit-section-pkg.el") :host github :repo "magit/magit") "git-timemachine" (git-timemachine :type git :flavor melpa :host gitlab :repo "pidu/git-timemachine") "org-bullets" (org-bullets :type git :flavor melpa :host github :repo "integral-dw/org-bullets") "ispell" nil "flyspell" nil "smart-quotes" nil "guess-language" (guess-language :type git :flavor melpa :files ("guess-language.el" "trigrams/*" "guess-language-pkg.el") :host github :repo "tmalsburg/guess-language.el") "writeroom-mode" (writeroom-mode :type git :flavor melpa :host github :repo "joostkremers/writeroom-mode") "visual-fill-column" (visual-fill-column :type git :flavor melpa :host github :repo "joostkremers/visual-fill-column") "flycheck" (flycheck :type git :flavor melpa :host github :repo "flycheck/flycheck") "pkg-info" (pkg-info :type git :flavor melpa :host github :repo "emacsorphanage/pkg-info") "epl" (epl :type git :flavor melpa :host github :repo "cask/epl") "let-alist" nil "seq" nil "company" (company :type git :flavor melpa :files (:defaults "icons" "company-pkg.el") :host github :repo "company-mode/company-mode") "company-box" (company-box :type git :flavor melpa :files (:defaults "images" "company-box-pkg.el") :host github :repo "sebastiencs/company-box") "dash-functional" (dash-functional :type git :flavor melpa :files ("dash-functional.el" "dash-functional-pkg.el") :host github :repo "magnars/dash.el") "frame-local" (frame-local :type git :flavor melpa :host github :repo "sebastiencs/frame-local") "web-mode" (web-mode :type git :flavor melpa :host github :repo "fxbois/web-mode") "yasnippet" (yasnippet :type git :flavor melpa :files ("yasnippet.el" "snippets" "yasnippet-pkg.el") :host github :repo "joaotavora/yasnippet") "markdown-mode" (markdown-mode :type git :flavor melpa :host github :repo "jrblevin/markdown-mode") "bibtex-completion" (bibtex-completion :type git :flavor melpa :files ("bibtex-completion.el" "bibtex-completion-pkg.el") :host github :repo "tmalsburg/helm-bibtex") "parsebib" (parsebib :type git :flavor melpa :host github :repo "joostkremers/parsebib") "f" (f :type git :flavor melpa :files ("f.el" "f-pkg.el") :host github :repo "rejeep/f.el") "biblio" (biblio :type git :flavor melpa :files (:defaults (:exclude "biblio-core.el") "biblio-pkg.el") :host github :repo "cpitclaudel/biblio.el") "biblio-core" (biblio-core :type git :flavor melpa :files ("biblio-core.el" "biblio-core-pkg.el") :host github :repo "cpitclaudel/biblio.el") "pdf-tools" (pdf-tools :type git :flavor melpa :files ("lisp/*.el" "README" ("build" "Makefile") ("build" "server") (:exclude "lisp/tablist.el" "lisp/tablist-filter.el") "pdf-tools-pkg.el") :host github :repo "vedang/pdf-tools") "tablist" (tablist :type git :flavor melpa :host github :repo "politza/tablist") "ivy-bibtex" (ivy-bibtex :type git :flavor melpa :files ("ivy-bibtex.el" "ivy-bibtex-pkg.el") :host github :repo "tmalsburg/helm-bibtex") "undo-tree" nil "which-key" (which-key :type git :flavor melpa :host github :repo "justbur/emacs-which-key") "focus" (focus :type git :flavor melpa :host github :repo "larstvei/Focus") "emacs-async" nil "org-ref" (org-ref :type git :flavor melpa :files (:defaults "org-ref.org" "org-ref.bib" "citeproc" "org-ref-pkg.el") :host github :repo "jkitchin/org-ref") "htmlize" (htmlize :type git :flavor melpa :host github :repo "hniksic/emacs-htmlize") "helm" (helm :type git :flavor melpa :files ("*.el" "emacs-helm.sh" (:exclude "helm.el" "helm-lib.el" "helm-source.el" "helm-multi-match.el" "helm-core-pkg.el") "helm-pkg.el") :host github :repo "emacs-helm/helm") "async" (async :type git :flavor melpa :host github :repo "jwiegley/emacs-async") "popup" (popup :type git :flavor melpa :files ("popup.el" "popup-pkg.el") :host github :repo "auto-complete/popup-el") "helm-core" (helm-core :type git :flavor melpa :files ("helm-core-pkg.el" "helm.el" "helm-lib.el" "helm-source.el" "helm-multi-match.el" "helm-core-pkg.el") :host github :repo "emacs-helm/helm") "helm-bibtex" (helm-bibtex :type git :flavor melpa :files ("helm-bibtex.el" "helm-bibtex-pkg.el") :host github :repo "tmalsburg/helm-bibtex") "key-chord" (key-chord :type git :flavor melpa :host github :repo "emacsorphanage/key-chord") "esxml" (esxml :type git :flavor melpa :files ("esxml.el" "esxml-query.el" "esxml-pkg.el") :host github :repo "tali713/esxml") "kv" (kv :type git :flavor melpa :files ("kv.el" "kv-pkg.el") :host github :repo "nicferrier/emacs-kv") "deft" (deft :type git :flavor melpa :host github :repo "jrblevin/deft") "neotree" (neotree :type git :flavor melpa :files (:defaults "icons" "neotree-pkg.el") :host github :repo "jaypei/emacs-neotree") "emmet-mode" (emmet-mode :type git :flavor melpa :host github :repo "smihica/emmet-mode") "lsp-mode" (lsp-mode :type git :flavor melpa :files (:defaults "clients/*.el" "lsp-mode-pkg.el") :host github :repo "emacs-lsp/lsp-mode") "ht" (ht :type git :flavor melpa :files ("ht.el" "ht-pkg.el") :host github :repo "Wilfred/ht.el") "spinner" nil "lsp-ui" (lsp-ui :type git :flavor melpa :files (:defaults "lsp-ui-doc.html" "resources" "lsp-ui-pkg.el") :host github :repo "emacs-lsp/lsp-ui") "lsp-ivy" (lsp-ivy :type git :flavor melpa :host github :repo "emacs-lsp/lsp-ivy") "lsp-treemacs" (lsp-treemacs :type git :flavor melpa :files (:defaults "icons" "lsp-treemacs-pkg.el") :host github :repo "emacs-lsp/lsp-treemacs") "treemacs" (treemacs :type git :flavor melpa :files (:defaults "Changelog.org" "icons" "src/elisp/treemacs*.el" "src/scripts/treemacs*.py" (:exclude "src/extra/*") "treemacs-pkg.el") :host github :repo "Alexander-Miller/treemacs") "ace-window" (ace-window :type git :flavor melpa :host github :repo "abo-abo/ace-window") "avy" (avy :type git :flavor melpa :host github :repo "abo-abo/avy") "pfuture" (pfuture :type git :flavor melpa :host github :repo "Alexander-Miller/pfuture") "cfrs" (cfrs :type git :flavor melpa :host github :repo "Alexander-Miller/cfrs") "posframe" (posframe :type git :flavor melpa :host github :repo "tumashu/posframe") "dap-mode" (dap-mode :type git :flavor melpa :files (:defaults "icons" "dap-mode-pkg.el") :host github :repo "emacs-lsp/dap-mode") "bui" (bui :type git :flavor melpa :host github :repo "alezost/bui.el") "company-lsp" nil "exec-path-from-shell" (exec-path-from-shell :type git :flavor melpa :host github :repo "purcell/exec-path-from-shell") "yaml-mode" (yaml-mode :type git :flavor melpa :host github :repo "yoshiki/yaml-mode") "smartparens" (smartparens :type git :flavor melpa :host github :repo "Fuco1/smartparens") "lsp-jedi" (lsp-jedi :type git :flavor melpa :host github :repo "fredcamps/lsp-jedi") "py-autopep8" (py-autopep8 :type git :flavor melpa :host github :repo "paetzke/py-autopep8.el") "company-jedi" (company-jedi :type git :flavor melpa :host github :repo "emacsorphanage/company-jedi") "jedi-core" (jedi-core :type git :flavor melpa :files ("jedi-core.el" "jediepcserver.py" "Makefile" "setup.py" "jedi-core-pkg.el") :host github :repo "tkf/emacs-jedi") "epc" (epc :type git :flavor melpa :files ("epc.el" "epcs.el" "epc-pkg.el") :host github :repo "kiwanami/emacs-epc") "concurrent" (concurrent :type git :flavor melpa :files ("concurrent.el" "concurrent-pkg.el") :host github :repo "kiwanami/emacs-deferred") "deferred" (deferred :type git :flavor melpa :files ("deferred.el" "deferred-pkg.el") :host github :repo "kiwanami/emacs-deferred") "ctable" (ctable :type git :flavor melpa :files ("ctable.el" "ctable-pkg.el") :host github :repo "kiwanami/emacs-ctable") "python-environment" (python-environment :type git :flavor melpa :host github :repo "tkf/emacs-python-environment") "graphviz-dot-mode" (graphviz-dot-mode :type git :flavor melpa :host github :repo "ppareit/graphviz-dot-mode") "company-graphviz-dot" nil "org-roam" (org-roam :type git :flavor melpa :files (:defaults "extensions/*" "org-roam-pkg.el") :host github :repo "org-roam/org-roam") "emacsql" (emacsql :type git :flavor melpa :files ("emacsql.el" "emacsql-compiler.el" "emacsql-system.el" "README.md" "emacsql-pkg.el") :host github :repo "skeeto/emacsql") "emacsql-sqlite" (emacsql-sqlite :type git :flavor melpa :files ("emacsql-sqlite.el" "sqlite" "emacsql-sqlite-pkg.el") :host github :repo "skeeto/emacsql") "org-noter" (org-noter :type git :flavor melpa :host github :repo "weirdNox/org-noter") "ox-epub" (ox-epub :type git :flavor melpa :host github :repo "ofosos/ox-epub") "org-download" (org-download :type git :flavor melpa :host github :repo "abo-abo/org-download") "md-roam" nil "major-mode-hydra" (major-mode-hydra :type git :flavor melpa :files ("major-mode-hydra.el" "major-mode-hydra-pkg.el") :host github :repo "jerrypnz/major-mode-hydra.el") "pretty-hydra" (pretty-hydra :type git :flavor melpa :files ("pretty-hydra.el" "pretty-hydra-pkg.el") :host github :repo "jerrypnz/major-mode-hydra.el"))) gnu-elpa-mirror #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (version 3 "emacsmirror-mirror" nil "straight" nil "cl-lib" nil "battery" nil "calendar" nil "ispell" nil "flyspell" nil "smart-quotes" nil "let-alist" (let-alist :type git :host github :repo "emacs-straight/let-alist" :files ("*" (:exclude ".git"))) "seq" nil "undo-tree" (undo-tree :type git :host github :repo "emacs-straight/undo-tree" :files ("*" (:exclude ".git"))) "emacs-async" nil "spinner" (spinner :type git :host github :repo "emacs-straight/spinner" :files ("*" (:exclude ".git"))) "company-lsp" nil "company-graphviz-dot" nil "md-roam" nil)) el-get #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (version 1 "emacsmirror-mirror" nil "straight" nil "cl-lib" nil "battery" nil "calendar" nil "ispell" nil "flyspell" nil "smart-quotes" `(smart-quotes :type git :host github :repo "gareth-rees/smart-quotes" :files (:defaults)) "seq" nil "emacs-async" `(emacs-async :type git :host github :repo "jwiegley/emacs-async" :files (:defaults)) "company-lsp" nil "company-graphviz-dot" nil "md-roam" nil)) emacsmirror-mirror #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (version 2 "straight" (straight :type git :host github :repo "emacsmirror/straight") "cl-lib" nil "battery" nil "calendar" nil "ispell" nil "flyspell" nil "seq" nil "company-lsp" (company-lsp :type git :host github :repo "emacsattic/company-lsp") "company-graphviz-dot" nil "md-roam" nil))))

("org-elpa" "melpa" "gnu-elpa-mirror" "el-get" "emacsmirror-mirror" "straight" "emacs" "use-package" "bind-key" "s" "undo-tree" "hydra" "cl-lib" "lv" "major-mode-hydra" "dash" "pretty-hydra" "which-key" "swiper" "ivy" "counsel" "evil-escape" "evil" "goto-chg" "evil-leader" "projectile" "exec-path-from-shell" "battery" "all-the-icons" "doom-themes" "cycle-themes" "smooth-scrolling" "calendar" "magit" "git-commit" "transient" "with-editor" "magit-section" "git-timemachine" "org-ref" "htmlize" "helm" "async" "popup" "helm-core" "helm-bibtex" "bibtex-completion" "parsebib" "f" "biblio" "biblio-core" "let-alist" "seq" "key-chord" "pdf-tools" "tablist" "org-roam" "org" "emacsql" "emacsql-sqlite" "md-roam" "markdown-mode" "org-bullets" "org-noter" "ox-epub" "org-download" "visual-fill-column" "ispell" "flyspell" "smart-quotes" "guess-language" "writeroom-mode" "esxml" "kv" "deft" "graphviz-dot-mode" "yaml-mode" "web-mode" "emmet-mode" "neotree" "smartparens" "flycheck" "pkg-info" "epl" "lsp-mode" "ht" "spinner" "lsp-ui" "lsp-ivy" "lsp-treemacs" "treemacs" "ace-window" "avy" "pfuture" "cfrs" "posframe" "dap-mode" "bui" "company-lsp" "company" "company-box" "dash-functional" "frame-local" "lsp-jedi" "py-autopep8" "yasnippet" "ivy-bibtex")

t
