
:tanat

"27.1"

#s(hash-table size 97 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("org-elpa" ("2021-02-04 12:30:53" nil (:local-repo nil :package "org-elpa" :type git)) "melpa" ("2021-02-04 12:30:53" nil (:type git :host github :repo "melpa/melpa" :no-build t :package "melpa" :local-repo "melpa")) "gnu-elpa-mirror" ("2021-02-04 12:30:53" nil (:type git :host github :repo "emacs-straight/gnu-elpa-mirror" :no-build t :package "gnu-elpa-mirror" :local-repo "gnu-elpa-mirror")) "emacsmirror-mirror" ("2021-02-04 12:30:53" nil (:type git :host github :repo "emacs-straight/emacsmirror-mirror" :no-build t :package "emacsmirror-mirror" :local-repo "emacsmirror-mirror")) "straight" ("2021-02-04 12:30:53" ("emacs") (:type git :host github :repo "raxod502/straight.el" :files ("straight*.el") :branch "master" :package "straight" :local-repo "straight.el")) "use-package" ("2021-02-04 12:30:53" ("emacs" "bind-key") (:type git :flavor melpa :files (:defaults (:exclude "bind-key.el" "bind-chord.el" "use-package-chords.el" "use-package-ensure-system-package.el") "use-package-pkg.el") :host github :repo "jwiegley/use-package" :package "use-package" :local-repo "use-package")) "bind-key" ("2021-02-04 12:30:53" nil (:flavor melpa :files ("bind-key.el" "bind-key-pkg.el") :package "bind-key" :local-repo "use-package" :type git :repo "jwiegley/use-package" :host github)) "hydra" ("2021-02-04 12:30:53" ("cl-lib" "lv") (:type git :host github :repo "abo-abo/hydra" :files (:defaults (:exclude "lv.el") "hydra-pkg.el") :package "hydra" :local-repo "hydra")) "lv" ("2021-02-04 12:30:53" nil (:flavor melpa :files ("lv.el" "lv-pkg.el") :package "lv" :local-repo "hydra" :type git :repo "abo-abo/hydra" :host github)) "which-key" ("2021-02-04 12:30:54" ("emacs") (:type git :host github :repo "justbur/emacs-which-key" :package "which-key" :local-repo "emacs-which-key")) "ivy" ("2021-02-04 12:30:54" ("emacs") (:type git :flavor melpa :files (:defaults (:exclude "swiper.el" "counsel.el" "ivy-hydra.el" "ivy-avy.el") "doc/ivy-help.org" "ivy-pkg.el") :host github :repo "abo-abo/swiper" :package "ivy" :local-repo "swiper")) "evil-leader" ("2021-02-04 12:30:54" ("evil") (:type git :host github :repo "cofi/evil-leader" :package "evil-leader" :local-repo "evil-leader")) "evil" ("2021-02-04 12:30:54" ("emacs" "goto-chg" "cl-lib") (:type git :flavor melpa :files (:defaults "doc/build/texinfo/evil.texi" (:exclude "evil-test-helpers.el") "evil-pkg.el") :host github :repo "emacs-evil/evil" :package "evil" :local-repo "evil")) "goto-chg" ("2021-02-04 12:30:54" nil (:type git :flavor melpa :host github :repo "emacs-evil/goto-chg" :package "goto-chg" :local-repo "goto-chg")) "evil-escape" ("2021-02-04 12:30:54" ("emacs" "evil" "cl-lib") (:type git :host github :repo "syl20bnr/evil-escape" :package "evil-escape" :local-repo "evil-escape")) "s" ("2021-02-04 12:30:54" nil (:type git :flavor melpa :files ("s.el" "s-pkg.el") :host github :repo "magnars/s.el" :package "s" :local-repo "s.el")) "el-patch" ("2021-02-04 12:31:02" ("emacs") (:type git :host github :repo "raxod502/el-patch" :package "el-patch" :local-repo "el-patch")) "deft" ("2021-02-04 12:31:02" nil (:type git :host github :repo "jrblevin/deft" :package "deft" :local-repo "deft")) "counsel" ("2021-02-04 12:31:02" ("emacs" "swiper") (:flavor melpa :files ("counsel.el" "counsel-pkg.el") :package "counsel" :local-repo "swiper" :type git :repo "abo-abo/swiper" :host github)) "swiper" ("2021-02-04 12:31:02" ("emacs" "ivy") (:flavor melpa :files ("swiper.el" "swiper-pkg.el") :package "swiper" :local-repo "swiper" :type git :repo "abo-abo/swiper" :host github)) "smooth-scrolling" ("2021-02-04 12:31:02" nil (:type git :host github :repo "aspiers/smooth-scrolling" :package "smooth-scrolling" :local-repo "smooth-scrolling")) "alert" ("2021-02-04 12:31:02" ("gntp" "log4e" "cl-lib") (:type git :host github :repo "jwiegley/alert" :package "alert" :local-repo "alert")) "gntp" ("2021-02-04 12:31:02" nil (:type git :flavor melpa :host github :repo "tekai/gntp.el" :package "gntp" :local-repo "gntp.el")) "log4e" ("2021-02-04 12:31:02" nil (:type git :flavor melpa :host github :repo "aki2o/log4e" :package "log4e" :local-repo "log4e")) "google-this" ("2021-02-04 12:31:02" ("emacs") (:type git :host github :repo "Malabarba/emacs-google-this" :files ("google-this.el" "google-this-pkg.el") :package "google-this" :local-repo "emacs-google-this")) "mu4e" ("2021-02-04 12:31:02" nil (:type git :host github :repo "emacsmirror/mu4e" :package "mu4e" :local-repo "mu4e")) "calfw" ("2021-02-01 18:01:18" nil (:type git :host github :repo "kiwanami/emacs-calfw" :files ("calfw.el" "calfw-pkg.el") :package "calfw" :local-repo "emacs-calfw")) "magit" ("2021-02-04 12:31:03" ("emacs" "dash" "git-commit" "transient" "with-editor") (:type git :host github :repo "magit/magit" :files ("lisp/magit" "lisp/magit*.el" "lisp/git-rebase.el" "Documentation/magit.texi" "Documentation/AUTHORS.md" "LICENSE" (:exclude "lisp/magit-libgit.el") "magit-pkg.el") :package "magit" :local-repo "magit")) "dash" ("2021-02-04 12:31:03" ("emacs") (:type git :flavor melpa :files ("dash.el" "dash.texi" "dash-pkg.el") :host github :repo "magnars/dash.el" :package "dash" :local-repo "dash.el")) "git-commit" ("2021-02-04 12:31:03" ("emacs" "dash" "transient" "with-editor") (:flavor melpa :files ("lisp/git-commit.el" "git-commit-pkg.el") :package "git-commit" :local-repo "magit" :type git :repo "magit/magit" :host github)) "transient" ("2021-02-04 12:31:03" ("emacs") (:type git :flavor melpa :files ("lisp/*.el" "docs/transient.texi" "transient-pkg.el") :host github :repo "magit/transient" :package "transient" :local-repo "transient")) "with-editor" ("2021-02-04 12:31:03" ("emacs") (:type git :flavor melpa :host github :repo "magit/with-editor" :package "with-editor" :local-repo "with-editor")) "git-gutter" ("2021-02-04 12:31:03" ("emacs") (:type git :host github :repo "emacsorphanage/git-gutter" :package "git-gutter" :local-repo "git-gutter")) "git-timemachine" ("2021-02-04 12:31:03" ("emacs" "transient") (:type git :host gitlab :repo "pidu/git-timemachine" :package "git-timemachine" :local-repo "git-timemachine")) "org" ("2021-02-04 12:31:03" nil (:type git :repo "https://code.orgmode.org/bzg/org-mode.git" :local-repo "org" :package "org")) "org-roam" ("2021-02-04 12:31:03" ("emacs" "dash" "f" "s" "org" "emacsql" "emacsql-sqlite3") (:type git :host github :repo "org-roam/org-roam" :package "org-roam" :local-repo "org-roam")) "f" ("2021-02-04 12:31:03" ("s" "dash") (:type git :flavor melpa :files ("f.el" "f-pkg.el") :host github :repo "rejeep/f.el" :package "f" :local-repo "f.el")) "emacsql" ("2021-02-04 12:31:03" ("emacs") (:type git :flavor melpa :files ("emacsql.el" "emacsql-compiler.el" "emacsql-system.el" "README.md" "emacsql-pkg.el") :host github :repo "skeeto/emacsql" :package "emacsql" :local-repo "emacsql")) "emacsql-sqlite3" ("2021-02-04 12:31:03" ("emacs" "emacsql") (:type git :flavor melpa :host github :repo "cireu/emacsql-sqlite3" :package "emacsql-sqlite3" :local-repo "emacsql-sqlite3")) "company-org-roam" ("2021-02-04 12:31:03" ("emacs" "company" "dash" "org-roam") (:type git :host github :repo "emacsattic/company-org-roam" :package "company-org-roam" :local-repo "company-org-roam")) "company" ("2021-02-04 12:31:03" ("emacs") (:type git :flavor melpa :host github :repo "company-mode/company-mode" :package "company" :local-repo "company-mode")) "org-roam-bibtex" ("2021-02-04 12:31:03" ("emacs" "org-roam" "bibtex-completion") (:type git :host github :repo "org-roam/org-roam-bibtex" :package "org-roam-bibtex" :local-repo "org-roam-bibtex")) "bibtex-completion" ("2021-02-04 12:31:03" ("parsebib" "s" "dash" "f" "cl-lib" "biblio" "emacs") (:type git :flavor melpa :files ("bibtex-completion.el" "bibtex-completion-pkg.el") :host github :repo "tmalsburg/helm-bibtex" :package "bibtex-completion" :local-repo "helm-bibtex")) "parsebib" ("2021-02-04 12:31:03" ("emacs") (:type git :flavor melpa :host github :repo "joostkremers/parsebib" :package "parsebib" :local-repo "parsebib")) "biblio" ("2021-02-04 12:31:03" ("emacs" "biblio-core") (:type git :flavor melpa :files (:defaults (:exclude "biblio-core.el") "biblio-pkg.el") :host github :repo "cpitclaudel/biblio.el" :package "biblio" :local-repo "biblio.el")) "biblio-core" ("2021-02-04 12:31:03" ("emacs" "let-alist" "seq" "dash") (:flavor melpa :files ("biblio-core.el" "biblio-core-pkg.el") :package "biblio-core" :local-repo "biblio.el" :type git :repo "cpitclaudel/biblio.el" :host github)) "let-alist" ("2021-02-04 12:31:03" ("emacs") (:type git :host github :repo "emacs-straight/let-alist" :files ("*" (:exclude ".git")) :package "let-alist" :local-repo "let-alist")) "org-journal" ("2021-02-04 12:31:03" ("emacs" "org") (:type git :host github :repo "bastibe/org-journal" :package "org-journal" :local-repo "org-journal")) "org-bullets" ("2021-02-04 12:31:03" nil (:type git :host github :repo "sabof/org-bullets" :package "org-bullets" :local-repo "org-bullets")) "org-ref" ("2021-02-04 12:31:03" ("dash" "htmlize" "helm" "helm-bibtex" "ivy" "hydra" "key-chord" "s" "f" "pdf-tools" "bibtex-completion") (:type git :host github :repo "jkitchin/org-ref" :files (:defaults "org-ref.org" "org-ref.bib" "citeproc" "org-ref-pkg.el") :package "org-ref" :local-repo "org-ref")) "htmlize" ("2021-02-04 12:31:03" nil (:type git :flavor melpa :host github :repo "hniksic/emacs-htmlize" :package "htmlize" :local-repo "emacs-htmlize")) "helm" ("2021-02-04 12:31:03" ("emacs" "async" "popup" "helm-core") (:type git :flavor melpa :files ("*.el" "emacs-helm.sh" (:exclude "helm.el" "helm-lib.el" "helm-source.el" "helm-multi-match.el" "helm-core-pkg.el") "helm-pkg.el") :host github :repo "emacs-helm/helm" :package "helm" :local-repo "helm")) "async" ("2021-02-04 12:31:03" ("emacs") (:type git :flavor melpa :host github :repo "jwiegley/emacs-async" :package "async" :local-repo "emacs-async")) "popup" ("2021-02-04 12:31:03" ("cl-lib") (:type git :flavor melpa :files ("popup.el" "popup-pkg.el") :host github :repo "auto-complete/popup-el" :package "popup" :local-repo "popup-el")) "helm-core" ("2021-02-04 12:31:03" ("emacs" "async") (:flavor melpa :files ("helm-core-pkg.el" "helm.el" "helm-lib.el" "helm-source.el" "helm-multi-match.el" "helm-core-pkg.el") :package "helm-core" :local-repo "helm" :type git :repo "emacs-helm/helm" :host github)) "helm-bibtex" ("2021-02-04 12:31:03" ("bibtex-completion" "helm" "cl-lib" "emacs") (:flavor melpa :files ("helm-bibtex.el" "helm-bibtex-pkg.el") :package "helm-bibtex" :local-repo "helm-bibtex" :type git :repo "tmalsburg/helm-bibtex" :host github)) "key-chord" ("2021-02-04 12:31:03" ("emacs") (:type git :flavor melpa :host github :repo "emacsorphanage/key-chord" :package "key-chord" :local-repo "key-chord")) "pdf-tools" ("2021-02-04 12:31:03" ("emacs" "tablist" "let-alist") (:type git :flavor melpa :files ("lisp/*.el" "README" ("build" "Makefile") ("build" "server") (:exclude "lisp/tablist.el" "lisp/tablist-filter.el") "pdf-tools-pkg.el") :host github :repo "politza/pdf-tools" :package "pdf-tools" :local-repo "pdf-tools")) "tablist" ("2021-02-04 12:31:03" ("emacs") (:type git :flavor melpa :host github :repo "politza/tablist" :package "tablist" :local-repo "tablist")) "org-noter" ("2021-02-04 12:31:04" ("emacs" "cl-lib" "org") (:type git :host github :repo "weirdNox/org-noter" :package "org-noter" :local-repo "org-noter")) "org-download" ("2021-02-04 12:31:04" ("emacs" "async") (:type git :host github :repo "abo-abo/org-download" :package "org-download" :local-repo "org-download")) "org-gcal" ("2021-02-04 12:31:04" ("request" "request-deferred" "alert" "persist" "emacs") (:type git :host github :repo "kidd/org-gcal.el" :package "org-gcal" :local-repo "org-gcal.el")) "request" ("2021-02-04 12:31:04" ("emacs") (:type git :flavor melpa :files ("request.el" "request-pkg.el") :host github :repo "tkf/emacs-request" :package "request" :local-repo "emacs-request")) "request-deferred" ("2021-02-04 12:31:04" ("deferred" "request") (:flavor melpa :files ("request-deferred.el" "request-deferred-pkg.el") :package "request-deferred" :local-repo "emacs-request" :type git :repo "tkf/emacs-request" :host github)) "deferred" ("2021-02-04 12:31:04" ("emacs") (:type git :flavor melpa :files ("deferred.el" "deferred-pkg.el") :host github :repo "kiwanami/emacs-deferred" :package "deferred" :local-repo "emacs-deferred")) "persist" ("2021-02-04 12:31:04" nil (:type git :host github :repo "emacs-straight/persist" :files ("*" (:exclude ".git")) :package "persist" :local-repo "persist")) "org2blog" ("2021-02-04 12:31:04" ("htmlize" "hydra" "xml-rpc" "metaweblog") (:type git :host github :repo "org2blog/org2blog" :files (:defaults "README.org" (:exclude "metaweblog.el") "org2blog-pkg.el") :package "org2blog" :local-repo "org2blog")) "xml-rpc" ("2021-02-04 12:31:04" nil (:type git :flavor melpa :host github :repo "xml-rpc-el/xml-rpc-el" :package "xml-rpc" :local-repo "xml-rpc-el")) "metaweblog" ("2021-02-04 12:31:04" ("emacs") (:flavor melpa :files ("metaweblog.el" "metaweblog-pkg.el") :package "metaweblog" :local-repo "org2blog" :type git :repo "org2blog/org2blog" :host github)) "all-the-icons" ("2021-02-04 12:31:04" ("emacs") (:type git :host github :repo "domtronn/all-the-icons.el" :files (:defaults "data" "all-the-icons-pkg.el") :package "all-the-icons" :local-repo "all-the-icons.el")) "doom-themes" ("2021-02-04 12:31:04" ("emacs" "cl-lib") (:type git :host github :repo "hlissner/emacs-doom-themes" :files (:defaults "themes/*.el" "doom-themes-pkg.el") :package "doom-themes" :local-repo "emacs-doom-themes")) "cycle-themes" ("2021-02-04 12:31:05" ("cl-lib") (:type git :host github :repo "toroidal-code/cycle-themes.el" :package "cycle-themes" :local-repo "cycle-themes.el")) "mini-modeline" ("2021-02-04 12:31:05" ("emacs" "dash") (:type git :host github :repo "kiennq/emacs-mini-modeline" :package "mini-modeline" :local-repo "emacs-mini-modeline")) "rainbow-delimiters" ("2021-02-04 12:31:05" nil (:type git :host github :repo "Fanael/rainbow-delimiters" :package "rainbow-delimiters" :local-repo "rainbow-delimiters")) "smartparens" ("2021-02-04 12:31:05" ("dash" "cl-lib") (:type git :host github :repo "Fuco1/smartparens" :package "smartparens" :local-repo "smartparens")) "company-box" ("2021-02-04 12:31:05" ("emacs" "dash" "dash-functional" "company" "frame-local") (:type git :host github :repo "sebastiencs/company-box" :files (:defaults "images" "company-box-pkg.el") :package "company-box" :local-repo "company-box")) "dash-functional" ("2021-02-04 12:31:05" ("emacs" "dash") (:flavor melpa :files ("dash-functional.el" "dash-functional-pkg.el") :package "dash-functional" :local-repo "dash.el" :type git :repo "magnars/dash.el" :host github)) "frame-local" ("2021-02-04 12:31:05" ("emacs") (:type git :flavor melpa :host github :repo "sebastiencs/frame-local" :package "frame-local" :local-repo "frame-local")) "guess-language" ("2021-02-04 12:31:05" ("cl-lib" "emacs") (:type git :host github :repo "tmalsburg/guess-language.el" :files ("guess-language.el" "trigrams/*" "guess-language-pkg.el") :package "guess-language" :local-repo "guess-language.el")) "yasnippet" ("2021-02-04 12:31:05" ("cl-lib") (:type git :host github :repo "joaotavora/yasnippet" :files ("yasnippet.el" "snippets" "yasnippet-pkg.el") :package "yasnippet" :local-repo "yasnippet")) "focus" ("2021-02-04 12:31:05" ("emacs" "cl-lib") (:type git :host github :repo "larstvei/Focus" :package "focus" :local-repo "Focus")) "ivy-bibtex" ("2021-02-04 12:31:05" ("bibtex-completion" "swiper" "cl-lib") (:flavor melpa :files ("ivy-bibtex.el" "ivy-bibtex-pkg.el") :package "ivy-bibtex" :local-repo "helm-bibtex" :type git :repo "tmalsburg/helm-bibtex" :host github)) "interleave" ("2021-02-04 12:31:05" nil (:type git :flavor melpa :host github :repo "rudolfochrist/interleave" :package "interleave" :local-repo "interleave")) "esxml" ("2021-02-04 12:31:03" nil (:type git :host github :repo "tali713/esxml" :files ("esxml.el" "esxml-query.el" "esxml-pkg.el") :package "esxml" :local-repo "esxml")) "solaire-mode" ("2021-02-02 18:50:08" ("emacs" "cl-lib") (:type git :host github :repo "hlissner/emacs-solaire-mode" :package "solaire-mode" :local-repo "emacs-solaire-mode")) "emojify" ("2021-02-02 02:14:33" ("seq" "ht" "emacs") (:type git :host github :repo "iqbalansari/emacs-emojify" :files (:defaults "data" "images" "emojify-pkg.el") :package "emojify" :local-repo "emacs-emojify")) "ht" ("2021-02-02 02:14:33" ("dash") (:type git :flavor melpa :files ("ht.el" "ht-pkg.el") :host github :repo "Wilfred/ht.el" :package "ht" :local-repo "ht.el")) "olivetti" ("2021-02-03 13:26:47" ("emacs") (:type git :host github :repo "rnkn/olivetti" :package "olivetti" :local-repo "olivetti")) "centered-window-mode" ("2021-02-03 13:41:45" nil (:type git :host github :repo "anler/centered-window-mode" :package "centered-window-mode" :local-repo "centered-window-mode")) "centered-window" ("2021-02-04 12:31:05" ("emacs") (:type git :host github :repo "anler/centered-window-mode" :package "centered-window" :local-repo "centered-window-mode"))))

#s(hash-table size 97 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("straight" ((straight straight-autoloads straight-x) (autoload 'straight-get-recipe "straight" "Interactively select a recipe from one of the recipe repositories.
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
recipe has a non-nil `:no-build' entry, then NO-BUILD is ignored
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
include a non-nil `:no-build' property, to unconditionally
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
      If non-nil, the test is run with `emacs-user-dir' set to STRING.
      Otherwise, a temporary directory is created and used.
      Unless absolute, paths are expanded relative to the variable
      `temproary-file-directory'.

ARGS are accessible within the :pre/:post-bootsrap phases via the
locally bound plist, straight-bug-report-args.

(fn &rest ARGS)" nil t) (function-put 'straight-bug-report 'lisp-indent-function '0) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "straight" '("straight-"))) (defvar straight-x-pinned-packages nil "List of pinned packages.") (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "straight-x" '("straight-x-"))) (provide 'straight-autoloads)) "bind-key" ((bind-key-autoloads bind-key) (autoload 'bind-key "bind-key" "Bind KEY-NAME to COMMAND in KEYMAP (`global-map' if not passed).

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
with the specified `:load-path' the module cannot be found." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "use-package-lint" '("use-package-lint-declaration"))) (provide 'use-package-autoloads)) "lv" ((lv-autoloads lv) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lv" '("lv-"))) (provide 'lv-autoloads)) "hydra" ((hydra-examples hydra hydra-ox hydra-autoloads) (autoload 'defhydra "hydra" "Create a Hydra - a family of functions with prefix NAME.

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

(fn NAME BODY &optional DOCSTRING &rest HEADS)" nil t) (function-put 'defhydra 'lisp-indent-function 'defun) (function-put 'defhydra 'doc-string-elt '3) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "hydra" '("defhydra" "hydra-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "hydra-examples" '("hydra-" "org-agenda-cts" "whitespace-mode"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "hydra-ox" '("hydra-ox"))) (provide 'hydra-autoloads)) "which-key" ((which-key which-key-autoloads) (defvar which-key-mode nil "Non-nil if Which-Key mode is enabled.
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
if there is space and the bottom otherwise." t nil) (autoload 'which-key-setup-side-window-bottom "which-key" "Apply suggested settings for side-window that opens on
bottom." t nil) (autoload 'which-key-setup-minibuffer "which-key" "Apply suggested settings for minibuffer.
Do not use this setup if you use the paging commands. Instead use
`which-key-setup-side-window-bottom', which is nearly identical
but more functional." t nil) (autoload 'which-key-add-keymap-based-replacements "which-key" "Replace the description of KEY using REPLACEMENT in KEYMAP.
KEY should take a format suitable for use in
`kbd'. REPLACEMENT is the string to use to describe the
command associated with KEY in the KEYMAP. You may also use a
cons cell of the form (STRING . COMMAND) for each REPLACEMENT,
where STRING is the replacement string and COMMAND is a symbol
corresponding to the intended command to be replaced. In the
latter case, which-key will verify the intended command before
performing the replacement. COMMAND should be nil if the binding
corresponds to a key prefix. For example,

(which-key-add-keymap-based-replacements global-map
  \"C-x w\" \"Save as\")

and

(which-key-add-keymap-based-replacements global-map
  \"C-x w\" '(\"Save as\" . write-file))

both have the same effect for the \"C-x C-w\" key binding, but
the latter causes which-key to verify that the key sequence is
actually bound to write-file before performing the replacement.

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
replacements are added to
`which-key-key-based-description-replacement-alist'.

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
prefix) if `which-key-use-C-h-commands' is non nil." t nil) (autoload 'which-key-show-keymap "which-key" "Show the top-level bindings in KEYMAP using which-key. KEYMAP
is selected interactively from all available keymaps.

If NO-PAGING is non-nil, which-key will not intercept subsequent
keypresses for the paging functionality.

(fn KEYMAP &optional NO-PAGING)" t nil) (autoload 'which-key-show-full-keymap "which-key" "Show all bindings in KEYMAP using which-key. KEYMAP is
selected interactively from all available keymaps.

(fn KEYMAP)" t nil) (autoload 'which-key-show-minor-mode-keymap "which-key" "Show the top-level bindings in KEYMAP using which-key. KEYMAP
is selected interactively by mode in `minor-mode-map-alist'.

(fn &optional ALL)" t nil) (autoload 'which-key-show-full-minor-mode-keymap "which-key" "Show all bindings in KEYMAP using which-key. KEYMAP
is selected interactively by mode in `minor-mode-map-alist'." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "which-key" '("which-key-"))) (provide 'which-key-autoloads)) "ivy" ((ivy-autoloads ivy-overlay ivy-faces ivy colir elpa) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "colir" '("colir-"))) (autoload 'ivy-resume "ivy" "Resume the last completion session, or SESSION if non-nil.
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

(fn &optional ARG)" t nil) (autoload 'ivy-switch-buffer "ivy" "Switch to another buffer." t nil) (autoload 'ivy-switch-view "ivy" "Switch to one of the window views stored by `ivy-push-view'." t nil) (autoload 'ivy-switch-buffer-other-window "ivy" "Switch to another buffer in another window." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ivy" '("ivy-" "with-ivy-window"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ivy-overlay" '("ivy-"))) (provide 'ivy-autoloads)) "goto-chg" ((goto-chg-autoloads goto-chg) (autoload 'goto-last-change "goto-chg" "Go to the point where the last edit was made in the current buffer.
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

(fn ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "goto-chg" '("glc-"))) (provide 'goto-chg-autoloads)) "evil" ((evil-jumps evil-states evil-autoloads evil-keybindings evil-core evil-digraphs evil evil-commands evil-command-window evil-search evil-maps evil-vars evil-development evil-macros evil-pkg evil-repeat evil-ex evil-integration evil-types evil-common) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-command-window" '("evil-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-commands" '("evil-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-common" '("bounds-of-evil-" "evil-" "forward-evil-"))) (autoload 'evil-mode "evil" nil t) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-core" '("evil-" "turn-o"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-digraphs" '("evil-digraph"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-ex" '("evil-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-integration" '("evil-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-jumps" '("evil-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-macros" '("evil-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-maps" '("evil-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-repeat" '("evil-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-search" '("evil-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-states" '("evil-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-types" '("evil-ex-get-optional-register-and-count"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-vars" '("evil-"))) (provide 'evil-autoloads)) "evil-leader" ((evil-leader evil-leader-autoloads) (autoload 'global-evil-leader-mode "evil-leader" "Global minor mode for <leader> support.

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

(fn MODE KEY DEF &rest BINDINGS)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-leader" '("evil-leader"))) (provide 'evil-leader-autoloads)) "evil-escape" ((evil-escape evil-escape-autoloads) (defvar evil-escape-mode nil "Non-nil if Evil-Escape mode is enabled.
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

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-escape" '("evil-escape"))) (provide 'evil-escape-autoloads)) "s" ((s-autoloads s) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "s" '("s-"))) (provide 's-autoloads)) "el-patch" ((el-patch el-patch-autoloads) (let ((loads (get 'el-patch 'custom-loads))) (if (member '"el-patch" loads) nil (put 'el-patch 'custom-loads (cons '"el-patch" loads)))) (defvar el-patch-deftype-alist nil "Alist of types of definitions that can be patched with `el-patch'.
The keys are definition types, like `defun', `define-minor-mode',
etc. The values are plists; the following keywords are accepted:

`:classify' - a function which may be called with a full
definition form (a list starting with e.g. `defun') and which
returns an alist detailing what it defines. In this alist, the
keys are symbols; only the values `function' and `variable' are
allowed. The values are names of functions and variables,
respectively, that are defined by the definition form. This
argument is mandatory.

`:locate' - a function which may be called with a full definition
form (a list starting with e.g. `defun') and which returns the
actual source code of the definition, as a list. If the patch
correct and up to date, then this will actually be the same as
the definition which was passed in. This argument is optional,
but required if you want patch validation to work.

`:declare' - a list to be put in a `declare' form of the
resulting `el-patch' macro, like:

    ((doc-string 2) (indent defun))

This argument is optional.

`:macro-name' - normally the name of the macro generated for
patching a `defun' is called `el-patch-defun', but you can
override that by providing this argument. This argument is
optional.") (custom-autoload 'el-patch-deftype-alist "el-patch" t) (defvar el-patch--patches (make-hash-table :test 'equal) "Hash table of patches that have been defined.
The keys are symbols naming the objects that have been patched.
The values are hash tables mapping definition types (symbols
`defun', `defmacro', etc.) to patch definitions, which are lists
beginning with `defun', `defmacro', etc.

Note that the symbols are from the versions of patches that have
been resolved in favor of the modified version, when a patch
renames a symbol.") (autoload 'el-patch-add "el-patch" "Patch directive for inserting forms.
In the original definition, the ARGS and their containing form
are removed. In the new definition, the ARGS are spliced into the
containing s-expression.

(fn &rest ARGS)" nil t) (function-put 'el-patch-add 'lisp-indent-function '0) (autoload 'el-patch-remove "el-patch" "Patch directive for removing forms.
In the original definition, the ARGS are spliced into the
containing s-expression. In the new definition, the ARGS and
their containing form are removed.

(fn &rest ARGS)" nil t) (function-put 'el-patch-remove 'lisp-indent-function '0) (autoload 'el-patch-swap "el-patch" "Patch directive for swapping forms.
In the original definition, OLD is spliced into the containing
s-expression. In the new definition, NEW is spliced instead.

(fn OLD NEW)" nil t) (function-put 'el-patch-swap 'lisp-indent-function '0) (autoload 'el-patch-wrap "el-patch" "Patch directive for wrapping forms.
TRIML and TRIMR are optional arguments. If only one is provided,
it is assumed to be TRIML. ARGS is required, and it must be a
list.

In the original definition, the ARGS are spliced into the
containing s-expression. If TRIML is provided, the first TRIML of
the ARGS are removed first. If TRIMR is provided, the last TRIMR
are also removed. In the new definition, the ARGS and their
containing list are spliced into the containing s-expression.

(fn &optional TRIML TRIMR ARGS)" nil t) (function-put 'el-patch-wrap 'lisp-indent-function 'defun) (autoload 'el-patch-splice "el-patch" "Patch directive for splicing forms.
TRIML and TRIMR are optional arguments. If only one is provided,
it is assumed to be TRIML. ARGS is required, and it must be a
list.

In the original definition, the ARGS and their containing list
are spliced into the containing s-expression. In the new
definition, the ARGS are spliced into the containing
s-expression. If TRIML is provided, the first TRIML of the ARGS
are removed first. If TRIMR is provided, the last TRIMR are also
removed.

(fn &optional TRIML TRIMR ARGS)" nil t) (function-put 'el-patch-splice 'lisp-indent-function 'defun) (autoload 'el-patch-let "el-patch" "Patch directive for creating local el-patch bindings.
Creates local bindings according to VARLIST, then splices ARGS
into both the original and new definitions. You may bind symbols
that are also patch directives, but the bindings will not have
effect if the symbols are used at the beginning of a list (they
will act as patch directives).

(fn VARLIST &rest ARGS)" nil t) (function-put 'el-patch-let 'lisp-indent-function '1) (autoload 'el-patch-literal "el-patch" "Patch directive for treating patch directives literally.
ARGS are spliced into the containing s-expression, but are not
processed further by el-patch.

(fn &rest ARGS)" nil t) (function-put 'el-patch-literal 'lisp-indent-function '0) (autoload 'el-patch-concat "el-patch" "Patch directive for modifying string literals.
ARGS should resolve to strings; those strings are passed to
`concat' and spliced into the containing s-expression in both the
original and new definitions.

(fn &rest ARGS)" nil t) (function-put 'el-patch-concat 'lisp-indent-function '0) (autoload 'el-patch--definition "el-patch" "Activate a PATCH-DEFINITION and update `el-patch--patches'.
PATCH-DEFINITION is an unquoted list starting with `defun',
`defmacro', etc., which may contain patch directives.

(fn PATCH-DEFINITION)" nil t) (autoload 'el-patch-unpatch "el-patch" "Remove the patch given by the PATCH-DEFINITION.
This restores the original functionality of the object being
patched. NAME and TYPE are as returned by `el-patch-get'.

(fn NAME TYPE)" t nil) (cl-defmacro el-patch-deftype (type &rest kwargs &key classify locate declare macro-name) "Allow `el-patch' to patch definitions of the given TYPE.
TYPE is a symbol like `defun', `define-minor-mode', etc. This
updates `el-patch-deftype-alist' (which see) with the provided
keyword arguments and defines a macro named like
`el-patch-defun', `el-patch-define-minor-mode', etc." (declare (indent defun)) (ignore locate) (unless classify (error "You must specify `:classify' in calls to `el-patch-deftype'")) `(progn (setf (alist-get ',type el-patch-deftype-alist) (copy-tree ',kwargs)) (defmacro ,(or macro-name (intern (format "el-patch-%S" type))) (name &rest args) ,(format "Use `el-patch' to override a `%S' form.
The ARGS are the same as for `%S'." type type) ,@(when declare `((declare ,@declare))) (list #'el-patch--definition (cl-list* ',type name args))))) (el-patch-deftype cl-defun :classify el-patch-classify-function :locate el-patch-locate-function :declare ((doc-string 3) (indent defun))) (el-patch-deftype defconst :classify el-patch-classify-variable :locate el-patch-locate-variable :declare ((doc-string 3) (indent defun))) (el-patch-deftype defcustom :classify el-patch-classify-variable :locate el-patch-locate-variable :declare ((doc-string 3) (indent defun))) (el-patch-deftype define-minor-mode :classify el-patch-classify-define-minor-mode :locate el-patch-locate-function :declare ((doc-string 2) (indent defun))) (el-patch-deftype defmacro :classify el-patch-classify-function :locate el-patch-locate-function :declare ((doc-string 3) (indent defun))) (el-patch-deftype defsubst :classify el-patch-classify-function :locate el-patch-locate-function :declare ((doc-string 3) (indent defun))) (el-patch-deftype defun :classify el-patch-classify-function :locate el-patch-locate-function :declare ((doc-string 3) (indent defun))) (el-patch-deftype defvar :classify el-patch-classify-variable :locate el-patch-locate-variable :declare ((doc-string 3) (indent defun))) (autoload 'el-patch-validate "el-patch" "Validate the patch with given NAME and TYPE.
This means el-patch will attempt to find the original definition
for the function, and verify that it is the same as the original
function assumed by the patch. A warning will be signaled if the
original definition for a patched function cannot be found, or if
there is a difference between the actual and expected original
definitions.

Interactively, use `completing-read' to select a function to
inspect the patch of.

NAME is a symbol naming the object being patched; TYPE is a
symbol `defun', `defmacro', etc.

Returns nil if the patch is not valid, and otherwise returns t.
If NOMSG is non-nil, does not signal a message when the patch is
valid.

If RUN-HOOKS is non-nil, runs `el-patch-pre-validate-hook' and
`el-patch-post-validate-hook'. Interactively, this happens unless
a prefix argument is provided.

See also `el-patch-validate-all'.

(fn NAME TYPE &optional NOMSG RUN-HOOKS)" t nil) (autoload 'el-patch-validate-all "el-patch" "Validate all currently defined patches.
Runs `el-patch-pre-validate-hook' and
`el-patch-post-validate-hook'.

See `el-patch-validate'." t nil) (autoload 'el-patch-feature "el-patch" "Declare that some patches are only defined after FEATURE is loaded.
This is a convenience macro that creates a function for invoking
`require' on that feature, and then adds it to
`el-patch-pre-validate-hook' so that your patches are loaded and
`el-patch' can properly validate them.

FEATURE should be an unquoted symbol. ARGS, if given, are passed
as quoted literals along with FEATURE to
`el-patch-require-function' when `el-patch-validate-all' is
called.

(fn FEATURE &rest ARGS)" nil t) (autoload 'el-patch-get "el-patch" "Return the patch for object NAME of the given TYPE.
NAME is a symbol for the name of the definition that was patched,
and TYPE is a symbol `defun', `defmacro', etc. If the patch could
not be found, return nil.

(fn NAME TYPE)" nil nil) (autoload 'el-patch-ediff-patch "el-patch" "Show the patch for an object in Ediff.
NAME and TYPE are as returned by `el-patch-get'.

(fn NAME TYPE)" t nil) (autoload 'el-patch-ediff-conflict "el-patch" "Show a patch conflict in Ediff.
This is a diff between the expected and actual values of a
patch's original definition. NAME and TYPE are as returned by
`el-patch-get'.

(fn NAME TYPE)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "el-patch" '("el-patch-"))) (provide 'el-patch-autoloads)) "deft" ((deft deft-autoloads) (autoload 'deft-find-file "deft" "Find FILE interactively using the minibuffer.
FILE must exist and be a relative or absolute path, with extension.
If FILE is not inside `deft-directory', fall back to using `find-file'.

(fn FILE)" t nil) (autoload 'deft-new-file "deft" "Create a new file quickly.
Use either an automatically generated filename or the filter string if non-nil
and `deft-use-filter-string-for-filename' is set.  If the filter string is
non-nil and title is not from filename, use it as the title." t nil) (autoload 'deft "deft" "Switch to *Deft* buffer and load files." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "deft" '("deft-" "org-deft-store-link"))) (provide 'deft-autoloads)) "swiper" ((swiper swiper-autoloads) (autoload 'swiper-avy "swiper" "Jump to one of the current swiper candidates." t nil) (autoload 'swiper-backward "swiper" "`isearch-backward' with an overview.
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

(fn &optional INITIAL-INPUT INITIAL-DIRECTORY)" t nil) (autoload 'counsel-ag "counsel" "Grep for a string in a root directory using ag.

By default, the root directory is the first directory containing a .git subdirectory.

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

(fn &optional INITIAL-INPUT)" t nil) (autoload 'counsel-rg "counsel" "Grep for a string in the current directory using rg.
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

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "counsel" '("counsel-" "ivy-function-called-at-point" "tmm-km-list"))) (provide 'counsel-autoloads)) "smooth-scrolling" ((smooth-scrolling-autoloads smooth-scrolling) (defvar smooth-scrolling-mode nil "Non-nil if Smooth-Scrolling mode is enabled.
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

(fn FUNC)" nil t) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smooth-scrolling" '("disable-smooth-scroll-for-function" "do-smooth-scroll" "enable-smooth-scroll-for-function-conditionally" "smooth-scroll-" "window-is-at-bob-p"))) (provide 'smooth-scrolling-autoloads)) "gntp" ((gntp gntp-autoloads) (autoload 'gntp-notify "gntp" "Send notification NAME with TITLE, TEXT, PRIORITY and ICON to SERVER:PORT.
PORT defaults to `gntp-server-port'

(fn NAME TITLE TEXT SERVER &optional PORT PRIORITY ICON)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gntp" '("gntp-"))) (provide 'gntp-autoloads)) "log4e" ((log4e-autoloads log4e) (autoload 'log4e-mode "log4e" "Major mode for browsing a buffer made by log4e.

\\<log4e-mode-map>
\\{log4e-mode-map}

(fn)" t nil) (autoload 'log4e:insert-start-log-quickly "log4e" "Insert logging statment for trace level log at start of current function/macro." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "log4e" '("log4e"))) (provide 'log4e-autoloads)) "alert" ((alert alert-autoloads) (autoload 'alert-add-rule "alert" "Programmatically add an alert configuration rule.

Normally, users should custoimze `alert-user-configuration'.
This facility is for module writers and users that need to do
things the Lisp way.

Here is a rule the author currently uses with ERC, so that the
fringe gets colored whenever people chat on BitlBee:

(alert-add-rule :status   \\='(buried visible idle)
                :severity \\='(moderate high urgent)
                :mode     \\='erc-mode
                :predicate
                #\\='(lambda (info)
                    (string-match (concat \"\\\\`[^&].*@BitlBee\\\\\\='\")
                                  (erc-format-target-and/or-network)))
                :persistent
                #\\='(lambda (info)
                    ;; If the buffer is buried, or the user has been
                    ;; idle for `alert-reveal-idle-time' seconds,
                    ;; make this alert persistent.  Normally, alerts
                    ;; become persistent after
                    ;; `alert-persist-idle-time' seconds.
                    (memq (plist-get info :status) \\='(buried idle)))
                :style \\='fringe
                :continue t)

(fn &key SEVERITY STATUS MODE CATEGORY TITLE MESSAGE PREDICATE ICON (STYLE alert-default-style) PERSISTENT CONTINUE NEVER-PERSIST APPEND)" nil nil) (autoload 'alert "alert" "Alert the user that something has happened.
MESSAGE is what the user will see.  You may also use keyword
arguments to specify additional details.  Here is a full example:

(alert \"This is a message\"
       :severity \\='high          ;; The default severity is `normal'
       :title \"Title\"           ;; An optional title
       :category \\='example       ;; A symbol to identify the message
       :mode \\='text-mode         ;; Normally determined automatically
       :buffer (current-buffer) ;; This is the default
       :data nil                ;; Unused by alert.el itself
       :persistent nil          ;; Force the alert to be persistent;
                                ;; it is best not to use this
       :never-persist nil       ;; Force this alert to never persist
       :id \\='my-id)              ;; Used to replace previous message of
                                ;; the same id in styles that support it
       :style \\='fringe)          ;; Force a given style to be used;
                                ;; this is only for debugging!

If no :title is given, the buffer-name of :buffer is used.  If
:buffer is nil, it is the current buffer at the point of call.

:data is an opaque value which modules can pass through to their
own styles if they wish.

Here are some more typical examples of usage:

  ;; This is the most basic form usage
  (alert \"This is an alert\")

  ;; You can adjust the severity for more important messages
  (alert \"This is an alert\" :severity \\='high)

  ;; Or decrease it for purely informative ones
  (alert \"This is an alert\" :severity \\='trivial)

  ;; Alerts can have optional titles.  Otherwise, the title is the
  ;; buffer-name of the (current-buffer) where the alert originated.
  (alert \"This is an alert\" :title \"My Alert\")

  ;; Further, alerts can have categories.  This allows users to
  ;; selectively filter on them.
  (alert \"This is an alert\" :title \"My Alert\"
         :category \\='some-category-or-other)

(fn MESSAGE &key (SEVERITY \\='normal) TITLE ICON CATEGORY BUFFER MODE DATA STYLE PERSISTENT NEVER-PERSIST ID)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "alert" '("alert-" "x-urgen"))) (provide 'alert-autoloads)) "google-this" ((google-this google-this-autoloads) (autoload 'google-this-search "google-this" "Write and do a google search.
Interactively PREFIX determines quoting.
Non-interactively SEARCH-STRING is the string to search.

(fn PREFIX &optional SEARCH-STRING)" t nil) (autoload 'google-this-lucky-and-insert-url "google-this" "Fetch the url that would be visited by `google-this-lucky'.

If you just want to do an \"I'm feeling lucky search\", use
`google-this-lucky-search' instead.

Interactively:
* Insert the URL at point,
* Kill the searched term, removing it from the buffer (it is killed, not
  deleted, so it can be easily yanked back if desired).
* Search term defaults to region or line, and always queries for
  confirmation.

Non-Interactively:
* Runs synchronously,
* Search TERM is an argument without confirmation,
* Only insert if INSERT is non-nil, otherwise return.

(fn TERM &optional INSERT)" t nil) (autoload 'google-this-lucky-search "google-this" "Exactly like `google-this-search', but use the \"I'm feeling lucky\" option.
PREFIX determines quoting.

(fn PREFIX)" t nil) (autoload 'google-this-string "google-this" "Google given TEXT, but ask the user first if NOCONFIRM is nil.
PREFIX determines quoting.

(fn PREFIX &optional TEXT NOCONFIRM)" nil nil) (autoload 'google-this-line "google-this" "Google the current line.
PREFIX determines quoting.
NOCONFIRM goes without asking for confirmation.

(fn PREFIX &optional NOCONFIRM)" t nil) (autoload 'google-this-ray "google-this" "Google text between the point and end of the line.
If there is a selected region, googles the region.
PREFIX determines quoting. Negative arguments invert the line segment.
NOCONFIRM goes without asking for confirmation.
NOREGION ignores the region.

(fn PREFIX &optional NOCONFIRM NOREGION)" t nil) (autoload 'google-this-word "google-this" "Google the current word.
PREFIX determines quoting.

(fn PREFIX)" t nil) (autoload 'google-this-symbol "google-this" "Google the current symbol.
PREFIX determines quoting.

(fn PREFIX)" t nil) (autoload 'google-this-region "google-this" "Google the current region.
PREFIX determines quoting.
NOCONFIRM goes without asking for confirmation.

(fn PREFIX &optional NOCONFIRM)" t nil) (autoload 'google-this "google-this" "Decide what the user wants to google (always something under point).
Unlike `google-this-search' (which presents an empty prompt with
\"this\" as the default value), this function inserts the query
in the minibuffer to be edited.
PREFIX argument determines quoting.
NOCONFIRM goes without asking for confirmation.

(fn PREFIX &optional NOCONFIRM)" t nil) (autoload 'google-this-noconfirm "google-this" "Decide what the user wants to google and go without confirmation.
Exactly like `google-this' or `google-this-search', but don't ask
for confirmation.
PREFIX determines quoting.

(fn PREFIX)" t nil) (autoload 'google-this-error "google-this" "Google the current error in the compilation buffer.
PREFIX determines quoting.

(fn PREFIX)" t nil) (autoload 'google-this-clean-error-string "google-this" "Parse error string S and turn it into googleable strings.

Removes unhelpful details like file names and line numbers from
simple error strings (such as c-like erros).

Uses replacements in `google-this-error-regexp' and stops at the first match.

(fn S)" t nil) (autoload 'google-this-cpp-reference "google-this" "Visit the most probable cppreference.com page for this word." t nil) (autoload 'google-this-forecast "google-this" "Search google for \"weather\".
With PREFIX, ask for location.

(fn PREFIX)" t nil) (defvar google-this-mode nil "Non-nil if Google-This mode is enabled.
See the `google-this-mode' command
for a description of this minor mode.") (custom-autoload 'google-this-mode "google-this" nil) (autoload 'google-this-mode "google-this" "Toggle Google-This mode on or off.

If called interactively, enable Google-This mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\\{google-this-mode-map}

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "google-this" '("google-this-"))) (provide 'google-this-autoloads)) "mu4e" (nil) "calfw" ((calfw calfw-autoloads) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "calfw" '("cfw:"))) (provide 'calfw-autoloads)) "dash" ((dash-autoloads dash) (autoload 'dash-fontify-mode "dash" "Toggle fontification of Dash special variables.

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
This allows Dash symbols to be looked up with \\[info-lookup-symbol]." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dash" '("!cdr" "!cons" "--" "->" "-a" "-butlast" "-c" "-d" "-e" "-f" "-gr" "-i" "-keep" "-l" "-m" "-non" "-only-some" "-p" "-r" "-s" "-t" "-u" "-value-to-list" "-when-let" "-zip" "dash-"))) (provide 'dash-autoloads)) "transient" ((transient transient-autoloads) (autoload 'transient-insert-suffix "transient" "Insert a SUFFIX into PREFIX before LOC.
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

(fn PREFIX LOC)" nil nil) (function-put 'transient-remove-suffix 'lisp-indent-function 'defun) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "transient" '("transient-"))) (provide 'transient-autoloads)) "with-editor" ((with-editor with-editor-autoloads) (autoload 'with-editor-export-editor "with-editor" "Teach subsequent commands to use current Emacs instance as editor.

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

(fn COMMAND &optional OUTPUT-BUFFER ERROR-BUFFER ENVVAR)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "with-editor" '("server-" "shell-command--shell-command-with-editor-mode" "start-file-process--with-editor-process-filter" "with-editor"))) (provide 'with-editor-autoloads)) "git-commit" ((git-commit git-commit-autoloads) (defvar global-git-commit-mode t "Non-nil if Global Git-Commit mode is enabled.
See the `global-git-commit-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-git-commit-mode'.") (custom-autoload 'global-git-commit-mode "git-commit" nil) (autoload 'global-git-commit-mode "git-commit" "Edit Git commit messages.
This global mode arranges for `git-commit-setup' to be called
when a Git commit message file is opened.  That usually happens
when Git uses the Emacsclient as $GIT_EDITOR to have the user
provide such a commit message.

If called interactively, enable Global Git-Commit mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (defconst git-commit-filename-regexp "/\\(\\(\\(COMMIT\\|NOTES\\|PULLREQ\\|MERGEREQ\\|TAG\\)_EDIT\\|MERGE_\\|\\)MSG\\|\\(BRANCH\\|EDIT\\)_DESCRIPTION\\)\\'") (autoload 'git-commit-setup-check-buffer "git-commit" nil nil nil) (autoload 'git-commit-setup "git-commit" nil nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "git-commit" '("git-commit-"))) (provide 'git-commit-autoloads)) "magit" ((magit-sequence magit-bookmark magit-branch magit magit-imenu magit-ediff magit-apply magit-notes magit-transient magit-wip magit-process magit-subtree magit-files magit-fetch magit-reset magit-remote magit-stash git-rebase magit-gitignore magit-extras magit-log magit-utils magit-section magit-repos magit-pull magit-reflog magit-commit magit-mode magit-pkg magit-tag magit-bisect magit-merge magit-submodule magit-core magit-margin magit-worktree magit-status magit-git magit-autorevert magit-obsolete magit-push magit-patch magit-refs magit-clone magit-autoloads magit-blame magit-diff) (autoload 'git-rebase-current-line "git-rebase" "Parse current line into a `git-rebase-action' instance.
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

These bindings may be added when `after-init-hook' is called.
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

Setting this variable after the hook has already been called
has no effect.

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

(fn REVISION)" t nil) (autoload 'magit-branch-create "magit-branch" "Create BRANCH at branch or revision START-POINT.

(fn BRANCH START-POINT)" t nil) (autoload 'magit-branch-and-checkout "magit-branch" "Create and checkout BRANCH at branch or revision START-POINT.

(fn BRANCH START-POINT)" t nil) (autoload 'magit-branch-or-checkout "magit-branch" "Hybrid between `magit-checkout' and `magit-branch-and-checkout'.

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

(fn BRANCH)" t nil) (autoload 'magit-branch-configure "magit-branch" nil t) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-branch" '("magit-"))) (autoload 'magit-clone "magit-clone" nil t) (autoload 'magit-clone-regular "magit-clone" "Create a clone of REPOSITORY in DIRECTORY.
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

(fn &optional COMMIT ARGS)" t nil) (autoload 'magit-commit-augment "magit-commit" "Create a squash commit, editing the squash message.

With a prefix argument the target COMMIT has to be confirmed.
Otherwise the commit at point may be used without confirmation
depending on the value of option `magit-commit-squash-confirm'.

(fn &optional COMMIT ARGS)" t nil) (autoload 'magit-commit-instant-fixup "magit-commit" "Create a fixup commit targeting COMMIT and instantly rebase.

(fn &optional COMMIT ARGS)" t nil) (autoload 'magit-commit-instant-squash "magit-commit" "Create a squash commit targeting COMMIT and instantly rebase.

(fn &optional COMMIT ARGS)" t nil) (autoload 'magit-commit-reshelve "magit-commit" "Change the committer date and possibly the author date of `HEAD'.

If you are the author of `HEAD', then both dates are changed,
otherwise only the committer date.  The current time is used
as the initial minibuffer input and the original author (if
that is you) or committer date is available as the previous
history element.

(fn DATE)" t nil) (autoload 'magit-commit-absorb-modules "magit-commit" "Spread modified modules across recent commits.

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

(fn &optional FOLLOW)" t nil) (autoload 'magit-do-async-shell-command "magit-extras" "Open FILE with `dired-do-async-shell-command'.
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

(fn REV)" t nil) (autoload 'magit-pop-revision-stack "magit-extras" "Insert a representation of a revision into the current buffer.

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
`magit-revision-stack'." t nil) (autoload 'magit-abort-dwim "magit-extras" "Abort current operation.
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

(fn RULE)" t nil) (autoload 'magit-gitignore-in-subdir "magit-gitignore" "Add the Git ignore RULE to a \".gitignore\" file.
Prompted the user for a directory and add the rule to the
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

(git merge --abort)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-merge" '("magit-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-mode" '("disable-magit-save-buffers" "inhibit-magit-refresh" "magit-"))) (autoload 'magit-notes "magit" nil t) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-notes" '("magit-notes-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-obsolete" '("magit--magit-popup-warning"))) (autoload 'magit-patch "magit-patch" nil t) (autoload 'magit-patch-create "magit-patch" nil t) (autoload 'magit-patch-apply "magit-patch" nil t) (autoload 'magit-patch-save "magit-patch" "Write current diff into patch FILE.

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

(fn COMMIT &optional HARD)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-reset" '("magit-reset-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-section" '("isearch-clean-overlays@magit-mode" "magit-"))) (autoload 'magit-sequencer-continue "magit-sequence" "Resume the current cherry-pick or revert sequence." t nil) (autoload 'magit-sequencer-skip "magit-sequence" "Skip the stopped at commit during a cherry-pick or revert sequence." t nil) (autoload 'magit-sequencer-abort "magit-sequence" "Abort the current cherry-pick or revert sequence.
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
also contains other useful hints.") (autoload 'magit-status-here "magit-status" "Like `magit-status' but with non-nil `magit-status-goto-file-position'." t nil) (autoload 'magit-status-setup-buffer "magit-status" "

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

(fn WORKTREE PATH)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-worktree" '("magit-"))) (provide 'magit-autoloads)) "git-gutter" ((git-gutter-autoloads git-gutter) (autoload 'git-gutter:linum-setup "git-gutter" "Setup for linum-mode." nil nil) (autoload 'git-gutter-mode "git-gutter" "Git-Gutter mode

If called interactively, enable Git-Gutter mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp, also
enable the mode if ARG is omitted or nil, and toggle it if ARG is
`toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (put 'global-git-gutter-mode 'globalized-minor-mode t) (defvar global-git-gutter-mode nil "Non-nil if Global Git-Gutter mode is enabled.
See the `global-git-gutter-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-git-gutter-mode'.") (custom-autoload 'global-git-gutter-mode "git-gutter" nil) (autoload 'global-git-gutter-mode "git-gutter" "Toggle Git-Gutter mode in all buffers.
With prefix ARG, enable Global Git-Gutter mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Git-Gutter mode is enabled in all buffers where
`git-gutter--turn-on' would do it.
See `git-gutter-mode' for more information on Git-Gutter mode.

(fn &optional ARG)" t nil) (autoload 'git-gutter "git-gutter" "Show diff information in gutter" t nil) (autoload 'git-gutter:toggle "git-gutter" "Toggle to show diff information." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "git-gutter" '("git-gutter"))) (provide 'git-gutter-autoloads)) "git-timemachine" ((git-timemachine-autoloads git-timemachine) (autoload 'git-timemachine-toggle "git-timemachine" "Toggle git timemachine mode." t nil) (autoload 'git-timemachine "git-timemachine" "Enable git timemachine for file of current buffer." t nil) (autoload 'git-timemachine-switch-branch "git-timemachine" "Enable git timemachine for current buffer, switching to GIT-BRANCH.

(fn GIT-BRANCH)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "git-timemachine" '("git-timemachine-"))) (provide 'git-timemachine-autoloads)) "org" ((ob-picolisp ob-eval ob-ocaml ob-abc ob-mscgen org-id ox-html org-goto org-clock ob-css ob-sqlite ox-man org-habit ox-md org-lint ob-sass org-crypt ob-processing org-archive ol-irc org-table ol ob-lisp ob-J ob-groovy ob-eshell ob-js ob-sql org-duration org-feed ob-dot org-num ob-emacs-lisp org-attach-git ox-texinfo ob-R ob-forth org-install ob-java ol-eshell org-ctags org-faces ox-beamer ox-publish org ol-bibtex ob-tangle ob-awk ol-rmail org-mouse ob-gnuplot ob-lua ob-haskell ol-mhe ol-bbdb ob-core ox org-compat org-entities org-keys ob-octave ob-ref ob-lob ol-info ob-ruby ob-org ob-coq ob-makefile org-datetree ox-icalendar ob-comint ob-ebnf ob-python ob-asymptote ob-shell ob-exp org-list ol-gnus ob-io org-agenda org-inlinetask org-timer ol-docview ob ob-fortran org-loaddefs org-mobile ob-matlab ob-calc ob-lilypond org-colview ob-plantuml org-macro ob-screen ox-odt org-tempo org-attach ob-hledger org-src ob-stan org-element ob-scheme org-pcomplete org-protocol org-capture ob-shen ob-C org-macs org-autoloads ob-latex ob-ledger org-plot org-refile ob-perl ob-maxima ob-clojure ob-ditaa ox-ascii org-footnote ox-latex ob-table ol-eww ol-w3m ob-sed ob-vala ox-org org-indent) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-C" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-J" '("obj-" "org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-R" '("ob-R-" "org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-abc" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-asymptote" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-awk" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-calc" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-clojure" '("ob-clojure-" "org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-comint" '("org-babel-comint-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-coq" '("coq-program-name" "org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-core" '("org-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-css" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-ditaa" '("org-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-dot" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-ebnf" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-emacs-lisp" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-eshell" '("ob-eshell-session-live-p" "org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-eval" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-exp" '("org-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-forth" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-fortran" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-gnuplot" '("*org-babel-gnuplot-" "org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-groovy" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-haskell" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-hledger" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-io" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-java" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-js" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-latex" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-ledger" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-lilypond" '("lilypond-mode" "org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-lisp" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-lob" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-lua" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-makefile" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-maxima" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-mscgen" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-ocaml" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-octave" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-org" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-perl" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-picolisp" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-plantuml" '("org-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-processing" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-python" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-ref" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-ruby" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-sass" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-scheme" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-screen" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-sed" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-shell" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-shen" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-sql" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-sqlite" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-stan" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-table" '("org-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-tangle" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-vala" '("org-babel-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ol" '("org-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ol-bbdb" '("org-bbdb-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ol-bibtex" '("org-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ol-docview" '("org-docview-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ol-eshell" '("org-eshell-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ol-eww" '("org-eww-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ol-gnus" '("org-gnus-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ol-info" '("org-info-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ol-irc" '("org-irc-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ol-mhe" '("org-mhe-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ol-rmail" '("org-rmail-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ol-w3m" '("org-w3m-"))) (autoload 'org-babel-do-load-languages "org" "Load the languages defined in `org-babel-load-languages'.

(fn SYM VALUE)" nil nil) (autoload 'org-babel-load-file "org" "Load Emacs Lisp source code blocks in the Org FILE.
This function exports the source code using `org-babel-tangle'
and then loads the resulting file using `load-file'.  With
optional prefix argument COMPILE, the tangled Emacs Lisp file is
byte-compiled before it is loaded.

(fn FILE &optional COMPILE)" t nil) (autoload 'org-version "org" "Show the Org version.
Interactively, or when MESSAGE is non-nil, show it in echo area.
With prefix argument, or when HERE is non-nil, insert it at point.
In non-interactive uses, a reduced version string is output unless
FULL is given.

(fn &optional HERE FULL MESSAGE)" t nil) (autoload 'org-load-modules-maybe "org" "Load all extensions listed in `org-modules'.

(fn &optional FORCE)" nil nil) (autoload 'org-clock-persistence-insinuate "org" "Set up hooks for clock persistence." nil nil) (autoload 'org-mode "org" "Outline-based notes management and organizer, alias
\"Carsten's outline-mode for keeping track of everything.\"

Org mode develops organizational tasks around a NOTES file which
contains information about projects as plain text.  Org mode is
implemented on top of Outline mode, which is ideal to keep the content
of large files well structured.  It supports ToDo items, deadlines and
time stamps, which magically appear in the diary listing of the Emacs
calendar.  Tables are easily created with a built-in table editor.
Plain text URL-like links connect to websites, emails (VM), Usenet
messages (Gnus), BBDB entries, and any files related to the project.
For printing and sharing of notes, an Org file (or a part of it)
can be exported as a structured ASCII or HTML file.

The following commands are available:

\\{org-mode-map}

(fn)" t nil) (autoload 'org-cycle "org" "TAB-action and visibility cycling for Org mode.

This is the command invoked in Org mode by the `TAB' key.  Its main
purpose is outline visibility cycling, but it also invokes other actions
in special contexts.

When this function is called with a `\\[universal-argument]' prefix, rotate the entire
buffer through 3 states (global cycling)
  1. OVERVIEW: Show only top-level headlines.
  2. CONTENTS: Show all headlines of all levels, but no body text.
  3. SHOW ALL: Show everything.

With a `\\[universal-argument] \\[universal-argument]' prefix argument, switch to the startup visibility,
determined by the variable `org-startup-folded', and by any VISIBILITY
properties in the buffer.

With a `\\[universal-argument] \\[universal-argument] \\[universal-argument]' prefix argument, show the entire buffer, including
any drawers.

When inside a table, re-align the table and move to the next field.

When point is at the beginning of a headline, rotate the subtree started
by this line through 3 different states (local cycling)
  1. FOLDED:   Only the main headline is shown.
  2. CHILDREN: The main headline and the direct children are shown.
               From this state, you can move to one of the children
               and zoom in further.
  3. SUBTREE:  Show the entire subtree, including body text.
If there is no subtree, switch directly from CHILDREN to FOLDED.

When point is at the beginning of an empty headline and the variable
`org-cycle-level-after-item/entry-creation' is set, cycle the level
of the headline by demoting and promoting it to likely levels.  This
speeds up creation document structure by pressing `TAB' once or several
times right after creating a new headline.

When there is a numeric prefix, go up to a heading with level ARG, do
a `show-subtree' and return to the previous cursor position.  If ARG
is negative, go up that many levels.

When point is not at the beginning of a headline, execute the global
binding for `TAB', which is re-indenting the line.  See the option
`org-cycle-emulate-tab' for details.

As a special case, if point is at the very beginning of the buffer, if
there is no headline there, and if the variable `org-cycle-global-at-bob'
is non-nil, this function acts as if called with prefix argument (`\\[universal-argument] TAB',
same as `S-TAB') also when called without prefix argument.

(fn &optional ARG)" t nil) (autoload 'org-global-cycle "org" "Cycle the global visibility.  For details see `org-cycle'.
With `\\[universal-argument]' prefix ARG, switch to startup visibility.
With a numeric prefix, show all headlines up to that level.

(fn &optional ARG)" t nil) (autoload 'org-run-like-in-org-mode "org" "Run a command, pretending that the current buffer is in Org mode.
This will temporarily bind local variables that are typically bound in
Org mode to the values they have in Org mode, and then interactively
call CMD.

(fn CMD)" nil nil) (autoload 'org-open-file "org" "Open the file at PATH.
First, this expands any special file name abbreviations.  Then the
configuration variable `org-file-apps' is checked if it contains an
entry for this file type, and if yes, the corresponding command is launched.

If no application is found, Emacs simply visits the file.

With optional prefix argument IN-EMACS, Emacs will visit the file.
With a double \\[universal-argument] \\[universal-argument] prefix arg, Org tries to avoid opening in Emacs
and to use an external application to visit the file.

Optional LINE specifies a line to go to, optional SEARCH a string
to search for.  If LINE or SEARCH is given, the file will be
opened in Emacs, unless an entry from `org-file-apps' that makes
use of groups in a regexp matches.

If you want to change the way frames are used when following a
link, please customize `org-link-frame-setup'.

If the file does not exist, throw an error.

(fn PATH &optional IN-EMACS LINE SEARCH)" nil nil) (autoload 'org-open-at-point-global "org" "Follow a link or a time-stamp like Org mode does.
Also follow links and emails as seen by `thing-at-point'.
This command can be called in any mode to follow an external
link or a time-stamp that has Org mode syntax.  Its behavior
is undefined when called on internal links like fuzzy links.
Raise a user error when there is nothing to follow." t nil) (autoload 'org-offer-links-in-entry "org" "Offer links in the current entry and return the selected link.
If there is only one link, return it.
If NTH is an integer, return the NTH link found.
If ZERO is a string, check also this string for a link, and if
there is one, return it.

(fn BUFFER MARKER &optional NTH ZERO)" nil nil) (autoload 'org-switchb "org" "Switch between Org buffers.

With `\\[universal-argument]' prefix, restrict available buffers to files.

With `\\[universal-argument] \\[universal-argument]' prefix, restrict available buffers to agenda files.

(fn &optional ARG)" t nil) (autoload 'org-cycle-agenda-files "org" "Cycle through the files in `org-agenda-files'.
If the current buffer visits an agenda file, find the next one in the list.
If the current buffer does not, find the first agenda file." t nil) (autoload 'org-submit-bug-report "org" "Submit a bug report on Org via mail.

Don't hesitate to report any problems or inaccurate documentation.

If you don't have setup sending mail from (X)Emacs, please copy the
output buffer into your mail program, as it gives us important
information about your Org version and configuration." t nil) (autoload 'org-reload "org" "Reload all Org Lisp files.
With prefix arg UNCOMPILED, load the uncompiled versions.

(fn &optional UNCOMPILED)" t nil) (autoload 'org-customize "org" "Call the customize function with org as argument." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org" '("org-" "turn-on-org-cdlatex"))) (autoload 'org-toggle-sticky-agenda "org-agenda" "Toggle `org-agenda-sticky'.

(fn &optional ARG)" t nil) (autoload 'org-agenda "org-agenda" "Dispatch agenda commands to collect entries to the agenda buffer.
Prompts for a command to execute.  Any prefix arg will be passed
on to the selected command.  The default selections are:

a     Call `org-agenda-list' to display the agenda for current day or week.
t     Call `org-todo-list' to display the global todo list.
T     Call `org-todo-list' to display the global todo list, select only
      entries with a specific TODO keyword (the user gets a prompt).
m     Call `org-tags-view' to display headlines with tags matching
      a condition  (the user is prompted for the condition).
M     Like `m', but select only TODO entries, no ordinary headlines.
e     Export views to associated files.
s     Search entries for keywords.
S     Search entries for keywords, only with TODO keywords.
/     Multi occur across all agenda files and also files listed
      in `org-agenda-text-search-extra-files'.
<     Restrict agenda commands to buffer, subtree, or region.
      Press several times to get the desired effect.
>     Remove a previous restriction.
#     List \"stuck\" projects.
!     Configure what \"stuck\" means.
C     Configure custom agenda commands.

More commands can be added by configuring the variable
`org-agenda-custom-commands'.  In particular, specific tags and TODO keyword
searches can be pre-defined in this way.

If the current buffer is in Org mode and visiting a file, you can also
first press `<' once to indicate that the agenda should be temporarily
(until the next use of `\\[org-agenda]') restricted to the current file.
Pressing `<' twice means to restrict to the current subtree or region
(if active).

(fn &optional ARG ORG-KEYS RESTRICTION)" t nil) (autoload 'org-batch-agenda "org-agenda" "Run an agenda command in batch mode and send the result to STDOUT.
If CMD-KEY is a string of length 1, it is used as a key in
`org-agenda-custom-commands' and triggers this command.  If it is a
longer string it is used as a tags/todo match string.
Parameters are alternating variable names and values that will be bound
before running the agenda command.

(fn CMD-KEY &rest PARAMETERS)" nil t) (autoload 'org-batch-agenda-csv "org-agenda" "Run an agenda command in batch mode and send the result to STDOUT.
If CMD-KEY is a string of length 1, it is used as a key in
`org-agenda-custom-commands' and triggers this command.  If it is a
longer string it is used as a tags/todo match string.
Parameters are alternating variable names and values that will be bound
before running the agenda command.

The output gives a line for each selected agenda item.  Each
item is a list of comma-separated values, like this:

category,head,type,todo,tags,date,time,extra,priority-l,priority-n

category     The category of the item
head         The headline, without TODO kwd, TAGS and PRIORITY
type         The type of the agenda entry, can be
                todo               selected in TODO match
                tagsmatch          selected in tags match
                diary              imported from diary
                deadline           a deadline on given date
                scheduled          scheduled on given date
                timestamp          entry has timestamp on given date
                closed             entry was closed on given date
                upcoming-deadline  warning about deadline
                past-scheduled     forwarded scheduled item
                block              entry has date block including g. date
todo         The todo keyword, if any
tags         All tags including inherited ones, separated by colons
date         The relevant date, like 2007-2-14
time         The time, like 15:00-16:50
extra        String with extra planning info
priority-l   The priority letter if any was given
priority-n   The computed numerical priority
agenda-day   The day in the agenda where this is listed

(fn CMD-KEY &rest PARAMETERS)" nil t) (autoload 'org-store-agenda-views "org-agenda" "Store agenda views.

(fn &rest PARAMETERS)" t nil) (autoload 'org-batch-store-agenda-views "org-agenda" "Run all custom agenda commands that have a file argument.

(fn &rest PARAMETERS)" nil t) (autoload 'org-agenda-list "org-agenda" "Produce a daily/weekly view from all files in variable `org-agenda-files'.
The view will be for the current day or week, but from the overview buffer
you will be able to go to other days/weeks.

With a numeric prefix argument in an interactive call, the agenda will
span ARG days.  Lisp programs should instead specify SPAN to change
the number of days.  SPAN defaults to `org-agenda-span'.

START-DAY defaults to TODAY, or to the most recent match for the weekday
given in `org-agenda-start-on-weekday'.

When WITH-HOUR is non-nil, only include scheduled and deadline
items if they have an hour specification like [h]h:mm.

(fn &optional ARG START-DAY SPAN WITH-HOUR)" t nil) (autoload 'org-search-view "org-agenda" "Show all entries that contain a phrase or words or regular expressions.

With optional prefix argument TODO-ONLY, only consider entries that are
TODO entries.  The argument STRING can be used to pass a default search
string into this function.  If EDIT-AT is non-nil, it means that the
user should get a chance to edit this string, with cursor at position
EDIT-AT.

The search string can be viewed either as a phrase that should be found as
is, or it can be broken into a number of snippets, each of which must match
in a Boolean way to select an entry.  The default depends on the variable
`org-agenda-search-view-always-boolean'.
Even if this is turned off (the default) you can always switch to
Boolean search dynamically by preceding the first word with  \"+\" or \"-\".

The default is a direct search of the whole phrase, where each space in
the search string can expand to an arbitrary amount of whitespace,
including newlines.

If using a Boolean search, the search string is split on whitespace and
each snippet is searched separately, with logical AND to select an entry.
Words prefixed with a minus must *not* occur in the entry.  Words without
a prefix or prefixed with a plus must occur in the entry.  Matching is
case-insensitive.  Words are enclosed by word delimiters (i.e. they must
match whole words, not parts of a word) if
`org-agenda-search-view-force-full-words' is set (default is nil).

Boolean search snippets enclosed by curly braces are interpreted as
regular expressions that must or (when preceded with \"-\") must not
match in the entry.  Snippets enclosed into double quotes will be taken
as a whole, to include whitespace.

- If the search string starts with an asterisk, search only in headlines.
- If (possibly after the leading star) the search string starts with an
  exclamation mark, this also means to look at TODO entries only, an effect
  that can also be achieved with a prefix argument.
- If (possibly after star and exclamation mark) the search string starts
  with a colon, this will mean that the (non-regexp) snippets of the
  Boolean search must match as full words.

This command searches the agenda files, and in addition the files
listed in `org-agenda-text-search-extra-files' unless a restriction lock
is active.

(fn &optional TODO-ONLY STRING EDIT-AT)" t nil) (autoload 'org-todo-list "org-agenda" "Show all (not done) TODO entries from all agenda file in a single list.
The prefix arg can be used to select a specific TODO keyword and limit
the list to these.  When using `\\[universal-argument]', you will be prompted
for a keyword.  A numeric prefix directly selects the Nth keyword in
`org-todo-keywords-1'.

(fn &optional ARG)" t nil) (autoload 'org-tags-view "org-agenda" "Show all headlines for all `org-agenda-files' matching a TAGS criterion.
The prefix arg TODO-ONLY limits the search to TODO entries.

(fn &optional TODO-ONLY MATCH)" t nil) (autoload 'org-agenda-list-stuck-projects "org-agenda" "Create agenda view for projects that are stuck.
Stuck projects are project that have no next actions.  For the definitions
of what a project is and how to check if it stuck, customize the variable
`org-stuck-projects'.

(fn &rest IGNORE)" t nil) (autoload 'org-diary "org-agenda" "Return diary information from org files.
This function can be used in a \"sexp\" diary entry in the Emacs calendar.
It accesses org files and extracts information from those files to be
listed in the diary.  The function accepts arguments specifying what
items should be listed.  For a list of arguments allowed here, see the
variable `org-agenda-entry-types'.

The call in the diary file should look like this:

   &%%(org-diary) ~/path/to/some/orgfile.org

Use a separate line for each org file to check.  Or, if you omit the file name,
all files listed in `org-agenda-files' will be checked automatically:

   &%%(org-diary)

If you don't give any arguments (as in the example above), the default value
of `org-agenda-entry-types' is used: (:deadline :scheduled :timestamp :sexp).
So the example above may also be written as

   &%%(org-diary :deadline :timestamp :sexp :scheduled)

The function expects the lisp variables `entry' and `date' to be provided
by the caller, because this is how the calendar works.  Don't use this
function from a program - use `org-agenda-get-day-entries' instead.

(fn &rest ARGS)" nil nil) (autoload 'org-agenda-check-for-timestamp-as-reason-to-ignore-todo-item "org-agenda" "Do we have a reason to ignore this TODO entry because it has a time stamp?

(fn &optional END)" nil nil) (autoload 'org-agenda-set-restriction-lock "org-agenda" "Set restriction lock for agenda to current subtree or file.
When in a restricted subtree, remove it.

The restriction will span over the entire file if TYPE is `file',
or if type is '(4), or if the cursor is before the first headline
in the file. Otherwise, only apply the restriction to the current
subtree.

(fn &optional TYPE)" t nil) (autoload 'org-calendar-goto-agenda "org-agenda" "Compute the Org agenda for the calendar date displayed at the cursor.
This is a command that has to be installed in `calendar-mode-map'." t nil) (autoload 'org-agenda-to-appt "org-agenda" "Activate appointments found in `org-agenda-files'.

With a `\\[universal-argument]' prefix, refresh the list of appointments.

If FILTER is t, interactively prompt the user for a regular
expression, and filter out entries that don't match it.

If FILTER is a string, use this string as a regular expression
for filtering entries out.

If FILTER is a function, filter out entries against which
calling the function returns nil.  This function takes one
argument: an entry from `org-agenda-get-day-entries'.

FILTER can also be an alist with the car of each cell being
either `headline' or `category'.  For example:

  \\='((headline \"IMPORTANT\")
    (category \"Work\"))

will only add headlines containing IMPORTANT or headlines
belonging to the \"Work\" category.

ARGS are symbols indicating what kind of entries to consider.
By default `org-agenda-to-appt' will use :deadline*, :scheduled*
(i.e., deadlines and scheduled items with a hh:mm specification)
and :timestamp entries.  See the docstring of `org-diary' for
details and examples.

If an entry has a APPT_WARNTIME property, its value will be used
to override `appt-message-warning-time'.

(fn &optional REFRESH FILTER &rest ARGS)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-agenda" '("org-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-archive" '("org-a"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-attach" '("org-attach-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-attach-git" '("org-attach-git-"))) (autoload 'org-capture-string "org-capture" "Capture STRING with the template selected by KEYS.

(fn STRING &optional KEYS)" t nil) (autoload 'org-capture "org-capture" "Capture something.
\\<org-capture-mode-map>
This will let you select a template from `org-capture-templates', and
then file the newly captured information.  The text is immediately
inserted at the target location, and an indirect buffer is shown where
you can edit it.  Pressing `\\[org-capture-finalize]' brings you back to the previous
state of Emacs, so that you can continue your work.

When called interactively with a `\\[universal-argument]' prefix argument GOTO, don't
capture anything, just go to the file/headline where the selected
template stores its notes.

With a `\\[universal-argument] \\[universal-argument]' prefix argument, go to the last note stored.

When called with a `C-0' (zero) prefix, insert a template at point.

When called with a `C-1' (one) prefix, force prompting for a date when
a datetree entry is made.

ELisp programs can set KEYS to a string associated with a template
in `org-capture-templates'.  In this case, interactive selection
will be bypassed.

If `org-capture-use-agenda-date' is non-nil, capturing from the
agenda will use the date at point as the default date.  Then, a
`C-1' prefix will tell the capture process to use the HH:MM time
of the day at point (if any) or the current HH:MM time.

(fn &optional GOTO KEYS)" t nil) (autoload 'org-capture-import-remember-templates "org-capture" "Set `org-capture-templates' to be similar to `org-remember-templates'." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-capture" '("org-capture-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-clock" '("org-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-colview" '("org-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-compat" '("org-"))) (autoload 'org-encrypt-entry "org-crypt" "Encrypt the content of the current headline." t nil) (autoload 'org-decrypt-entry "org-crypt" "Decrypt the content of the current headline." t nil) (autoload 'org-encrypt-entries "org-crypt" "Encrypt all top-level entries in the current buffer." t nil) (autoload 'org-decrypt-entries "org-crypt" "Decrypt all entries in the current buffer." t nil) (autoload 'org-crypt-use-before-save-magic "org-crypt" "Add a hook to automatically encrypt entries before a file is saved to disk." nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-crypt" '("org-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-ctags" '("org-ctags-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-datetree" '("org-datetree-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-duration" '("org-duration-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-element" '("org-element-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-entities" '("org-entit"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-faces" '("org-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-feed" '("org-feed-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-footnote" '("org-footnote-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-goto" '("org-goto-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-habit" '("org-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-id" '("org-id-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-indent" '("org-indent-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-inlinetask" '("org-inlinetask-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-keys" '("org-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-lint" '("org-lint-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-list" '("org-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-macro" '("org-macro-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-macs" '("org-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-mobile" '("org-mobile-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-mouse" '("org-mouse-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-num" '("org-num-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-pcomplete" '("org-" "pcomplete/org-mode/"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-plot" '("org-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-protocol" '("org-protocol-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-refile" '("org-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-src" '("org-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-table" '("org"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-tempo" '("org-tempo-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-timer" '("org-timer-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox" '("org-export-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-ascii" '("org-ascii-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-beamer" '("org-beamer-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-html" '("org-html-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-icalendar" '("org-icalendar-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-latex" '("org-latex-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-man" '("org-man-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-md" '("org-md-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-odt" '("org-odt-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-org" '("org-org-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-publish" '("org-publish-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-texinfo" '("org-texinfo-"))) (provide 'org-autoloads)) "f" ((f f-autoloads) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "f" '("f-"))) (provide 'f-autoloads)) "emacsql" ((emacsql emacsql-compiler emacsql-autoloads) (autoload 'emacsql-show-last-sql "emacsql" "Display the compiled SQL of the s-expression SQL expression before point.
A prefix argument causes the SQL to be printed into the current buffer.

(fn &optional PREFIX)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emacsql" '("emacsql-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emacsql-compiler" '("emacsql-"))) (provide 'emacsql-autoloads)) "emacsql-sqlite3" ((emacsql-sqlite3 emacsql-sqlite3-autoloads) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emacsql-sqlite3" '("emacsql-sqlite3-"))) (provide 'emacsql-sqlite3-autoloads)) "org-roam" ((org-roam-graph org-roam-doctor org-roam-buffer org-roam-macs org-roam-dev org-roam org-roam-compat org-roam-faces org-roam-protocol org-roam-capture org-roam-dailies org-roam-completion org-roam-link org-roam-autoloads org-roam-db) (defvar org-roam-mode nil "Non-nil if Org-Roam mode is enabled.
See the `org-roam-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `org-roam-mode'.") (custom-autoload 'org-roam-mode "org-roam" nil) (autoload 'org-roam-mode "org-roam" "Minor mode for Org-roam.

This mode sets up several hooks, to ensure that the cache is updated on file
changes, renames and deletes. It is also in charge of graceful termination of
the database connection.

When called interactively, toggle `org-roam-mode'. with prefix
ARG, enable `org-roam-mode' if ARG is positive, otherwise disable
it.

When called from Lisp, enable `org-roam-mode' if ARG is omitted,
nil, or positive. If ARG is `toggle', toggle `org-roam-mode'.
Otherwise, behave as if called interactively.

(fn &optional ARG)" t nil) (defalias 'org-roam 'org-roam-buffer-toggle-display) (autoload 'org-roam-diagnostics "org-roam" "Collect and print info for `org-roam' issues." t nil) (autoload 'org-roam-find-file "org-roam" "Find and open an Org-roam file.
INITIAL-PROMPT is the initial title prompt.
COMPLETIONS is a list of completions to be used instead of
`org-roam--get-title-path-completions`.
FILTER-FN is the name of a function to apply on the candidates
which takes as its argument an alist of path-completions.  See
`org-roam--get-title-path-completions' for details.
If NO-CONFIRM, assume that the user does not want to modify the initial prompt.

(fn &optional INITIAL-PROMPT COMPLETIONS FILTER-FN NO-CONFIRM)" t nil) (autoload 'org-roam-find-directory "org-roam" "Find and open `org-roam-directory'." t nil) (autoload 'org-roam-find-ref "org-roam" "Find and open an Org-roam file from a ref.
ARG is used to forward interactive calls to
`org-roam--get-ref-path-completions'
FILTER can either be a string or a function:
- If it is a string, it should be the type of refs to include as
candidates (e.g. \"cite\" ,\"website\" ,etc.)
- If it is a function, it should be the name of a function that
takes three arguments: the type, the ref, and the file of the
current candidate.  It should return t if that candidate is to be
included as a candidate.

(fn ARG &optional FILTER)" t nil) (autoload 'org-roam-random-note "org-roam" "Find a random Org-roam file." t nil) (autoload 'org-roam-insert "org-roam" "Find an Org-roam file, and insert a relative org link to it at point.
Return selected file if it exists.
If LOWERCASE is non-nil, downcase the link description.
LINK-TYPE is the type of link to be created. It defaults to \"file\".
COMPLETIONS is a list of completions to be used instead of
`org-roam--get-title-path-completions`.
FILTER-FN is the name of a function to apply on the candidates
which takes as its argument an alist of path-completions.
If DESCRIPTION is provided, use this as the link label.  See
`org-roam--get-title-path-completions' for details.

(fn &optional LOWERCASE COMPLETIONS FILTER-FN DESCRIPTION LINK-TYPE)" t nil) (autoload 'org-roam-insert-immediate "org-roam" "Find an Org-roam file, and insert a relative org link to it at point.
This variant of `org-roam-insert' inserts the link immediately by
using the template in `org-roam-capture-immediate-template'. The
interactive ARG and ARGS are passed to `org-roam-insert'.
See `org-roam-insert' for details.

(fn ARG &rest ARGS)" t nil) (autoload 'org-roam-find-file-immediate "org-roam" "Find and open an Org-roam file.
This variant of `org-roam-find-file' uses the template in
`org-roam-capture-immediate-template', avoiding the capture
process. The interactive ARG and ARGS are passed to
`org-roam-find-file'. See `org-roam-find-file' for details.

(fn ARG &rest ARGS)" t nil) (autoload 'org-roam-jump-to-index "org-roam" "Find the index file in `org-roam-directory'.
The path to the index can be defined in `org-roam-index-file'.
Otherwise, the function will look in your `org-roam-directory'
for a note whose title is 'Index'.  If it does not exist, the
command will offer you to create one." t nil) (autoload 'org-roam-alias-add "org-roam" "Add an alias to Org-roam file.

Return added alias." t nil) (autoload 'org-roam-alias-delete "org-roam" "Delete an alias from Org-roam file." t nil) (autoload 'org-roam-switch-to-buffer "org-roam" "Switch to an existing Org-roam buffer." t nil) (autoload 'org-roam-version "org-roam" "Return `org-roam' version.
Interactively, or when MESSAGE is non-nil, show in the echo area.

(fn &optional MESSAGE)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam" '("org-roam-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-buffer" '("org-roam-buffer"))) (autoload 'org-roam-capture "org-roam-capture" "Launches an `org-capture' process for a new or existing note.
This uses the templates defined at `org-roam-capture-templates'.
Arguments GOTO and KEYS see `org-capture'.

(fn &optional GOTO KEYS)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-capture" '("org-roam-capture-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-completion" '("org-roam-completion-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-dailies" '("org-roam-dailies-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-db" '("org-roam-db"))) (autoload 'org-roam-dev-mode "org-roam-dev" "Minor mode for setting the dev environment of Org-roam.

If called interactively, enable Org-Roam-Dev mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (autoload 'org-roam-doctor "org-roam-doctor" "Perform a check on the current buffer to ensure cleanliness.
If CHECKALL, run the check for all Org-roam files.

(fn &optional CHECKALL)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-doctor" '("org-roam-doctor-"))) (autoload 'org-roam-graph "org-roam-graph" "Build and possibly display a graph for FILE from NODE-QUERY.
If FILE is nil, default to current buffer's file name.
ARG may be any of the following values:
  - nil       show the graph.
  - `\\[universal-argument]'     show the graph for FILE.
  - `\\[universal-argument]' N   show the graph for FILE limiting nodes to N steps.
  - `\\[universal-argument] \\[universal-argument]' build the graph.
  - `\\[universal-argument]' -   build the graph for FILE.
  - `\\[universal-argument]' -N  build the graph for FILE limiting nodes to N steps.

(fn &optional ARG FILE NODE-QUERY)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-graph" '("org-roam-graph-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-link" '("org-roam-link-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-macs" '("org-roam-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-protocol" '("org-roam-protocol-"))) (provide 'org-roam-autoloads)) "company" ((company-clang company-dabbrev company-autoloads company-yasnippet company-dabbrev-code company-capf company-tng company-semantic company-css company-template company-keywords company company-abbrev company-nxml company-cmake company-elisp company-bbdb company-etags company-files company-gtags company-ispell company-oddmuse company-tempo) (autoload 'company-mode "company" "\"complete anything\"; is an in-buffer completion framework.
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

(fn COMMAND &optional ARG &rest IGNORE)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-yasnippet" '("company-yasnippet-"))) (provide 'company-autoloads)) "company-org-roam" ((company-org-roam-autoloads company-org-roam) (autoload 'company-org-roam "company-org-roam" "Define a company backend for Org-roam.
COMMAND and ARG are as per the documentation of `company-backends'.

(fn COMMAND &optional ARG &rest _)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-org-roam" '("company-org-roam-"))) (provide 'company-org-roam-autoloads)) "parsebib" ((parsebib-autoloads parsebib) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "parsebib" '("parsebib-"))) (provide 'parsebib-autoloads)) "let-alist" ((let-alist-autoloads let-alist) (autoload 'let-alist "let-alist" "Let-bind dotted symbols to their cdrs in ALIST and execute BODY.
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

(fn ALIST &rest BODY)" nil t) (function-put 'let-alist 'lisp-indent-function '1) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "let-alist" '("let-alist--"))) (provide 'let-alist-autoloads)) "biblio-core" ((biblio-core biblio-core-autoloads) (autoload 'biblio-lookup "biblio-core" "Perform a search using BACKEND, and QUERY.
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

(fn DOI &optional CLEANUP)" t nil) (defalias 'dissemin-lookup 'biblio-dissemin-lookup) (autoload 'biblio-dissemin--register-action "biblio-dissemin" "Add Dissemin to list of `biblio-selection-mode' actions." nil nil) (add-hook 'biblio-selection-mode-hook #'biblio-dissemin--register-action) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "biblio-dissemin" '("biblio-dissemin--"))) (autoload 'doi-insert-bibtex "biblio-doi" "Insert BibTeX entry matching DOI.

(fn DOI)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "biblio-doi" '("biblio-doi-"))) (autoload 'biblio-download--register-action "biblio-download" "Add download to list of `biblio-selection-mode' actions." nil nil) (add-hook 'biblio-selection-mode-hook #'biblio-download--register-action) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "biblio-download" '("biblio-download-"))) (autoload 'biblio-hal-backend "biblio-hal" "A HAL backend for biblio.el.
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

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "bibtex-completion" '("bibtex-completion-"))) (provide 'bibtex-completion-autoloads)) "org-roam-bibtex" ((orb-compat org-roam-bibtex orb-anystyle orb-ivy orb-core org-roam-bibtex-autoloads orb-helm orb-pdf-scrapper orb-utils) (autoload 'orb-anystyle "orb-anystyle" "Run anystyle COMMAND with `shell-command'.
ARGS is a plist with the following recognized keys:

Anystyle CLI options
==========
1) EXEC :exec      => string (valid executable)
- default value can be set through `orb-anystyle-executable'

2) COMMAND :command   => symbol or string
- valid values: find parse help check license train

3) Global options can be passed with the following keys.

FMODEL    :finder-model => string (valid file path)
PMODEL    :parser-model => string (valid file path)
PDFINFO   :pdfinfo      => string (valid executable)
PDFTOTEXT :pdftotext    => string (valid executable)
ADAPTER   :adapter      => anything
STDOUT    :stdout       => boolean
HELP      :help         => boolean
VERBOSE   :verbose      => boolean
VERSION   :version      => boolean
OVERWRITE :overwrite    => boolean
FORMAT    :format       => string, symbol or list of unquoted symbols

- FORMAT must be one or more output formats accepted by anystyle commands:
  parse => bib csl json ref txt xml
  find  => bib csl json ref txt ttx xml
- string must be space- or comma-separated, additional spaces are
  ignored

Default values for some of these options can be set globally via
the following variables: `orb-anystyle-finder-model',
`orb-anystyle-parser-model', `orb-anystyle-pdfinfo-executable',
`orb-anystyle-pdftotext-executable'.

4) Command options can be passed with the following keys:

CROP   :crop         => integer or cons cell of integers
LAYOUT :layout       => boolean
SOLO   :solo         => boolean

- Command options are ignored for commands other than find
- anystyle help -c flag is not supported

Default values for these options can be set globally via the
following variables: `orb-anystyle-find-crop',
`orb-anystyle-find-layout', `orb-anystyle-find-solo'.

5) INPUT  :input   => string (file path)

6) OUTPUT :output  => string (file path)

`shell-command'-related options
==========

7) BUFFER :buffer  => buffer-or-name

- `shell-command''s OUTPUT-BUFFER
- can be a cons cell (OUTPUT-BUFFER . ERROR-BUFFER)
- when nil, defaults to `orb-anystyle-default-buffer'

anystyle CLI command synopsis:
anystyle [global options] command [command options] [arguments...].

Homepage: https://anystyle.io
Github: https://github.com/inukshuk/anystyle-cli
Courtesy of its authors.

(fn COMMAND &key (EXEC orb-anystyle-executable) VERBOSE HELP VERSION ADAPTER ((:finder-model FMODEL) orb-anystyle-finder-model) ((:parser-model PMODEL) orb-anystyle-parser-model) (PDFINFO orb-anystyle-pdfinfo-executable) (PDFTOTEXT orb-anystyle-pdftotext-executable) FORMAT STDOUT OVERWRITE (CROP orb-anystyle-find-crop) (SOLO orb-anystyle-find-solo) (LAYOUT orb-anystyle-find-layout) INPUT OUTPUT (BUFFER orb-anystyle-default-buffer))" nil nil) (function-put 'orb-anystyle 'lisp-indent-function '1) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "orb-anystyle" '("orb-anystyle-"))) (autoload 'orb-process-file-field "orb-core" "Process the 'file' BibTeX field and resolve if there are multiples.
Search the disk for the document associated with this BibTeX
entry.  The disk matching is based on looking in the
`bibtex-completion-library-path' for a file with the
CITEKEY.

If variable `orb-file-field-extensions' is non-nil, return only
the file paths with the respective extensions.

(Mendeley, Zotero, normal paths) are all supported.  If there
are multiple files found the user is prompted to select which one
to enter.

(fn CITEKEY)" nil nil) (autoload 'orb-autokey-generate-key "orb-core" "Generate citation key from ENTRY according to `orb-autokey-format'.
Return a string.  If optional CONTROL-STRING is non-nil, use it
instead of `orb-autokey-format'.

(fn ENTRY &optional CONTROL-STRING)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "orb-core" '("orb-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "orb-helm" '("helm-source-orb-insert" "orb-helm-insert"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "orb-ivy" '("orb-i"))) (autoload 'orb-pdf-scrapper-run "orb-pdf-scrapper" "Run Orb PDF Scrapper interactive process.
KEY is note's citation key.

(fn KEY)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "orb-pdf-scrapper" '("orb-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "orb-utils" '("orb-"))) (autoload 'orb-edit-notes "org-roam-bibtex" "Open an Org-roam note associated with the CITEKEY or create a new one.

This function allows to use Org-roam as a backend for managing
bibliography notes.  It relies on `bibtex-completion' to get
retrieve bibliographic information from a BibTeX file.

Implementation details and features:

1. This function first calls `org-roam-find-ref' trying to find
the note file associated with the CITEKEY.  The Org-roam key can
be set with '#+ROAM_KEY:' in-buffer keyword.

2. If the Org-roam reference has not been found, the function
calls `org-roam-find-file' passing to it the title associated
with the CITEKEY as retrieved by `bibtex-completion-get-entry'.
The prompt presented by `org-roam-find-file' will thus be
pre-populated with the record title.

3. The template used to create the note is stored in
`orb-templates'.  If the variable is not defined, revert to using
`org-roam-capture-templates'.  In the former case, a new file
will be created and filled according to the template, possibly
preformatted (see below) without additional user interaction.  In
the latter case, an interactive `org-capture' process will be
run.

4. Optionally, when `orb-preformat-templates' is non-nil, any
prompt wildcards in `orb-templates' or
`org-roam-capture-templates', associated with the bibtex record
fields as specified in `orb-preformat-templates', will be
preformatted.  Both `org-capture-templates' (%^{}) and
`org-roam-capture-templates' (`s-format', ${}) prompt syntaxes
are supported.

See `orb-preformat-keywords' for more details on how
to properly specify prompts for replacement.

Please pay attention when using this feature that by setting
title for preformatting, it will be impossible to change it in
the `org-roam-find-file' interactive prompt since all the
template expansions will have taken place by then.  All the title
wildcards will be replace with the BibTeX field value.

5. Optionally, if you are using Projectile and Persp-mode and
have a dedicated workspace to work with your Org-roam collection,
you may want to set the perspective name and project path in
`orb-persp-project' and `orb-switch-persp' to t.  In this case,
the perspective will be switched to the Org-roam notes project
before calling any Org-roam functions.

(fn CITEKEY)" nil nil) (autoload 'orb-insert "org-roam-bibtex" "Insert a link to an Org-roam bibliography note.
If the note does not exist, create it.
Use candidate selection interface specified in
`orb-insert-interface'.  Available interfaces are `helm-bibtex',
`ivy-bibtex' and `orb-insert-generic'.

When using `helm-bibtex' or `ivy-bibtex', the action \"Edit note
& insert a link\" should be chosen to insert the desired link.
For convenience, this action is made default for the duration of
an `orb-insert' session.  It will not persist when `helm-bibtex'
or `ivy-bibtex' proper are run.  It is absolutely possible to run
other `helm-bibtex' or `ivy-bibtex' actions.  When action another
than \"Edit note & insert a link\" is run, no link will be
inserted, although the session can be resumed later with
`helm-resulme' or `ivy-resume', respectively, to select the
\"Edit note & insert a link\" action.

When using `orb-insert-generic' as interface, a simple list of
available citation keys is presented using `completion-read' and
after choosing a candidate the appropriate link will be inserted.

If the note does not exist yet, it will be created using
`orb-edit-notes' function.

\\<universal-argument-map>\\<org-roam-bibtex-mode-map> The
customization option `orb-insert-link-description' determines
what will be used as the link's description.  It is possible to
override the default value with numerical prefix ARG:

`C-1' \\[orb-insert] will force `title'
`C-2' \\[orb-insert] will force `citekey'
`C-0' \\[orb-insert] will force `citation'

If a region of text is active (selected) when calling `orb-insert',
the text in the region will be replaced with the link and the
text string will be used as the link's description \342\200\224 similar to
`org-roam-insert'.

Normally, the case of the link description will be preserved.  It
is possible to force lowercase by supplying either one or three
universal arguments `\\[universal-argument]'.

Finally, `bibtex-completion-cache' will be re-populated if either
two or three universal arguments `\\[universal-argument]' are supplied.

(fn &optional ARG)" t nil) (autoload 'orb-find-non-ref-file "org-roam-bibtex" "Find and open an Org-roam, non-ref file.
INITIAL-PROMPT is the initial title prompt.
See `org-roam-find-files' and
`orb--get-non-ref-path-completions' for details.

(fn &optional INITIAL-PROMPT)" t nil) (autoload 'orb-insert-non-ref "org-roam-bibtex" "Find a non-ref Org-roam file, and insert a relative org link to it at point.
If PREFIX, downcase the title before insertion.  See
`org-roam-insert' and `orb--get-non-ref-path-completions' for
details.

(fn PREFIX)" t nil) (autoload 'orb-note-actions "org-roam-bibtex" "Run an interactive prompt to offer note-related actions.
The prompt interface can be set in `orb-note-actions-interface'.
In addition to default actions, which are not supposed to be
modified, there is a number of prefined extra actions
`orb-note-actions-extra' that can be customized.  Additionally,
user actions can be set in `orb-note-actions-user'." t nil) (defvar org-roam-bibtex-mode nil "Non-nil if Org-Roam-Bibtex mode is enabled.
See the `org-roam-bibtex-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `org-roam-bibtex-mode'.") (custom-autoload 'org-roam-bibtex-mode "org-roam-bibtex" nil) (autoload 'org-roam-bibtex-mode "org-roam-bibtex" "Sets `orb-edit-notes' as a function for editing bibliography notes.
Affects Org-ref and Helm-bibtex/Ivy-bibtex.

When called interactively, toggle `org-roam-bibtex-mode'. with
prefix ARG, enable `org-roam-bibtex-mode' if ARG is positive,
otherwise disable it.

When called from Lisp, enable `org-roam-mode' if ARG is omitted,
nil, or positive.  If ARG is `toggle', toggle `org-roam-mode'.
Otherwise, behave as if called interactively.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-bibtex" '("orb-" "org-roam-bibtex-mode-map" "with-orb-cleanup"))) (provide 'org-roam-bibtex-autoloads)) "org-journal" ((org-journal-autoloads org-journal) (add-hook 'calendar-today-visible-hook 'org-journal-mark-entries) (add-hook 'calendar-today-invisible-hook 'org-journal-mark-entries) (autoload 'org-journal-mode "org-journal" "Mode for writing or viewing entries written in the journal.

(fn)" t nil) (define-obsolete-function-alias 'org-journal-open-next-entry 'org-journal-next-entry "2.1.0") (define-obsolete-function-alias 'org-journal-open-previous-entry 'org-journal-previous-entry "2.1.0") (autoload 'org-journal-convert-created-property-timestamps "org-journal" "Convert CREATED property timestamps to `org-journal-created-property-timestamp-format'.

(fn OLD-FORMAT)" t nil) (autoload 'org-journal-new-entry "org-journal" "Open today's journal file and start a new entry.

With a PREFIX arg, open the today's file, create a heading if it doesn't exist yet,
but do not create a new entry.

If given a TIME, create an entry for the time's day. If no TIME was given,
use the current time (which is interpreted as belonging to yesterday if
smaller than `org-extend-today-until`).

Whenever a journal entry is created the `org-journal-after-entry-create-hook'
hook is run.

(fn PREFIX &optional TIME)" t nil) (autoload 'org-journal-new-date-entry "org-journal" "Open the journal for the date indicated by point and start a new entry.

If the date is not today, it won't be given a time heading. With one prefix (C-u),
don't add a new heading.

If the date is in the future, create a schedule entry, unless two universal prefix
arguments (C-u C-u) are given. In that case insert just the heading.

(fn PREFIX &optional EVENT)" t nil) (autoload 'org-journal-new-scheduled-entry "org-journal" "Create a new entry in the future with an active timestamp.

With non-nil prefix argument create a regular entry instead of a TODO entry.

(fn PREFIX &optional SCHEDULED-TIME)" t nil) (autoload 'org-journal-reschedule-scheduled-entry "org-journal" "Reschedule an entry in the future.

(fn &optional TIME)" t nil) (autoload 'org-journal-open-current-journal-file "org-journal" "Open the current journal file" t nil) (autoload 'org-journal-invalidate-cache "org-journal" "Clear `org-journal--dates' hash table, and the cache file." t nil) (autoload 'org-journal-mark-entries "org-journal" "Mark days in the calendar for which a journal entry is present." t nil) (autoload 'org-journal-read-entry "org-journal" "Open journal entry for selected date for viewing.

(fn ARG &optional EVENT)" t nil) (autoload 'org-journal-display-entry "org-journal" "Display journal entry for selected date in another window.

(fn ARG &optional EVENT)" t nil) (autoload 'org-journal-read-or-display-entry "org-journal" "Read an entry for the TIME and either select the new window when NOSELECT
is nil or avoid switching when NOSELECT is non-nil.

(fn TIME &optional NOSELECT)" nil nil) (autoload 'org-journal-next-entry "org-journal" "Go to the next journal entry." t nil) (autoload 'org-journal-previous-entry "org-journal" "Go to the previous journal entry." t nil) (autoload 'org-journal-search "org-journal" "Search for a string in the journal files.

See `org-read-date' for information on ways to specify dates.
If a prefix argument is given, search all dates.

(fn STR &optional PERIOD-NAME)" t nil) (autoload 'org-journal-search-calendar-week "org-journal" "Search for a string within a current calendar-mode week entries.

(fn STR)" t nil) (autoload 'org-journal-search-calendar-month "org-journal" "Search for a string within a current calendar-mode month entries.

(fn STR)" t nil) (autoload 'org-journal-search-calendar-year "org-journal" "Search for a string within a current calendar-mode year entries.

(fn STR)" t nil) (autoload 'org-journal-search-forever "org-journal" "Search for a string within all entries.

(fn STR)" t nil) (autoload 'org-journal-search-future "org-journal" "Search for a string within all future entries.

(fn STR)" t nil) (autoload 'org-journal-search-future-scheduled "org-journal" "Search for TODOs within all future entries." t nil) (add-hook 'org-journal-mode-hook (lambda nil (add-hook org-journal-encrypt-on 'org-journal-encryption-hook nil t))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-journal" '("org-journal-"))) (provide 'org-journal-autoloads)) "org-bullets" ((org-bullets-autoloads org-bullets) (autoload 'org-bullets-mode "org-bullets" "UTF8 Bullets for org-mode

If called interactively, enable Org-Bullets mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-bullets" '("org-bullets-"))) (provide 'org-bullets-autoloads)) "htmlize" ((htmlize-autoloads htmlize) (autoload 'htmlize-buffer "htmlize" "Convert BUFFER to HTML, preserving colors and decorations.

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

(fn ARG &optional TARGET-DIRECTORY)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "htmlize" '("htmlize-"))) (provide 'htmlize-autoloads)) "async" ((dired-async async-autoloads smtpmail-async async async-pkg async-bytecomp) (autoload 'async-start-process "async" "Start the executable PROGRAM asynchronously named NAME.  See `async-start'.
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
     'ignore)

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

(fn FILE)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "async-bytecomp" '("async-byte"))) (defvar dired-async-mode nil "Non-nil if Dired-Async mode is enabled.
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

NOTE: SUBKEY and OTHER-SUBKEYS bindings support only char syntax
and vectors, so don't use strings to define them.

(fn MAP KEY SUBKEY COMMAND &optional OTHER-SUBKEYS PROMPT EXIT-FN)" nil nil) (function-put 'helm-define-key-with-subkeys 'lisp-indent-function '1) (autoload 'helm-debug-open-last-log "helm" "Open Helm log file or buffer of last Helm session." t nil) (autoload 'helm "helm" "Main function to execute helm sources.

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
in Emacs < to 25.1.50.1 (See Emacs Bug#24432).

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

- COLLECTION can be a list, vector, obarray or hash-table.
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

- ALISTP: (default is non--nil) See `helm-comp-read-get-candidates'.

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

(fn URL &optional IGNORE)" t nil) (autoload 'helm-browse-url-next "helm-net" "Browse URL with next browser.

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

With a prefix ARG the cache is invalidated and the bibliography
reread.

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

(fn KEYMAP KEYS COMMAND)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "key-chord" '("key-chord-"))) (provide 'key-chord-autoloads)) "tablist" ((tablist tablist-autoloads tablist-filter) (autoload 'tablist-minor-mode "tablist" "Toggle Tablist minor mode on or off.

If called interactively, enable Tablist minor mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\\{tablist-minor-mode-map}

(fn &optional ARG)" t nil) (autoload 'tablist-mode "tablist" "

(fn)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "tablist" '("tablist-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "tablist-filter" '("tablist-filter-"))) (provide 'tablist-autoloads)) "pdf-tools" ((pdf-tools pdf-virtual pdf-annot pdf-sync pdf-view pdf-loader pdf-util pdf-misc pdf-cache pdf-dev pdf-history pdf-links pdf-info pdf-outline pdf-tools-autoloads pdf-isearch pdf-occur) (autoload 'pdf-annot-minor-mode "pdf-annot" "Support for PDF Annotations.

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

(fn &optional NO-QUERY-P SKIP-DEPENDENCIES-P NO-ERROR-P FORCE-DEPENDENCIES-P)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pdf-loader" '("pdf-loader--"))) (autoload 'pdf-misc-minor-mode "pdf-misc" "FIXME:  Not documented.

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

(fn &optional FILENAMES BUFFER-NAME DISPLAY-P)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pdf-virtual" '("pdf-virtual-"))) (provide 'pdf-tools-autoloads)) "org-ref" ((nist-webbook org-ref-reftex org-ref-isbn org-ref-autoloads doi-utils org-ref-ivy-cite org-ref-scifinder org-ref-helm-cite org-ref-bibtex org-ref org-ref-latex org-ref-url-utils org-ref-scopus org-ref-helm org-ref-helm-bibtex x2bib org-ref-pgk org-ref-pubmed org-ref-citeproc org-ref-ivy org-ref-worldcat org-ref-core org-ref-utils org-ref-sci-id org-ref-glossary org-ref-arxiv org-ref-pdf org-ref-wos) (autoload 'doi-utils-get-bibtex-entry-pdf "doi-utils" "Download pdf for entry at point if the pdf does not already exist locally.
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

(fn PMID)" t nil) (autoload 'pubmed-get-medline-xml "org-ref-pubmed" "Get MEDLINE xml for PMID as a string.

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

(fn MEDFILE &optional VERBOSE)" t nil) (autoload 'clean-entries "x2bib" "Map over bibtex entries and clean them." t nil) (provide 'org-ref-autoloads)) "org-noter" ((org-noter org-noter-autoloads) (autoload 'org-noter "org-noter" "Start `org-noter' session.

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

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-noter" '("org-noter-"))) (provide 'org-noter-autoloads)) "org-download" ((org-download-autoloads org-download) (autoload 'org-download-enable "org-download" "Enable org-download." nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-download" '("org-download-"))) (provide 'org-download-autoloads)) "request" ((request-autoloads request) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "request" '("request-"))) (provide 'request-autoloads)) "deferred" ((deferred-autoloads deferred) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "deferred" '("deferred:"))) (provide 'deferred-autoloads)) "request-deferred" ((request-deferred request-deferred-autoloads) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "request-deferred" '("request-deferred"))) (provide 'request-deferred-autoloads)) "persist" ((persist persist-autoloads) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "persist" '("persist-"))) (provide 'persist-autoloads)) "org-gcal" ((org-gcal org-gcal-autoloads) (autoload 'org-gcal-sync "org-gcal" "Import events from calendars.
Export the ones to the calendar if unless
SKIP-EXPORT.  Set SILENT to non-nil to inhibit notifications.

(fn &optional SKIP-EXPORT SILENT)" t nil) (autoload 'org-gcal-fetch "org-gcal" "Fetch event data from google calendar." t nil) (autoload 'org-gcal-sync-buffer "org-gcal" "Sync entries containing Calendar events in the currently-visible portion of the
buffer.

Updates events on the server unless SKIP-EXPORT is set. In this case, events
modified on the server will overwrite entries in the buffer.
Set SILENT to non-nil to inhibit notifications.

(fn &optional SKIP-EXPORT SILENT)" t nil) (autoload 'org-gcal-fetch-buffer "org-gcal" "Fetch changes to events in the currently-visible portion of the buffer, not
writing any changes to Calendar.

(fn &optional SKIP-EXPORT SILENT)" t nil) (autoload 'org-gcal-post-at-point "org-gcal" "Post entry at point to current calendar. This overwrites the event on the
server with the data from the entry, except if the \342\200\230org-gcal-etag-property\342\200\231 is
present and is out of sync with the server, in which case the entry is
overwritten with data from the server instead.

If SKIP-IMPORT is not nil, don\342\200\231t overwrite the entry with data from the server.
If SKIP-EXPORT is not nil, don\342\200\231t overwrite the event on the server.

(fn &optional SKIP-IMPORT SKIP-EXPORT)" t nil) (autoload 'org-gcal-delete-at-point "org-gcal" "Delete entry at point to current calendar." t nil) (autoload 'org-gcal-sync-tokens-clear "org-gcal" "Clear all Calendar API sync tokens.

Use this to force retrieving all events in \342\200\230org-gcal-sync\342\200\231 or
\342\200\230org-gcal-fetch\342\200\231." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-gcal" '("org-gcal-"))) (provide 'org-gcal-autoloads)) "xml-rpc" ((xml-rpc xml-rpc-autoloads) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "xml-rpc" '("xml-"))) (provide 'xml-rpc-autoloads)) "metaweblog" ((metaweblog metaweblog-autoloads) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "metaweblog" '("metaweblog-"))) (provide 'metaweblog-autoloads)) "org2blog" ((ox-wp org2blog-test-system org2blog org2blog-pkg org2blog-autoloads) (autoload 'org2blog/wp-mode "org2blog" "Toggle org2blog/wp mode.
With no argument, the mode is toggled on/off.
Non-nil argument turns mode on.
Nil argument turns mode off.

Commands:
\\{org2blog/wp-entry-mode-map}

Entry to this mode calls the value of `org2blog/wp-mode-hook'.

(fn &optional ARG)" t nil) (autoload 'org2blog/wp-login "org2blog" "Logs into the blog. Initializes the internal data structures.

(fn)" t nil) (autoload 'org2blog/wp-new-entry "org2blog" "Creates a new blog entry.

(fn)" t nil) (autoload 'org2blog/wp-post-subtree "org2blog" "Post the current entry as a draft. Publish if PUBLISH is non-nil.

(fn &optional PUBLISH)" t nil) (autoload 'org2blog/wp-track-buffer "org2blog" "Save details of current buffer in the tracking file.

(fn)" t nil) (autoload 'org2blog/wp-track-subtree "org2blog" "Save details of current subtree in the tracking file.

(fn)" t nil) (autoload 'org2blog/wp-preview-buffer-post "org2blog" "Preview the present buffer in browser, if posted.

(fn)" t nil) (autoload 'org2blog/wp-preview-subtree-post "org2blog" "Preview the present subtree in browser, if posted.

(fn)" t nil) (provide 'org2blog-autoloads)) "all-the-icons" ((all-the-icons-faces all-the-icons-autoloads all-the-icons) (autoload 'all-the-icons-icon-for-dir "all-the-icons" "Get the formatted icon for DIR.
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

(fn &optional ARG FAMILY)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "all-the-icons" '("all-the-icons-"))) (provide 'all-the-icons-autoloads)) "doom-themes" ((doom-tomorrow-day-theme doom-laserwave-theme doom-monokai-pro-theme doom-themes doom-vibrant-theme doom-molokai-theme doom-gruvbox-theme doom-opera-light-theme doom-solarized-light-theme doom-moonlight-theme doom-themes-ext-org doom-one-light-theme doom-dracula-theme doom-sourcerer-theme doom-henna-theme doom-monokai-classic-theme doom-tomorrow-night-theme doom-acario-dark-theme doom-material-theme doom-outrun-electric-theme doom-peacock-theme doom-manegarm-theme doom-themes-autoloads doom-gruvbox-light-theme doom-fairy-floss-theme doom-city-lights-theme doom-monokai-spectrum-theme doom-oceanic-next-theme doom-palenight-theme doom-spacegrey-theme doom-wilmersdorf-theme doom-horizon-theme doom-themes-ext-treemacs doom-themes-base doom-rouge-theme doom-nord-theme doom-challenger-deep-theme doom-plain-theme doom-flatwhite-theme doom-Iosvkem-theme doom-themes-ext-visual-bell doom-themes-ext-neotree doom-one-theme doom-nova-theme doom-zenburn-theme doom-dark+-theme doom-acario-light-theme doom-ephemeral-theme doom-miramare-theme doom-opera-theme doom-nord-light-theme doom-snazzy-theme doom-old-hope-theme doom-solarized-dark-theme) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-Iosvkem-theme" '("doom-Iosvkem"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-acario-dark-theme" '("doom-acario-dark"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-acario-light-theme" '("doom-acario-light"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-challenger-deep-theme" '("doom-challenger-deep"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-city-lights-theme" '("doom-city-lights"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-dark+-theme" '("doom-dark+"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-dracula-theme" '("doom-dracula"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-ephemeral-theme" '("doom-ephemeral"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-fairy-floss-theme" '("doom-fairy-floss"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-flatwhite-theme" '("doom-f"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-gruvbox-light-theme" '("doom-gruvbox-light"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-gruvbox-theme" '("doom-gruvbox"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-henna-theme" '("doom-henna"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-horizon-theme" '("doom-horizon"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-laserwave-theme" '("doom-laserwave"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-manegarm-theme" '("doom-manegarm"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-material-theme" '("doom-material"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-miramare-theme" '("doom-miramare"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-molokai-theme" '("doom-molokai"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-monokai-classic-theme" '("doom-monokai-classic"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-monokai-pro-theme" '("doom-monokai-pro"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-monokai-spectrum-theme" '("doom-monokai-spectrum"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-moonlight-theme" '("doom-moonlight"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-nord-light-theme" '("doom-nord-light"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-nord-theme" '("doom-nord"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-nova-theme" '("doom-nova"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-oceanic-next-theme" '("doom-oceanic-next"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-old-hope-theme" '("doom-old-hope"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-one-light-theme" '("doom-one-light"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-one-theme" '("doom-one"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-opera-light-theme" '("doom-opera-light"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-opera-theme" '("doom-opera"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-outrun-electric-theme" '("doom-outrun-electric"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-palenight-theme" '("doom-palenight"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-peacock-theme" '("doom-peacock"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-plain-theme" '("doom-plain"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-rouge-theme" '("doom-rouge"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-snazzy-theme" '("doom-snazzy"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-solarized-dark-theme" '("doom-solarized-dark"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-solarized-light-theme" '("doom-solarized-light"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-sourcerer-theme" '("doom-sourcerer"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-spacegrey-theme" '("doom-spacegrey"))) (autoload 'doom-name-to-rgb "doom-themes" "Retrieves the hexidecimal string repesented the named COLOR (e.g. \"red\")
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

Includes an Atom-esque icon theme and highlighting based on filetype." nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-themes-ext-neotree" '("doom-"))) (autoload 'doom-themes-org-config "doom-themes-ext-org" "Enable custom fontification & improves theme integration with org-mode." nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-themes-ext-org" '("doom-"))) (autoload 'doom-themes-treemacs-config "doom-themes-ext-treemacs" "Install doom-themes' treemacs configuration.

Includes an Atom-esque icon theme and highlighting based on filetype." nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-themes-ext-treemacs" '("doom-themes-"))) (autoload 'doom-themes-visual-bell-fn "doom-themes-ext-visual-bell" "Blink the mode-line red briefly. Set `ring-bell-function' to this to use it." nil nil) (autoload 'doom-themes-visual-bell-config "doom-themes-ext-visual-bell" "Enable flashing the mode-line on error." nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-tomorrow-day-theme" '("doom-tomorrow-day"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-tomorrow-night-theme" '("doom-tomorrow-night"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-vibrant-theme" '("doom-vibrant"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-wilmersdorf-theme" '("doom-wilmersdorf"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "doom-zenburn-theme" '("doom-zenburn"))) (provide 'doom-themes-autoloads)) "cycle-themes" ((cycle-themes-autoloads cycle-themes) (defvar cycle-themes-mode nil "Non-nil if Cycle-Themes mode is enabled.
See the `cycle-themes-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `cycle-themes-mode'.") (custom-autoload 'cycle-themes-mode "cycle-themes" nil) (autoload 'cycle-themes-mode "cycle-themes" "Minor mode for cycling between themes.

If called interactively, enable Cycle-Themes mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "cycle-themes" '("cycle-themes"))) (provide 'cycle-themes-autoloads)) "mini-modeline" ((mini-modeline-autoloads mini-modeline) (defvar mini-modeline-mode nil "Non-nil if Mini-Modeline mode is enabled.
See the `mini-modeline-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `mini-modeline-mode'.") (custom-autoload 'mini-modeline-mode "mini-modeline" nil) (autoload 'mini-modeline-mode "mini-modeline" "Enable modeline in minibuffer.

If called interactively, enable Mini-Modeline mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mini-modeline" '("mini-modeline-"))) (provide 'mini-modeline-autoloads)) "rainbow-delimiters" ((rainbow-delimiters rainbow-delimiters-autoloads) (autoload 'rainbow-delimiters-mode "rainbow-delimiters" "Highlight nested parentheses, brackets, and braces according to their depth.

If called interactively, enable Rainbow-Delimiters mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (autoload 'rainbow-delimiters-mode-enable "rainbow-delimiters" "Enable `rainbow-delimiters-mode'." nil nil) (autoload 'rainbow-delimiters-mode-disable "rainbow-delimiters" "Disable `rainbow-delimiters-mode'." nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rainbow-delimiters" '("rainbow-delimiters-"))) (provide 'rainbow-delimiters-autoloads)) "smartparens" ((smartparens-javascript smartparens-scala smartparens-autoloads smartparens-ml smartparens-haskell smartparens-crystal smartparens-ruby smartparens smartparens-elixir smartparens-ess smartparens-html smartparens-text sp-sublimetext-like smartparens-org smartparens-racket smartparens-latex smartparens-python smartparens-markdown smartparens-config smartparens-pkg smartparens-c smartparens-rst smartparens-clojure smartparens-lua smartparens-rust) (autoload 'sp-cheat-sheet "smartparens" "Generate a cheat sheet of all the smartparens interactive functions.

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

(fn &optional ARG)" t nil) (autoload 'turn-on-show-smartparens-mode "smartparens" "Turn on `show-smartparens-mode'." t nil) (autoload 'turn-off-show-smartparens-mode "smartparens" "Turn off `show-smartparens-mode'." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens" '("smartparens-" "sp-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-clojure" '("sp-clojure-prefix"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-config" '("sp-lisp-invalid-hyperlink-p"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-crystal" '("sp-crystal-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-elixir" '("sp-elixir-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-ess" '("sp-ess-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-haskell" '("sp-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-html" '("sp-html-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-latex" '("sp-latex-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-lua" '("sp-lua-post-keyword-insert"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-markdown" '("sp-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-org" '("sp-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-python" '("sp-python-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-rst" '("sp-rst-point-after-backtick"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-ruby" '("sp-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-rust" '("sp-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-scala" '("sp-scala-wrap-with-indented-newlines"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-text" '("sp-text-mode-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sp-sublimetext-like" '("sp-point-not-before-word"))) (provide 'smartparens-autoloads)) "dash-functional" ((dash-functional dash-functional-autoloads) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dash-functional" '("-a" "-c" "-f" "-iteratefn" "-juxt" "-not" "-o" "-prodfn" "-rpartial"))) (provide 'dash-functional-autoloads)) "frame-local" ((frame-local-autoloads frame-local) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "frame-local" '("frame-local-"))) (provide 'frame-local-autoloads)) "company-box" ((company-box company-box-doc company-box-autoloads company-box-icons) (autoload 'company-box-mode "company-box" "Company-box minor mode.

If called interactively, enable Company-Box mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-box" '("company-box-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-box-doc" '("company-box-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-box-icons" '("company-box-icons-"))) (provide 'company-box-autoloads)) "guess-language" ((guess-language guess-language-autoloads) (autoload 'guess-language-mode "guess-language" "Toggle guess-language mode.

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

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "guess-language" '("guess-language"))) (provide 'guess-language-autoloads)) "yasnippet" ((yasnippet-autoloads yasnippet) (autoload 'yas-minor-mode "yasnippet" "Toggle YASnippet mode.

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

(fn &optional ARG)" t nil) (autoload 'snippet-mode "yasnippet" "A mode for editing yasnippets" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "yasnippet" '("help-snippet-def" "snippet-mode-map" "yas"))) (provide 'yasnippet-autoloads)) "focus" ((focus-autoloads focus) (autoload 'focus-mode "focus" "Dim the font color of text in surrounding sections.

If called interactively, enable Focus mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (autoload 'focus-read-only-mode "focus" "A read-only mode optimized for `focus-mode'.

If called interactively, enable Focus-Read-Only mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "focus" '("focus-"))) (provide 'focus-autoloads)) "ivy-bibtex" ((ivy-bibtex-autoloads ivy-bibtex) (autoload 'ivy-bibtex "ivy-bibtex" "Search BibTeX entries using ivy.

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

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ivy-bibtex" '("ivy-bibtex-"))) (provide 'ivy-bibtex-autoloads)) "interleave" ((interleave-autoloads interleave) (define-obsolete-variable-alias 'interleave--pdf-current-page-fn 'interleave-pdf-current-page-fn "1.3.0") (define-obsolete-variable-alias 'interleave--pdf-next-page-fn 'interleave-pdf-next-page-fn "1.3.0") (define-obsolete-variable-alias 'interleave--pdf-previous-page-fn 'interleave-pdf-previous-page-fn "1.3.0") (define-obsolete-variable-alias 'interleave--pdf-goto-page-fn 'interleave-pdf-goto-page-fn "1.3.0") (define-obsolete-variable-alias 'interleave--pdf-scroll-up-or-next-page-fn 'interleave-pdf-scroll-up-or-next-page-fn "1.3.0") (define-obsolete-variable-alias 'interleave--pdf-scroll-down-or-previous-page-fn 'interleave-pdf-scroll-down-or-previous-page-fn "1.3.0") (define-obsolete-function-alias 'interleave--open-notes-file-for-pdf 'interleave-open-notes-file-for-pdf "1.3.0") (autoload 'interleave-open-notes-file-for-pdf "interleave" "Open the notes org file for the current pdf file if it exists.
Else create it.

It is assumed that the notes org file will have the exact same base name
as the pdf file (just that the notes file will have a .org extension instead
of .pdf)." t nil) (define-obsolete-variable-alias 'interleave 'interleave-mode "1.3.0") (define-obsolete-variable-alias 'interleave-hook 'interleave-mode-hook "1.3.0") (define-obsolete-function-alias 'interleave 'interleave-mode "1.3.0") (autoload 'interleave-mode "interleave" "Interleaving your text books since 2015.

If called interactively, enable Interleave mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

In the past, textbooks were sometimes published as 'interleaved' editions.
That meant, each page was followed by a blank page and the ambitious student/
scholar had the ability to take their notes directly in their copy of the
textbook. Newton and Kant were prominent representatives of this technique.

Nowadays textbooks (or lecture material) come in PDF format. Although almost
every PDF Reader has the ability to add some notes to the PDF itself, it is
not as powerful as it could be.

This is what this minor mode tries to accomplish. It presents your PDF side by
side to an [[http://orgmode.org][Org Mode]] buffer with your notes, narrowing
down to just those passages that are relevant to the particular page in the
document viewer.

Usage:

- Create a Org file that will keep your notes. In the Org headers section, add
#+INTERLEAVE_PDF: /the/path/to/your/pdf.pdf
- Start `interleave-mode' with `M-x interleave-mode'.
- To insert a note for a page, type `i'.
- Navigation is the same as in `doc-view-mode'/`pdf-view-mode'.

The split direction is determined by the customizable variable
`interleave-split-direction'. When `interleave-mode' is invoked
with a prefix argument the inverse split direction is used
e.g. if `interleave-split-direction' is 'vertical the buffer is
split horizontally.

Keybindings (`doc-view-mode'/`pdf-view-mode'):

\\{interleave-pdf-mode-map}

Keybindings (org-mode buffer):

\\{interleave-map}

(fn &optional ARG)" t nil) (autoload 'interleave-pdf-mode "interleave" "Interleave view for the pdf.

If called interactively, enable Interleave-Pdf mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "interleave" '("interleave-"))) (provide 'interleave-autoloads)) "esxml" ((esxml-autoloads esxml-query esxml-pkg esxml) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "esxml" '("attr" "esxml-" "pp-esxml-to-xml" "string-trim-whitespace" "sxml-to-" "xml-to-esxml"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "esxml-query" '("esxml-"))) (provide 'esxml-autoloads)) "solaire-mode" ((solaire-mode solaire-mode-autoloads) (autoload 'solaire-mode "solaire-mode" "Make source buffers grossly incandescent by remapping common faces (see
`solaire-mode-remap-alist') to their solaire-mode variants.

If called interactively, enable Solaire mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (put 'solaire-global-mode 'globalized-minor-mode t) (defvar solaire-global-mode nil "Non-nil if Solaire-Global mode is enabled.
See the `solaire-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `solaire-global-mode'.") (custom-autoload 'solaire-global-mode "solaire-mode" nil) (autoload 'solaire-global-mode "solaire-mode" "Toggle Solaire mode in all buffers.
With prefix ARG, enable Solaire-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Solaire mode is enabled in all buffers where
`turn-on-solaire-mode' would do it.
See `solaire-mode' for more information on Solaire mode.

(fn &optional ARG)" t nil) (autoload 'turn-on-solaire-mode "solaire-mode" "Conditionally enable `solaire-mode' in the current buffer.

Does nothing if the current buffer doesn't satisfy the function in
`solaire-mode-real-buffer-fn'." t nil) (autoload 'turn-off-solaire-mode "solaire-mode" "Disable `solaire-mode' in the current buffer." t nil) (autoload 'solaire-mode-in-minibuffer "solaire-mode" "Highlight the minibuffer whenever it is active." nil nil) (autoload 'solaire-mode-reset "solaire-mode" "Reset all buffers with `solaire-mode' enabled.

The purpose for this is to reset faces that cannot be buffer-local such as the
fringe, which can be changed by loading a new theme or opening an Emacs client
frame with a different display (via emacsclient).

(fn &rest _)" t nil) (advice-add #'load-theme :before (lambda (theme &optional _ no-enable) (unless no-enable (setq solaire-mode--current-theme theme)))) (advice-add #'load-theme :after (lambda (theme &rest _) (when (memq theme custom-enabled-themes) (setq solaire-mode--bg-swapped nil) (when (featurep 'solaire-mode) (solaire-mode--swap-bg-faces-maybe))))) (autoload 'solaire-mode-swap-bg "solaire-mode" "Does nothing. Set `solaire-mode-auto-swap-bg' instead." nil nil) (make-obsolete 'solaire-mode-swap-bg 'solaire-mode-auto-swap-bg '"1.1.4") (autoload 'solaire-mode-restore-persp-mode-buffers "solaire-mode" "Restore `solaire-mode' in buffers when `persp-mode' loads a session.

(fn &rest _)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "solaire-mode" '("solaire-mode-"))) (provide 'solaire-mode-autoloads)) "ht" ((ht ht-autoloads) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ht" 'nil)) (provide 'ht-autoloads)) "emojify" ((emojify emojify-autoloads) (autoload 'emojify-set-emoji-styles "emojify" "Set the type of emojis that should be displayed.

STYLES is the styles emoji styles that should be used, see `emojify-emoji-styles'

(fn STYLES)" nil nil) (autoload 'emojify-mode "emojify" "Emojify mode

If called interactively, enable Emojify mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (put 'global-emojify-mode 'globalized-minor-mode t) (defvar global-emojify-mode nil "Non-nil if Global Emojify mode is enabled.
See the `global-emojify-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-emojify-mode'.") (custom-autoload 'global-emojify-mode "emojify" nil) (autoload 'global-emojify-mode "emojify" "Toggle Emojify mode in all buffers.
With prefix ARG, enable Global Emojify mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Emojify mode is enabled in all buffers where
`emojify-mode' would do it.
See `emojify-mode' for more information on Emojify mode.

(fn &optional ARG)" t nil) (autoload 'emojify-mode-line-mode "emojify" "Emojify mode line

If called interactively, enable Emojify-Mode-Line mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (put 'global-emojify-mode-line-mode 'globalized-minor-mode t) (defvar global-emojify-mode-line-mode nil "Non-nil if Global Emojify-Mode-Line mode is enabled.
See the `global-emojify-mode-line-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-emojify-mode-line-mode'.") (custom-autoload 'global-emojify-mode-line-mode "emojify" nil) (autoload 'global-emojify-mode-line-mode "emojify" "Toggle Emojify-Mode-Line mode in all buffers.
With prefix ARG, enable Global Emojify-Mode-Line mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Emojify-Mode-Line mode is enabled in all buffers where
`emojify-mode-line-mode' would do it.
See `emojify-mode-line-mode' for more information on Emojify-Mode-Line mode.

(fn &optional ARG)" t nil) (autoload 'emojify-apropos-emoji "emojify" "Show Emojis that match PATTERN.

(fn PATTERN)" t nil) (autoload 'emojify-insert-emoji "emojify" "Interactively prompt for Emojis and insert them in the current buffer.

This respects the `emojify-emoji-styles' variable." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emojify" '("emojify-"))) (provide 'emojify-autoloads)) "olivetti" ((olivetti olivetti-autoloads) (autoload 'olivetti-mode "olivetti" "Olivetti provides a nice writing environment.

If called interactively, enable Olivetti mode if ARG is
positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or
nil, and toggle it if ARG is `toggle'; disable the mode
otherwise.

Window margins are set to relative widths to accomodate a text
body width set with `olivetti-body-width'.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "olivetti" '("olivetti-"))) (provide 'olivetti-autoloads)) "centered-window-mode" ((centered-window-mode-autoloads centered-window) (autoload 'centered-window-mode-toggle "centered-window" nil nil nil) (defvar centered-window-mode nil "Non-nil if Centered-Window mode is enabled.
See the `centered-window-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `centered-window-mode'.") (custom-autoload 'centered-window-mode "centered-window" nil) (autoload 'centered-window-mode "centered-window" "Minor mode to center text on the current buffer

If called interactively, enable Centered-Window mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "centered-window" '("cwm-"))) (provide 'centered-window-mode-autoloads)) "centered-window" ((centered-window centered-window-autoloads) (autoload 'centered-window-mode-toggle "centered-window" nil nil nil) (defvar centered-window-mode nil "Non-nil if Centered-Window mode is enabled.
See the `centered-window-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `centered-window-mode'.") (custom-autoload 'centered-window-mode "centered-window" nil) (autoload 'centered-window-mode "centered-window" "Minor mode to center text on the current buffer

If called interactively, enable Centered-Window mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "centered-window" '("cwm-"))) (provide 'centered-window-autoloads))))

#s(hash-table size 65 test eq rehash-size 1.5 rehash-threshold 0.8125 data (org-elpa #s(hash-table size 97 test equal rehash-size 1.5 rehash-threshold 0.8125 data (version 1 "melpa" nil "gnu-elpa-mirror" nil "emacsmirror-mirror" nil "straight" nil "use-package" nil "bind-key" nil "hydra" nil "cl-lib" nil "lv" nil "which-key" nil "ivy" nil "evil-leader" nil "evil" nil "goto-chg" nil "evil-escape" nil "s" nil "el-patch" nil "deft" nil "counsel" nil "swiper" nil "smooth-scrolling" nil "alert" nil "gntp" nil "log4e" nil "google-this" nil "mu4e" nil "calendar" nil "calfw" nil "magit" nil "dash" nil "git-commit" nil "transient" nil "with-editor" nil "git-gutter" nil "git-timemachine" nil "org" (org :type git :repo "https://code.orgmode.org/bzg/org-mode.git" :local-repo "org") "org-roam" nil "f" nil "emacsql" nil "emacsql-sqlite3" nil "company-org-roam" nil "company" nil "org-roam-bibtex" nil "bibtex-completion" nil "parsebib" nil "biblio" nil "biblio-core" nil "let-alist" nil "seq" nil "org-journal" nil "org-bullets" nil "org-ref" nil "htmlize" nil "helm" nil "async" nil "popup" nil "helm-core" nil "helm-bibtex" nil "key-chord" nil "pdf-tools" nil "tablist" nil "org-noter" nil "org-download" nil "org-gcal" nil "request" nil "request-deferred" nil "deferred" nil "persist" nil "org2blog" nil "xml-rpc" nil "metaweblog" nil "all-the-icons" nil "doom-themes" nil "cycle-themes" nil "battery" nil "mini-modeline" nil "rainbow-delimiters" nil "smartparens" nil "company-box" nil "dash-functional" nil "frame-local" nil "ispell" nil "flyspell" nil "guess-language" nil "yasnippet" nil "focus" nil "ivy-bibtex" nil "interleave" nil "esxml" nil "solaire-mode" nil "emojify" nil "ht" nil "olivetti" nil "centered-window-mode" nil "centered-window" nil)) melpa #s(hash-table size 97 test equal rehash-size 1.5 rehash-threshold 0.8125 data (version 2 "gnu-elpa-mirror" nil "emacsmirror-mirror" nil "straight" nil "use-package" (use-package :type git :flavor melpa :files (:defaults (:exclude "bind-key.el" "bind-chord.el" "use-package-chords.el" "use-package-ensure-system-package.el") "use-package-pkg.el") :host github :repo "jwiegley/use-package") "bind-key" (bind-key :type git :flavor melpa :files ("bind-key.el" "bind-key-pkg.el") :host github :repo "jwiegley/use-package") "hydra" (hydra :type git :flavor melpa :files (:defaults (:exclude "lv.el") "hydra-pkg.el") :host github :repo "abo-abo/hydra") "cl-lib" nil "lv" (lv :type git :flavor melpa :files ("lv.el" "lv-pkg.el") :host github :repo "abo-abo/hydra") "which-key" (which-key :type git :flavor melpa :host github :repo "justbur/emacs-which-key") "ivy" (ivy :type git :flavor melpa :files (:defaults (:exclude "swiper.el" "counsel.el" "ivy-hydra.el" "ivy-avy.el") "doc/ivy-help.org" "ivy-pkg.el") :host github :repo "abo-abo/swiper") "evil-leader" (evil-leader :type git :flavor melpa :host github :repo "cofi/evil-leader") "evil" (evil :type git :flavor melpa :files (:defaults "doc/build/texinfo/evil.texi" (:exclude "evil-test-helpers.el") "evil-pkg.el") :host github :repo "emacs-evil/evil") "goto-chg" (goto-chg :type git :flavor melpa :host github :repo "emacs-evil/goto-chg") "evil-escape" (evil-escape :type git :flavor melpa :host github :repo "syl20bnr/evil-escape") "s" (s :type git :flavor melpa :files ("s.el" "s-pkg.el") :host github :repo "magnars/s.el") "el-patch" (el-patch :type git :flavor melpa :host github :repo "raxod502/el-patch") "deft" (deft :type git :flavor melpa :host github :repo "jrblevin/deft") "counsel" (counsel :type git :flavor melpa :files ("counsel.el" "counsel-pkg.el") :host github :repo "abo-abo/swiper") "swiper" (swiper :type git :flavor melpa :files ("swiper.el" "swiper-pkg.el") :host github :repo "abo-abo/swiper") "smooth-scrolling" (smooth-scrolling :type git :flavor melpa :host github :repo "aspiers/smooth-scrolling") "alert" (alert :type git :flavor melpa :host github :repo "jwiegley/alert") "gntp" (gntp :type git :flavor melpa :host github :repo "tekai/gntp.el") "log4e" (log4e :type git :flavor melpa :host github :repo "aki2o/log4e") "google-this" (google-this :type git :flavor melpa :files ("google-this.el" "google-this-pkg.el") :host github :repo "Malabarba/emacs-google-this") "mu4e" nil "calendar" nil "calfw" (calfw :type git :flavor melpa :files ("calfw.el" "calfw-pkg.el") :host github :repo "kiwanami/emacs-calfw") "magit" (magit :type git :flavor melpa :files ("lisp/magit" "lisp/magit*.el" "lisp/git-rebase.el" "Documentation/magit.texi" "Documentation/AUTHORS.md" "LICENSE" (:exclude "lisp/magit-libgit.el") "magit-pkg.el") :host github :repo "magit/magit") "dash" (dash :type git :flavor melpa :files ("dash.el" "dash.texi" "dash-pkg.el") :host github :repo "magnars/dash.el") "git-commit" (git-commit :type git :flavor melpa :files ("lisp/git-commit.el" "git-commit-pkg.el") :host github :repo "magit/magit") "transient" (transient :type git :flavor melpa :files ("lisp/*.el" "docs/transient.texi" "transient-pkg.el") :host github :repo "magit/transient") "with-editor" (with-editor :type git :flavor melpa :host github :repo "magit/with-editor") "git-gutter" (git-gutter :type git :flavor melpa :host github :repo "emacsorphanage/git-gutter") "git-timemachine" (git-timemachine :type git :flavor melpa :host gitlab :repo "pidu/git-timemachine") "org-roam" (org-roam :type git :flavor melpa :host github :repo "org-roam/org-roam") "f" (f :type git :flavor melpa :files ("f.el" "f-pkg.el") :host github :repo "rejeep/f.el") "emacsql" (emacsql :type git :flavor melpa :files ("emacsql.el" "emacsql-compiler.el" "emacsql-system.el" "README.md" "emacsql-pkg.el") :host github :repo "skeeto/emacsql") "emacsql-sqlite3" (emacsql-sqlite3 :type git :flavor melpa :host github :repo "cireu/emacsql-sqlite3") "company-org-roam" nil "company" (company :type git :flavor melpa :host github :repo "company-mode/company-mode") "org-roam-bibtex" (org-roam-bibtex :type git :flavor melpa :host github :repo "org-roam/org-roam-bibtex") "bibtex-completion" (bibtex-completion :type git :flavor melpa :files ("bibtex-completion.el" "bibtex-completion-pkg.el") :host github :repo "tmalsburg/helm-bibtex") "parsebib" (parsebib :type git :flavor melpa :host github :repo "joostkremers/parsebib") "biblio" (biblio :type git :flavor melpa :files (:defaults (:exclude "biblio-core.el") "biblio-pkg.el") :host github :repo "cpitclaudel/biblio.el") "biblio-core" (biblio-core :type git :flavor melpa :files ("biblio-core.el" "biblio-core-pkg.el") :host github :repo "cpitclaudel/biblio.el") "let-alist" nil "seq" nil "org-journal" (org-journal :type git :flavor melpa :host github :repo "bastibe/org-journal") "org-bullets" (org-bullets :type git :flavor melpa :host github :repo "integral-dw/org-bullets") "org-ref" (org-ref :type git :flavor melpa :files (:defaults "org-ref.org" "org-ref.bib" "citeproc" "org-ref-pkg.el") :host github :repo "jkitchin/org-ref") "htmlize" (htmlize :type git :flavor melpa :host github :repo "hniksic/emacs-htmlize") "helm" (helm :type git :flavor melpa :files ("*.el" "emacs-helm.sh" (:exclude "helm.el" "helm-lib.el" "helm-source.el" "helm-multi-match.el" "helm-core-pkg.el") "helm-pkg.el") :host github :repo "emacs-helm/helm") "async" (async :type git :flavor melpa :host github :repo "jwiegley/emacs-async") "popup" (popup :type git :flavor melpa :files ("popup.el" "popup-pkg.el") :host github :repo "auto-complete/popup-el") "helm-core" (helm-core :type git :flavor melpa :files ("helm-core-pkg.el" "helm.el" "helm-lib.el" "helm-source.el" "helm-multi-match.el" "helm-core-pkg.el") :host github :repo "emacs-helm/helm") "helm-bibtex" (helm-bibtex :type git :flavor melpa :files ("helm-bibtex.el" "helm-bibtex-pkg.el") :host github :repo "tmalsburg/helm-bibtex") "key-chord" (key-chord :type git :flavor melpa :host github :repo "emacsorphanage/key-chord") "pdf-tools" (pdf-tools :type git :flavor melpa :files ("lisp/*.el" "README" ("build" "Makefile") ("build" "server") (:exclude "lisp/tablist.el" "lisp/tablist-filter.el") "pdf-tools-pkg.el") :host github :repo "politza/pdf-tools") "tablist" (tablist :type git :flavor melpa :host github :repo "politza/tablist") "org-noter" (org-noter :type git :flavor melpa :host github :repo "weirdNox/org-noter") "org-download" (org-download :type git :flavor melpa :host github :repo "abo-abo/org-download") "org-gcal" (org-gcal :type git :flavor melpa :host github :repo "kidd/org-gcal.el") "request" (request :type git :flavor melpa :files ("request.el" "request-pkg.el") :host github :repo "tkf/emacs-request") "request-deferred" (request-deferred :type git :flavor melpa :files ("request-deferred.el" "request-deferred-pkg.el") :host github :repo "tkf/emacs-request") "deferred" (deferred :type git :flavor melpa :files ("deferred.el" "deferred-pkg.el") :host github :repo "kiwanami/emacs-deferred") "persist" nil "org2blog" (org2blog :type git :flavor melpa :files (:defaults "README.org" (:exclude "metaweblog.el") "org2blog-pkg.el") :host github :repo "org2blog/org2blog") "xml-rpc" (xml-rpc :type git :flavor melpa :host github :repo "xml-rpc-el/xml-rpc-el") "metaweblog" (metaweblog :type git :flavor melpa :files ("metaweblog.el" "metaweblog-pkg.el") :host github :repo "org2blog/org2blog") "all-the-icons" (all-the-icons :type git :flavor melpa :files (:defaults "data" "all-the-icons-pkg.el") :host github :repo "domtronn/all-the-icons.el") "doom-themes" (doom-themes :type git :flavor melpa :files (:defaults "themes/*.el" "doom-themes-pkg.el") :host github :repo "hlissner/emacs-doom-themes") "cycle-themes" (cycle-themes :type git :flavor melpa :host github :repo "toroidal-code/cycle-themes.el") "battery" nil "mini-modeline" (mini-modeline :type git :flavor melpa :host github :repo "kiennq/emacs-mini-modeline") "rainbow-delimiters" (rainbow-delimiters :type git :flavor melpa :host github :repo "Fanael/rainbow-delimiters") "smartparens" (smartparens :type git :flavor melpa :host github :repo "Fuco1/smartparens") "company-box" (company-box :type git :flavor melpa :files (:defaults "images" "company-box-pkg.el") :host github :repo "sebastiencs/company-box") "dash-functional" (dash-functional :type git :flavor melpa :files ("dash-functional.el" "dash-functional-pkg.el") :host github :repo "magnars/dash.el") "frame-local" (frame-local :type git :flavor melpa :host github :repo "sebastiencs/frame-local") "ispell" nil "flyspell" nil "guess-language" (guess-language :type git :flavor melpa :files ("guess-language.el" "trigrams/*" "guess-language-pkg.el") :host github :repo "tmalsburg/guess-language.el") "yasnippet" (yasnippet :type git :flavor melpa :files ("yasnippet.el" "snippets" "yasnippet-pkg.el") :host github :repo "joaotavora/yasnippet") "focus" (focus :type git :flavor melpa :host github :repo "larstvei/Focus") "ivy-bibtex" (ivy-bibtex :type git :flavor melpa :files ("ivy-bibtex.el" "ivy-bibtex-pkg.el") :host github :repo "tmalsburg/helm-bibtex") "interleave" (interleave :type git :flavor melpa :host github :repo "rudolfochrist/interleave") "esxml" (esxml :type git :flavor melpa :files ("esxml.el" "esxml-query.el" "esxml-pkg.el") :host github :repo "tali713/esxml") "solaire-mode" (solaire-mode :type git :flavor melpa :host github :repo "hlissner/emacs-solaire-mode") "emojify" (emojify :type git :flavor melpa :files (:defaults "data" "images" "emojify-pkg.el") :host github :repo "iqbalansari/emacs-emojify") "ht" (ht :type git :flavor melpa :files ("ht.el" "ht-pkg.el") :host github :repo "Wilfred/ht.el") "olivetti" (olivetti :type git :flavor melpa :host github :repo "rnkn/olivetti") "centered-window-mode" nil "centered-window" (centered-window :type git :flavor melpa :host github :repo "anler/centered-window-mode"))) gnu-elpa-mirror #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (version 3 "emacsmirror-mirror" nil "straight" nil "cl-lib" nil "mu4e" nil "calendar" nil "company-org-roam" nil "let-alist" (let-alist :type git :host github :repo "emacs-straight/let-alist" :files ("*" (:exclude ".git"))) "seq" nil "persist" (persist :type git :host github :repo "emacs-straight/persist" :files ("*" (:exclude ".git"))) "battery" nil "ispell" nil "flyspell" nil "centered-window-mode" nil)) emacsmirror-mirror #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (version 2 "straight" (straight :type git :host github :repo "emacsmirror/straight") "cl-lib" nil "mu4e" (mu4e :type git :host github :repo "emacsmirror/mu4e") "calendar" nil "company-org-roam" (company-org-roam :type git :host github :repo "emacsattic/company-org-roam") "seq" nil "battery" nil "ispell" nil "flyspell" nil "centered-window-mode" nil))))

("org-elpa" "melpa" "gnu-elpa-mirror" "emacsmirror-mirror" "straight" "emacs" "use-package" "bind-key" "hydra" "cl-lib" "lv" "which-key" "ivy" "evil-leader" "evil" "goto-chg" "evil-escape" "s" "el-patch" "deft" "counsel" "swiper" "smooth-scrolling" "alert" "gntp" "log4e" "google-this" "mu4e" "calendar" "magit" "dash" "git-commit" "transient" "with-editor" "git-gutter" "git-timemachine" "org-journal" "org" "org-roam" "f" "emacsql" "emacsql-sqlite3" "company-org-roam" "company" "org-roam-bibtex" "bibtex-completion" "parsebib" "biblio" "biblio-core" "let-alist" "seq" "org-bullets" "org-ref" "htmlize" "helm" "async" "popup" "helm-core" "helm-bibtex" "key-chord" "pdf-tools" "tablist" "esxml" "org-noter" "org-download" "org-gcal" "request" "request-deferred" "deferred" "persist" "org2blog" "xml-rpc" "metaweblog" "all-the-icons" "doom-themes" "cycle-themes" "battery" "mini-modeline" "rainbow-delimiters" "smartparens" "company-box" "dash-functional" "frame-local" "ispell" "flyspell" "guess-language" "yasnippet" "focus" "centered-window" "ivy-bibtex" "interleave")

t
