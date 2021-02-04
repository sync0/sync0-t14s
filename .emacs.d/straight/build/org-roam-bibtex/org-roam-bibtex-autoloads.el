;;; org-roam-bibtex-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "orb-anystyle" "orb-anystyle.el" (0 0 0 0))
;;; Generated autoloads from orb-anystyle.el

(autoload 'orb-anystyle "orb-anystyle" "\
Run anystyle COMMAND with `shell-command'.
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

\(fn COMMAND &key (EXEC orb-anystyle-executable) VERBOSE HELP VERSION ADAPTER ((:finder-model FMODEL) orb-anystyle-finder-model) ((:parser-model PMODEL) orb-anystyle-parser-model) (PDFINFO orb-anystyle-pdfinfo-executable) (PDFTOTEXT orb-anystyle-pdftotext-executable) FORMAT STDOUT OVERWRITE (CROP orb-anystyle-find-crop) (SOLO orb-anystyle-find-solo) (LAYOUT orb-anystyle-find-layout) INPUT OUTPUT (BUFFER orb-anystyle-default-buffer))" nil nil)

(function-put 'orb-anystyle 'lisp-indent-function '1)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "orb-anystyle" '("orb-anystyle-")))

;;;***

;;;### (autoloads nil "orb-core" "orb-core.el" (0 0 0 0))
;;; Generated autoloads from orb-core.el

(autoload 'orb-process-file-field "orb-core" "\
Process the 'file' BibTeX field and resolve if there are multiples.
Search the disk for the document associated with this BibTeX
entry.  The disk matching is based on looking in the
`bibtex-completion-library-path' for a file with the
CITEKEY.

If variable `orb-file-field-extensions' is non-nil, return only
the file paths with the respective extensions.

\(Mendeley, Zotero, normal paths) are all supported.  If there
are multiple files found the user is prompted to select which one
to enter.

\(fn CITEKEY)" nil nil)

(autoload 'orb-autokey-generate-key "orb-core" "\
Generate citation key from ENTRY according to `orb-autokey-format'.
Return a string.  If optional CONTROL-STRING is non-nil, use it
instead of `orb-autokey-format'.

\(fn ENTRY &optional CONTROL-STRING)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "orb-core" '("orb-")))

;;;***

;;;### (autoloads nil "orb-helm" "orb-helm.el" (0 0 0 0))
;;; Generated autoloads from orb-helm.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "orb-helm" '("helm-source-orb-insert" "orb-helm-insert")))

;;;***

;;;### (autoloads nil "orb-ivy" "orb-ivy.el" (0 0 0 0))
;;; Generated autoloads from orb-ivy.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "orb-ivy" '("orb-i")))

;;;***

;;;### (autoloads nil "orb-pdf-scrapper" "orb-pdf-scrapper.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from orb-pdf-scrapper.el

(autoload 'orb-pdf-scrapper-run "orb-pdf-scrapper" "\
Run Orb PDF Scrapper interactive process.
KEY is note's citation key.

\(fn KEY)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "orb-pdf-scrapper" '("orb-")))

;;;***

;;;### (autoloads nil "orb-utils" "orb-utils.el" (0 0 0 0))
;;; Generated autoloads from orb-utils.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "orb-utils" '("orb-")))

;;;***

;;;### (autoloads nil "org-roam-bibtex" "org-roam-bibtex.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from org-roam-bibtex.el

(autoload 'orb-edit-notes "org-roam-bibtex" "\
Open an Org-roam note associated with the CITEKEY or create a new one.

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

\(fn CITEKEY)" nil nil)

(autoload 'orb-insert "org-roam-bibtex" "\
Insert a link to an Org-roam bibliography note.
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
text string will be used as the link's description — similar to
`org-roam-insert'.

Normally, the case of the link description will be preserved.  It
is possible to force lowercase by supplying either one or three
universal arguments `\\[universal-argument]'.

Finally, `bibtex-completion-cache' will be re-populated if either
two or three universal arguments `\\[universal-argument]' are supplied.

\(fn &optional ARG)" t nil)

(autoload 'orb-find-non-ref-file "org-roam-bibtex" "\
Find and open an Org-roam, non-ref file.
INITIAL-PROMPT is the initial title prompt.
See `org-roam-find-files' and
`orb--get-non-ref-path-completions' for details.

\(fn &optional INITIAL-PROMPT)" t nil)

(autoload 'orb-insert-non-ref "org-roam-bibtex" "\
Find a non-ref Org-roam file, and insert a relative org link to it at point.
If PREFIX, downcase the title before insertion.  See
`org-roam-insert' and `orb--get-non-ref-path-completions' for
details.

\(fn PREFIX)" t nil)

(autoload 'orb-note-actions "org-roam-bibtex" "\
Run an interactive prompt to offer note-related actions.
The prompt interface can be set in `orb-note-actions-interface'.
In addition to default actions, which are not supposed to be
modified, there is a number of prefined extra actions
`orb-note-actions-extra' that can be customized.  Additionally,
user actions can be set in `orb-note-actions-user'." t nil)

(defvar org-roam-bibtex-mode nil "\
Non-nil if Org-Roam-Bibtex mode is enabled.
See the `org-roam-bibtex-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `org-roam-bibtex-mode'.")

(custom-autoload 'org-roam-bibtex-mode "org-roam-bibtex" nil)

(autoload 'org-roam-bibtex-mode "org-roam-bibtex" "\
Sets `orb-edit-notes' as a function for editing bibliography notes.
Affects Org-ref and Helm-bibtex/Ivy-bibtex.

When called interactively, toggle `org-roam-bibtex-mode'. with
prefix ARG, enable `org-roam-bibtex-mode' if ARG is positive,
otherwise disable it.

When called from Lisp, enable `org-roam-mode' if ARG is omitted,
nil, or positive.  If ARG is `toggle', toggle `org-roam-mode'.
Otherwise, behave as if called interactively.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-bibtex" '("orb-" "org-roam-bibtex-mode-map" "with-orb-cleanup")))

;;;***

;;;### (autoloads nil nil ("orb-compat.el") (0 0 0 0))

;;;***

(provide 'org-roam-bibtex-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-roam-bibtex-autoloads.el ends here
