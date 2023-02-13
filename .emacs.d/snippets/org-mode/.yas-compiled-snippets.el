;;; Compiled snippets and support files for `org-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'org-mode
                     '(("<vi" "[[${1:link of the video}][file:${2:link of the image}]" "video" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/video" nil nil)
                       ("<v" "#+begin_verse\n$0\n#+end_verse" "verse" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/verse" nil nil)
                       ("use-package" "(use-package ${1:name}\n  :defer t\n  $0)" "use-package" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/use-package" nil nil)
                       ("uml" "#+begin_uml\n$1\n#+end_uml" "uml" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/uml" nil nil)
                       ("<ti" "#+title: $0" "title" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/title" nil nil)
                       ("color" "\\textcolor{${1:red}}{${2:write_here}}$0" "latex_xcolor" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/text_xcolor" nil nil)
                       ("tinaudible" "\\begin{center}\n[inaudible]\n\\end{center}\n$0" "tex_inaudible" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/tex_inaudible" nil nil)
                       ("tquote" "#+ATTR_LATEX: :options [{\\cite[${2:`(sync0-last-cited-page-three)`}]{${1:`(sync0-ivy-bibtex)`}}}]\n#+BEGIN_displayquote\n$3\n#+END_displayquote\n$0" "test_quote" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/test_quote" nil nil)
                       ("template" "\\subject{${1:Histoire de la Pensée Économique}} \n\\title{${2:Comme viaggiare con un salmone}}\n\\subtitle{${3:Il salmone}} \n\\author{carlos alberto rivera carreño} \n\\date{\\today} \n# \\publishers{Sociologie des comportements sociaux} \n# \\dedication{Die Philosophen haben die Welt nur verschieden interpretiert; \\\\ es kömmt drauf an, sie zu verändern.} \n\\maketitle\n* ${4:Introduction}\n$0\n* Einstellungen :noexport:ARCHIVE:\n** Einführung\n#+LANGUAGE: fr\n#+OPTIONS: toc:nil \\n:nil ::t |:t ^:t f:t *:t ':t pro:nil timestamp:nil\n#+OPTIONS: LaTeX:t TeX:t d:nil pri:t p:t tags:nil inline:t todo:nil\n#+LATEX_CLASS: scrartcl\n#+LATEX_CLASS_OPTIONS: [paper=A4,portrait,twoside=false,twocolumn=false,headinclude=false,footinclude=false,fontsize=11,BCOR=0mm,DIV=8,pagesize=auto,abstract=false,mpinclude=false,headings=normal]\n#+LATEX_CMD: xelatex\n#+TAGS: vocabulary(v) noexport export\n#+EXPORT_SELECT_TAGS: export\n#+EXPORT_EXCLUDE_TAGS: noexport\n#+STARTUP: noindent hidestars content logdrawer\n** Paketverladung\n#+LATEX_HEADER: \\usepackage{polyglossia} \n#+LATEX_HEADER: \\setmainlanguage{french} \n#+LATEX_HEADER: \\setotherlanguages{german,italian,english} \n##+LATEX_HEADER: \\usepackage{xeCJK}\n##+LATEX_HEADER: \\setCJKmainfont{Baekmuk Batang}\n#+LATEX_HEADER: \\usepackage[french=guillemets]{csquotes}\n##+LATEX_HEADER: \\usepackage[backend=biber,style=authoryear,doi=false,isbn=false,url=true]{biblatex}\n##+LATEX_HEADER: \\addbibresource{~/Documents/mendeley/library.bib}\n#+LATEX_HEADER: \\usepackage{xunicode}\n##+LATEX_HEADER: \\usepackage{balance}\n##+LATEX_HEADER: \\usepackage{booktabs}\n##+LATEX_HEADER: \\usepackage{tablefootnote}\n#+LATEX_HEADER: \\usepackage[french]{fmtcount} \n#+LATEX_HEADER: \\fmtcountsetoptions{french=france}\n#+LATEX_HEADER: \\usepackage[singlespacing]{setspace}\n#+LATEX_HEADER: \\usepackage[super]{nth}\n#+LATEX_HEADER: \\usepackage{xcolor}\n##+LATEX_HEADER: \\usepackage{minted}\n#+LATEX_HEADER: \\usepackage{microtype}\n#+LATEX_HEADER: \\microtypecontext{kerning=french}\n#+LATEX_HEADER: \\usepackage{ragged2e}\n##+LATEX_HEADER: \\usepackage{lipsum}\n#+LATEX_HEADER: \\usepackage{amsmath}\n#+LATEX_HEADER: \\usepackage{amsthm}\n#+LATEX_HEADER: \\usepackage{amssymb}\n#+LATEX_HEADER: \\usepackage{hyperref}\n#+LATEX_HEADER: \\hypersetup{colorlinks,urlcolor=blue,linkcolor=bibleblue,citecolor=red,filecolor=black}\n#+LATEX_HEADER: \\usepackage{enumitem}\n#+LATEX_HEADER: \\usepackage{adforn}\n** Schriftart\n#+LATEX_HEADER: \\usepackage{fontspec}\n#+LATEX_HEADER:\\setmainfont[Numbers={OldStyle},SmallCapsFeatures={LetterSpace=4,Ligatures={NoCommon, NoDiscretionary, NoHistoric, NoRequired, NoContextual}}]{Linux Libertine}\n** Seitenstil\n##+LATEX_HEADER:\\usepackage{scrlayer-scrpage}\n##+LATEX_HEADER:\\pagestyle{scrheadings}\n##+LATEX_HEADER:\\renewcommand*{\\chapterpagestyle}{plain}\n##+LATEX_HEADER:\\clearscrheadfoot\n##+LATEX_HEADER:\\automark{chapter}\n##+LATEX_HEADER:\\chead{\\headmark} \n##+LATEX_HEADER:\\ifoot*{\\thepage} \n** Absätze\n#+LATEX_HEADER: \\AfterTOCHead{\\singlespacing}\n#+LATEX_HEADER: \\setkomafont{disposition}{\\normalfont\\normalcolor}\n*** Titelblatt\n#+LATEX_HEADER: \\addtokomafont{author}{\\scshape\\normalsize\\lowercase}\n#+LATEX_HEADER: \\addtokomafont{date}{\\normalsize}\n#+LATEX_HEADER: \\addtokomafont{subtitle}{\\itshape}\n##+LATEX_HEADER: \\addtokomafont{dedication}{\\itshape}\n*** Part\n##+LATEX_HEADER: \\renewcommand*{\\partformat}{\\adforn{21}~\\thepart~\\adforn{49}}\n##+LATEX_HEADER: \\addtokomafont{part}{\\scshape\\Huge\\lowercase}\n*** Chapter \n##+LATEX_HEADER:\\renewcommand{\\raggedchapter}{\\centering}\n##+LATEX_HEADER: \\addtokomafont{chapter}{\\scshape\\Huge\\lowercase}\n*** Section\n#+LATEX_HEADER: \\addtokomafont{section}{\\scshape\\Large\\lowercase}\n*** Subsection\n#+LATEX_HEADER: \\addtokomafont{subsection}{\\bfseries\\large}\n*** Subsubsection\n#+LATEX_HEADER: \\addtokomafont{subsubsection}{\\itshape\\large}\n\n** User-defined elements\n#+LATEX_HEADER:\\newenvironment{summary}{\\begin{addmargin}{3em}\\itshape}{\\end{addmargin}} \n#+LATEX_HEADER:\\newcommand{\\annotation}[1]{\\marginpar{\\small\\itshape #1}}\n#+LATEX_HEADER:\\renewcommand*\\labelitemi{\\adforn{14}}\n#+LATEX_HEADER:\\renewcommand*\\labelitemii{\\adforn{14}}\n#+LATEX_HEADER:\\renewcommand*\\labelitemiii{\\adforn{14}}\n#+LATEX_HEADER:\\renewcommand*\\labelitemiv{\\adforn{14}}\n#+LATEX_HEADER: \\definecolor{bibleblue}{HTML}{00339a}\n*** Dictum\n#+LATEX_HEADER: \\renewcommand*{\\dictumwidth}{.8\\textwidth}\n#+LATEX_HEADER: \\renewcommand*{\\raggeddictum}{\\centering}\n#+LATEX_HEADER: \\renewcommand*{\\raggeddictumtext}{\\centering}\n#+LATEX_HEADER: \\addtokomafont{dictum}{\\large\\rmfamily}\n\n** Andere\n#+LATEX_HEADER: \\setcounter{secnumdepth}{\\partnumdepth}\n#+LATEX_HEADER: \\recalctypearea\n#+LATEX_HEADER: \\setlist[1]{itemsep=\\parskip}\n##+LATEX_HEADER: \\setlength{\\marginparwidth}{3\\marginparwidth}\n##+LATEX_HEADER: \\setlength{\\columnsep}{2.5em}" "fr_essay_template" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/template_fr_essay" nil nil)
                       ("td" "** Travail dirigé du `(insert (shell-command-to-string \"echo -n $(date +%d)\"))` `(insert (shell-command-to-string \"echo -n $(date +%B)\"))`\n*** $0" "td_snippet" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/td_lazy_snippet" nil nil)
                       ("tablecap" "#+ATTR_LATEX: :environment tabular :booktabs :caption \\captionabove{${1:Lorem Ipsum}} $0" "koma_table_caption" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/table_caption" nil nil)
                       ("<ta" "#+caption: ${1: caption of the table}\n|${2:column 1} | ${3: column 2} |\n|--------------+----------------|\n" "table" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/table" nil nil)
                       ("question" "#+ATTR_LATEX: :options {`(insert (shell-command-to-string \"echo -n $(date +'%Y/%m/%d')\"))`}\n#+BEGIN_question\n$1\n#+END_question\n\n$0" "sync0_question" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/sync0_question" nil nil)
                       ("note" "#+ATTR_LATEX: :options {`(insert (shell-command-to-string \"echo -n $(date +'%Y/%m/%d')\"))`}\n#+BEGIN_note\n$1\n#+END_note\n\n$0" "sync0_note" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/sync0_note" nil nil)
                       ("def" "#+ATTR_LATEX: :options {${1:title}} {${2:$$(unless yas-modified-p (sync0-guess-language-set-parts-of-speech))}}\n#+BEGIN_definition\n${3:Definition}\n\\tcblower\n${4:Example}\n#+END_definition\n$0" "sync0_definition" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/sync0_definition" nil nil)
                       ("<st" "#+style: <link rel=\"stylesheet\" type=\"text/css\" href=\"$1\" />" "style" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/style" nil nil)
                       ("<src" "#+begin_src $1\n  $0\n#+end_src\n" "src" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/src" nil nil)
                       ("spanish" "\\textspanish{\\itshape ${1:Hola!}}$0" "spanish_text" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/spanish_text" nil nil)
                       ("sc" "\\textsc{$1}$0" "smallcaps_text" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/small_caps" nil nil)
                       ("set" "#+setupfile: $0" "setup" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/setup" nil nil)
                       ("<rib" "    :properties:\n    :reveal_background: ${1: path of the image}\n    :reveal_background_trans: ${2: default||cube||page||concave||zoom||linear||fade||none||slide}\n    :end:" "reveal_image_background" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/rv_image_background" nil nil)
                       ("<rsb" ":properties:\n:reveal_background: ${1: #123456}\n:end:" "reveal_single_colored_background" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/rv_background" nil nil)
                       ("ricetta" "*** ${1:Ricetta}\n**** Ingredienti\n\\begin{ingredienti} \n$0 \\\\\n\\end{ingredienti}\n**** Preparazione\n" "ricetta" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/ricetta" nil nil)
                       ("flushleft" "\\begin{FlushLeft}\n${1:enter_text_here}\n\\end{FlushLeft}" "ragged2e_flushleft" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/ragged2e_flushleft" nil nil)
                       ("<q" "#+begin_quote\n$0\n#+end_quote" "quote" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/quote" nil nil)
                       ("lit_python" "#+BEGIN_SRC python :noweb yes :tangle literate_python/literate.py\n    \"\"\"A docstring for the literate.py module\"\"\"\n\n    # imports\n    import sys\n    <<literate-main-imports>>\n\n    # constants\n\n    # exception classes\n\n    # interface funchtions\n\n    # classes\n\n\n    <<LiterateClass-definition>>\n\n    # internal functions & classes\n\n    <<literate-main>>\n\n\n    if __name__ == \"__main__\":\n        status = main()\n        sys.exit(status)\n  #+END_SRC" "python-literate-src-block" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/python-literate-src-block" nil nil)
                       ("py_" "#+begin_src python\n$0\n#+end_src" "python" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/python" nil nil)
                       ("func" "*${1:function_name}:* ${2:function_definition}\n/example:/ ~${3:code}~\n/output:/ ${4:output}\n$0" "programming function" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/programming_function" nil nil)
                       ("timeline" "#+ATTR_LATEX: :booktabs :environment tabular :align @{\\,}r<{\\hskip 2pt} !{\\timebullet} p{7cm} \n|${1:1492}|${2:Arrivee de Chirstophe Colombus}|" "org-tabular-timeline" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/org_tabular_timeline" nil nil)
                       ("table" "#+ATTR_LATEX: :booktabs ${1::environment tabular} ${2::width \\columnwidth} ${3::align l|lp{3cm}r|l} ${4::caption \\captionabove{${5:Lorem Ipsum}}} $0" "org_table_properties" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/org_table_properties" nil nil)
                       ("caption" "\\#+ATTR_LATEX: :tabular environment: tabular :rmlines\n\\#+CAPTION: ${1:Lorem ipsum dolor sit amet}\n$0\n" "org_table_export_options" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/org_table_export_options" nil nil)
                       ("spanish" "#+BEGIN_spanish\n$1\n#+END_spanish\n\n$0" "org_spanish" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/org_spanish" nil nil)
                       ("code" "#+BEGIN_SRC sas\n${1:DATA base1}\n#+END_SRC\n$0" "org_source_code" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/org_source_code" nil nil)
                       ("ref" "[[${1:$$(yas-choose-value '(\"auto\" \"paren\" \"text\" \"foot\"))}cite:${2:`(sync0-last-cited-author)`}][${3:$$(unless yas-modified-p (sync0-last-cited-page))}]] $0" "org_ref_citation" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/org_ref_citation" nil nil)
                       ("oquote" "#+BEGIN_QUOTE\n${1:Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.}\n\n\\RaggedLeft $2, \\linebreak \\footnotesize $3\n#+END_QUOTE\n$0" "org_quote" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/org_quote" nil nil)
                       ("logp" "*  `(sync0-log-today-timestamp)`\n:PROPERTIES:\n:CREATED: `(format-time-string \"<%Y-%m-%d>\")` \n:LAST_MODIFIED: `(format-time-string \"<%Y-%m-%d>\")` \n:END:\n\n$0" "org_log_properties" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/org_log_properties" nil nil)
                       ("log" "* `(sync0-log-today-timestamp)`\n\n$0" "org_log" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/org_log" nil nil)
                       ("link" "[[${1:link}][${2:description}]] $0" "org-link" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/org_link" nil nil)
                       ("summary" "#+BEGIN_summary\n$1\n#+END_summary $0" "org_latex_summary" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/org_latex_summary" nil nil)
                       ("labeling" "#+ATTR_LATEX: :options [:]{} \n#+BEGIN_labeling\n\\item[${1:Category_1}] $2 ${3:\n\\item[${4:Category_2}] $5}\n#+END_labeling\n$0" "org_labeling" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/org_labeling" nil nil)
                       ("italian" "#+BEGIN_italian\n$1\n#+END_italian\n\n$0" "org_italian" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/org_italian" nil nil)
                       ("inaudible" "#+BEGIN_center\n[inaudible]\n#+END_center\n$0" "org_inaudible" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/org_inaudible" nil nil)
                       ("german" "#+BEGIN_german\n$1\n#+END_german\n\n$0" "org_german" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/org_german" nil nil)
                       ("french" "#+BEGIN_french\n$1\n#+END_french\n\n$0" "org_french" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/org_french" nil nil)
                       ("reference" "[[footcite:${1:`(org-entry-get-with-inheritance \"Custom_ID\")`}][p. ${2:43}, par. ${3:2}]] $0" "org_footnote_reference" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/org_footnote_reference" nil nil)
                       ("equation" "#+BEGIN_equation*\n$1 = $2\n#+END_equation* $0" "org_equation_environment" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/org_equation" nil nil)
                       ("english" "#+BEGIN_english\n$1\n#+END_english\n\n$0" "org_english" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/org_english" nil nil)
                       ("description" "#+ATTR_LATEX: :options [leftmargin=12em, style=multiline, noitemsep]\n#+BEGIN_description\n\\item[${1:Category_1}] $2 ${3:\n\\item[${4:Category_2}] $5}\n#+END_description\n$0" "org_description" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/org_description" nil nil)
                       ("align" "#+BEGIN_align*\n$1 &= $2 \\\\\\\n$3 &= $4\n#+END_align* $0" "org_align_environment" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/org_align" nil nil)
                       ("test" "test me" "org-test" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/org-test" nil nil)
                       ("lt" "| Eu   | ${1:sou}   |\n| Tu   | ${2:es}    |\n| Ele  | ${3:é}     |\n| Nós  | ${4:somos} |\n| Vós  | ${5:sois}  |\n| Eles | ${6:são}   |\n\n$0" "org-table-conjugation" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/org-table-conjugation" nil nil)
                       ("straight" ":straight (${1:the-package} :type git :host github :repo \"${2:john-doe/awesome-package}\") $0" "org-straight" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/org-straight" nil nil)
                       ("re" "** `(format-time-string \"%Y%m%d%H%M%S\")`\n$0" "org-roam-notetaking-headline" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/org-roam-notetaking-headline" nil nil)
                       ("anot" "** ${1:Titre} \n:PROPERTIES:\n:ID:    `(org-id-new)`${2:\n:ROAM_ALIASES:   ${3:Aliases}}\n:CREATED:  `(format-time-string \"%Y-%m-%d\")`\n:LAST_MODIFIED: `(format-time-string \"%Y-%m-%d\")`\n:ZETTEL_TYPE: annotation\n:END:\n\n$0" "org-roam-annotation-infile" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/org-roam-annotation-infile" nil nil)
                       ("inlinecite" "[[parencite:${1:`(sync0-print-bibtex-key)`}][p. ${2:`(sync0-last-cited-page-two)`}]] $0\n" "org-ref_inline_citation" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/org-ref_inline_citation" nil nil)
                       ("dialogue" "#+BEGIN_dialogue\n\\speak\\{${1:Vladimir}\\} ${2:Whare are all these corpses from?}\n\\speak\\{${3:Estragon}\\} ${4:These skeletons.}\n${5:\\direct\\{\nEstragon has exited offstage to right and left and come panting back\nand fallen into Vladimir’s arms\\}}\n$0\n#+END_dialogue" "org-latex-dialogue-environment" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/org-latex-dialogue-environment" nil nil)
                       ("abstract" "#+BEGIN_abstract\n$1\n#+END_abstract\n$0" "org-latex-abstract" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/org-latex-abstract" nil nil)
                       ("export" "#+EXPORT_FILE_NAME: `(sync0-org-format-export-keyword)`" "org-export-keyword" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/org-export-keyword" nil nil)
                       ("info" ":INFO:\n:AUTHOR: \n:CROSSREF: \n:PARENT: \n:NOTES: `(sync0-org-insert-link-to-notes-from-bibkey)` \n:END:" "org-drawer-info" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/org-drawer-info" nil nil)
                       ("assdate" "((format-time-string \"%Y-%m-%d\"))\n" "org-date" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/org-date" nil nil)
                       ("comment" "#+BEGIN_COMMENT\n$1\n#+END_COMMENT\n$0" "org-comment" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/org-comment" nil nil)
                       ("date" "(`(format-time-string \"%Y-%m-%d\")`) $0" "org-brackets-today-timestamp" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/org-brackets-today-timestamp" nil nil)
                       ("<op" "#+options: h:${1:1} num:${2:t||nil} toc:${3:t||nil}$0" "options" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/options" nil nil)
                       ("th" "\\nth{$1}$0" "nth" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/nth" nil nil)
                       ("pg" "(p. ${1:$$(unless yas-modified-p (sync0-reference-last-cited-pages))})$0" "note-taking-citation-pages" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/note-taking-citation-pages" nil nil)
                       ("matrix_" "\\left \\(\n\\begin{array}{${1:ccc}}\n${2:v1 & v2} \\\\\n$0\n\\end{array}\n\\right \\)" "matrix" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/matrix" nil nil)
                       ("math" "\\$${1:}\\$$0\n" "math_text" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/math_mode" nil nil)
                       ("newtext" "** ${1:title}\n:PROPERTIES:\n:YEAR: ${2:1929} \n:JOURNAL: \n:VOLUME: \n:ISSUE:\n:PAGES: \n:ADDED: [`(insert (format-time-string \"%Y-%m-%d\"))`]\n:END:\n$0" "master_text" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/master_text" nil nil)
                       ("mquote" "#+BEGIN_QUOTE\n$1 [[cite:`(sync0-print-bibtex-key)`][p. ${2:`(sync0-last-cited-page)`}, par. ${3:1}]].\n#+END_QUOTE\n$0" "master_quote" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/master_quote" nil nil)
                       ("author" "** ${1:title}\n:PROPERTIES:\n:YEAR: \n:JOURNAL: \n:PAGES: 133-145\n:ADDED: [(insert ((format-time-string \"%Y-%m-%d\")))]\n:END:\n$0" "master_author" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/master_author" nil nil)
                       ("mnote" "\\annotation{${1:add margin annotation}} $0" "margin_note" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/margin_note" nil nil)
                       ("<li" "[[${1:link}][${2:description}]]\n" "link" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/link" nil nil)
                       ("lec" "* Leçon du `(insert (shell-command-to-string \"echo -n $(date +%d)\"))` `(insert (shell-command-to-string \"echo -n $(date +%B)\"))`\n** $0\n" "lecon_snip" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/lecon_lazy_snippet" nil nil)
                       ("vspace" "\\vspace{${1:\\baselineskip}} $0" "latex_vspace" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/latex_vspace" nil nil)
                       ("vec" "\\vec{${1:x}} $0 " "latex_vector" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/latex_vector" nil nil)
                       ("sum" "\\sum_{${1:i=1}}^{${2:\\infty}} $0 \n" "latex_sum" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/latex_sum" nil nil)
                       ("lquote" "\\`\\`${1:Lorem ipsum dolor sit amet}''$0" "latex_quotation_marks" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/latex_quotation_marks" nil nil)
                       ("prod" "\\prod_{${1:i=1}}^{${2:\\infty}} $0 \n" "latex_product" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/latex_product" nil nil)
                       ("problem" "\\begin{problem*}\n$1 \n\\end{problem*} $0" "latex_problem_environment" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/latex_problem" nil nil)
                       ("nth" "\\nth{${1:20}}$0" "latex_nth" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/latex_nth" nil nil)
                       ("modified" "#+BEGIN_modified\nfullcite:`(org-entry-get-with-inheritance \"Custom_ID\")`\n\\tcblower\nLast modified: {{{property(MODIFIED)}}}\n#+END_modified\n$0" "latex_tcolorbox_modified" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/latex_modified" nil nil)
                       ("text" "\\text{$1}$0" "math_text" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/latex_math_text" nil nil)
                       ("lim" "\\lim_{${1:x\\to\\infty}} $0 \n" "latex_limit" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/latex_limit" nil nil)
                       ("lecture" "\\begin{lecture*}\n$1 \n\\end{lecture*} $0" "latex_lecture_environment" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/latex_lecture" nil nil)
                       ("last" "\\begin{labeling}[:]{} \n\\item[Last modified] `(shell-command-to-string \"echo -n $(date +'%Y/%m/%d')\")`\n\\end{labeling}\n$0" "latex_last_modified" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/latex_last_modified" nil nil)
                       ("int" "\\int_{${1:a}}^{${2:\\infty}} $0 \n" "latex_integral" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/latex_integral" nil nil)
                       ("hat" "\\widehat{${1:XY}} $0 \n" "latex_wide_hat" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/latex_hat" nil nil)
                       ("genfrac" "\\genfrac{}{}{0pt}{0}{${1:\\partial^2 \\sin x}}{${2:\\partial x^2}} $0 " "latex_genfrac" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/latex_genfrac" nil nil)
                       ("sie" "\\sie{$1}$0" "latex_francais_siecle" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/latex_francais_siecle" nil nil)
                       ("frac" "\\frac{${1:\\partial^2 \\sin x}}{${2:\\partial x^2}} $0 " "latex_frac" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/latex_frac" nil nil)
                       ("lequation" "\\begin{equation*}\n$1 = $2\n\\end{equation*} $0" "latex_equation_environment" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/latex_equation" nil nil)
                       ("latex-cite" "`(yas-choose-value '(\"\\\\paren\" \"\\\\text\" \"\\\\auto\" \"\\\\foot\"))`cite${1:[${2:prenote}]}${3:[${4:postnote}]}{${5:key}} $0" "latex_cite" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/latex_cite" nil nil)
                       ("bold" "\\mathbf{${1:X}}_${2:i} $0 \n" "latex_bold_vector" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/latex_bold_vector" nil nil)
                       ("bar" "\\bar{${1:X}} $0 \n" "latex_bar" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/latex_bar" nil nil)
                       ("lalign" "\\begin{align*}\n$1 &= $2 \\\\\\\n$3 &= $4\n\\end{align*} $0" "latex_align_environment" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/latex_align" nil nil)
                       ("todo" "\\todo{${1:my_todo_text}}$0" "latex-todo" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/latex-todo" nil nil)
                       ("speak" "\\speak\\{${1:Estragon}\\} ${2:I'm in hell!} $0" "latex-dialogue-speak" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/latex-dialogue-speak" nil nil)
                       ("direct" "\\direct\\{${1:Estragon has exited offstage and fallen into Vladimir’s arms.}\\} $0" "latex-dialogue-direct" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/latex-dialogue-direct" nil nil)
                       ("<lan" "#+language: ${1:en}" "language" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/language" nil nil)
                       ("lsummary" "\\begin{summary}\n${1:Schreiben Sie Ihre Zusammenfassung}\n\\end{summary}$0\n" "KOMA_summary" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/koma_summary" nil nil)
                       ("kquote" "#+BEGIN_QUOTE\n$1 [[parencite:${2:$$(unless yas-modified-p (yas-choose-value bibtex-reference-keys))}][${3:$4p. ${5:`(sync0-last-cited-page)`}, par. ${6:1}}]]\n#+END_QUOTE\n$0\n" "koma_quote" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/koma_quote" nil nil)
                       ("kquestion" "\\begin{labeling}[~]{Question} \n\\item[Question] $1 \n\\end{labeling}\n$0" "koma_question" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/koma_question" nil nil)
                       ("pquote" "\\begin{quote}\n$1\n\n\\RaggedLeft p. $2, par. $3 \n\\end{quote}\n$0" "koma_page_quote" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/koma_page_quote" nil nil)
                       ("minisec" "\\minisec{${1:Titre}}$0" "latex_koma_minisec" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/koma_minisec" nil nil)
                       ("item" "\\item[${1:Category}] *$2*. $0" "koma_labeling_item" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/koma_labeling_item" nil nil)
                       ("klabeling" "\\begin{labeling}[:]{} \n\\item[${1:Commentaire}] $2\n\\end{labeling}$0" "labeling_koma" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/koma_labeling" nil nil)
                       ("kcomment" "\\begin{labeling}[:]{} \n\\item[Comment] $1 \n\\end{labeling}\n$0" "koma_comment" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/koma_comment" nil nil)
                       ("<ke" "#+keywords: $0" "keywords" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/keywords" nil nil)
                       ("it" "\\textit{$1}$0" "italics_text" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/italics_text" nil nil)
                       ("ipy_" "#+begin_src ipython :session ${1:session01} :file ${2:$$(concat (make-temp-name \"./ipython-\") \".png\")} :exports ${3:both}\n$0\n#+end_src" "ipython" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/ipython" nil nil)
                       ("ing" "\\begin{ingredienti}\n$0 \\\\\n\\end{ingredienti}\n\n" "ingredienti" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/ingredienti" nil nil)
                       ("<i" "#+include: $0" "include" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/include" nil nil)
                       ("img_" "<img src=\"$1\" alt=\"$2\" align=\"${3:left}\" title=\"${4:image title}\" class=\"img\" $5/>$0" "img" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/img" nil nil)
                       ("<im" "#+caption: ${1:caption of the image}\n[[file:${2:image_path}]]$0" "image" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/image" nil nil)
                       ("<ht" "#+html:$1" "html" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/html" nil nil)
                       ("ne" "ne $1pas $0" "french_ne" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/french_ne" nil nil)
                       ("fig_" "#+caption: ${1:caption}\n#+attr_latex: ${2:scale=0.75}\n#+label: fig:${3:label}$0" "figure" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/figure" nil nil)
                       ("elisp" "#+BEGIN_SRC emacs-lisp\n${1}\n#+END_SRC $0" "org_fast_elisp" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/fast_elisp" nil nil)
                       ("<ex" "#+begin_export ${1:type}\n$0\n#+end_export" "export" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/export" nil nil)
                       ("<e" "#+begin_example\n$0\n#+end_example" "example" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/exampleblock" nil nil)
                       ("entry_" "#+begin_html\n---\nlayout: ${1:default}\ntitle: ${2:title}\n---\n#+end_html\n" "entry" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/entry" nil nil)
                       ("em" "\\emph{$1}$0" "emph" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/emph" nil nil)
                       ("emb_" "src_${1:lang}${2:[${3:where}]}{${4:code}}" "embedded" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/embedded" nil nil)
                       ("<em" "#+email: $0" "email" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/email" nil nil)
                       ("emacs-lisp_" "#+begin_src emacs-lisp :tangle yes\n$0\n#+end_src" "emacs-lisp" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/emacs-lisp" nil nil)
                       ("elisp_" "#+begin_src emacs-lisp :tangle yes\n$0\n#+end_src" "elisp" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/elisp" nil nil)
                       ("einstellungen" "Einstellungen :noexport:ARCHIVE:\n#+LATEX_CMD: xelatex\n#+TITLE: ${1}\n#+SUBTITLE: ${2}\n#+AUTHOR: ${3}\n#+EMAIL: ${4:cantorlunae@gmail.com}\n#+DATE: ${5}\n#+LANGUAGE: ${6:en} \n#+OPTIONS: num:nil toc:nil \\n:nil @:t ::t |:t ^:t f:t *:t timestamp:nil ':t email:nil\n#+OPTIONS: TeX:t LaTeX:t d:nil pri:nil tags:not-in-toc\n#+LATEX_CLASS: ${7:scrartcl}\n#+BIBLIOGRAPHY: library chicagoa\n#+LATEX_CLASS_OPTIONS: [fontsize=${8:12pt}, koma, paper=${9:letter}, pagesize=pdftex]\n#+LATEX_HEADER: \\newlength{\\alphabet}\n#+LATEX_HEADER: \\settowidth{\\alphabet}{\\normalfont abcdefghijklmnopqrstuvwxyz}\n#+LATEX_HEADER: \\usepackage[letterpaper, textwidth=2.3\\alphabet, hmarginratio={1:1}, top=2.5cm, bottom=2.5cm]{geometry}\n#+LATEX_HEADER: \\usepackage[${10:french}]{babel} \n#+LATEX_HEADER: \\usepackage[backend=biber,style=authoryear-icomp,doi=false,isbn=false,url=false,block=space,mergedate=basic,labeldateparts=true]{biblatex}\n#+LATEX_HEADER: \\addbibresource{library.bib}\n#+LATEX_HEADER: \\renewbibmacro*{date}{\\iffieldundef{origyear}{}{\\setunit*{\\addspace}\\printtext[brackets]{\\printorigdate}}}\n#+LATEX_HEADER: \\usepackage[autostyle, english=american]{csquotes}\n#+LATEX_HEADER: \\usepackage[super]{nth}\n#+LATEX_HEADER: \\usepackage{xunicode}\n#+LATEX_HEADER: \\usepackage{microtype}\n#+LATEX_HEADER: \\usepackage{hyperref}\n#+LATEX_HEADER: \\hypersetup{colorlinks,urlcolor=blue,linkcolor=red,citecolor=red,filecolor=black}\n#+LATEX_HEADER: \\usepackage{ragged2e}\n#+LATEX_HEADER: \\usepackage{setspace}\n#+LATEX_HEADER: ${11:\\singlespacing}\n${12:#+LATEX_HEADER: \\frenchspacing}\n#+LATEX_HEADER: \\setlength{\\JustifyingParindent}{0pt}\n#+LATEX_HEADER: \\usepackage{titlesec}\n#+LATEX_HEADER: \\usepackage[extramarks]{titleps}\n#+LATEX_HEADER: \\usepackage{fontspec}\n#+LATEX_HEADER: \\setmainfont{Linux Libertine}[SmallCapsFont={Linux Libertine Capitals}, SmallCapsFeatures={Letters=SmallCaps, LetterSpace=8.0}]\n#+LATEX_HEADER: \\defaultfontfeatures[Linux Libertine]{Ligatures=TeX}\n#+LATEX_HEADER: \\newfontfamily\\sectionheadfont{Linux Libertine Capitals}[WordSpace=3, SmallCapsFeatures={LetterSpace=6.0}]\n#+LATEX_HEADER: \\newfontfamily\\subsectionheadfont{Linux Libertine Capitals}[WordSpace=2, SmallCapsFeatures={LetterSpace=6.0}]\n#+LATEX_HEADER: \\newfontfamily\\subsubsectionheadfont{Linux Libertine}[SmallCapsFeatures={LetterSpace=6.0}]\n#+LATEX_HEADER: \\newcommand{\\sectionheadstyle}{\\flushleft\\sectionheadfont\\Huge\\textsc}\n#+LATEX_HEADER: \\newcommand{\\subsectionheadstyle}{\\flushleft\\subsectionheadfont\\Large\\textsc}\n#+LATEX_HEADER: \\newcommand{\\subsubsectionheadstyle}{\\flushright\\subsubsectionheadfont\\large\\textit}\n#+LATEX_HEADER: \\titleformat{\\section}[display]{}{}{0pt}{\\sectionheadstyle}[\\vspace{1ex}\\titlerule] \n#+LATEX_HEADER: \\titleformat{\\subsection}[block]{}{}{0pt}{\\subsectionheadstyle}[\\vspace{1ex}] \n#+LATEX_HEADER: \\titleformat{\\subsubsection}[display]{}{}{0pt}{\\subsubsectionheadstyle}[\\vspace{-1.5ex}] \n#+LATEX_HEADER: \\titlespacing*{\\section}{0pt}{*1}{*4} \n#+LATEX_HEADER: \\titlespacing*{\\subsection}{0pt}{*1}{*1} \n#+TAGS: vocabulary(v) noexport\n#+EXPORT_SELECT_TAGS: export\n#+EXPORT_EXCLUDE_TAGS: noexport\n#+STARTUP: noindent hidestars content" "latex_export_settings" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/einstellungen" nil nil)
                       ("dot_" "#+begin_src dot :file ${1:file} :cmdline -t${2:pdf} :exports none :results silent\n$0\n#+end_src\n[[file:${3:path}]]" "dot" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/dot" nil nil)
                       ("ent" "| `(sync0-doctorat-complete-biblatex-key)` | `(sync0-doctorat-complete-person-id)` | `(sync0-doctorat-complete-reference-types)` | `(sync0-doctorat-complete-explicit)` | ${5:32} | `(sync0-doctorat-complete-languages)` | $0 |" "doctorat-mentioned-people-table-complete" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/doctorat-mentioned-people-table-complete" nil nil)
                       ("desc" "#+description: $0" "description" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/description" nil nil)
                       ("<da" "#+date: ${1:year}:${2:month}:${3:day}" "date" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/date" nil nil)
                       ("lfquote" "\\\\foreignquote{${1:Lorem ipsum dolor sit amet}}$0" "latex_csquotes_foreignquote" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/csquotes_foreignquote" nil nil)
                       ("fffquote" "\\foreignblockquote{${1:english}}[${2:Marcus Tullius Cicero}]{${3:Lorem ipsum dolor sit amet}}$0" "csquotes_foreignblockquote" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/csquotes_foreignblockquote" nil nil)
                       ("fcquote" "\\foreignblockcquote{${1:english}}{${2:Cicero45}}{${3:Lorem ipsum dolor sit amet}}$0" "csquotes_foreignblockcquote" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/csquotes_foreignblockcquote" nil nil)
                       ("fquote" "#+ATTR_LATEX: :options {`(yas-choose-value '(\"english\" \"french\"))`}[{\\cite[${2:`(sync0-last-cited-page)`}]{`(sync0-last-cited-author)`}}]\n#+BEGIN_foreigndisplayquote\n$3\n#+END_foreigndisplayquote\n$0" "csquotes_foreignblockquote" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/csquotes_foreign_displayquote" nil nil)
                       ("enquote" "\\\\enquote{${1:Lorem ipsum dolor sit amet}}$0" "latex_csquotes_enquote" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/csquotes_enquote" nil nil)
                       ("quote" "#+ATTR_LATEX: :options [{\\cite${1:$$(unless yas-modified-p (sync0-last-cited-page))}{`(sync0-last-cited-author)`}}]\n#+BEGIN_displayquote\n$2\n#+END_displayquote\n$0" "csquotes_displayquote" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/csquotes_displayquote" nil nil)
                       ("bquote" "\\blockquote[${1:Marcus Tullius Cicero}]{${2:Lorem ipsum dolor sit amet}}$0" "csquotes_blockquote" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/csquotes_blockquote" nil nil)
                       ("cquote" "\\blockcquote{${1:Cicero45}}{${2:Lorem ipsum dolor sit amet}}$0" "csquotes_blockcquote" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/csquotes_blockcquote" nil nil)
                       ("<c" "#+begin_center\n$0\n#+end_center" "center" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/center" nil nil)
                       ("bf" "\\textbf{$1}$0\n" "boldface_text" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/bold_text_function" nil nil)
                       ("beamer" "*** ${1:Première colonne}\n    :PROPERTIES:\n    :BEAMER_col: 0.4\n    :BEAMER_env: alertblock\n    :END:\n${2:Lorem ipsum dolor sit amet, consectetur adipiscing elit,\nsed do eiusmod tempor incididunt ut labore et dolore magna aliqua.\nUt enim ad minim veniam, quis nostrud exercitation ullamco\nlaboris nisi ut aliquip ex ea commodo consequat.}     \n***  ${3:Deuxième colonne}\n    :PROPERTIES:\n    :BEAMER_col: 0.6\n    :BEAMER_env: alertblock\n    :END:\n    #+ATTR_LaTeX: :width 0.8\\textwidth :float t :placement [H] \n    [[./images/${4:image.png}]]\n    $0" "two_column_beamer" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/beamer_two_column" nil nil)
                       ("beamer" "*** Note\n :PROPERTIES:\n :BEAMER_ENV: note\n :END:\n$0" "notes_beamer" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/beamer_notes" nil nil)
                       ("beamer" "    :PROPERTIES:\n    :BEAMER_env: alertblock\n    :END:\n    #+BEGIN_EXAMPLE\n    \\begin{alertblock}{${1:Lorem ipsum dolor}}\n    $2\n    \\end{alertblock}\n    #+END_EXAMPLE\n    $0\n" "alert_block_beamer" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/beamer_alert_block" nil nil)
                       ("<au" "#+author: $0" "author" nil nil nil "/home/sync0/.emacs.d/snippets/org-mode/author" nil nil)))


;;; Do not edit! File generated at Thu Feb  9 16:34:23 2023
