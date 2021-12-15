;; /u/sy/beebe/emacs/bibtex-regs.el, Sat Sep 16 08:33:15 2017
;; Edit by Nelson H. F. Beebe <beebe@math.utah.edu>
;; Change DOI field value to new DOI-agency recommended form
;; /u/sy/beebe/emacs/bibtex-regs.el, Sat Jun 30 11:18:29 2012
;; Edit by Nelson H. F. Beebe <beebe@math.utah.edu>
;; Add C-e, C-i, C-l, C-n, and S register contents.
;; /u/sy/beebe/emacs/bibtex-regs.el, Tue Jun 28 08:32:40 2011
;; Edit by Nelson H. F. Beebe <beebe@math.utah.edu>
;; Add DOI variant with URL prefix in C-d register.
;; /u/sy/beebe/emacs/bibtex-regs.el, Fri Mar  4 10:57:06 2011
;; Edit by Nelson H. F. Beebe <beebe@math.utah.edu>
;; Add fjournal in f register.
;; /u/sy/beebe/emacs/bibtex-regs.el, Fri May 21 08:57:40 2010
;; Edit by Nelson H. F. Beebe <beebe@math.utah.edu>
;; Change content for Z register from author to ZMnumber.
;; /u/sy/beebe/emacs/bibtex-regs.el, Sat Sep 20 13:13:39 2003
;; Edit by Nelson H. F. Beebe <beebe@math.utah.edu>
;; Add `remark' in r register.
;; /u/sy/beebe/emacs/bibtex-regs.el, Fri Jul 27 05:15:59 2001
;; Edit by Nelson H. F. Beebe <beebe@math.utah.edu>
;; Add `DOI' in D register, `bookpages' in C-b register, and swap
;; `key' and `keywords' from k and K registers to K and k registers,
;; because `keywords' is needed much more often.
;; /u/sy/beebe/emacs/bibtex-regs.el, Thu Sep 30 06:19:42 1999
;; Edit by Nelson H. F. Beebe <beebe@math.utah.edu>
;; Add B register contents.
;; /u/sy/beebe/emacs/bibtex-regs.el, Mon Dec 28 06:18:11 1998
;; Edit by Nelson H. F. Beebe <beebe@math.utah.edu>
;; Add % register contents.
;; /u/sy/beebe/emacs/bibtex-regs.el, Thu Jun 18 07:22:58 1998
;; Edit by Nelson H. F. Beebe <beebe@math.utah.edu>
;; Add l register contents.
;; /u/sy/beebe/emacs/bibtex-regs.el, Sat Mar 21 07:35:50 1998
;; Edit by Nelson H. F. Beebe <beebe@plot79.math.utah.edu>
;; Add T register contents
;; /u/sy/beebe/emacs/bibtex-regs.el, Mon Apr 14 07:30:04 1997
;; Edit by Nelson H. F. Beebe <beebe@plot79.math.utah.edu>
;; Add M register contents.
;; /u/sy/beebe/emacs/bibtex-regs.el, Mon Feb 17 07:04:44 1997
;; Edit by  <beebe@sunrise.math.utah.edu>
;; Add (provide 'bibtex-regs)
;; /u/sy/beebe/emacs/bibtex-regs.el, Fri Jan  3 15:13:36 1997
;; Edit by Nelson H. F. Beebe <beebe@plot79.math.utah.edu>
;; /u/sy/beebe/emacs/bibtex-regs.el, Sun Jun 23 10:11:32 1996
;; Edit by Nelson H. F. Beebe <beebe@plot79.math.utah.edu>
;; /u/sy/beebe/emacs/bibtex-regs.el, Wed Jun  5 06:56:31 1996
;; Edit by Nelson H. F. Beebe <beebe@plot79.math.utah.edu>

(provide 'bibtex-regs)

;; Some handy register contents for BibTeX editing work

(set-register ?\C-a "  address =      \"\",\n")
(set-register ?\C-b "  bookpages =    \"\",\n")
(set-register ?\C-c "  note =         \"See correction \\cite{}.\",\n")
;; (set-register ?\C-d "  DOI =          \"http://dx.doi.org/\",\n")
;; [16-Sep-2017] Switch to new DOI-agency recommended prefix
(set-register ?\C-d "  DOI =          \"https://doi.org/\",\n")
(set-register ?\C-e "  note =         \"See erratum \\cite{}.\",\n")
(set-register ?\C-i "  ISSN-L =       \"\",\n")
(set-register ?\C-l "  lastaccess =   \"\",\n")

(set-register ?# "  number =       \"\",\n")
(set-register ?$ "  price =        \"US\\$\",\n")
(set-register ?/ "\\slash ")
(set-register ?. "\\ldots{} ")
(set-register ?% "%%% -*-BibTeX-*-\n")

(set-register ?A "  abstract =     \"\",\n")
(set-register ?B "  bibsource =    \"\",\n")
(set-register ?D "  DOI =          \"\",\n")
(set-register ?E "  editor =       \"\",\n")
(set-register ?I "  ISBN =         \"\",\n")
(set-register ?K "  key =          \"\",\n")
(set-register ?L "  LCCN =         \"\",\n")
(set-register ?M "  MRnumber =     \"\",\n  MRclass =      \"\",\n")
(set-register ?N "  note =         \"See corrigendum \\cite{}.\",\n")
(set-register ?O "  note =         \"See \\cite{}.\",\n")
(set-register ?P "  publisher =    \"\",\n")
(set-register ?S "  subject =      \"\",\n")
(set-register ?T "  type =         \"\",\n")
(set-register ?U "  URL =          \"\",\n")
(set-register ?Z "  ZMnumber =     \"\",\n")

(set-register ?a "  acknowledgement = ack-nhfb,\n")
(set-register ?b "  booktitle =    \"\",\n")
(set-register ?c "  CODEN =        \"\",\n")
(set-register ?d "  day =          \"\",\n")
(set-register ?e "  edition =      \"\",\n")
(set-register ?f "  fjournal =     \"\",\n")
(set-register ?i "  ISSN =         \"\",\n")
(set-register ?j "  journal =      \"\",\n")
(set-register ?k "  keywords =     \"\",\n")
(set-register ?l "  language =     \"\",\n")
(set-register ?m "  month =        ,\n")
(set-register ?n "  note =         \"\",\n")
(set-register ?o "  onlinedate =   \"\",\n")
(set-register ?p "  pages =        \"\",\n")
(set-register ?r "  remark =       \"\",\n")
(set-register ?s "  series =       \"\",\n")
(set-register ?t "  title =        \"\",\n")
(set-register ?v "  volume =       \"\",\n")
(set-register ?x "  crossref =     \"\",\n")
(set-register ?y "  year =         \"\",\n")
