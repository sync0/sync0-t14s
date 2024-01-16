
(defun sync0-search-in-catalogs ()
  "Searches for a term in different catalogs."
  (interactive)
  (let* ((search-term (read-string "Enter search term: "))
         (catalogs '(("Anna’s Archive" . "https://annas-archive.org/search?q=%s")
                     ("Sudoc" . "https://www.sudoc.abes.fr/cbs/xslt//DB=2.1/SET=4/TTL=1/CMD?ACT=SRCHA&IKT=1016&SRT=YOP&TRM=%s")
                     ("BnF" . "https://catalogue.bnf.fr/rechercher.do?motRecherche=%s&critereRecherche=0&depart=0&facetteModifiee=ok")
                     ("Diderot (Lyon)" . "https://rechercher.bibliotheque-diderot.fr/discovery/search?vid=33ENSL_INST:33ENSL_INST&tab=Everything&search_scope=MyInst_and_CI&lang=fr&query=any,contains,%s")
                     ("Worldcat" . "https://www.worldcat.org/search?q=%s")
                     ("Lyon 2" . "https://catalogue.univ-lyon2.fr/cgi-bin/koha/opac-search.pl?idx=&q=%s&weight_search=1")
                     ("Fonds Triangle" . "https://smultidoc.msh-lse.fr/cgi-bin/koha/opac-search.pl?idx=&q=%s&limit=&weight_search=1"))))
    (let* ((catalog-names (mapcar 'car catalogs))
           (chosen-catalogs (completing-read-multiple "Select catalogs to search: " catalog-names)))
      (dolist (chosen-catalog chosen-catalogs)
        (browse-url (format (cdr (assoc chosen-catalog catalogs)) (url-encode-url search-term)))))))

(provide 'sync0-bibtex-url)
