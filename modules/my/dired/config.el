;; [[file:../../../../../../var/folders/qt/lhqmcxqs5lnfgst5md2t8tfr0000gn/T/config.org.61j08Y::*dired+][dired+:2]]
(use-package! dired+)
;; I want to see the details
(add-hook 'dired-mode-hook
            (lambda ()
              (dired-hide-details-mode -1)))
;; dired+:2 ends here

;; [[file:../../../../../../var/folders/qt/lhqmcxqs5lnfgst5md2t8tfr0000gn/T/config.org.61j08Y::*ranger][ranger:1]]
(setq ranger-override-dired-mode nil)
(setq ranger-override-dired nil)
;; ranger:1 ends here

;; [[file:../../../../../../var/folders/qt/lhqmcxqs5lnfgst5md2t8tfr0000gn/T/config.org.61j08Y::*config][config:1]]
(setq dired-recursive-deletes 'always)
;; config:1 ends here

;; [[file:../../../../../../var/folders/qt/lhqmcxqs5lnfgst5md2t8tfr0000gn/T/config.org.61j08Y::*config][config:2]]
(setq dired-listing-switches "-aBhl")
;; config:2 ends here

;; [[file:../../../../../../var/folders/qt/lhqmcxqs5lnfgst5md2t8tfr0000gn/T/config.org.61j08Y::*config][config:3]]
(map! :map dired-mode-map
  :desc "open"  :n "z" #'me/mac-open)
;; config:3 ends here
