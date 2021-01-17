;; [[file:../../../../../../var/folders/v5/8_bvmqtj6nn2jd8l581ylfb80000gp/T/config.org.pELcr6::*dired+][dired+:2]]
(use-package! dired+)
;; I want to see the details
(add-hook 'dired-mode-hook
            (lambda ()
              (dired-hide-details-mode -1)))
;; dired+:2 ends here

;; [[file:../../../../../../var/folders/v5/8_bvmqtj6nn2jd8l581ylfb80000gp/T/config.org.pELcr6::*ranger][ranger:1]]
(setq ranger-override-dired-mode nil)
(setq ranger-override-dired nil)
;; ranger:1 ends here

;; [[file:../../../../../../var/folders/v5/8_bvmqtj6nn2jd8l581ylfb80000gp/T/config.org.pELcr6::*config][config:1]]
(setq dired-recursive-deletes 'always)
;; config:1 ends here

;; [[file:../../../../../../var/folders/v5/8_bvmqtj6nn2jd8l581ylfb80000gp/T/config.org.pELcr6::*config][config:2]]
(setq dired-listing-switches "-aBhl")
;; config:2 ends here

;; [[file:../../../../../../var/folders/v5/8_bvmqtj6nn2jd8l581ylfb80000gp/T/config.org.pELcr6::*config][config:3]]
(map! :map dired-mode-map
  :desc "open"  :n "z" #'me/mac-open)
;; config:3 ends here
