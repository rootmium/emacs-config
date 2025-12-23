(use-package ob-http)

(use-package org
  :ensure nil
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (http . t)))
  )

;; Directory for org files
(setq org-directory "~/Projects/org")
(setq org-agenda-files (list org-directory))

;; Custom TODO keywords
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "STARTED(s)" "|" "DONE(d)")
        (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))

;; Colors for keywords
(setq org-todo-keyword-faces
      '(("NEXT" . "orange") ("STARTED" . "yellow")
        ("WAITING" . "purple") ("HOLD" . "magenta")))

;; Log time when a task is completed
(setq org-log-done 'time)
(setq org-log-into-drawer t)

(setq org-default-notes-file (concat org-directory "/inbox.org"))

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "inbox.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("m" "Meeting" entry (file "meetings.org")
         "* MEETING with %? :MEETING:\n%U")))

(global-set-key (kbd "C-c i") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

;; Refile tasks easily to other files
(setq org-refile-targets
      '((nil :maxlevel . 3)
        (org-agenda-files :maxlevel . 3)))
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-use-outline-path 'file)

(provide 'root-org-mode)
