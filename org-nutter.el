;;; org-nutter.el --- Provide commands to search directories of org files

;; Version: 0.0.0
;; Package-Requires: (helm-org-rifle)

(require 'f)
(provide 'org-nutter)

(defcustom org-nutter-root "~/.org-nutter" "Directory that contains directories to search" :type 'directory)

(defvar org-nutter-last-search-directories nil "Last selected nutter search directories")
(defvar org-nutter-capture-target-directory nil "Directory where a captured note will be stored")

(defun find-nutter-dirs ()
  "Return the names of the nutter directories."
  (f-directories org-nutter-root))

(defun select-nutter-dirs ()
  "Return the name of the nutter directories that the user selects.
If there is only one such directory, this function returns its
name without asking the user."
  (let ((nutter-dirs (find-nutter-dirs)))
    (if (eql (length nutter-dirs) 1)
        nutter-dirs
      (helm-comp-read "Select nutter directories: " nutter-dirs :marked-candidates t))))

(defun select-single-nutter-dir ()
  "Return the name of the nutter directory that the user selects.
If there is only one such directory, this function returns its
name without asking the user."
  (let ((nutter-dirs (find-nutter-dirs)))
    (if (eql (length nutter-dirs) 1)
        nutter-dirs
      (helm-comp-read "Select single nutter directory: " nutter-dirs))))

;; From https://stackoverflow.com/a/53738442

(defun psachin/create-notes-file ()
  "Create an org file in a subdirectory of the org-nutter root."
  (interactive)
  (let ((name (read-string "Filename: ")))
    (expand-file-name (format "%s.org"
                              name)
                      (concat (file-name-as-directory org-nutter-capture-target-directory)))))

(setq org-capture-templates
      '(("n" "Notes" entry
         (file psachin/create-notes-file)
         "*  %?")))

(defun org-nutter-rifle-new-search-directories ()
  (interactive)
  (setq org-nutter-last-search-directories (select-nutter-dirs))
  (when org-nutter-last-search-directories
    (helm-org-rifle-directories org-nutter-last-search-directories)))

(defun org-nutter-rifle ()
  (interactive)
  (if org-nutter-last-search-directories
      (helm-org-rifle-directories org-nutter-last-search-directories)
    (org-nutter-rifle-new-search-directories)))

(defun org-nutter-capture-in-new-target-directory ()
  (interactive)
  (setq org-nutter-capture-target-directory (select-single-nutter-dir))
  (when org-nutter-capture-target-directory
    (org-capture nil "n")))

(defun org-nutter-capture ()
  (interactive)
  (if org-nutter-capture-target-directory
      (org-capture nil "n")
    (org-nutter-capture-in-new-target-directory)))

;;; org-nutter.el ends here
