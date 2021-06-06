;;; org-nutter.el --- Provide commands to search directories of org files

;; Version: 0.0.0
;; Package-Requires: (helm-org-rifle)

(require 'f)
(require 'helm-org-rifle)

(provide 'org-nutter)

(defcustom org-nutter-root "~/.org-nutter" "Directory that contains the nutter directories" :type 'directory)

(defvar org-nutter-dirs-to-search nil "Default nutter directories to search with org-rifle")
(defvar org-nutter-dir-for-new-note nil "Default nutter directory for a new note")

(defun find-nutter-dirs ()
  "Return the names of the nutter directories."
  (mapcar 'f-filename (f-directories org-nutter-root)))

(defun select-nutter-dirs ()
  "Return the name of the nutter directories that the user selects.
If there is only one such directory, this function returns its
name without asking the user."
  (let ((nutter-dirs (find-nutter-dirs)))
    (if (eql (length nutter-dirs) 1)
        nutter-dirs
      (mapcar (lambda (dir-name) (concat (file-name-as-directory org-nutter-root) dir-name))
              (helm-comp-read "Select nutter directories: " nutter-dirs :marked-candidates t)))))

(defun select-single-nutter-dir ()
  "Return the name of the nutter directory that the user selects.
If there is only one such directory, this function returns its
name without asking the user."
  (let ((nutter-dirs (find-nutter-dirs)))
    (if (eql (length nutter-dirs) 1)
        nutter-dirs
      (concat (file-name-as-directory org-nutter-root)
              (helm-comp-read "Select single nutter directory: " nutter-dirs)))))

(defun build-filename-prompt ()
  (concat "[" (f-filename org-nutter-dir-for-new-note) "] Filename: "))

;; From https://stackoverflow.com/a/53738442

(defun psachin/create-notes-file ()
  "Create an org file in a subdirectory of the org-nutter root."
  (interactive)
  (let ((name (read-string (build-filename-prompt))))
    (expand-file-name (format "%s.org"
                              name)
                      (concat (file-name-as-directory org-nutter-dir-for-new-note)))))

(setq org-capture-templates
      '(("n" "Notes" entry
         (file psachin/create-notes-file)
         "*  %?")))

(defun org-nutter-rifle-new-search-directories ()
  (interactive)
  (setq org-nutter-dirs-to-search (select-nutter-dirs))
  (when org-nutter-dirs-to-search
    (helm-org-rifle-directories org-nutter-dirs-to-search)))

(defun org-nutter-rifle ()
  (interactive)
  (if org-nutter-dirs-to-search
      (helm-org-rifle-directories org-nutter-dirs-to-search)
    (org-nutter-rifle-new-search-directories)))

(defun org-nutter-capture-in-new-target-directory ()
  (interactive)
  (setq org-nutter-dir-for-new-note (select-single-nutter-dir))
  (when org-nutter-dir-for-new-note
    (org-capture nil "n")))

(defun org-nutter-capture ()
  (interactive)
  (if org-nutter-dir-for-new-note
      (org-capture nil "n")
    (org-nutter-capture-in-new-target-directory)))

;;; org-nutter.el ends here
