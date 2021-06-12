;;; nutter.el --- Provide commands to search directories of org files

;; Version: 0.0.0
;; Package-Requires: (helm-org-rifle)

(require 'f)
(require 'helm-org-rifle)
(require 'yasnippet)

(provide 'nutter)

(defcustom nutter-root "~/.nutter" "Directory that contains the nutter directories" :type 'directory)

(defcustom nutter-yasnippet-for-new-note "title" "yasnippet to insert for a new note" :type 'string)

(defvar nutter-dirs-to-search nil "Default nutter directories to search with org-rifle")
(defvar nutter-dir-for-new-note nil "Default nutter directory for a new note")

(defun find-nutter-dirs ()
  "Return the names of the nutter directories."
  (mapcar 'f-filename (f-directories nutter-root)))

(defun select-nutter-dirs ()
  "Return the name of the nutter directories that the user selects.
If there is only one such directory, this function returns its
name without asking the user."
  (let ((nutter-dirs (find-nutter-dirs)))
    (if (eql (length nutter-dirs) 1)
        nutter-dirs
      (mapcar (lambda (dir-name) (concat (file-name-as-directory nutter-root) dir-name))
              (helm-comp-read "Select nutter directories: " nutter-dirs :marked-candidates t)))))

(defun select-single-nutter-dir ()
  "Return the name of the nutter directory that the user selects.
If there is only one such directory, this function returns its
name without asking the user."
  (let ((nutter-dirs (find-nutter-dirs)))
    (if (eql (length nutter-dirs) 1)
        nutter-dirs
      (concat (file-name-as-directory nutter-root)
              (helm-comp-read "Select single nutter directory: " nutter-dirs)))))

(defun build-filename-prompt ()
  (concat "[" (f-filename nutter-dir-for-new-note) "] Filename: "))

;; From https://stackoverflow.com/a/53738442

(defun psachin/create-notes-file ()
  "Create an org file in a subdirectory of the nutter root."
  (interactive)
  (let ((name (read-string (build-filename-prompt))))
    (expand-file-name (format "%s.org"
                              name)
                      (concat (file-name-as-directory nutter-dir-for-new-note)))))

(defun nutter-rifle-select-directories ()
  (interactive)
  (setq nutter-dirs-to-search (select-nutter-dirs))
  (when nutter-dirs-to-search
    (helm-org-rifle-directories nutter-dirs-to-search)))

(defun nutter-rifle ()
  (interactive)
  (if nutter-dirs-to-search
      (helm-org-rifle-directories nutter-dirs-to-search)
    (nutter-rifle-select-directories)))

(defun nutter-capture-select-directory ()
  (interactive)
  (setq nutter-dir-for-new-note (select-single-nutter-dir))
  (when nutter-dir-for-new-note
    (nutter-capture)))

(defun nutter-capture ()
  (interactive)
  (if nutter-dir-for-new-note
      (let ((note-path (psachin/create-notes-file)))
        (find-file note-path)
        (yas-expand-snippet (yas-lookup-snippet nutter-yasnippet-for-new-note)))
    (nutter-capture-select-directory)))

;;; nutter.el ends here
