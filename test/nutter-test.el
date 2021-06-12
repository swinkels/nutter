;;; nutter-test.el --- Tests for nutter

(require 'noflet)

(require 'nutter)

(ert-deftest test-automatically-adds-dotorg-extension()
  (let ((nutter-dir-for-new-note "/home/user/.nutter/general"))
    (noflet ((nutter--ask-file-name () (identity "a-new-note")))
            (should (string= "/home/user/.nutter/general/a-new-note.org" (nutter--ask-new-note-path))))))

(ert-deftest test-automatically-adds-dotorg-extension-unless-already-present()
  (let ((nutter-dir-for-new-note "/home/user/.nutter/general"))
    (noflet ((nutter--ask-file-name () (identity "a-new-note.org")))
            (should (string= "/home/user/.nutter/general/a-new-note.org" (nutter--ask-new-note-path))))))
