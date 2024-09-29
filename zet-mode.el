;;; package --- Summary
;;; Commentary:
;;; A mode for managing your Zettelkasten in Emacs.
;;; Code:

(require 'org-roam)

(setq zet--org-link-id-regex "^[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}$")
(setq zet--org-file-id-regex "^:ID: *\\([a-z0-9-]*\\)")

(defun zet--generate-random-string (length)
  "Generate a random string lowercase characters of a given LENGTH."
  (let ((charset "abcdefghijklmnopqrstuvwxyz")
        (result ""))
    (dotimes (_ length result)
      (setq result (concat result (string (elt charset (random (length charset)))))))))

(defun zet-org-roam-capture-random-node ()
  "Capture a new org roam note for the zettelkasten."
  (interactive)
  (let ((title (concat "zet-" (zet--generate-random-string 6))))
    (org-roam-capture- :node (org-roam-node-create :title title))))

(defun zet-org-roam-capture-random-bib (author-year)
  "Capture a new bibliographic note for the zettelkasten.
AUTHOR-YEAR is a string of the form <name>-<year>, for example, pirsig-74."
  (interactive "sEnter author:")
  (let ((title (concat "bib-" author-year "-" (zet--generate-random-string 6))))
    (org-roam-capture- :node (org-roam-node-create :title title))))

(defun zet--org-roam-capture-continuation-note (parent-file)
  "Capture a new note and insert a link to PARENT-FILE."
  (let ((title (concat "zet_" (zet--generate-random-string 6))))
    (org-roam-capture-
     :node (org-roam-node-create :title title)
     :templates
     `(("d" "default" plain "%?"
        :target (file+head ,(concat org-roam-directory "/" title ".org") ,(concat "#+title: ${title}\n%U\n" "Parent: " (zet--link-text parent-file)))
        :unnarrowed t)))))

(defun zet--org-roam-capture-continuation-bib (parent-file)
  "Capture a new bibliographic note and insert a link to PARENT-FILE."
  (let ((title (concat "bib_" (read-string "Enter author-year:") "-" (zet--generate-random-string 6))))
    (org-roam-capture-
     :node (org-roam-node-create :title title)
     :templates
     `(("d" "default" plain "%?"
        :target (file+head ,(concat org-roam-directory "/" title ".org") ,(concat "#+title: ${title}\n%U\n" "Parent: " (zet--link-text parent-file)))
        :unnarrowed t)))))

(defun zet--files-like (like-string)
  "Return all files whose names names match the SQL query defined by LIKE-STRING, with ? as an escape character."
  (zet--unique-strings
   (mapcar (lambda (result) (zet--strip-directory (car result)))
           (org-roam-db-query
            [:select [file]
                     :from nodes
                     :where (like file $s1)
                     :escape $r2]
            like-string
            "?"))))

(defun zet--org-roam-files-with-no-links ()
  "Return a list of Org-roam files with no incoming or outgoing links."
  (interactive)
  (let* ((query "SELECT file FROM nodes WHERE id NOT IN (SELECT source FROM links) AND id NOT IN (SELECT dest FROM links);")
         (result (org-roam-db-query query)))
    (if result
        (let ((files (mapcar (lambda (result) (zet--strip-directory (car result))) result)))
          files)
      (message "No files found with no links."))))


(defun zet--files-containing-string (files string)
  "Return the sublist of FILES which contain STRING."
  (let (result)
    (dolist (file files)
      (if (zerop (call-process "grep" nil nil nil "-q" "-i" "--" string file))
          (push (zet--strip-directory file) result)))
    (nreverse result)))

(defun zet--files-containing-strings (files strings)
  "Return the sublist of FILES which contain all of the specified STRINGS."
  (let ((result (zet--all-files-with-directory)))
    (dolist (string strings)
      (setq result (zet--files-containing-string result string)))
    result))

(defun zet-selected-files-containing (string)
  "Return the list of nodes which contain all of the space-separated strings passed by STRING."
  (interactive "sEnter target strings:")
  (zet--files-containing-strings (zet--all-files-with-directory) (split-string string " ")))

(defun zet--unique-strings (list)
  "Return LIST sorted alphabetically and with non-unique occurrences removed."
  (let ((sorted (sort list #'string-lessp))
        (result nil)
        (last ""))
    (dolist (item list)
      (if (not (string-equal last item))
          (setq result (cons item result)))
      (setq last item))
    result))

(defun zet--zet-files ()
  "Return all files holding zettelkasten notes."
  (zet--files-like "%zet?_%.org"))

(defun zet--bib-files ()
  "Return all files containing bibliographic notes."
  (zet--files-like "%bib?_%.org"))

(defun zet--all-files ()
  "Return a list containing all zettelkasten notes and bibliographic notes."
  (append (zet--zet-files) (zet--bib-files)))

(defun zet--all-files-with-directory ()
  "Return a list containing all zettelkasten notes and bibliographic notes, with the parent directory attached."
  (mapcar (lambda (f) (concat (expand-file-name org-roam-directory) "/" f))
          (zet--all-files)))

(defun zet--line-length ()
  "Return the length of the current line."
  (- (line-end-position) (line-beginning-position)))

(defun zet--remove-newlines (str)
  "Return STR with newlines removed."
  (replace-regexp-in-string "\n" "" str))

(defun zet--line-underneath-p ()
  "Return non-nil iff there is a line underneath the current one."
  (save-excursion
    (forward-line 1)
    (end-of-line)
    (not (eobp))))

(defun zet--truncate (str len)
  "Truncate STR to at most LEN characters."
  (substring str
             0
             (if (> len (length str))
                 (length str)
               len)))

(defun zet--print-columns (columns)
  "Print COLUMNS nicely in the current buffer."
  (setq buffer-read-only nil)
  (erase-buffer)
  (setq zet--column-object-to-file (make-hash-table :test #'equal))
  (let ((n-col 0)
        (width (/ (window-width) (length columns))))
    (dolist (column columns)
      (goto-char (point-min))
      (dolist (item column)
        (end-of-line)
        (dotimes (_ (- (* n-col width) (zet--line-length)))
          (insert " "))
        (let* ((string-item (format "%s" item))
               (formatted-item (zet--truncate string-item (- width 1))))
          (insert formatted-item)
          (puthash formatted-item item zet--column-object-to-file))
        (if (zet--line-underneath-p)
            (forward-line 1)
          (newline)))
      (setq n-col (+ n-col 1)))))

(defun zet--text-object-at-point ()
  "Get the contiguous text object under the point."
  (let ((start (point))
        (end (point))
        (original (point)))
    (while (and (not (bolp))
                (not (looking-back "[ \t\n]" 1)))
      (setq start (- start 1))
      (goto-char start))
    (while (and (not (eolp))
                (not (looking-at "[ \t\n]")))
      (setq end (+ end 1))
      (goto-char end))
    (goto-char original)
    (zet--remove-newlines (buffer-substring-no-properties start end))))

(defun zet-next-text-object ()
  "Move forward to the next text object on the line."
  (interactive)
  (while (and (not (looking-at-p "[ \n\t]"))
              (not (eolp)))
    (goto-char (+ (point) 1)))
  (while (and (looking-at-p "[ \n\t]")
              (not (eolp)))
    (goto-char (+ (point) 1))))

(defun zet-prev-text-object ()
  "Move backward to the previous text object on the line."
  (interactive)
  (while (and (not (looking-at-p "[ \n\t]"))
              (not (bolp)))
    (goto-char (- (point) 1)))
  (while (and (looking-at-p "[ \n\t]")
              (not (bolp)))
    (goto-char (- (point) 1))))

(defun zet--files-linking-to (id)
  "Return a list of nodes linking to ID."
  (mapcar (lambda (result) (zet--strip-directory (car result)))
          (org-roam-db-query
           [:select [file]
                    :from links
                    :inner :join nodes
                    :on (= links:source nodes:id)
                    :where (= links:dest $s1)]
           id)))

(defun zet--files-linked-from (id)
  "Return a list of nodes which are linked to from the node with id ID."
  (mapcar (lambda (result) (zet--strip-directory (car result)))
          (org-roam-db-query
           [:select [file]
                    :from links
                    :inner :join nodes
                    :on (= links:dest nodes:id)
                    :where (= links:source $s1)]
           id)))

(defun zet--dead-links ()
  "Return a list of links with no target."
  (let* ((query "SELECT source, dest, file
               FROM links INNER JOIN nodes
               ON links.source = nodes.id
               AND dest NOT IN (SELECT id FROM nodes)")
         (results (org-roam-db-query query))
         (to-modify '()))
    (dolist (result results)
      (if (string-match-p zet--org-link-id-regex (cadr result))
          (setq to-modify (cons result to-modify))))
    (setq to-modify
          (sort to-modify #'(lambda (a b) (string-lessp (caddr a) (caddr b)))))
    to-modify))

(defun zet-prune-dead-links ()
  "Visit all files in the zettelkasten and remove their dead links, replacing the org link with its text representation."
  (interactive)
  (let ((n-pruned 0)
        (to-modify (zet--dead-links)))
    (if to-modify
        (save-excursion
          (dolist (modify to-modify)
            (if (y-or-n-p (format "Really prune %s in %s?" (cadr modify) (caddr modify)))
                (progn
                  (find-file (caddr modify))
                  (goto-char (point-min))
                  (while (re-search-forward (org-id-link-regex (cadr modify)) nil t)
                    (replace-match "\\1")
                    (message "Replaced lin")
                    (setq n-pruned (1+ n-pruned))))))
          (message (format "Pruned %s links" n-pruned)))
      (message "No dead links, congratulations!"))))

(defun zet--prepend-directory (file)
  "Prepend the org-roam-directory to FILE if it is not yet an absolute path."
  (if (string-match-p "^/.*" file)
      file
    (concat (file-name-as-directory (expand-file-name org-roam-directory)) file)))

(defun zet--strip-directory (file)
  "Remove the directory component of FILE if it is an absolute path."
  (if (string-match-p "^/.*" file)
      (zet--last (string-split file "/"))
    file))

(defun zet--org-id (file)
  "Get the org roam id of FILE."
  (with-temp-buffer
    (insert-file-contents (zet--prepend-directory file))
    (goto-char (point-min))
    (if (re-search-forward zet--org-file-id-regex nil t)
        (match-string 1)
      nil)))

(defun zet--org-timestamp (file)
  "Return the org timestamp of FILE if it exists, else nil."
  (let ((full-file (zet--prepend-directory file)))
    (if (file-exists-p full-file)
        (with-temp-buffer
          (insert-file-contents full-file)
          (goto-char (point-min))
          (if (re-search-forward org-ts-regexp-inactive nil t)
              (match-string 0)
            nil))
      nil)))

(defun zet--sorted-by-ts (files)
  "Sort the org roam FILES by their timestamp, with nil timestamp files at the start."
  (let ((file-to-ts
         (mapcar
          (lambda (file) (cons file (zet--org-timestamp file)))
          files)))
    (mapcar #'car
            (sort file-to-ts
                  (lambda (a b)
                    (let ((ta (cdr a))
                          (tb (cdr b)))
                      (cond
                       ((and (not ta) (not tb)) nil)
                       ((not ta) t)
                       ((not tb) nil)
                       (t (time-less-p (org-time-string-to-time ta)
                                       (org-time-string-to-time tb))))))))))

(defun zet--all-files-sorted-by-ts ()
  "Return a list of all files in the zettelkasten sorted by their timestamp."
  (zet--sorted-by-ts (zet--all-files)))

(defun zet--last (list)
  "Return the car of the last element of LIST."
  (car (last list)))

(defun zet--last-file ()
  "Return the last file which was added to the zettelkasten.
Useful for identifying a node to continue your train of thought from."
  (zet--last (zet--all-files-sorted-by-ts)))

(defun zet--current-node ()
  "Return the 'current' node of the zettelkasten, which is either the last added node or the selected node."
  (let ((current (or zet--selected-node (zet--last-file))))
    (if (null zet--selected-node)
        (setq zet--selected-node current))
    current))

(defun zet--set-selected-node ()
  "Set the selected node to the file currently under the point."
  (setq zet--selected-node (zet--strip-directory (zet--file-at-point)))
  (message (format "Selected node %s" zet--selected-node)))

(defun zet-randomize-selected-node ()
  "Randomize the current selected node.
Useful for taking a random walk through your slipbox."
  (interactive)
  (setq zet--selected-node (choose (zet--all-files))))

(defun zet--files-linking-to-current ()
  "Return a list of files linking to the curren file."
  (zet--files-linking-to (zet--org-id (zet--current-node))))

(defun zet--files-linked-from-current ()
  "Return a list of files linked to from the current file."
  (zet--files-linked-from (zet--org-id (zet--current-node))))

(defun zet--stringmul (scalar string)
  "Return the string formed by concatenating STRING to itself SCALAR times."
  (let ((result ""))
    (dotimes (_ scalar)
      (setq result (concat result string)))
    result))

(defun zet-print-link-map ()
  "Print three columns. The files which link to the current file, the current file, and files linked to from the current file."
  (interactive)
  (zet--print-columns
   (list
    (zet--files-linked-from-current)
    (list (zet--current-node))
    (zet--files-linking-to-current)))
  (goto-char (point-min))
  (zet-next-text-object))

(defun zet--print-all-files ()
  "Print all files in the zettelkasten, sorted by timestamp."
  (interactive)
  (zet--print-columns
   (list (zet--all-files-sorted-by-ts)))
  (goto-char (point-min)))

(defun zet-print-isolated-files ()
  "Print the files in the zettelkasten which are not linked to and link to no other files."
  (interactive)
  (zet--print-columns
   (list (zet--sorted-by-ts (zet--org-roam-files-with-no-links))))
  (goto-char (point-min)))

(defun zet-print-files-containing-strings (strings)
  "Print the files which contain all space-separated strings passed in STRINGS."
  (interactive "sEnter strings:")
  (zet--print-columns
   (list (zet--files-containing-strings (zet--all-files-with-directory) (split-string strings " "))))
  (goto-char (point-min)))

(define-derived-mode zet-mode special-mode "Zet"
  "Major mode for interacting with an org roam zettelkasten."
  (zet--initialize))

(defun zet--file-at-point ()
  "Return the fully qualified file name of the file under the point."
  (zet--prepend-directory
   (gethash (zet--text-object-at-point) zet--column-object-to-file)))

(defun zet-view-file ()
  "Open a file for viewing."
  (interactive)
  (let ((file (zet--file-at-point)))
    (if file
        (progn
          (other-window 1)
          (find-file file)
          (other-window -1)))))

(defun zet--mark-from ()
  "Mark the file under the point as the file to link from."
  (interactive)
  (setq zet--from-file (zet--file-at-point))
  (message (format "From link is now %s" zet--from-file)))

(defun zet--mark-to ()
  "Mark the file under the point as the file to link to."
  (interactive)
  (setq zet--to-file (zet--file-at-point))
  (message (format "To link is now %s" zet--to-file)))

(defun zet-mark-for-deletion ()
  "Mark the file under the point for deletion."
  (interactive)
  (push (zet--file-at-point) zet--to-trash)
  (message (format "Marked %s for deletion" (zet--file-at-point))))

(defun zet-move-file-to-trash (file)
  "Move FILE to ~/.trash."
  (interactive "fMove file to trash: ")
  (let ((trash-dir (expand-file-name "~/.trash"))
        (filename (file-name-nondirectory file)))
    (unless (file-directory-p trash-dir)
      (make-directory trash-dir t))
    (rename-file file (expand-file-name filename trash-dir))
    (message "Moved %s to %s" file trash-dir)))

(defun zet-trash-files ()
  "Move all files marked for deletion to the trash."
  (interactive)
  (setq zet--to-trash
        (zet--filter (lambda (f) (file-exists-p f)) zet--to-trash))
  (dolist (file zet--to-trash)
    (message (format "Trashing %s" file))
    (zet-move-file-to-trash file)))

(defun zet--filter (filter list)
  "Return the sublist of LIST of elements which pass the FILTER, a function of a single argument."
  (let ((result '()))
    (dolist (thing list)
      (if (funcall filter thing)
          (push thing result)))
    result))

(defun zet--link-text (file)
  "Return the org link text for linking to FILE."
  (let ((id (zet--org-id file)))
    (format "[[id:%s][%s]]" id (zet--strip-directory file))))

(defun zet-link ()
  "Insert a link in the 'from' file to the 'to' file."
  (interactive)
  (if (null zet--from-file)
      (user-error "Must set from file"))
  (if (null zet--to-file)
      (user-error "Must set to file"))
  (save-window-excursion
    (let ((link-text (zet--link-text zet--from-file)))
      (find-file zet--to-file)
      (goto-char (point-min))
      (message (format "Inserting link in %s" zet--to-file))
      (if (re-search-forward org-ts-regexp-inactive nil t)
          (progn
            (newline)
            (insert link-text)
            (zet-view-file))
        (user-error "No timestamp to link after")))))

(defun zet--replace (text replacement)
  "Replace the next instance of TEXT with REPLACEMENT."
  (save-excursion
    (if (search-forward text nil t)
        (progn
          (goto-char (- (point) (length text)))
          (delete-char (min
                        (max (length text) (length replacement))
                        (- (line-end-position) (point))))
          (insert replacement))
      (user-error "No occurrence of" text))))

(defun zet--initialize ()
  "Initialize zet mode, setting state variables to their initial values."
  (setq zet--selected-node nil)
  (setq zet--from-file nil)
  (setq zet--to-file nil)
  (setq zet--to-trash nil))

(defun zet-main ()
  "Entrypoint for zet mode. Call the initialization function and open an initial view into the zettelkasten."
  (interactive)
  (let ((buffer (get-buffer-create "*Zettelkasten*")))
    (switch-to-buffer buffer)
    (if (= 1 (length (window-list)))
        (split-window-right))
    (zet--print-all-files)
    (zet-view-file)
    (zet-mode)))

(defun zet--current-ids ()
  "Return a list of the org ids of each file in the zettelkasten."
  (mapcar #'car
          (org-roam-db-query
           [:select [id]
                    :from nodes
                    :where (like file "%zet?_%.org")
                    :or (like file "%bib?_%.org")
                    :escape $r1]
           "?")))

(defmacro zet--local (&rest body)
  "Macro for defining functions more easily using general.el. Function body is the list of forms in BODY."
  `(lambda () (interactive) ,@body))

(global-set-key (kbd "C-c z") 'zet-main)
(provide 'zet-mode)
;;; zet-mode.el ends here
