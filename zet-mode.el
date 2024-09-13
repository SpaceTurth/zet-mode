(defun zet--generate-random-string (length)
  "Generate a random string of a given LENGTH."
  (let ((charset "abcdefghijklmnopqrstuvwxyz")
        (result ""))
    (dotimes (_ length result)
      (setq result (concat result (string (elt charset (random (length charset)))))))))

(defun zet-org-roam-capture-random-node ()
  "Capture a new org roam note for the zettelkasten."
  (interactive)
  (let ((title (concat "zet-" (zet--generate-random-string 6))))
    (org-roam-capture- :node (org-roam-node-create :title title))))

(defun zet-org-roam-capture-random-bib (author)
  "Capture a new org roam bibliographic note for the zettelkasten."
  (interactive "sEnter author:")
  (let ((title (concat "bib-" author "-" (zet--generate-random-string 6))))
    (org-roam-capture- :node (org-roam-node-create :title title))))

(defun zet--org-roam-capture-continuation-note (parent-file)
  (let ((title (concat "zet_" (zet--generate-random-string 6))))
    (org-roam-capture-
     :node (org-roam-node-create :title title)
     :templates
     `(("d" "default" plain "%?"
        :target (file+head ,(concat org-roam-directory "/" title ".org") ,(concat "#+title: ${title}\n%U\n" "Parent: " (zet--link-text parent-file)))
        :unnarrowed t)))))

(defun zet--org-roam-capture-continuation-bib (parent-file)
  (let ((title (concat "bib_" (read-string "Enter author-year:") "-" (zet--generate-random-string 6))))
    (org-roam-capture-
     :node (org-roam-node-create :title title)
     :templates
     `(("d" "default" plain "%?"
        :target (file+head ,(concat org-roam-directory "/" title ".org") ,(concat "#+title: ${title}\n%U\n" "Parent: " (zet--link-text parent-file)))
        :unnarrowed t)))))

(defun zet--files-like (like-string)
  (zet--unique-strings
   (mapcar (lambda (result) (zet--strip-directory (car result)))
           (org-roam-db-query
            [:select [file]
                     :from nodes
                     :where (like file $s1)
                     :escape $r2]
            like-string
            "?"))))

(defun zet--files-containing-string (files string)
  (let (result)
    (dolist (file files)
      (if (zerop (call-process "grep" nil nil nil "-q" "-i" "--" string file))
          (push (zet--strip-directory file) result)))
    (nreverse result)))

(defun zet--files-containing-strings (files strings)
  (let ((result (zet--all-files-with-directory)))
    (dolist (string strings)
      (setq result (zet--files-containing-string result string)))
    result))

(defun zet-selected-files-containing (string)
  (interactive "sEnter target strings:")
  (zet--files-containing-strings (zet--all-files-with-directory) (split-string string " ")))

(defun zet--unique-strings (list)
  (let ((sorted (sort list #'string-lessp))
        (result nil)
        (last ""))
    (dolist (item list)
      (if (not (string-equal last item))
          (setq result (cons item result)))
      (setq last item))
    result))

(defun zet--zet-files ()
  (zet--files-like "%zet?_%.org"))

(defun zet--bib-files ()
  (zet--files-like "%bib?_%.org"))

(defun zet--all-files ()
  (append (zet--zet-files) (zet--bib-files)))

(defun zet--all-files-with-directory ()
  (mapcar (lambda (f) (concat (expand-file-name org-roam-directory) "/" f))
          (zet--all-files)))

(defun zet--line-length ()
  (- (line-end-position) (line-beginning-position)))

(defun zet--remove-newlines (str)
  (replace-regexp-in-string "\n" "" str))

(defun zet--line-underneath-p ()
  (save-excursion
    (forward-line 1)
    (end-of-line)
    (not (eobp))))

(defun zet--truncate (str len)
  (substring str
             0
             (if (> len (length str))
                 (length str)
               len)))

(defun zet--print-columns (columns)
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
  (interactive)
  (while (and (not (looking-at-p "[ \n\t]"))
              (not (eolp)))
    (goto-char (+ (point) 1)))
  (while (and (looking-at-p "[ \n\t]")
              (not (eolp)))
    (goto-char (+ (point) 1))))

(defun zet-prev-text-object ()
  (interactive)
  (while (and (not (looking-at-p "[ \n\t]"))
              (not (bolp)))
    (goto-char (- (point) 1)))
  (while (and (looking-at-p "[ \n\t]")
              (not (bolp)))
    (goto-char (- (point) 1))))

(defun zet--files-linking-to (id)
  (mapcar (lambda (result) (zet--strip-directory (car result)))
          (org-roam-db-query
           [:select [file]
                    :from links
                    :inner :join nodes
                    :on (= links:source nodes:id)
                    :where (= links:dest $s1)]
           id)))

(defun zet--files-linked-from (id)
  (mapcar (lambda (result) (zet--strip-directory (car result)))
          (org-roam-db-query
           [:select [file]
                    :from links
                    :inner :join nodes
                    :on (= links:dest nodes:id)
                    :where (= links:source $s1)]
           id)))

(defun zet-purge ()
  (interactive)
  (let ((n-deleted 0)
        (id (org-id-store-link))
        (files (zet--files-linking-to (buffer-file-name))))
    (if files
        (save-excursion
          (dolist (file files)
            (find-file file)
            (goto-char (point-min))
            (while (re-search-forward (org-id-link-regex id) nil t)
              (replace-match "\\1")
              (setq n-deleted (1+ n-deleted))))
          (message (format "Deleted %s links" n-deleted))))))

(defun zet--prepend-directory (file)
  (if (string-match-p "^/.*" file)
      file
    (concat (file-name-as-directory (expand-file-name org-roam-directory)) file)))

(defun zet--strip-directory (file)
  (if (string-match-p "^/.*" file)
      (zet--last (string-split file "/"))
    file))

(setq zet--org-id-regex "^:ID: *\\([a-z0-9-]*\\)")
(defun zet--org-id (file)
  (with-temp-buffer
    (insert-file-contents (zet--prepend-directory file))
    (goto-char (point-min))
    (if (re-search-forward zet--org-id-regex nil t)
        (match-string 1)
      nil)))

(defun zet--org-timestamp (file)
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
  (zet--sorted-by-ts (zet--all-files)))

(defun zet--last (list)
  (car (last list)))

(defun zet--last-file ()
  (zet--last (zet--all-files-sorted-by-ts)))

(setq zet--selected-node nil)
(defun zet--current-node ()
  (let ((current (or zet--selected-node (zet--last-file))))
    (if (null zet--selected-node)
        (setq zet--selected-node current))
    current))

(defun zet--set-selected-node ()
  (setq zet--selected-node (zet--strip-directory (zet--file-at-point)))
  (message (format "Selected node %s" zet--selected-node)))

(defun zet-randomize-selected-node ()
  (interactive)
  (setq zet--selected-node (choose (zet--all-files))))

(defun zet--files-linking-to-current ()
  (zet--files-linking-to (zet--org-id (zet--current-node))))

(defun zet--files-linked-from-current ()
  (zet--files-linked-from (zet--org-id (zet--current-node))))

(defun zet--stringmul (scalar string)
  (let ((result ""))
    (dotimes (_ scalar)
      (setq result (concat result string)))
    result))

(defun zet-print-link-map ()
  (interactive)
  (zet--print-columns
   (list
    (zet--files-linked-from-current)
    (list (zet--current-node))
    (zet--files-linking-to-current)))
  (goto-char (point-min))
  (zet-next-text-object))

(defun zet--print-all-files ()
  (interactive)
  (zet--print-columns
   (list (zet--all-files-sorted-by-ts)))
  (goto-char (point-min)))

(defun zet-print-files-containing-strings (strings)
  (interactive "sEnter strings:")
  (zet--print-columns
   (list (zet--files-containing-strings (zet--all-files-with-directory) (split-string strings " "))))
  (goto-char (point-min)))

(define-derived-mode zet-mode special-mode "Zet"
  "Major mode for interacting with an org roam zettelkasten."
  (zet--initialize))

(defun zet--file-at-point ()
  (zet--prepend-directory
   (gethash (zet--text-object-at-point) zet--column-object-to-file)))

(defun zet-view-file ()
  (interactive)
  (let ((file (zet--file-at-point)))
    (if file
        (progn
          (other-window 1)
          (find-file file)
          (other-window -1)))))

(defun zet--mark-from ()
  (interactive)
  (setq zet--from-file (zet--file-at-point))
  (message (format "From link is now %s" zet--from-file)))

(defun zet--mark-to ()
  (interactive)
  (setq zet--to-file (zet--file-at-point))
  (message (format "To link is now %s" zet--to-file)))

(defun zet--link-text (file)
  (let ((id (zet--org-id file)))
    (format "[[id:%s][%s]]" id (zet--strip-directory file))))

(defun zet-link ()
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
  (setq zet--from-file nil)
  (setq zet--to-file nil))

(defun zet-main ()
  (interactive)
  (let ((buffer (get-buffer-create "*Zettelkasten*")))
    (switch-to-buffer buffer)
    (if (= 1 (length (window-list)))
        (split-window-right))
    (zet--print-all-files)
    (zet-view-file)
    (zet-mode)))

(global-set-key (kbd "C-c z") 'zet-main)

(defmacro zet--local (&rest body)
  `(lambda () (interactive) ,@body))

(general-define-key
 :keymaps 'zet-mode-map
 :states 'normal
 "p" 'zet-print-link-map
 "u" 'zet--print-all-files
 "s" 'zet-print-files-containing-strings
 "l" (zet--local (zet-next-text-object) (zet-view-file))
 "h" (zet--local (zet-prev-text-object) (zet-view-file))
 "x" 'zet-link
 "v" 'zet-view-file
 "a" 'zet--mark-from
 "b" 'zet--mark-to
 "c" (zet--local (zet--org-roam-capture-continuation-note (zet--file-at-point)))
 "i" (zet--local (zet--org-roam-capture-continuation-bib (zet--file-at-point)))
 "RET" (zet--local (zet--set-selected-node) (zet-print-link-map))
 "j" (zet--local (evil-next-visual-line) (zet-view-file))
 "k" (zet--local (evil-previous-visual-line) (zet-view-file))
 ;; TODO Remove this hack
 "r" (zet--local (zet-randomize-selected-node) (zet-print-link-map)))
