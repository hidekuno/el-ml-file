;;
;; This is the mailing-list file view program by emacs lisp
;;
;; The tested Mailing List is shown below.
;;
;;   fuji-ml          (http://www.pro.or.jp/~fuji/information/mailing-list.html)
;;   ubuntu-jp        (https://lists.ubuntu.com/archives/ubuntu-jp/)
;;   gauche-devel-jp  (https://osdn.net/projects/gauche/lists/archive/devel-jp/)
;;   freebsd-users-jp (https://lists.freebsd.org/pipermail/freebsd-users-jp/)
;;
;; Build
;;   emacs -batch -f batch-byte-compile ml-file.el
;;
;; hidekuno@gmail.com
;;

;; global variable
(defvar-local ml-prog-name "ml-file")
(defvar-local ml-tree-prog (concat (getenv "HOME") "/" "el-ml-file" "/" "treeml.py"))
(defvar-local ml-grep-buffer-name "ml-grep")
(defvar-local ml-error-buffer "stderr")
(defvar ml-name-dir "~")
(defvar hl-line-face)

;;; major mode
(define-derived-mode ml-file-mode
  text-mode ml-prog-name
  "Major mode for ml-file.
\\{ml-file-mode-map}"
  (setq hl-line-face 'underline)
  (hl-line-mode)
  (setq case-fold-search t))

(define-derived-mode ml-file-detail-mode
  text-mode
  "Major mode for ml-file-detail.
\\{ml-file-detail-mode-map}")

(define-derived-mode ml-grep-mode
  text-mode
  "Major mode for ml-grep.
\\{ml-file-grep-mode-map}"
  (setq hl-line-face 'underline)
  (hl-line-mode)
  (setq case-fold-search t))

;;(set-face-attribute hl-line-face nil :underline t)
;;(put-text-property 1 10 'face 'underline)
;;(put-text-property 1 10 'face 'default)

;;; key map
(define-key ml-file-mode-map
  "\C-m" 'ml-file-link)
(define-key ml-file-mode-map
  "f"  'ml-file-link)
(define-key ml-file-mode-map
  "q" 'kill-current-buffer)
(define-key ml-file-mode-map
  "g" 'ml-grep-word)
(define-key ml-file-mode-map
  "b" 'ml-grep-back)
(define-key ml-file-mode-map
  " " 'scroll-up-command)

(define-key ml-file-detail-mode-map
  "q" 'kill-current-buffer)
(define-key ml-file-detail-mode-map
  "\C-m" 'kill-current-buffer)
(define-key ml-file-detail-mode-map
  " " 'scroll-up-command)
(define-key ml-file-detail-mode-map
  "g" 'ml-file-region-link)

(define-key ml-grep-mode-map
  "\C-m" 'ml-grep-link)
(define-key ml-grep-mode-map
  "f"  'ml-grep-link)
(define-key ml-grep-mode-map
  "q" 'kill-current-buffer)
(define-key ml-grep-mode-map
  "g" 'ml-switch-threads-tree)
(define-key ml-grep-mode-map
  " " 'scroll-up-command)

;; file open & read
(defun ml-file-open(filename)
  (find-file-read-only filename)
  (re-search-forward "^$")
  (ml-file-detail-mode))

;; (goto-line 1)
;; :Warning: goto-line is for interactive use only;
;;           use forward-line instead.
(defun ml-goto-1st-line ()
  (forward-line (- (line-number-at-pos))))

;; (replace-string "\r" "")
;; Warning: replace-string is for interactive use only;
;;          use search-forward and replace-match instead.
(defun ml-replace-string-batch()
  (while (search-forward "\r" nil t)
    (replace-match "" nil t)))

;; get range of search
(defun ml-range-of-search (str)
  (let ((s 0)(e 0))
    (setq s (point))
    (search-forward str)
    (setq e (- (point) 1))
    (cons s e)))

;; get substring of search
(defun ml-substring-of-search (str)
  (let ((range (ml-range-of-search str)))
    (buffer-substring-no-properties (car range)(cdr range))))

;; switch buffer threads-tree
(defun ml-switch-threads-tree()
  (interactive)
  (cd ml-name-dir)

  (beginning-of-line)
  (let ((filename (ml-substring-of-search ":")))
    (if (get-buffer ml-prog-name)
        (progn
          (switch-to-buffer ml-prog-name)
          (ml-goto-1st-line)
          (search-forward filename))
      (message "No ml-file"))))

;; file view by region to mail header
(defun ml-file-region-link()
  (interactive)
  (cd ml-name-dir)

  (let ((ref-id (buffer-substring-no-properties (region-beginning)(region-end))))
    (find-file-read-only (concat ml-name-dir "/idx1"))
    (ml-goto-1st-line)

    (cond
     ((search-forward ref-id nil t)
      (beginning-of-line)
      (let ((data-file (ml-substring-of-search ":")))
        (kill-current-buffer)
        (ml-file-open (concat ml-name-dir "/" data-file))))
      (t
       (message "No match message file")
       (kill-current-buffer)))))

;; file view
(defun ml-file-link()
  (interactive)
  (cd ml-name-dir)

  (unless (= 1 (line-number-at-pos))
    (beginning-of-line)
    (re-search-forward "^[ ]*")
    (ml-file-open (ml-substring-of-search " "))))

;; file view
(defun ml-grep-link()
  (interactive)
  (cd ml-name-dir)

  (beginning-of-line)

  ;; filename
  (let* ((filename (ml-substring-of-search ":"))
        ;; line number
         (line (string-to-number (ml-substring-of-search ":")))

         ;; buffer name
         (ml-buffer-name (replace-regexp-in-string "^.*/" "" filename)))
    (find-file-read-only filename)

    (switch-to-buffer ml-buffer-name)
    (forward-line (- line 1))
    (ml-file-detail-mode)))

;; grep
(defun ml-grep-word(&optional arg)
  (interactive "P")
  (cd ml-name-dir)

  (let ((word ""))
    (while (string= word "")
      (setq word (read-string "search word: ")))
    (if (get-buffer ml-grep-buffer-name)(kill-buffer ml-grep-buffer-name))
    (switch-to-buffer ml-grep-buffer-name)
    (call-process "grep" nil t nil
                  "-r" "-i" "-nH"
                  "--exclude=idx1"
                  "--exclude=idx2"
                  "-e"
                  word ".")

    (cond ((= 0 (buffer-size))
           (read-only-mode)
           (ml-grep-mode)
           (message "No matches found"))
          (t
           (ml-goto-1st-line)

           (ml-replace-string-batch)
           (sort-lines nil (point-min) (point-max))

           (ml-goto-1st-line)
           (beginning-of-line)
           (while (re-search-forward word nil t)
             (let* ((e (point))
                    (s (- e (length word))))
               (overlay-put (make-overlay s e) 'face '(t :inverse-video t))))
           (ml-goto-1st-line)
           (read-only-mode)
           (ml-grep-mode)))))

;; back ml-grep
(defun ml-grep-back()
  (interactive)
  (cd ml-name-dir)

  (cond ((get-buffer ml-grep-buffer-name)
         (switch-to-buffer ml-grep-buffer-name))
        (t
         (message "No grep results"))))

;; starup routine
(defun ml-file(&optional arg)
  (interactive "P")
  (let ((ml-name ""))
    (while (string= ml-name "")
      (setq ml-name (read-string "Mailing List Name: ")))

    (shell-command (mapconcat #'shell-quote-argument (list ml-tree-prog ml-name) " ")
                   ml-prog-name ml-error-buffer)
    (delete-other-windows)
    (cond
     ((get-buffer ml-error-buffer)
      (switch-to-buffer ml-error-buffer)
      (let ((error-message (buffer-string)))
        (kill-buffer)
        (message error-message)))
     (t
      (setq ml-name-dir (concat "~/" ml-name))
      (switch-to-buffer ml-prog-name)
      (ml-file-mode)
      (goto-char (point-min))
      (forward-line)
      (while (search-forward " " nil t)
        (beginning-of-line)
        (re-search-forward "^[ ]*")
        (let ((range (ml-range-of-search " ")))
          (put-text-property (car range) (cdr range) 'invisible t)
          (end-of-line)))
      (read-only-mode)
      (ml-goto-1st-line)))))
