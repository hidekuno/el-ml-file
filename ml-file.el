;;
;; This is the mailing-list file view program by emacs lisp
;;
;; test ml is fuji-ml("http://www.pro.or.jp/~fuji/information/mailing-list.html")
;;
;; hidekuno@gmail.com
;;

;; global variable
(defvar-local ml-prog-name "ml-file")
(defvar-local ml-tree-prog "./treeml.py") ;; install to /usr/local/bin
(setq ml-name-dir "~")

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

;; file view by region to mail header
(defun ml-file-region-link()
  (interactive)
  (setq ref-id (buffer-substring-no-properties (region-beginning)(region-end)))
  (find-file-read-only (concat ml-name-dir "/idx1"))
  (goto-line 1)

  (cond
   ((search-forward ref-id nil t)
    (beginning-of-line)
    (setq s (point))
    (search-forward ":")
    (setq e (- (point) 1))
    (setq data-file (buffer-substring-no-properties s e))
    (kill-current-buffer)
    (find-file-read-only (concat ml-name-dir "/" data-file))
    (ml-file-detail-mode))
   (t
    (message "No match message file")
    (kill-current-buffer))))

;; file view
(defun ml-file-link()
  (interactive)
  (unless (= 1 (line-number-at-pos))
    (beginning-of-line)
    (re-search-forward "^[ ]*")
    (setq s (point))
    (search-forward " ")
    (setq e (- (point) 1))
    (find-file-read-only (buffer-substring-no-properties s e))
    (ml-file-detail-mode)))

;; file view
(defun ml-grep-link()
  (interactive)
  (beginning-of-line)

  ;; filename
  (setq s (point))
  (search-forward ":")
  (setq e (- (point) 1))
  (setq filename (buffer-substring-no-properties s e))

  ;; line number
  (setq s (point))
  (search-forward ":")
  (setq e (- (point) 1))
  (setq line (string-to-number (buffer-substring-no-properties s e)))

  ;; buffer name
  (setq ml-buffer-name (replace-regexp-in-string "^.*/" "" filename))
  (find-file-read-only filename)

  (switch-to-buffer ml-buffer-name)
  (goto-line line)
  (ml-file-detail-mode))

;; grep
(defun ml-grep-word(&optional arg)
  (interactive "P")
  (let ((word ""))
    (while (string= word "")
      (setq word (read-string "search word: ")))
    (switch-to-buffer "ml-grep")
    (call-process "grep" nil t nil
                  "-r" "-i" "-nH"
                  "--exclude=idx1"
                  "--exclude=idx2"
                  "-e"
                  word "."))

  (cond ((= 0 (buffer-size))
         (toggle-read-only)
         (ml-grep-mode)
         (message "No matches found"))
        (t
         (goto-line 1)
         (replace-string "\r" "")
         (sort-lines nil (point-min) (point-max))
         (goto-line 1)
         (toggle-read-only)
         (ml-grep-mode))))

;; starup routine
(defun ml-file(&optional arg)
  (interactive "P")
  (let ((ml-name ""))
    (while (string= ml-name "")
      (setq ml-name (read-string "Mailing List Name: ")))

    (setq ml-name-dir (concat "~/" ml-name))
    (cd ml-name-dir)
    (switch-to-buffer ml-prog-name)
    (ml-file-mode)
    (call-process ml-tree-prog nil t nil ml-name)
    (toggle-read-only)
    (goto-line 1)))
