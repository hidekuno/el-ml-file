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
;; hidekuno@gmail.com
;;

;; global variable
(defvar-local ml-prog-name "ml-file")
(defvar-local ml-tree-prog "treeml.py") ;; install to /usr/local/bin
(defvar-local ml-grep-buffer-name "ml-grep")
(defvar-local ml-error-buffer "stderr")
(defvar ml-name-dir "~")

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

;; switch buffer threads-tree
(defun ml-switch-threads-tree()
  (cd ml-name-dir)
  (interactive)

  (beginning-of-line)
  (setq s (point))
  (search-forward ":")
  (setq e (- (point) 1))
  (setq filename (buffer-substring-no-properties s e))

  (if (get-buffer ml-prog-name)
      (progn
        (switch-to-buffer ml-prog-name)
        (goto-line 1)
        (search-forward filename))
    (message "No ml-file")))

;; file view by region to mail header
(defun ml-file-region-link()
  (interactive)
  (cd ml-name-dir)

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
  (cd ml-name-dir)

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
  (cd ml-name-dir)

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
           (toggle-read-only)
           (ml-grep-mode)
           (message "No matches found"))
          (t
           (goto-line 1)
           (replace-string "\r" "")
           (sort-lines nil (point-min) (point-max))

           (goto-line 1)
           (beginning-of-line)
           (while (re-search-forward word nil t)
             (setq e (point))
             (setq s (- e (length word)))
             (overlay-put (make-overlay s e) 'face '(t :inverse-video t)))

           (goto-line 1)
           (toggle-read-only)
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

    (shell-command (concat ml-tree-prog " " ml-name) ml-prog-name ml-error-buffer)
    (delete-other-windows)
    (cond
     ((get-buffer ml-error-buffer)
      (switch-to-buffer ml-error-buffer)
      (setq error-message (buffer-string))
      (kill-buffer)
      (message error-message))
     (t
      (setq ml-name-dir (concat "~/" ml-name))
      (switch-to-buffer ml-prog-name)
      (ml-file-mode)
      (goto-char (point-min))
      (forward-line)
      (while (search-forward " " nil t)
        (beginning-of-line)
        (re-search-forward "^[ ]*")
        (setq s (point))
        (search-forward " ")
        (setq e (- (point) 1))
        (put-text-property s e 'invisible t)
        (end-of-line))
      (toggle-read-only)
      (goto-line 1)))))
