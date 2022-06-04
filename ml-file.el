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

(define-key ml-file-detail-mode-map
  "q" 'kill-current-buffer)
(define-key ml-file-detail-mode-map
  "\C-m" 'kill-current-buffer)

;; callback routine
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

;; starup routine
(defun ml-file(&optional arg)
  (interactive "P")
  (let ((ml-name ""))
    (while (string= ml-name "")
      (setq ml-name (read-string "Mailing List Name: ")))

    (cd (concat "~/" ml-name))
    (switch-to-buffer ml-prog-name)
    (ml-file-mode)
    (call-process ml-tree-prog nil t nil ml-name)
    (toggle-read-only)
    (goto-line 1)))
