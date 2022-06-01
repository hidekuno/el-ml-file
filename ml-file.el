;;
;; This is the mailing-list file view program for emacs lisp
;;
;; test ml is fuji-ml("http://www.pro.or.jp/~fuji/information/mailing-list.html")
;;
;; hidekuno@gmail.com
;;

;; global variable
(defvar-local ml-prog-name "ml-file")
(defvar-local ml-name "fuji-ml") ;; this is the ml for testing

;;; major mode
(define-derived-mode ml-file-mode
  text-mode ml-prog-name
  "Major mode for ml-file.
\\{ml-file-mode-map}"
  (setq hl-line-face 'underline)
  (hl-line-mode)
  (setq case-fold-search t))

;;(set-face-attribute hl-line-face nil :underline t)
;;(put-text-property 1 10 'face 'underline)
;;(put-text-property 1 10 'face 'default)

;;; key map
(define-key ml-file-mode-map
  "f" 'ml-file-link)

(define-key ml-file-mode-map
  "\C-m" 'ml-file-link)

;; callback routine
(defun ml-file-link()
  (interactive)
  (beginning-of-line)
  (re-search-forward "^[ ]*")
  (setq s (point))
  (re-search-forward "[ ]")
  (setq e (- (point) 1))
  (find-file-read-only
   (concat "~/" ml-name "/" (buffer-substring-no-properties s e))))

;; starup routine
(defun ml-file()
  (interactive)
  (let ((prog "./treeml.py"))
    (switch-to-buffer ml-prog-name)
    (ml-file-mode)
    (call-process prog nil t nil ml-name)
    (toggle-read-only)
    (goto-line 1)))
