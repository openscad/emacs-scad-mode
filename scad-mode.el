;;; scad-mode.el --- A major mode for editing OpenSCAD code -*- lexical-binding: t -*-

;; Author: Len Trigg, Łukasz Stelmach, zk_phi, Daniel Mendler
;; Maintainer: Len Trigg <lenbok@gmail.com>, Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2010
;; Keywords: languages
;; URL: https://github.com/openscad/emacs-scad-mode
;; Package-Requires: ((emacs "28.1") (compat "30"))
;; Version: 96.0

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This is a major-mode to implement the SCAD constructs and
;; font-locking for OpenSCAD.  Install the package from NonGNU ELPA or
;; MELPA:
;;
;; M-x install-package RET scad-mode RET

;;; Code:

(require 'compat)
(require 'cc-mode)
(eval-when-compile
  (require 'cc-langs)
  (require 'cc-fonts)
  (require 'cl-lib))

(defgroup scad nil
  "A major mode for editing OpenSCAD code."
  :link '(url-link :tag "Website" "https://github.com/openscad/emacs-scad-mode")
  :link '(emacs-library-link :tag "Library Source" "scad-mode.el")
  :group 'languages
  :prefix "scad-")

(defcustom scad-command
  "openscad"
  "Path to openscad executable."
  :type 'string)

(defcustom scad-extra-args
  nil
  "Additional command line arguments to pass to openscad.
For example '--enable=manifold'."
  :type '(repeat string))

(defcustom scad-keywords
  '("return" "undef" "true" "false" "for" "each" "if" "else" "let" "intersection_for"
    "function" "use" "include" "module")
  "SCAD keywords."
  :type '(repeat string))

(defcustom scad-functions
  '("cos" "acos" "sin" "asin" "tan" "atan" "atan2"                      ;;func.cc
    "abs" "sign" "rands" "min" "max"
    "round" "ceil" "floor"
    "pow" "sqrt" "exp" "log" "ln"
    "str"
    "lookup" "version" "version_num" "len" "search"
    "dxf_dim" "dxf_cross"                                               ;;dxfdim.cc
    "norm" "cross"                                                      ;;2014.03
    "concat" "chr"                                                      ;;2015.03
    "assert" "ord"                                                      ;;2019.05
    "is_undef" "is_list" "is_num" "is_bool" "is_string" "is_function")  ;;2019.05 type test
  "SCAD functions."
  :type '(repeat string))

(defcustom scad-modules
  '("children" "echo"                                                   ;;control.cc
    "cube" "sphere" "cylinder" "polyhedron" "square" "circle" "polygon" ;;primitives.cc
    "scale" "rotate" "translate" "mirror" "multmatrix"                  ;;transform.cc
    "union" "difference" "intersection"                                 ;;csgops.cc
    "render"                                                            ;;render.cc
    "color"                                                             ;;color.cc
    "surface"                                                           ;;surface.cc
    "linear_extrude"                                                    ;;linearextrude.cc
    "rotate_extrude"                                                    ;;rotateextrude.cc
    "import"                                                            ;;import.cc
    "group"                                                             ;;builtin.cc
    "projection"                                                        ;;projection.cc
    "minkowski" "hull" "resize"                                         ;;cgaladv.cc
    "parent_module"                                                     ;;2014.03
    "offset" "text")                                                    ;;2015.03
  "SCAD modules."
  :type '(repeat string))

(defcustom scad-deprecated
  '("child" "assign" "dxf_linear_extrude" "dxf_rotate_extrude"
    "import_stl" "import_off" "import_dxf")
  "SCAD deprecated modules and functions."
  :type '(repeat string))

(defcustom scad-operators
  '("+" "-" "*" "/" "%"
    "&&" "||" "!"
    "<" "<=" "==" "!=" ">" ">="
    "?" ":" "=")
  "SCAD operators."
  :type '(repeat string))

(defcustom scad-preview-projection 'perspective
  "Preview projection."
  :type '(choice (const ortho) (const perspective)))

(defcustom scad-preview-camera '(0 0 0 50 0 20 500)
  "Default parameters for the Gimbal camera."
  :type '(repeat integer))

(defcustom scad-preview-refresh 1.0
  "Delay in seconds until updating preview."
  :type '(choice (const nil) number))

(defcustom scad-preview-colorscheme '("Tomorrow" . "Tomorrow Night")
  "Color scheme for rendering preview.
Can be pair of light and dark scheme, used depending on the current
Emacs theme."
  :type '(choice string (cons string string)))

(defcustom scad-preview-view '("axes" "scales")
  "List of views to be rendered.
Options are axes, crosshairs, edges, scales, wireframe."
  :type '(repeat string))

(defcustom scad-export-extension ".stl"
  "Extension (file type) for output data file via `scad-export'.
Options are .stl, .off, .amf, .3mf, .csg, .dxf, .svg, .pdf, .png,
.echo, .ast, .term, .nef3, .nefdbg."
  :type 'string)

(defvar-keymap scad-mode-map
  :doc "Keymap for `scad-mode'."
  :parent c-mode-base-map
  "C-c C-c" #'scad-preview
  "C-c C-o" #'scad-open
  "C-c C-e" #'scad-export
  "TAB" #'indent-for-tab-command
  "M-TAB" #'completion-at-point)

(defvar scad-mode-syntax-table
  (let ((st (make-syntax-table)))
    (c-populate-syntax-table st)
    st)
  "Syntax table for `scad-mode'.")

(defvar scad-font-lock-keywords
  `(("\\(module\\|function\\)[ \t]+\\(\\sw+\\)" (1 'font-lock-keyword-face nil) (2 'font-lock-function-name-face nil t))
    ("\\(use\\|include\\)[ \t]*<\\([^>]+\\)>" (1 'font-lock-preprocessor-face nil) (2 'font-lock-type-face nil t))
    ("<\\(\\sw+\\)>\\|$\\(\\sw+\\)" . font-lock-builtin-face)
    (,(regexp-opt scad-keywords 'words)   . font-lock-keyword-face)
    (,(regexp-opt scad-modules 'words)    . font-lock-builtin-face)
    (,(regexp-opt scad-functions 'words)  . font-lock-function-name-face)
    (,(regexp-opt scad-deprecated 'words) . font-lock-warning-face)
    ;(,(regexp-opt scad-operators) . font-lock-operator-face) ;; This actually looks pretty ugly
    ;("\\(\\<\\S +\\>\\)\\s *(" 1 font-lock-function-name-face t) ;; Seems to override other stuff (e.g. in comments and builtins)
    )
  "Keyword highlighting specification for `scad-mode'.")
(defconst scad-font-lock-keywords-1 scad-font-lock-keywords)
(defconst scad-font-lock-keywords-2 scad-font-lock-keywords)
(defconst scad-font-lock-keywords-3 scad-font-lock-keywords)

(defvar scad-completions
  (append scad-keywords scad-functions scad-modules)
  "List of known words for completion.")

(put 'scad-mode 'c-mode-prefix "scad-")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.scad\\'" . scad-mode))

;;;###autoload
(define-derived-mode scad-mode prog-mode "SCAD"
  "Major mode for editing OpenSCAD code."
  :group 'scad
  :after-hook (c-update-modeline)
  (add-hook 'flymake-diagnostic-functions #'scad-flymake nil 'local)
  (add-hook 'completion-at-point-functions
            #'scad-completion-at-point nil 'local)
  (c-initialize-cc-mode t)
  (c-init-language-vars scad-mode)
  (c-common-init 'scad-mode)
  (c-set-offset 'cpp-macro 0 nil)
  (c-run-mode-hooks 'c-mode-common-hook))

(defun scad-completion-at-point ()
  "Completion at point function."
  (when-let (bounds (bounds-of-thing-at-point 'word))
    (list (car bounds) (cdr bounds)
          scad-completions
          :exclusive 'no)))

(defun scad-open ()
  "Open current buffer with `scad-command'."
  (interactive nil scad-mode)
  (save-buffer)
  (call-process scad-command nil 0 nil (buffer-file-name)))

(defun scad-export (file)
  "Render and export current SCAD model to FILE."
  (interactive
   (list (read-file-name
          "Export to: "
          nil nil nil
          (concat (file-name-base (buffer-file-name))
                  scad-export-extension)))
   scad-mode)
  (save-buffer)
  (compile (string-join (append (list scad-command)
                                (list "-o"
                                      (shell-quote-argument (expand-file-name file))
                                      (shell-quote-argument (buffer-file-name)))
                                scad-extra-args) " ")))

(defvar-local scad--preview-buffer      nil)
(defvar-local scad--preview-proc        nil)
(defvar-local scad--preview-image       nil)
(defvar-local scad--preview-mode-status nil)
(defvar-local scad--preview-mode-camera nil)
(defvar-local scad--preview-timer       nil)

(defvar-keymap scad-preview-mode-map
  :doc "Keymap for SCAD preview buffers."
  "p" #'scad-preview-projection
  "-" #'scad-preview-distance-
  "+" #'scad-preview-distance+
  "<right>" #'scad-preview-rotate-z-
  "<left>" #'scad-preview-rotate-z+
  "<up>" #'scad-preview-rotate-x+
  "<down>" #'scad-preview-rotate-x-
  "M-<left>" #'scad-preview-translate-x+
  "M-<right>" #'scad-preview-translate-x-
  "M-<up>" #'scad-preview-translate-z-
  "M-<down>" #'scad-preview-translate-z+)

(defun scad--preview-status (status)
  "Update mode line of preview buffer with STATUS."
  (setq scad--preview-mode-camera (apply #'format "[%d %d %d] [%d %d %d] %d"
                                         scad-preview-camera)
        scad--preview-mode-status status)
  (force-mode-line-update))

(defun scad-preview ()
  "Preview SCAD models in real-time within Emacs."
  (interactive nil scad-mode)
  (unless (buffer-live-p scad--preview-buffer)
    (setq scad--preview-buffer
          (with-current-buffer (get-buffer-create (format "*scad preview: %s*" (buffer-name)))
            (scad-preview-mode)
            (current-buffer))))
  (when scad-preview-refresh
    (add-hook 'after-change-functions #'scad--preview-change nil 'local))
  (let ((orig-buffer (current-buffer)))
    (with-current-buffer scad--preview-buffer
      (setq scad--preview-buffer orig-buffer)
      (scad--preview-reset))))

(defun scad--preview-change (&rest _)
  "Buffer changed, trigger rerendering."
  (if (not (buffer-live-p scad--preview-buffer))
      (remove-hook 'after-change-functions #'scad--preview-change 'local)
    (let ((buffer scad--preview-buffer))
      (with-current-buffer buffer
        (scad--preview-kill)
        (scad--preview-status "Stale")
        (setq scad--preview-timer
              (run-with-timer
               scad-preview-refresh nil
               (lambda ()
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (setq scad--preview-timer nil)
                     (scad--preview-render))))))))))

(defun scad--preview-colorscheme ()
  "Color scheme depending on Emacs theme."
  (cond
   ((stringp scad-preview-colorscheme)
    scad-preview-colorscheme)
   ((color-dark-p (color-name-to-rgb (face-background 'default)))
    (cdr scad-preview-colorscheme))
   (t (car scad-preview-colorscheme))))

(defun scad--preview-reset (&rest _)
  "Reset camera settings and render."
  (setq-local scad-preview-camera (copy-sequence (default-value 'scad-preview-camera))
              scad-preview-projection (default-value 'scad-preview-projection))
  (scad--preview-render))

;; Based on https://github.com/zk-phi/scad-preview
(defun scad--preview-render (&rest _)
  "Render image from current buffer."
  (if (not (buffer-live-p scad--preview-buffer))
      (scad--preview-status "Dead")
    (scad--preview-kill)
    (scad--preview-status "Render")
    (let* ((infile (make-temp-file "scad-preview-" nil ".scad"))
           (basefile (file-name-sans-extension infile))
           (outfile (concat basefile ".tmp.png"))
           (buffer (current-buffer))
           (win (or (get-buffer-window buffer)
                    (display-buffer
                     buffer '(nil (inhibit-same-window . t))))))
      (with-current-buffer scad--preview-buffer
        (save-restriction
          (widen)
          (write-region (point-min) (point-max) infile nil 'nomsg)))
      (with-environment-variables
          ;; Setting the OPENSCADPATH to the current directory allows openscad to pick
          ;; up other local files with `include <file.scad>'.
          (("OPENSCADPATH"
            (if-let ((path (getenv "OPENSCADPATH")))
                (concat default-directory path-separator path)
              default-directory)))
        (setq scad--preview-proc
              (make-process
               :noquery t
               :connection-type 'pipe
               :name "scad-preview"
               :buffer "*scad preview output*"
               :sentinel
               (lambda (proc _event)
                 (unwind-protect
                     (when (and (buffer-live-p buffer)
                                (memq (process-status proc) '(exit signal)))
                       (with-current-buffer buffer
                         (setq scad--preview-proc nil)
                         (if (not (ignore-errors
                                    (and (file-exists-p outfile)
                                         (> (file-attribute-size (file-attributes outfile)) 0))))
                             (scad--preview-status "Error")
                           (with-silent-modifications
                             (scad--preview-delete)
                             (setq scad--preview-image (concat basefile ".png"))
                             (rename-file outfile scad--preview-image)
                             (erase-buffer)
                             (insert (propertize "#" 'display `(image :type png :file ,scad--preview-image))))
                           (scad--preview-status "Done"))))
                   (delete-file infile)
                   (delete-file outfile)))
               :command
               (append
                (list scad-command
                      "-o" outfile
                      "--preview"
                      (format "--projection=%s" scad-preview-projection)
                      (format "--imgsize=%d,%d"
                              (window-pixel-width win)
                              (window-pixel-height win))
                      (format "--view=%s"
                              (mapconcat #'identity scad-preview-view ","))
                      (format "--camera=%s"
                              (mapconcat #'number-to-string scad-preview-camera ","))
                      (format "--colorscheme=%s" (scad--preview-colorscheme))
                      infile)
                scad-extra-args)))))))

(defun scad--preview-kill ()
  "Kill current rendering."
  (when (process-live-p scad--preview-proc)
    (delete-process scad--preview-proc)
    (setq scad--preview-proc nil))
  (when scad--preview-timer
    (cancel-timer scad--preview-timer)
    (setq scad--preview-timer nil)))

(defun scad--preview-delete ()
  "Delete current image."
  (when scad--preview-image
    (delete-file scad--preview-image)
    (setq scad--preview-image nil)))

(define-derived-mode scad-preview-mode special-mode "SCAD/Preview"
  "Major mode for SCAD preview buffers."
  :interactive nil :abbrev-table nil :syntax-table nil
  (setq-local buffer-read-only t
              line-spacing nil
              cursor-type nil
              cursor-in-non-selected-windows nil
              left-fringe-width 1
              right-fringe-width 1
              left-margin-width 0
              right-margin-width 0
              truncate-lines nil
              show-trailing-whitespace nil
              display-line-numbers nil
              fringe-indicator-alist '((truncation . nil))
              revert-buffer-function #'scad--preview-reset
              mode-line-position '(" " scad--preview-mode-camera)
              mode-line-process '(" " scad--preview-mode-status)
              mode-line-modified nil
              mode-line-mule-info nil
              mode-line-remote nil)
  (add-hook 'kill-buffer-hook #'scad--preview-kill nil 'local)
  (add-hook 'kill-buffer-hook #'scad--preview-delete nil 'local)
  (add-hook 'window-size-change-functions
            (let ((buf (current-buffer)))
              (lambda (_)
                (with-current-buffer buf
                  (scad--preview-render))))
            nil 'local))

(defun scad-preview-projection ()
  "Toggle projection."
  (interactive nil scad-preview-mode)
  (setq-local scad-preview-projection
              (if (eq scad-preview-projection 'ortho)
                  'perspective
                'ortho))
  (scad--preview-render))

(defmacro scad--define-preview-move (name idx off)
  "Define camera move function NAME which increments IDX by OFF."
  `(defun ,(intern (format "scad-preview-%s" name)) (&optional offset)
     "Move camera by OFFSET."
     (interactive "P" scad-preview-mode)
     (cl-incf (nth ,idx scad-preview-camera)
              (* (cl-signum ,off)
                 (if offset (prefix-numeric-value offset) ,(abs off))))
     (scad--preview-render)))

(scad--define-preview-move translate-x+ 0 10)
(scad--define-preview-move translate-x- 0 -10)
(scad--define-preview-move translate-y+ 1 10)
(scad--define-preview-move translate-y- 1 -10)
(scad--define-preview-move translate-z+ 2 10)
(scad--define-preview-move translate-z- 2 -10)
(scad--define-preview-move rotate-x+ 3 20)
(scad--define-preview-move rotate-x- 3 -20)
(scad--define-preview-move rotate-y+ 4 20)
(scad--define-preview-move rotate-y- 4 -20)
(scad--define-preview-move rotate-z+ 5 20)
(scad--define-preview-move rotate-z- 5 -20)
(scad--define-preview-move distance- 6 100)
(scad--define-preview-move distance+ 6 -100)

(defvar-local scad--flymake-proc nil)

(defun scad-flymake (report-fn &rest _args)
  "Flymake backend, diagnostics are passed to REPORT-FN."
  (unless (executable-find scad-command)
    (error "Cannot find `%s'" scad-command))
  (when (process-live-p scad--flymake-proc)
    (delete-process scad--flymake-proc))
  (let* ((buffer (current-buffer))
         (infile (make-temp-file "scad-flymake-" nil ".scad"))
         (outfile (concat (file-name-sans-extension infile) ".ast")))
    (save-restriction
      (widen)
      (write-region (point-min) (point-max) infile nil 'nomsg))
    (with-environment-variables
        ;; Setting the OPENSCADPATH to the current directory allows openscad to pick
        ;; up other local files with `include <file.scad>'.
        (("OPENSCADPATH"
          (if-let ((path (getenv "OPENSCADPATH")))
              (concat default-directory path-separator path)
            default-directory)))
      (setq
       scad--flymake-proc
       (make-process
        :name "scad-flymake"
        :noquery t
        :connection-type 'pipe
        :buffer (generate-new-buffer " *scad-flymake*")
        :command (append (list scad-command "-o" outfile infile) scad-extra-args)
        :sentinel
        (lambda (proc _event)
          (when (memq (process-status proc) '(exit signal))
            (unwind-protect
                (when (and (buffer-live-p buffer)
                           (eq proc (buffer-local-value 'scad--flymake-proc buffer)))
                  (with-current-buffer (process-buffer proc)
                    (goto-char (point-min))
                    (let (diags)
                      (while (search-forward-regexp "^\\(ERROR\\|WARNING\\): \\(.*?\\),? in file [^,]+, line \\([0-9]+\\)" nil t)
                        (let ((msg (match-string 2))
                              (type (if (equal (match-string 1) "ERROR") :error :warning))
                              (region (flymake-diag-region buffer (string-to-number (match-string 3)))))
                          (push (flymake-make-diagnostic buffer (car region) (cdr region) type msg) diags)))
                      (funcall report-fn (nreverse diags)))))
              (delete-file outfile)
              (delete-file infile)
              (kill-buffer (process-buffer proc))))))))))

;; Print warning if the scad-preview package is installed
(when (require 'scad-preview nil 'noerror)
  (warn "The scad-preview package should be removed, it has been merged into scad-mode"))

(provide 'scad-mode)
;;; scad-mode.el ends here
