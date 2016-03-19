;; .emacs --- fichier de configuration général de GNU Emacs

;; ***Date de création : 14/03/2016***
;; ***Dernière modification le 19/03/2016 à 09:34:28***

;; Raccourcis intéressants :
;; C-M-n : curseur à la fin d'un bloc
;; C-M-p : curseur au début d'un bloc
;; C-h b : tous les raccourcis possibles pour le buffer courant
;; C-t : inverse deux lettres
;; C-x <backspace> : supprime le paragraphe précédent
;; C-x SPC : marque pour sélection en rectangle
;; C-x 4 C-f : affiche un nouveau buffer avec le fichier recherché
;; C-x r t : remplace un rectangle par les caractères renseignés
;; C-x C-b : liste des buffers
;; C-x C-o : une seule ligne vide entre deux lignes trop espacées
;; C-x C-t : inverse deux lignes
;; M-% : demande remplacement mot
;; M-a : place le curseur au début d'une phrase
;; M-d : supprime le mot suivant le curseur
;; M-e : curseur en fin de phrase
;; M-k : supprime le prochain paragraphe
;; M-m : place le curseur au début d'une ligne indentée
;; M-t : inverse deux mots
;; M-x describe-minor-mode : raccourcis clavier utilisé par un mode mineur
;; M-x hs-minor-mode : affiche/cache des bouts de codes
;; M-SPC : une seule espace entre deux mots d'une seule ligne

;; Liste des répertoires où Emacs doit chercher ses fichiers de configuration
(let ((default-directory "~/.emacs.d/lisp"))
  (setq load-path
	(append
	 (let ((load-path (copy-sequence load-path))) ;; Shadow
	   (append
	    (copy-sequence (normal-top-level-add-to-load-path '(".")))
	    (normal-top-level-add-subdirs-to-load-path)))
	 load-path)))

(blink-cursor-mode 0) ;;;;;;;;;;;;;;;;;;;;;;;;;;;; curseur fixe
(defun startup-echo-area-message () "") ;;;;;;;;;; pas de message dans echo area
(menu-bar-mode 0) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; pas de barre de menu
(require 'alstheme) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; alstheme = tangotango-theme + ample-theme
(scroll-bar-mode 0) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; pas de barre déroulante
(set-fringe-mode '(0 . 0)) ;;;;;;;;;;;;;;;;;;;;;;; épaisseur des lignes verticales
(setq column-number-mode t) ;;;;;;;;;;;;;;;;;;;;;; affiche le numéro de colonne
(setq display-time-default-load-average nil) ;;;;; temps de chargement désactivé
(setq inhibit-startup-screen t) ;;;;;;;;;;;;;;;;;; pas de message de bienvenue
(setq initial-scratch-message nil) ;;;;;;;;;;;;;;; pas de message dans *scratch*
;;(toggle-frame-fullscreen) ;;;;;;;;;;;;;;;;;;;;;; remplacé par "emacs.fullscreen:fullboth" dans .Xresources
(tool-bar-mode 0) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; pas de barre d'outils
(defalias 'yes-or-no-p 'y-or-n-p) ;;;;;;;;;;;;;;;; "y/n" au lieu de "yes/no"
(delete-selection-mode) ;;;;;;;;;;;;;;;;;;;;;;;;;; écrire dans une sélection la remplace
(display-battery-mode) ;;;;;;;;;;;;;;;;;;;;;;;;;;; pourcentage batterie dans la mode-line
(electric-pair-mode) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;; gestions des paires
(global-font-lock-mode) ;;;;;;;;;;;;;;;;;;;;;;;;;; coloration syntaxique pour tous les modes
(global-linum-mode) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; numéro de ligne
(global-set-key (kbd "C-<tab>") 'hippie-expand) ;; auto-complétion pour tous les modes
(global-set-key (kbd "C-M-y") 'yas-global-mode) ;; Yasnippet activé par C-M-y
(global-visual-line-mode) ;;;;;;;;;;;;;;;;;;;;;;;; longues lignes
(savehist-mode) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; historique des commandes et des fichiers ouverts
(setq x-select-enable-clipboard t) ;;;;;;;;;;;;;;; copier-coller hors d'Emacs
(setq make-backup-files nil) ;;;;;;;;;;;;;;;;;;;;; pas de fichiers de sauvegarde créés
(setq read-file-name-completion-ignore-case t) ;;; recherche insensible à la casse
(setq scroll-step 1) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;; déroulement vertical ligne par ligne
(show-paren-mode) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; appariemment visuel des paires
(windmove-default-keybindings 'meta) ;;;;;;;;;;;;; M-<flèche> pour changer de fenêtres

;; Paquets pour Emacs
(require 'package)
(when (>= emacs-major-version 24)
  (setq package-archives
	(append
	 '(("elpa" . "http://tromey.com/elpa/")
	   ("marmalade" . "http://marmalade-repo.org/packages/")
	   ("melpa" . "http://melpa.org/packages/"))
	 package-archives)))
(package-initialize)

;; header-line : ligne de pouvoir en haut
(setq-default
 header-line-format
 (list
  "%e"
  "(" '(:propertize "%2l" face (:foreground "yellow" :weight bold)) "/"
  '(:eval (format "%d" (line-number-at-pos (point-max)))) ","
  '(:propertize "%2c" face (:foreground "yellow4")) ")" " "
  '(:eval
    (concat
     (if (or (string-match "^/su\\(do\\)?:" default-directory) (equal user-login-name "root"))
	 (replace-regexp-in-string "^.*/\\(.*\\)/" "\\1/" (propertize default-directory 'face '(:foreground "red2" :weight bold)))
       (replace-regexp-in-string "^.*/\\(.*\\)/" "\\1/" (propertize default-directory 'face 'directory-name-face)))
     (propertize (file-name-nondirectory (or load-file-name buffer-file-name)) 'face 'mode-line-buffer-id)))
  '(:eval (propertize (when (equal (buffer-file-name) nil) (buffer-name)) 'face 'mode-line-buffer-id))
  '(:eval (cond (buffer-read-only (propertize "[Lecture seule]" 'face '(:foreground "red3")))))
  '(:eval (cond ((buffer-modified-p) (propertize "[Modifié]" 'face '(:foreground "orange4"))))) " "
  "%M" " "
  "%-"))
(setq-default mode-line-format nil)

;; Affiche la date
(setq display-time-format " %A %_-1e %B %Y %H:%M")
(display-time)

;; Surligner ligne pour ne pas perdre de vue le curseur
(require 'hl-line+)
(toggle-hl-line-when-idle t)
(hl-line-when-idle-interval 1)
(set-face-background 'hl-line "gray15")

;; Redimensionnement des buffers
(global-set-key (kbd "C-S-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-S-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-S-<down>") 'shrink-window)
(global-set-key (kbd "C-S-<up>") 'enlarge-window)

;; Incrémente ou décrémente le nombre sous le curseur
(defun thing-at-point-goto-end-of-integer ()
  "Go to end of integer at point."
  (let ((inhibit-changing-match-data t))
    ;; Skip over optional sign
    (when (looking-at "[+-]")
      (forward-char 1))
    ;; Skip over digits
    (skip-chars-forward "[[:digit:]]")
    ;; Check for at least one digit
    (unless (looking-back "[[:digit:]]")
      (error "Aucun nombre ici"))))
(put 'integer 'beginning-op 'thing-at-point-goto-end-of-integer)

(defun thing-at-point-goto-beginning-of-integer ()
  "Go to beginning of integer at point."
  (let ((inhibit-changing-match-data t))
    ;; Skip backward over digits
    (skip-chars-backward "[[:digit:]]")
    ;; Check for digits and optional sign
    (unless (looking-at "[+-]?[[:digit:]]")
      (error "Aucun nombre ici"))
    ;; Skip backward over optional sign
    (when (looking-back "[+-]")
	(backward-char 1))))
(put 'integer 'beginning-op 'thing-at-point-goto-beginning-of-integer)

(defun thing-at-point-bounds-of-integer-at-point ()
  "Get boundaries of integer at point."
  (save-excursion
    (let (beg end)
      (thing-at-point-goto-beginning-of-integer)
      (setq beg (point))
      (thing-at-point-goto-end-of-integer)
      (setq end (point))
      (cons beg end))))
(put 'integer 'bounds-of-thing-at-point 'thing-at-point-bounds-of-integer-at-point)

(defun thing-at-point-integer-at-point ()
  "Get integer at point."
  (let ((bounds (bounds-of-thing-at-point 'integer)))
    (string-to-number (buffer-substring (car bounds) (cdr bounds)))))
(put 'integer 'thing-at-point 'thing-at-point-integer-at-point)

(defun increment-integer-at-point (&optional inc)
  "Increment integer at point by one. With numeric prefix arg INC, increment the integer by INC amount."
  (interactive "p")
  (let ((inc (or inc 1))
	(n (thing-at-point 'integer))
	(bounds (bounds-of-thing-at-point 'integer)))
    (delete-region (car bounds) (cdr bounds))
    (insert (int-to-string (+ n inc)))))

(defun decrement-integer-at-point (&optional dec)
  "Decrement integer at point by one. With numeric prefix arg DEC, decrement the integer by DEC amount."
  (interactive "p")
  (increment-integer-at-point (- (or dec 1))))

(global-set-key (kbd "C-<") 'increment-integer-at-point)
(global-set-key (kbd "C->") 'decrement-integer-at-point)

;; Arrière-plan désactivé pour le terminal
(defun on-after-init ()
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))
    (set-face-background 'mode-line "unspecified-bg" (selected-frame))))
(add-hook 'window-setup-hook 'on-after-init)

;; Sauvegarde la position du curseur avant la fermeture
(require 'saveplace)
(toggle-save-place)

;; Timestamps
(add-hook 'before-save-hook 'time-stamp)
(add-hook 'write-file-hooks 'time-stamp)
(setq time-stamp-pattern "8/Dernière modification le\\\\?[[:space:]]+%02d/%02m/%:y à %02H:%02M:%02S\\\\?[***]")

;; Ligne courante visible
(require 'hlinum)
(hlinum-activate)
(setq linum-highlight-in-all-buffersp t)
(set-face-attribute 'linum-highlight-face nil :foreground "yellow" :background "gray15")
