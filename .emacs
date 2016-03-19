;; .emacs --- fichier de configuration général de GNU Emacs

;; ***Date de création : 14/03/2016***
;; ***Dernière modification le 19/03/2016 à 20:02:30***

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

;; Thème
(require 'solarized-dark-theme)
(set-face-attribute 'header-line nil :underline nil :overline "#093b3f")
(set-face-attribute 'vertical-border nil :foreground "#093b3f")

(menu-bar-mode 0) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; .Xresources : "emacs.menuBar: off"
(scroll-bar-mode 0) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; .Xresources : "emacs.verticalScrollBars: off"
(tool-bar-mode 0) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; .Xresources : "emacs.toolBar: off"
(blink-cursor-mode 0) ;;;;;;;;;;;;;;;;;;;;;;;;;;;; curseur fixe
(defun startup-echo-area-message () "") ;;;;;;;;;; pas de message dans echo area
(set-fringe-mode '(0 . 0)) ;;;;;;;;;;;;;;;;;;;;;;; épaisseur des lignes verticales
(setq column-number-mode t) ;;;;;;;;;;;;;;;;;;;;;; affiche le numéro de colonne
(setq display-time-default-load-average nil) ;;;;; temps de chargement désactivé
(setq inhibit-startup-screen t) ;;;;;;;;;;;;;;;;;; pas de message de bienvenue
(setq initial-scratch-message nil) ;;;;;;;;;;;;;;; pas de message dans *scratch*
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
	 (replace-regexp-in-string "^.*/\\(.*\\)/" "\\1/" (propertize default-directory 'face '(:foreground "#990a1b" :weight bold)))
       (replace-regexp-in-string "^.*/\\(.*\\)/" "\\1/" (propertize default-directory 'face '(:foreground "#00629d" :weight bold))))
     (propertize (file-name-nondirectory (or load-file-name buffer-file-name)) 'face 'mode-line-buffer-id)))
  '(:eval (propertize (when (equal (buffer-file-name) nil) (buffer-name)) 'face 'mode-line-buffer-id))
  '(:eval (cond (buffer-read-only (propertize "[Lecture seule]" 'face '(:foreground "red3")))))
  '(:eval (cond ((buffer-modified-p) (propertize "[Modifié]" 'face '(:foreground "orange4"))))) " "
  "%M" " "))
(setq-default mode-line-format nil)

;; Affiche la date
(setq display-time-format " %A %_-1e %B %Y %H:%M")
(display-time)

;; Surligner ligne pour ne pas perdre de vue le curseur
(require 'hl-line+)
(toggle-hl-line-when-idle)
(hl-line-when-idle-interval 1)
(set-face-background 'hl-line "#103b46")

;; Redimensionnement des buffers
(global-set-key (kbd "C-S-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-S-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-S-<down>") 'shrink-window)
(global-set-key (kbd "C-S-<up>") 'enlarge-window)

;; Incrémente ou décrémente le nombre sous le curseur
(require 'integer-at-point)
(global-set-key (kbd "C-<") 'increment-integer-at-point)
(global-set-key (kbd "C->") 'decrement-integer-at-point)

;; Arrière-plan désactivé pour le terminal
(defun on-after-init ()
  (unless (display-graphic-p (selected-frame))
    (dolist (face '(default header-line linum))
      (set-face-background face "unspecified-bg" (selected-frame)))
    (menu-bar-mode 0)
    ))
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
(set-face-attribute 'linum-highlight-face nil :foreground "yellow" :background "#103b46")
