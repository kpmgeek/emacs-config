;;; groovy-splash.el --- A groovy Emacs splash screen -*- lexical-binding: t -*-

;;; Commentary:
;; This used to live in my README.org/init.el, but got big and complicated enough that
;; it was worth breaking it out into its own file, if only so that I don't have to
;; recompile init.el every time I edit it.
;; TODO: better commentary

;;; Code:

(require 'dash)
(require 'all-the-icons)
(require 'page-break-lines)
(require 'recentf)

(defgroup groovy-splash nil
  "A groovy Emacs splash screen"
  :group 'applications
  :prefix "groovy-splash-")


;;; Utility Functions

(defun groovy-splash--center-line ()
  "Visually center the current line."

  (move-to-column 0)
  (-let [(w . _) (window-text-pixel-size (get-buffer-window)
                                         (point) (line-end-position))]
    (insert (propertize " "
                        'display `(space . (:align-to (- center (0.5 . (,w))))))))
  (end-of-line))


;;; Vertical fill segments
;; These return 0 when asked for their height, and draw themselves at the height
;; requested by the HEIGHT parameter.

(defun groovy-splash-blank-line (width height &optional no-draw)
  "Insert a blank line.
Returns the minimum height in lines of the section. WIDTH is the
buffer width, HEIGHT is the allocated height. If NO-DRAW is
non-nil, don't actually insert the section."

  (ignore width height)
  (unless no-draw (insert-char ?\n 1))
  1)

(defun groovy-splash-rule (width height &optional no-draw)
  "Insert a horizontal rule.
Returns the minimum height in lines of the section. WIDTH is the
buffer width, HEIGHT is the allocated height. If NO-DRAW is
non-nil, don't actually insert the section."
  (ignore width height)
  (unless no-draw (insert "\f\n"))
  1)

(defun groovy-splash-blank-fill (width height &optional no-draw)
  "Insert blank fill.
Returns the minimum height in lines of the section. WIDTH is the
buffer width, HEIGHT is the allocated height. If NO-DRAW is
non-nil, don't actually insert the section."

  (ignore width)
  (unless no-draw (insert-char ?\n height))
  0)

(defun groovy-splash-groovy-fill (width height &optional no-draw)
  "Insert groovy rainbow fill.
Returns the minimum height in lines of the section. WIDTH is the
buffer width, HEIGHT is the allocated height. If NO-DRAW is
non-nil, don't actually insert the section."

  (ignore width)
  (unless no-draw
    (-dotimes height
      (lambda (_)
        (insert (mapconcat (lambda (color)
                             (propertize "\N{FULL BLOCK}\N{FULL BLOCK}"
                                         'face `(:foreground ,color :background ,color)))
                           '("#fb4934" "#fabd2f" "#b8bb26" "#83a598" "#d3869b")
                           ""))
        (groovy-splash--center-line) (insert "\n"))))
  0)


;;; Banner segments

(defun groovy-splash-logo (width height &optional no-draw)
  "Insert logo and version into buffer.
Returns the minimum height in lines of the section. WIDTH is the
buffer width, HEIGHT is the allocated height. If NO-DRAW is
non-nil, don't actually insert the section."

  (ignore width height)
  (let ((icon-height 10))
    (unless no-draw
      ;; This is a little tricky because we want to specify the height in lines, but the
      ;; icon will make the line a little too big. So what we do is insert it, measure
      ;; it, then adjust the size.
      (insert " ")                    ; extra space to calculate line height
      (-let [(_ . base-px-height)
             (window-text-pixel-size (get-buffer-window)
                                     (line-beginning-position) (point))]
        (insert (all-the-icons-fileicon "emacs"
                                        :height icon-height
                                        :v-adjust 0))
        (-let* (((_ .  initial-px-height)
                 (window-text-pixel-size (get-buffer-window)
                                         (line-beginning-position) (point)))
                (px-height (* icon-height base-px-height))
                (new-height (/ (* (float icon-height) px-height) initial-px-height)))
          (delete-char -2)
          (insert (all-the-icons-fileicon "emacs"
                                          :height new-height
                                          :v-adjust 0
                                          :face 'all-the-icons-purple))))
      (groovy-splash--center-line)
      (insert (concat
               "\n\n"
               (propertize "GNU Emacs" 'face 'bold)
               " "
               (format "%d.%d" emacs-major-version emacs-minor-version)))
      (groovy-splash--center-line) (insert "\n"))
    (+ icon-height 2)))


;;; Button segments

(defun groovy-splash-recover-session-button (width height &optional no-draw)
  "Insert a button to recover the last session.
Returns the minimum height in lines of the section. WIDTH is the
buffer width, HEIGHT is the allocated height. If NO-DRAW is
non-nil, don't actually insert the section."
  (ignore width height)
  (unless no-draw
    (when (and auto-save-list-file-prefix
               (let ((auto-save-dir (file-name-directory auto-save-list-file-prefix)))
                 (and (file-directory-p auto-save-dir)
                      (> (length (directory-files auto-save-dir)) 2))))
      (insert-text-button "[Recover Session]"
                          'action (lambda (_) (call-interactively 'recover-session))
                          'help-echo "Recover previous session"
                          'face 'warning
                          'follow-link t)
      (groovy-splash--center-line))
    (insert "\n"))
  1)


;;; Oracle segment

(defconst groovy-splash-oracle-phrases-8ball
  '("IT IS CERTAIN"
    "IT IS DECIDEDLY SO"
    "WITHOUT A DOUBT"
    "YES DEFINITELY"
    "YOU MAY RELY ON IT"
    "AS I SEE IT YES"
    "MOST LIKELY"
    "OUTLOOK GOOD"
    "YES"
    "SIGNS POINT TO YES"
    "REPLY HAZY TRY AGAIN"
    "ASK AGAIN LATER"
    "BETTER NOT TELL YOU NOW"
    "CANNOT PREDICT NOW"
    "CONCENTRATE AND ASK AGAIN"
    "DON'T COUNT ON IT"
    "MY REPLY IS NO"
    "MY SOURCES SAY NO"
    "OUTLOOK NOT SO GOOD"
    "VERY DOUBTFUL")
  "Magic 8-ball oracle phrases.")

(defconst groovy-splash-oracle-phrases-zen
  `(;; The Human Route
    "LIFE IS LIKE A FLOATING CLOUD WHICH APPEARS"
    "DEATH IS LIKE A FLOATING CLOUD WHICH DISAPPEARS"

    ;; The Gateless Gate
    ;; TODO: more of these?
    ;; http://www.ibiblio.org/zen/cgi-bin/koan-index.pl
    "MU"                                ; 1. Joshu's Dog
    "ONE WITH THE LAW OF CAUSATION"     ; 2. Hyakujo's Fox
    "YOU HAD BETTER WASH YOUR BOWL"     ; 7. Joshu Washes the Bowl
    "BECOME SOBER"                      ; 12. Zuigan Calls His Own Master
    "SNATCH THE SWORD"                  ; 14. Nansen Cuts the Cat in Two
    "ANOTHER'S WORDS NOT MINE TO GIVE"  ; 24. Without Words, Without Silence
    "NOT THE WIND NOT THE FLAG"         ; 29. Not the Wind, Not the Flag
    "A STICK IN THE SOFT MUD"           ; 31. Joshu Investigates

    ;; Collection of Stone and Sand
    ;; http://www.ashidakim.com/zenkoans/zenindex.html
    "EMPTY YOUR CUP"  ; 1. A Cup of Tea
    "IS THAT SO?"     ; 3. Is That So?
    "THROW IT OUT"    ; 41. Joshu's Zen
    "CARRY IT OUT"    ; 41. Joshu's Zen

    ;; Well known
    "THE SOUND OF ONE HAND CLAPPING"

    ;; Not Zen
    "42"
    )
  "Zen oracle phrases.")

(defcustom groovy-splash-oracle-phrases
  groovy-splash-oracle-phrases-8ball
  "A list of strings for the oracle to choose from."
  :type '(repeat string)
  :group 'groovy-splash)

(defvar groovy-splash--oracle-current-phrase nil)

(defun groovy-splash--oracle-phrase ()
  "Get the current oracle phrase.
If one hasn't been picked yet, pick one."
  (or groovy-splash--oracle-current-phrase
      (groovy-splash--refresh-oracle)))

(defun groovy-splash--refresh-oracle ()
  "Get a new oracle phrase."
  (setq groovy-splash--oracle-current-phrase
        (nth (random (length groovy-splash-oracle-phrases)) groovy-splash-oracle-phrases)))

(defun groovy-splash-oracle (width height &optional no-draw)
  "Insert a magic 8 ball phrase.
Returns the minimum height in lines of the section. WIDTH is the
buffer width, HEIGHT is the allocated height. If NO-DRAW is
non-nil, don't actually insert the section."
  (ignore width height)

  (unless no-draw
    (let ((eight (propertize "➑" 'face 'default)))
      (insert-text-button
       (format "%s %s %s"
               eight
               (propertize (groovy-splash--oracle-phrase)
                           'face '(:foreground "#83a598"))
               eight)
       'action (lambda (_)
                 (groovy-splash--refresh-oracle)
                 (groovy-splash--redraw))
       'help-echo "Ask the mystic oracle another question"
       ;; 'face 'warning
       'follow-link t))
    (groovy-splash--center-line) (insert "\n"))
  1)


;;; Recent Files Segment

(defcustom groovy-splash-recentf-count 5
  "Number of recent files to display."
  :type 'integer
  :group 'groovy-splash)

(defun groovy-splash-recentf (width height &optional no-draw)
  "Insert recent files.
Returns the minimum height in lines of the section. WIDTH is the
buffer width, HEIGHT is the allocated height. If NO-DRAW is
non-nil, don't actually insert the section."
  (ignore width height)
  (unless no-draw
    (--each (-take groovy-splash-recentf-count recentf-list)
      (insert-text-button (abbreviate-file-name it)
                          'action (lambda (_) (find-file-existing it))
                          'follow-link t)
      (groovy-splash--center-line)
      (insert "\n")))
  groovy-splash-recentf-count)


;;; Main drawing code

(defcustom groovy-splash-segments
  '(groovy-splash-groovy-fill
    groovy-splash-blank-line
    groovy-splash-logo
    groovy-splash-blank-fill
    groovy-splash-recover-session-button
    groovy-splash-blank-line
    groovy-splash-oracle)
  "Segments to display on the splash screen."
  :type '(repeat (choice (function :tag "Segment function")
                         (string :tag "Literal text")))
  :group 'groovy-splash)

(defun groovy-splash--redraw (&optional force)
  "Redraw the splash screen.
If FORCE is non-nil, redraw the screen even if it isn't in a
visible buffer."
  (let* ((splash-buffer (get-buffer-create "*groovy-splash*"))
         (splash-window (get-buffer-window splash-buffer)))
    (when (or force splash-window)
      (with-current-buffer splash-buffer
        (read-only-mode -1)
        (erase-buffer)

        ;; Buffer local settings
        ;; (setq mode-line-format nil)
        (setq cursor-type nil)
        (setq vertical-scroll-bar nil)
        (setq horizontal-scroll-bar nil)
        (face-remap-add-relative 'link :underline nil)

        (let* ((height (window-body-height splash-window))
               (width (window-body-width splash-window))

               ;; Measure the section heights
               (startup-items
                (--map-when (not (functionp it))
                            (lambda (_0 _1 &optional no-draw)
                              (unless no-draw
                                (insert it)
                                (groovy-splash--center-line) (insert "\n"))
                              1)
                            groovy-splash-segments))
               (section-heights (--map (funcall it width height t) startup-items))
               (fill-section-count (-count 'zerop section-heights))
               (leftover-height (- height (-sum section-heights)))
               (usual-fill-height (/ leftover-height fill-section-count))
               (final-fill-height (- leftover-height (* usual-fill-height
                                                        (1- fill-section-count)))))
          (setq fill-column width)
          (->> (-replace-last 0 final-fill-height section-heights)
               (-replace 0 usual-fill-height)
               (--zip-with (funcall it width other) startup-items)))

        (goto-char 0)
        (read-only-mode +1)))))

(defun groovy-splash-show ()
  "Show the splash screen."
  (interactive)

  (let ((splash-buffer (get-buffer-create "*groovy-splash*")))
    (buffer-disable-undo splash-buffer)
    (groovy-splash--redraw t)
    (switch-to-buffer splash-buffer))

  (add-hook 'window-size-change-functions 'groovy-splash--redraw))

(add-to-list 'evil-buffer-regexps '("^\\*groovy-splash\\*" . nil))

(provide 'groovy-splash)
;;; groovy-splash.el ends here
