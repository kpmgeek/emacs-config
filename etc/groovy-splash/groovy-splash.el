;;; groovy-splash.el --- A groovy Emacs splash screen -*- lexical-binding: t -*-

;;; Commentary:
;; This used to live in my README.org/init.el, but got big and complicated enough that
;; it was worth breaking it out into its own file, if only so that I don't have to
;; recompile init.el every time I edit it.
;; TODO: better commentary
;;
;; Segment functions take one integer parameter, HEIGHT, and behave as follows:
;;
;;   - If HEIGHT is nil, just returns a cons of integers (MIN . MAX), where MIN and MAX
;;     are the minimum and maximum heights in integers. MAX can also be nil, in which
;;     case there is no maximum height.
;;
;;   - If HEIGHT is an integer, draws the segment in that many rows. HEIGHT will be
;;     within the range returned by the function in a call above.

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


;;; Spacing segments

(defun groovy-splash-blank-line (&optional height)
  "Insert a blank line.

If HEIGHT is non-nil, display the segment at the given height.
Otherwise, return the height range for the widget."

  (when height
    (insert-char ?\n 1))
  '(1 . 1))

(defun groovy-splash-rule (&optional height)
  "Insert a horizontal rule.
If HEIGHT is non-nil, display the segment at the given height.
Otherwise, return the height range for the widget."

  (when height
    (insert "\f\n"))
  '(1 . 1))

(defun groovy-splash-blank-fill (&optional height)
  "Insert blank fill.

If HEIGHT is non-nil, display the segment at the given height.
Otherwise, return the height range for the widget."

  (when height
    (insert-char ?\n height))

  ;; Always inserts at least one blank line so that this can be used do divide sections.
  '(1 . nil))

(defcustom groovy-splash-groovy-fill-min 3
  "Minimum height of groovy fill."
  :type 'integer
  :group 'groovy-splash)

(defcustom groovy-splash-groovy-colors
  '("#fb4934" "#fabd2f" "#b8bb26" "#83a598" "#d3869b")
  "Colors to use for the groovy fill."
  :type '(repeat color)
  :group 'groovy-splash)

(defcustom groovy-splash-groovy-stripe-chars
  "\N{FULL BLOCK}\N{FULL BLOCK}"
  "Characters for one stripe in one row of the groovy fill."
  :type 'string
  :group 'groovy-splash)

(defun groovy-splash-groovy-fill (&optional height)
  "Insert groovy rainbow fill.

If HEIGHT is non-nil, display the segment at the given height.
Otherwise, return the height range for the widget."

  (when height
    (--dotimes height
      (insert (mapconcat (lambda (color)
                           (propertize groovy-splash-groovy-stripe-chars
                                       'face `(:foreground ,color)))
                         groovy-splash-groovy-colors ""))
      (groovy-splash--center-line) (insert "\n")))
  `(,groovy-splash-groovy-fill-min . nil))


;;; Banner segments

(defcustom groovy-splash-banner-max-height 12
  "Maximum height of any banners in lines."
  :type 'integer
  :group 'groovy-splash)

(defun groovy-splash--line-pixel-height ()
  "Calculate the pixel height of the line at point in the current buffer.

Point should be at the end of the line."

  ;; Why not line-pixel-height? That calculates the height of the line at point in the
  ;; currently-selected *window*, which is not what we want.
  (cdr (window-text-pixel-size (get-buffer-window)
                               (line-beginning-position) (point))))

(defun groovy-splash-logo (&optional height)
  "Insert logo and version into buffer.

If HEIGHT is non-nil, display the segment at the given height.
Otherwise, return the height range for the widget."

  (when height
    ;; Draw on line line if the icon would be too small.
    (let* ((one-line-p (< height 5))
           (icon-height (- height (if one-line-p 0 2))))

      ;; This is a little tricky because we want to specify the height in lines, but the
      ;; font-icons tend to be a little taller than a usual line. We get around this by
      ;; inserting it, measuring it, then using that to refine the size.
      (insert " ")                 ; space so that we can calculate the base line-height
      (let ((base-px-height (groovy-splash--line-pixel-height)))
        (insert (all-the-icons-fileicon "emacs" :height icon-height :v-adjust 0))
        (let* ((initial-px-height (groovy-splash--line-pixel-height))
               (px-height (* icon-height base-px-height))
               (new-height (/ (* (float icon-height) px-height) initial-px-height)))
          (delete-char -2)
          (insert (all-the-icons-fileicon "emacs"
                                          :height new-height
                                          :v-adjust (if one-line-p nil 0)
                                          :face 'all-the-icons-purple))))
      (if one-line-p
          (insert " ")
        (groovy-splash--center-line)
        (insert "\n\n"))
      (insert (concat (propertize "GNU Emacs" 'face 'bold)
                      " "
                      (format "%d.%d" emacs-major-version emacs-minor-version)))
      (groovy-splash--center-line)
      (insert "\n")))
  `(2 . ,(max 1 groovy-splash-banner-max-height)))


;;; Button segments

(defun groovy-splash-recover-session-button (&optional height)
  "Insert a button to recover the last session.

If HEIGHT is non-nil, display the segment at the given height.
Otherwise, return the height range for the widget."

  (when height
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
  '(1 . 1))


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

(defcustom groovy-splash-oracle-brackets '("➑ " . " ➑")
  "Brackets with which to surround oracle text."
  :type '(cons string string)
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

(defun groovy-splash-oracle (&optional height)
  "Insert a mystic oracle.

If HEIGHT is non-nil, display the segment at the given height.
Otherwise, return the height range for the widget."

  (when height
    (insert-text-button
     (concat (propertize (car groovy-splash-oracle-brackets)
                         'face 'default)
             (groovy-splash--oracle-phrase)
             (propertize (cdr groovy-splash-oracle-brackets)
                         'face 'default))
     'action (lambda (_)
               (groovy-splash--refresh-oracle)
               (groovy-splash--redraw))
     'help-echo "Ask the mystic oracle another question"
     'follow-link t)
    (groovy-splash--center-line) (insert "\n"))
  '(1 . 1))


;;; Recent Files Segment

(defcustom groovy-splash-recentf-max-count 5
  "Maximum number of recent files to display."
  :type '(choice (const :tag "None" nil)
                 integer)
  :group 'groovy-splash)

(defun groovy-splash-recentf (&optional height)
  "Insert list of recent files.

If HEIGHT is non-nil, display the segment at the given height.
Otherwise, return the height range for the widget."

  (when height
    (let ((entries (-take height recentf-list)))
      (--each entries
        (insert-text-button (abbreviate-file-name it)
                            'action (lambda (_) (find-file-existing it))
                            'follow-link t)
        (groovy-splash--center-line) (insert "\n"))
      (--dotimes (- height (length entries))
        (insert "\n"))))
  (if groovy-splash-recentf-max-count
      `(0 . ,(min groovy-splash-recentf-max-count (length recentf-list)))
    `(0 . nil)))


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
        (page-break-lines-mode +1)

        (let* ((height (window-body-height splash-window))
               (splash-segments
                (--map-when (not (functionp it))
                            (lambda (&optional h)
                              (when h
                                (insert it)
                                (groovy-splash--center-line) (insert "\n"))
                              '(1 . 1))
                            groovy-splash-segments))

               ;; Measure the segment heights
               (segment-heights (-map 'funcall splash-segments))
               (min-height (-sum (-map 'car segment-heights)))
               (preferred-height (-sum (--map (or (cdr it) (car it)) segment-heights))))
          (->> (cond ((<= height min-height)
                      (groovy-splash--min-height-layout height segment-heights))
                     ((< height preferred-height)
                      (groovy-splash--height-stealing-layout height segment-heights))
                     (t
                      (groovy-splash--height-sharing-layout height segment-heights)))
               (--zip-with (funcall it other) splash-segments)))

        (goto-char 0)
        (read-only-mode +1)))))

(defun groovy-splash--min-height-layout (height segment-heights)
  "Give every segment its minimum height.

Layouts select values for each range in SEGMENT-HEIGHTS so that
they sum to HEIGHT. This layout is allowed to cheat at that and
return a set of heights that is too tall if necessary."
  (ignore height)
  (-map 'car segment-heights))

(defun groovy-splash--height-stealing-layout (height segment-heights)
  "Give every segment its preferred height, then steal the deficit.

Layouts select values for each range in SEGMENT-HEIGHTS so that
they sum to HEIGHT. This layout accomplishes that by giving
everything its preferred height (the max if it has one, min
otherwise) and then stealing as much height as it can from each
segment starting from the first until they all fit."

  (let* ((bounded-heights (--map-when (null (cdr it)) (cons (car it) (car it))
                                      segment-heights))
         (preferred-height (-sum (-map 'cdr bounded-heights)))
         (height-deficit (- preferred-height height)))

    (->> bounded-heights
         (--map-when (and (< 0 height-deficit)  (< (car it) (cdr it)))
                     (let ((theft (min height-deficit (- (cdr it) (car it)))))
                       (setq height-deficit (- height-deficit theft))
                       (cons (car it) (- (cdr it) theft))))
         (-map 'cdr))))

(defun groovy-splash--height-sharing-layout (height segment-heights)
  "Give every segment its preferred height, then share the surplus.

Layouts select values for each range in SEGMENT-HEIGHTS so that
they sum to HEIGHT. This layout accomplishes that by giving
everything its preferred height (the max if it has one, min
otherwise) and then sharing height equally between the segments
with no maximum height."

  ;; This shares the extra equally, so we can have nice things like centering.

  (let* ((preferred-height (-sum (--map (or (cdr it) (car it)) segment-heights)))
         (stretchy-count (-count 'null (-map 'cdr segment-heights)))
         (surplus-height (- height preferred-height))
         (usual-share (/ surplus-height stretchy-count))
         (final-share (- surplus-height (* usual-share (1- stretchy-count)))))
    (->> segment-heights
         (--map-last (null (cdr it)) (cons (car it) (+ (car it) final-share)))
         (--map-when (null (cdr it)) (cons (car it) (+ (car it) usual-share)))
         (-map 'cdr))))

(defun groovy-splash-show ()
  "Show the splash screen."
  (interactive)

  (let ((splash-buffer (get-buffer-create "*groovy-splash*")))
    (buffer-disable-undo splash-buffer)
    (groovy-splash--redraw t)
    (switch-to-buffer splash-buffer))

  (add-hook 'window-size-change-functions 'groovy-splash--redraw))

(when (boundp 'evil-buffer-regexps)
  (add-to-list 'evil-buffer-regexps '("^\\*groovy-splash\\*" . nil)))

(provide 'groovy-splash)
;;; groovy-splash.el ends here
