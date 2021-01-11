;;; my/dired/autoload.el -*- lexical-binding: t; -*-
;;

;; https://jblevins.org/log/dired-open
;; Open files in dired mode using 'open'
;;;###autoload
(defun me/mac-open ()
  (interactive)
  (let ((fn (dired-get-file-for-visit)))
   (start-process "default-app" nil "open" fn)))

(require 'cl-lib)
;; https://github.com/xenodium/dotsies/blob/main/emacs/features/fe-dired.el
;;;###autoload
(defun ar/dired-convert-image (&optional arg)
    "Convert image files to other formats."
    (interactive "P")
    (cl-assert (or (executable-find "convert") (executable-find "magick.exe")) nil "Install imagemagick")
    (let* ((dst-fpath)
           (src-fpath)
           (src-ext)
           (last-ext)
           (dst-ext))
      (mapc
       (lambda (fpath)
         (setq src-fpath fpath)
         (setq src-ext (downcase (file-name-extension src-fpath)))
         (when (or (null dst-ext)
                   (not (string-equal dst-ext last-ext)))
           (setq dst-ext (completing-read "to format: "
                                          (seq-remove (lambda (format)
                                                        (string-equal format src-ext))
                                                      '("jpg" "png")))))
         (setq last-ext dst-ext)
         (setq dst-fpath (format "%s.%s" (file-name-sans-extension src-fpath) dst-ext))
         (message "convert %s to %s ..." (file-name-nondirectory dst-fpath) dst-ext)
         (set-process-sentinel
          (if (string-equal system-type "windows-nt")
              (start-process "convert"
                             (generate-new-buffer (format "*convert %s*" (file-name-nondirectory src-fpath)))
                             "magick.exe" "convert" src-fpath dst-fpath)
            (start-process "convert"
                           (generate-new-buffer (format "*convert %s*" (file-name-nondirectory src-fpath)))
                           "convert" src-fpath dst-fpath))
          (lambda (process state)
            (if (= (process-exit-status process) 0)
                (message "convert %s ✔" (file-name-nondirectory dst-fpath))
              (message "convert %s ❌" (file-name-nondirectory dst-fpath))
              (message (with-current-buffer (process-buffer process)
                         (buffer-string))))
            (kill-buffer (process-buffer process)))))
       (dired-map-over-marks (dired-get-filename) arg))))

;;;###autoload
(defun ar/dired-video-to-gif (&optional arg)
    (interactive "P")
    (cl-assert (executable-find "ffmpeg") nil "ffmpeg not installed")
    (cl-assert (executable-find "gifsicle") nil "gifsicle not installed")
    (mapc
     (lambda (fpath)
       (let* ((src-fpath fpath)
              (base-name (file-name-nondirectory (file-name-sans-extension src-fpath)))
              (dst-fpath (format "%s.gif" (file-name-sans-extension src-fpath)))
              (every (string-to-number
                      (completing-read "Speed up x times: " '("1" "2" "3" "4"))))
              (process (lambda (&rest args)
                         (with-temp-buffer
                           (let ((out (list :exit-status
                                            (apply 'call-process (seq-concatenate
                                                                  'list
                                                                  (list (car args) nil t nil)
                                                                  (cdr args)))
                                            :output
                                            (buffer-string))))
                             (cl-assert (eq (map-elt out :exit-status)
                                            0) nil (map-elt out :output))
                             out)))))
         (message "Processing %s" (file-name-nondirectory src-fpath))
         (funcall process "ffmpeg" "-y" "-i" src-fpath "-pix_fmt" "rgb24" "-r" "15" dst-fpath)
         (apply process (seq-concatenate
                         'list
                         (list "gifsicle" "-U" dst-fpath)
                         ;; Returns #0 #1 #2 ... #framecount.
                         (seq-map (lambda (n)
                                    (format "#%d" n))
                                  (range 0 (string-to-number
                                            ;; Get total grame count.
                                            (seq-first (process-lines "identify" "-format" "%n\n" dst-fpath)))
                                         every))
                         (list "-O2" "-o" dst-fpath)))
         (message "Created %s" (file-name-nondirectory dst-fpath))))
     (dired-map-over-marks (dired-get-filename) arg)))

;;;###autoload
(defun ar/dired-convert-audio-to-mp3 (&optional arg)
    "Convert audio file to mp3."
    (interactive "P")
    (cl-assert (executable-find "convert") nil "Install ffmpeg")
    (mapc
     (lambda (fpath)
       (let* ((src-fpath fpath)
              (ext (file-name-extension src-fpath))
              (dst-fpath (concat (file-name-sans-extension src-fpath)
                                 ".mp3")))
         (message "ffmpeg %s ..." (file-name-nondirectory dst-fpath))
         (set-process-sentinel (start-process "ffmpeg"
                                              (generate-new-buffer (format "*ffmpeg %s*" (file-name-nondirectory src-fpath)))
                                              "ffmpeg" "-loglevel" "error" "-n" "-i" src-fpath "-acodec" "libmp3lame" dst-fpath)
                               (lambda (process state)
                                 (if (= (process-exit-status process) 0)
                                     (message "ffmpeg %s ✔" (file-name-nondirectory dst-fpath))
                                   (message "ffmpeg %s ❌" (file-name-nondirectory dst-fpath))
                                   (message (with-current-buffer (process-buffer process)
                                              (buffer-string))))
                                 (kill-buffer (process-buffer process))))))
     (dired-map-over-marks (dired-get-filename) arg)))
