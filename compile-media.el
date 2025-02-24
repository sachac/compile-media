;;; compile-media.el --- Combine video or audio files using FFmpeg  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Sacha Chua

;; Author: Sacha Chua <sacha@sachachua.com>
;; Keywords: multimedia

;; This program is free software; you can redistribute it and/or modify
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

;; compile-media takes a list of sources of the form:
;;
;; ((video (:source filename :start-ms start-ts :stop-ms stop-ts)
;;         (:source filename :start-ms start-ts :stop-ms stop-ts)
;;         (:source filename :start-ms start-ts :stop-ms stop-ts))
;;  (video (:text "text text" :start-ms start-ts :stop-ms stop-ts))
;;  (audio (:source filename :start-ms start-ts :stop-ms stop-ts)
;;         (:source filename :start-ms start-ts :stop-ms stop-ts))
;;  (subtitles (:source filename)))
;;
;; plist keys:
;; :source - filename of a video, animated GIF, or static image
;; :start-ms - start time in milliseconds, if clipping
;; :stop-ms - end time in milliseconds, if clipping
;; :duration-ms - speed up or slow down the video to fit in this duration, if specified
;; :text - text to display
;;; Code:

(defgroup compile-media nil
  "Functions for combining video or audio files."
  :group 'multimedia)

(defcustom compile-media-ffmpeg-executable "ffmpeg"
  "FFmpeg command."
  :group 'compile-media
  :type 'string)

(defcustom compile-media-ffmpeg-arguments '("-c:v" "vp8" "-vsync" "2" "-b:v" "800k" "-auto-alt-ref" "0")
  "Extra arguments to pass to FFmpeg."
  :type '(repeat string)
  :group 'compile-media)

(defcustom compile-media-ffmpeg-keyframe-buffer 60000 "Milliseconds to check for keyframes."
  :type 'integer
  :group 'compile-media)

(defcustom compile-media-description-height 70
  "Number of pixels for top description in video.
If nil, omit the description."
  :type 'integer :group 'compile-media)

(defcustom compile-media-output-video-width 1280 "Video will be the specified number of pixels wide."
  :type 'integer :group 'compile-media)
(defcustom compile-media-output-video-height 720 "Video will be the specified number of pixels tall."
  :type 'integer :group 'compile-media)
(defcustom compile-media-output-video-fps nil "If non-nil and greater than 0, use these frames per second."
	:type 'integer :group 'compile-media)
(defcustom compile-media-description-drawtext-filter-params "fontcolor=white:x=5:y=5:fontsize=40"
  "Additional filter arguments for drawing the visual description."
  :type 'string :group 'compile-media)
(defvar compile-media--conversion-process nil "Process for compiling.")

(defun compile-media-timestamp-to-msecs (time-string)
  "Find HH:MM:SS.MS pattern in TIME-STRING and convert it to milliseconds.
Return nil if TIME-STRING doesn't match the pattern."
  (if (numberp time-string) (* time-string 1000.0)
    (save-match-data
      (when (string-match "\\(\\([0-9]+\\):\\)?\\([0-9]+\\):\\([0-9]+\\)\\(?:\\.\\([0-9]+\\)\\)?"
													time-string)
        (let ((hours (string-to-number (or (match-string 2 time-string) "0")))
              (mins  (string-to-number (match-string 3 time-string)))
              (secs  (string-to-number (match-string 4 time-string)))
              (msecs (string-to-number (string-pad
                                        (or (match-string 5 time-string) "0")
                                        3 ?0))))
          (+ (* (truncate hours) 3600000)
             (* (truncate mins) 60000)
             (* (truncate secs) 1000)
             (truncate msecs)))))))

(defun compile-media-msecs-to-timestamp (msecs)
  "Convert MSECS to string in the format HH:MM:SS.MS."
  (concat (format-seconds "%02h:%02m:%02s" (/ msecs 1000))
          "." (format "%03d" (mod msecs 1000))))

(defun compile-media-string-to-msecs (s)
  "Convert S to milliseconds.
Accepts a timestamp or a number.  If the number has a decimal
point, consider it as the number of seconds instead of milliseconds."
  (cond
   ((or (numberp s) (null s)) s)
   ((string-match "\\(?:-?\\([0-9]+\\):\\)?-?\\([0-9]+\\):\\([0-9]+\\)\\(?:[\\.,]\\([0-9]+\\)\\)?" s)
    (let ((negative (save-match-data (string-match "-" s)))
          (hours (string-to-number (or (match-string 1 s) "0")))
          (mins  (string-to-number (match-string 2 s)))
          (secs  (string-to-number (match-string 3 s)))
          (msecs (string-to-number (or (string-pad (match-string 4 s) 3 ?0) "0"))))
      (* (+ (* (truncate hours) 3600000)
            (* (truncate mins) 60000)
            (* (truncate secs) 1000)
            (truncate msecs))
         (if negative -1 1))))
   ((string-match "\\." s)
    (truncate (* (string-to-number s) 1000)))
   (t (truncate (string-to-number s)))))

(defun compile-media-ffmpeg-get-closest-keyframe-in-msecs (video-file-or-frame-list time direction)
  "Check VIDEO-FILE-OR-FRAME-LIST for the keyframe closest to TIME in DIRECTION.
DIRECTION can be <=, >=, <, or >. If TIME has a keyframe, return that.
Returns milliseconds."
  (let* ((msecs (compile-media-string-to-msecs time))
         (frames (if (listp video-file-or-frame-list)
                     video-file-or-frame-list
                   (compile-media-ffmpeg-get-keyframes-around video-file-or-frame-list time))))
    (cond
     ((eq direction '>=) (seq-find (lambda (o) (>= o msecs)) frames))
     ((eq direction '>) (seq-find (lambda (o) (> o msecs)) frames))
     ((eq direction '<=) (seq-find (lambda (o) (<= o msecs)) (reverse frames)))
     ((eq direction '<) (seq-find (lambda (o) (< o msecs)) (reverse frames)))
     (t (error "Direction needed")))))

(defun compile-media-ffmpeg-get-keyframes-around (video-file msecs &optional ms-buffer)
  "Return a list of milliseconds for VIDEO-FILE keyframes around MSECS.
If non-nil, check MS-BUFFER milliseconds around MSECS."
  (setq msecs (compile-media-string-to-msecs msecs))
  (compile-media-ffmpeg-get-keyframes-between
   video-file
   (- msecs (or ms-buffer compile-media-ffmpeg-keyframe-buffer))
   (+ msecs (or ms-buffer compile-media-ffmpeg-keyframe-buffer))))

(defun compile-media-ffmpeg-get-keyframes-between (video-file start-time end-time)
  "Return VIDEO-FILE keyframes between START-TIME and END-TIME."
  (mapcar 'string-to-number
          (split-string
           (shell-command-to-string
            (format
             "ffprobe -select_streams v -read_intervals %.3f%%%.3f -show_entries frame=pkt_pts,pict_type -hide_banner %s -of csv=print_section=0 -v quiet | grep I | cut -d ',' -f 1"
             (/ (compile-media-string-to-msecs start-time) 1000.0)
             (/ (compile-media-string-to-msecs end-time) 1000.0)
             (shell-quote-argument video-file))))))

(defun compile-media-ffmpeg-get-frames-between (video-file start-time end-time)
  "Return VIDEO-FILE frames between START-TIME and END-TIME."
  (mapcar 'string-to-number
          (split-string
           (shell-command-to-string
            (format
             "ffprobe -select_streams v -read_intervals %.3f%%%.3f -show_entries frame=pkt_pts,pict_type -hide_banner %s -of csv=print_section=0 -v quiet | cut -d ',' -f 1"
             (/ (compile-media-string-to-msecs start-time) 1000.0)
             (/ (compile-media-string-to-msecs end-time) 1000.0)
             (shell-quote-argument video-file))))))

(defun compile-media-video-dimensions (file)
  "Return (width . height) of FILE."
  (let ((result
         (mapcar 'string-to-number
                 (split-string
                  (shell-command-to-string
                   (concat
                    "ffprobe -v error -select_streams v:0 -show_entries stream=width,height -of csv=p=0 "
                    (shell-quote-argument file)))
                  ","))))
    (cons (car result) (cadr result))))

(defun compile-media--description-filter (o)
  "Return the FFmpeg filter needed to add O's :description or :text."
  (when (or (plist-get o :description) (plist-get o :text))
    (concat "drawtext=" compile-media-description-drawtext-filter-params ":text="
						(replace-regexp-in-string
						 "[\]\[,:/!]"
						 "\\\\\\&"
						 (replace-regexp-in-string
							"'"
							(concat (make-string 28 ?\\) "'")
							(or (plist-get o :description) (plist-get o :text))))
						(if (compile-media--between o)
								(format ":enable='%s'"
												(compile-media--between o))
							""))))
;; (insert "\n" (compile-media--description-filter (list :description "that's okay")))

(defun compile-media--format-text-track (text output)
	(list :filter
				(format "%s[%s]"
								(mapconcat
								 (lambda (o)
									 (when (plist-get o :text)
										 (compile-media--description-filter o)))
								 (if (eq (car text) 'text) (cdr text) text)
								 ",")
								output)))

(defun compile-media--format-visuals-for-a-single-track (visuals output)
	(let* (input-list
				 filter-list
				 (info
					(seq-map-indexed
					 (lambda (o i)
						 (cond
							((plist-get o :source)	; visual specified
							 (let* ((source (plist-get o :source)))
								 (funcall
									(cond
									 ((string-match "mp4\\|webm\\|mkv" source) 'compile-media--prepare-video)
									 ((string-match "gif$" source) 'compile-media--prepare-animated-gif)
									 (t 'compile-media--prepare-static-image))
									(append
									 o
									 (list :index i
												 :filter
												 (concat
													(if (and compile-media-output-video-fps  (> compile-media-output-video-fps 0))
															(format "fps=%d," compile-media-output-video-fps)
														"")
													(compile-media--scale-filter o))))
									(if (= (length visuals) 1) output (format "r%d" i)))))
							((plist-get o :text)				; drawtext
							 (append o (list :filter (compile-media--description-filter o))))))
					 visuals)))
		(setq filter-list
					(append
					 (mapcar (lambda (o) (plist-get o :filter)) info)
					 (when (> (length info) 1)
						 (list (compile-media--concat (length visuals) "v=1:a=0" "r" output)))))
		(setq input-list
					(append (seq-mapcat (lambda (o) (plist-get o :input)) info)
									input-list))
		(list :input input-list :filter (string-join filter-list ";")
					:input-count (length visuals))))

(defun compile-media--format-open-captions (subtitles output)
	"Determine the arguments for SUBTITLES.
Overlay on the video stream from INPUT.
Returns a plist with :input, :filter, and :output."
	(let* ((options (if (listp (plist-get (car subtitles) :open-captions))
											(plist-get (car subtitles) :open-captions)
										nil))
				 (bg-color (or (plist-get options :bg) "&H00000000"))
				 (outline-color (or (plist-get options :outline) "&H66000000"))
				 (fg-color (or (plist-get options :fg) "&HFFFFFF"))
				 (bold (or (plist-get options :bold) "0"))
				 (font-name (or (plist-get options :font-name) "Arial"))
				 (font-size (or (plist-get options :font-size) 24))
				 (alignment (or (plist-get options :alignment) 10)))
		(list
		 :input
		 nil
		 :filter
		 (format "subtitles=%s:force_style='Fontname=%s,Fontsize=%s,PrimaryColour=%s,OutlineColour=%s,BorderStyle=3,Outline=3,Alignment=%s,Bold=%s,BackColour=%s'[%s]"
						 (plist-get (car subtitles) :source)
						 font-name
						 font-size
						 fg-color
						 outline-color
						 alignment
						 bold
						 bg-color
						 output)
		 :output
		 nil)))

(defun compile-media--format-visuals (visual-tracks &optional output)
  "Determine the arguments for VISUALS.
There may be multiple video tracks. If so, they are overlaid.
If OUTPUT is specified, use that as the name of the output stream.
Returns a plist with :input, :filter, and :output.
"
	(when (eq (car visual-tracks) 'video)
		(setq visual-tracks (list visual-tracks))) ; old style, just one video track
	(let ((offset 0)
				input-list
				filter-list
				output-list
				video-track-output
				(input-count 0)
				(video-tracks (seq-filter (lambda (o) (eq (car o) 'video)) visual-tracks))
				(text-track (assoc-default 'text visual-tracks))
				(open-captions-track (assoc-default 'subtitles visual-tracks)))
		;; TODO process video-tracks, then draw text on top
		(when video-tracks
			(seq-map-indexed
			 (lambda (visuals track-i)
				 (let* ((output (format "v-input-%d" track-i))
								info filter input)
					 ;; v-input-# will contain the visuals for that track
					 (setq info
								 (pcase (car visuals)
									 ('video (compile-media--format-visuals-for-a-single-track (cdr visuals)
																																output))
									 ('text (compile-media--format-text-track (cdr visuals) output))))
					 (setq input-list (append input-list (plist-get info :input)))
					 (setq input-count (+ input-count (or (plist-get info :input-count) 0)))
					 (push (plist-get info :filter)
								 filter-list)
					 ;; overlay as we go along
					 (if (> track-i 0)
							 (push (format "[v-%s-%d][v-input-%d]overlay[v-overlaid-%d]"
														 (if (= track-i 1)
																 "input"
															 "overlaid"))
										 filter-list))))
			 video-tracks))
		;; at the end of this, we either have v-input-0 (for one track) or v-overlaid-# (len - 1)
		(setq video-track-output (pcase (length video-tracks)
															 (0 nil)
															 (1 "v-input-0")
															 (_ (format "v-overlaid-%d" (1- (length video-tracks))))))
		;; no video input? use a black screen
		(if (and (null input-list) (or text-track open-captions-track))
				(setq input-list `("-f" "lavfi" "-i" ,(format "color=size=%dx%d:color=black" compile-media-output-video-width compile-media-output-video-height))
							input-count (1+ input-count)))
		(when text-track
			(push
			 (concat (if video-track-output (format "[%s]" video-track-output) "")
							 (plist-get (compile-media--format-open-captions open-captions-track "after-text") :filter))
			 filter-list)
			(setq video-track-output "after-text"))
		(when open-captions-track
			(push
			 (format "[%s]%s"
							 video-track-output
							 (plist-get (compile-media--format-open-captions open-captions-track "open-captions"):filter))
			 filter-list)
			(setq video-track-output "open-captions")
			;; (if video-track-output
			;; 		(progn
			;; 			;; (push (format "[%s][open-captions]overlay[after-captions]" video-track-output) filter-list)
			;; 			(setq video-track-output "after-captions"))
			;; 	(setq video-track-output "open-captions"))
			)
		(list :input input-list
					:filter (when filter-list (string-join (reverse filter-list) ";"))
					:output (when (and (not output) video-track-output) (list "-map:v" (format "[%s]" video-track-output)))
					:input-count input-count)))

(defun compile-media--prepare-static-image (info &optional output)
  "Return arguments for static image in INFO."
  (list
   :input
   (append (list "-loop" "1" "-t" (format "%.3f" (/ (or
																										 (plist-get info :duration-ms)
																										 (plist-get info :duration)
																										 (- (plist-get info :stop-ms)
																												(plist-get info :start-ms)))
																										1000.0)))
					 (plist-get info :before-input)
					 (list "-i" (plist-get info :source))
					 (plist-get info :after-input))
   :filter
   (format "[%d:v]%s[%s]"
           (plist-get info :index)
					 (plist-get info :filter)
					 (or output
							 (format "r%d" (plist-get info :index))))))

(defun compile-media--prepare-animated-gif (info &optional output)
  "Return arguments for animated gif specified in INFO."
  (let ((gif-frames (compile-media-get-animated-gif-frames
                     (plist-get info :source)))
				(duration (/ (or
											(plist-get info :duration-ms)
											(plist-get info :duration)
											(- (plist-get info :stop-ms)
												 (plist-get info :start-ms)))
										 1000.0)))
    (list
     :input
		 (if (and (plist-get info :loop-if-shorter)
							(< (compile-media-get-file-duration-ms (plist-get info :source))
								 (* duration 1000)))
				 (list "-stream_loop" "-1"
							 "-t"
							 duration
							 "-i"
							 (plist-get info :source))
			 (list "-r" (format "%.3f" (/ gif-frames duration)) "-i"
																									(plist-get info :source)))
     :filter
     ;; (format "-i %s" filename)
     (format "[%d:v]%s[%s]"
             (plist-get info :index)
						 (if (stringp (plist-get info :filter))
								 (plist-get info :filter)
							 (string-join
								(plist-get info :filter)
								","))
						 (or output (format "r%d" (plist-get info :index)))))))

(defun compile-media--between (info)
	(let* ((start-ms (plist-get info :start-ms))
         (stop-ms (plist-get info :stop-ms))
         (start-s (and start-ms (/ start-ms 1000.0)))
         (stop-s (and stop-ms (/ stop-ms 1000.0))))
		(cond
     ((and start-ms stop-ms) (format "between(t,%.3f,%.3f)" start-s stop-s))
     (start-ms (format "gte(t,%.3f)" start-s))
     (stop-ms (format "lte(t,%.3f)" stop-s)))))

(defun compile-media--prepare-video (info &optional output)
  "Return ffmpeg arguments for videos specified by INFO."
	(let* ((start-ms (plist-get info :start-ms))
         (stop-ms (plist-get info :stop-ms))
         (start-s (and start-ms (/ start-ms 1000.0)))
         (stop-s (and stop-ms (/ stop-ms 1000.0)))
         (duration (or (plist-get info :duration-ms) (plist-get info :duration)))
         (video-duration (if duration (or (plist-get info :video-duration)
                                          (compile-media-get-file-duration-ms
                                           (plist-get info :source))))))
		(list
		 :input
		 (if (and (plist-get info :loop-if-shorter)
							(< video-duration duration))
				 (list "-stream_loop" "-1" "-t" (/ duration 1000.0) "-i" (plist-get info :source))
			 (delq nil
						 (append
							(list "-i" (plist-get info :source))
							(if (plist-get info :same-edits)
									(list "-t"
												(format
												 "%.3f"
												 (/
													(plist-get (car (last (plist-get info :same-edits)))
																		 :stop-ms)
													1000.0)))))))
		 :filter
		 (format "[%d:v]%s[%s]"
						 (plist-get info :index)
						 (string-join
							(delq nil
										(list
										 (cond
											((plist-get info :same-edits)
											 (format "select='%s',setpts='N/FRAME_RATE/TB'"
                                      (mapconcat
                                       #'compile-media--select-spans
                                       (compile-media--combine-times (plist-get info :same-edits))
                                       "+")))
											((and start-ms stop-ms) (format "select='between(t,%.3f,%.3f)'" start-s stop-s))
											(start-ms (format "select='gte(t,%.3f)'" start-s))
											(stop-ms (format "select='lte(t,%.3f)'" stop-s)))
										 (cond
											((plist-get info :same-edits) "setpts=PTS-STARTPTS")
											((and (plist-get info :keep-original-duration)
														(<= video-duration duration))
											 (format "setpts=PTS-STARTPTS,tpad=stop_mode=clone:stop_duration=%.3f" (/ (- duration video-duration) 1000.0)))
											(duration (format "setpts=(PTS-STARTPTS)*%.3f,setpts=PTS-STARTPTS" (/ duration (* 1.0 video-duration))))
											(t "setpts=PTS-STARTPTS"))
										 (plist-get info :filter)))
							",")
						 (or output
								 (format "r%d" (plist-get info :index)))))))

(defun compile-media-get-animated-gif-frames (filename)
  "Return the number of frames for an animated GIF at FILENAME."
  (string-to-number
   (shell-command-to-string
    (concat "ffprobe -v error -select_streams v:0 -count_packets -show_entries stream=nb_read_packets -of csv=p=0 "
            (shell-quote-argument filename)))))

(defun compile-media-get-file-duration-ms (filename)
  "Return the duration of FILENAME in milliseconds."
  (* 1000
     (string-to-number
      (shell-command-to-string
       (concat "ffprobe -v error -show_entries format=duration -of default=noprint_wrappers=1:nokey=1 "
               (shell-quote-argument filename))))))

(defun compile-media-max-dimensions (&rest files)
  "Return the maximum dimensions for FILES."
  (let ((dimensions (mapcar #'compile-media-video-dimensions (if (listp (car files)) (car files) files))))
    (cons (apply #'max (mapcar 'car dimensions))
          (apply #'max (mapcar 'cdr dimensions)))))

(defun compile-media--scale-filter (o)
  "Return the complex filter for scaling O."
	(let ((size (compile-media-video-dimensions (plist-get o :source))))
		;; TODO: handle x1, y1, x2, y2, description
		(if	(and
				 (null (plist-get o :description))
				 (or (null compile-media-output-video-width) (= (car size) compile-media-output-video-width))
				 (or (null compile-media-output-video-height) (= (cdr size) compile-media-output-video-height)))
				"scale"
			(format "%sscale=%d:%d:force_original_aspect_ratio=decrease,setsar=sar=1,pad=%d:%d:(ow-iw)/2:%d+(oh-%d-ih)/2"
							(if (and (plist-get o :x1) (plist-get o :y1)
											 (plist-get o :x2) (plist-get o :y2))
									(format "crop=x=%d:y=%d:w=%d:h=%d,"
													(plist-get o :x1) (plist-get o :y1)
													(- (plist-get o :x2) (plist-get o :x1))
													(- (plist-get o :y2) (plist-get o :y1)))
								"")
							compile-media-output-video-width
							(if (plist-get o :description)
									(- compile-media-output-video-height
										 (or compile-media-description-height 0))
								compile-media-output-video-height)
							compile-media-output-video-width
							compile-media-output-video-height
							(or (and (plist-get o :description) compile-media-description-height) 0)
							(or (and (plist-get o :description) compile-media-description-height) 0)))))

;; (defun compile-media-ffmpeg-input (file start-ms stop-ms &optional frame-before)
;;   "Return the input arguments for FILE from START-MS to STOP-MS.
;; If FRAME-BEFORE is specified, seek to that beforehand.
;; Make sure to adjust other timestamps based on FRAME-BEFORE."
;;   (if frame-before
;;       (format "-ss %.3f -i %s -ss %.3f -t %.3f"
;;               (/ frame-before 1000.0)
;;               file
;;               (/ (- start-ms frame-before) 1000.0)
;;               (/ (- stop-ms start-ms) 1000.0))
;;     (format "-i %s -ss %.3f -t %.3f"
;;             file
;;             (/ start-ms 1000.0)
;;             (/ (- stop-ms start-ms) 1000.0))))


(defun compile-media--input-seq-as-list (start count input-prefix)
	(mapcar (lambda (i) (format "%s%d" input-prefix i)) (number-sequence start (1- (+ start count)))))

(defun compile-media--input-seq (start count input-prefix)
	(mapconcat (lambda (i) (format "[%s%d]" input-prefix i)) (number-sequence start (1- (+ start count))) ""))

(defun compile-media--concat (n type input-prefix output-name)
  "Return the concat filter for N inputs of TYPE using INPUT-PREFIX.
The output is sent to OUTPUT-NAME. TYPE is v=1:a=0 or a=0:v=1."
  (concat
	 (compile-media--input-seq 0 n input-prefix)
   (format "concat=n=%d:%s[%s]" n type output-name)))

(defun compile-media--overlay (input-sequence output-name)
  "Return the overlay filters for N inputs of TYPE using INPUT-PREFIX.
The output is sent to OUTPUT-NAME. TYPE is v=1:a=0 or a=0:v=1.
Overlays are done two at a time."
	(let (result (last (car (last input-sequence))))
		(car
		 (seq-reduce
			(lambda (prev val)
				(let ((output (if (string= val last)
													output-name
												(concat val "-overlaid"))))
					(cons
					 (format "%s[%s][%s]overlay[%s]"
									 (if (car prev) (concat (car prev) ",") "")
									 (cdr prev)
									 val
									 output)
					 output)))
			(cdr input-sequence)
			(cons nil (car input-sequence))))))

;;https://emacs.stackexchange.com/questions/48256/how-to-have-a-buffer-interpret-c-m-as-an-actual-carriage-return
(defun compile-media--process-filter-function (proc input-string)
  "Handle ^M for progress reporting for PROC given INPUT-STRING."
  (let ((proc-buf (process-buffer proc)))
    (when (buffer-live-p proc-buf)
      (with-current-buffer proc-buf
        (let ((inhibit-read-only t))
          (save-excursion
            (goto-char (point-max))
            (if (not (string= "\r" (substring input-string 0 1)))
                (insert input-string)
              (delete-region (line-beginning-position) (line-end-position))
              (insert (substring input-string 1)))))))))

(defun compile-media-split-tracks (sources)
  "Split SOURCES, a list of the form ((:source ... :include '(video)) ...).
Return a list of the form ((video (:source ...) (:source ...))
\(audio (:source ...) (:source ...))).
If :include is not specified, include it for all the tracks."
  (mapcar
   (lambda (track)
     (cons track (seq-filter (lambda (o) (or (null (plist-get o :include)) (member track (plist-get o :include)))) sources)))
   '(video audio)))

(defun compile-media (sources output-file &rest args)
  "Combine SOURCES into OUTPUT-FILE. Pass ARGS to ffmpeg.
SOURCES should be a list of the form
((video (:source filename :start-ms start-ts :stop-ms stop-ts)
        (:source filename :start-ms start-ts :stop-ms stop-ts)
        (:source filename :start-ms start-ts :stop-ms stop-ts))
 (video (:text \"text text\" :start-ms start-ts :stop-ms stop-ts))
 (audio (:source filename :start-ms start-ts :stop-ms stop-ts)
        (:source filename :start-ms start-ts :stop-ms stop-ts))
 (subtitles (:source filename)))
"
  (let ((ffmpeg-cmd (compile-media-get-command sources output-file)))
		(when (assoc-default 'subtitles sources)
			(cond
				((plist-get (assoc-default 'subtitles sources) :source)

				 (funcall (if (plist-get (assoc-default 'subtitles sources) :temporary)
											#'rename-file
										#'copy-file)
									(plist-get (assoc-default 'subtitles sources) :source)
									(concat (file-name-sans-extension output-file) ".vtt")
									t))
				((plist-get (assoc-default 'subtitles sources) :subtitles)
				 (subed-create-file
					(concat (file-name-sans-extension output-file) ".vtt")
					(plist-get (assoc-default 'subtitles sources) :subtitles)))))
    (with-current-buffer (get-buffer-create (format "*ffmpeg-%s*" output-file))
      (when (process-live-p compile-media--conversion-process)
        (quit-process compile-media--conversion-process))
      (erase-buffer)
      (insert ffmpeg-cmd "\n")
      (setq compile-media--conversion-process
            (start-process-shell-command "ffmpeg" (current-buffer) ffmpeg-cmd))
      (set-process-coding-system compile-media--conversion-process 'utf-8-dos 'utf-8-dos)
      (set-process-filter compile-media--conversion-process 'compile-media--process-filter-function)
      (set-process-sentinel compile-media--conversion-process
                            (lambda (process event)
                              ;; (when (save-match-data (string-match "finished" event))
                              ;;   (when subtitle-file (delete-file subtitle-file)))
                              (when (plist-get args :sentinel)
                                (funcall (plist-get args :sentinel) process event))))
      (display-buffer (current-buffer)))))

(defun compile-media-sync (sources output-file &rest args)
  "Combine SOURCES into OUTPUT-FILE. Pass ARGS."
  (let ((ffmpeg-cmd (compile-media-get-command sources output-file)))
    (with-current-buffer (get-buffer-create (format "*ffmpeg-%s*" output-file))
      (erase-buffer)
      (insert ffmpeg-cmd "\n")
      (shell-command ffmpeg-cmd (current-buffer))
			(when (assoc-default 'subtitles sources)
				(shell-command (format "ffmpeg -y -i %s %s.vtt"
															 output-file
															 (file-name-sans-extension output-file))
											 (current-buffer)))
			(when (plist-get args :sentinel)
        (funcall (plist-get args :sentinel) nil "finished"))
			(display-buffer (current-buffer)))))

(defun compile-media--select-spans (current)
  "Return select filter for CURRENT."
  (mapconcat
   (lambda (o)
     (format "between(t,%.3f,%.3f)"
             (/ (plist-get o :start-ms) 1000.0)
             (/ (plist-get o :stop-ms) 1000.0)))
   current
   "+"))

(defun compile-media--handle-padding (list)
	"Add entries for silence-padding."
	(reverse
	 (seq-reduce
		(lambda (prev val)
			;; :pad-left
			(when (plist-get val :pad-left)
				(setq prev (cons
										(list :source 'silence
													:duration-ms (plist-get val :pad-left))
										prev))
				(plist-put val :pad-left nil))
			;; current
			(setq prev (cons val prev))
			;; :pad-right
			(when (plist-get val :pad-right)
				(setq prev (cons
										(list :source 'silence
													:duration-ms (plist-get val :pad-right))
										prev))
				(plist-put (car prev) :pad-right nil))
			prev)
		list nil)))

(defun compile-media--combine-sources (list)
  "Combine sources in LIST."
  (let ((temp list) current previous result)
    (while temp
      (setq current
            (seq-take-while
             (lambda (o)
               (prog1 (or (null previous)
                          (and
													 ;; same source, monotonically increasing in time
													 (stringp (plist-get o :source))
													 (stringp (plist-get previous :source))
                           (string= (plist-get o :source) (plist-get previous :source))
                           (plist-get o :start-ms)
                           (plist-get previous :stop-ms)
                           (>= (plist-get o :start-ms) (plist-get previous :stop-ms))))
                 (setq previous o)))
             temp))
      (setq temp (seq-drop temp (length current)))
      (setq previous nil)
      (setq result (cons current result)))
    (reverse result)))

(defun compile-media--combine-times (list)
	"Combine :start-ms and :stop-ms that are only 1 ms apart."
	(mapcar
	 (lambda (entry)
		 (reverse
			(seq-reduce
			 (lambda (prev val)
				 (cond
					((null prev) (list val))
					((and
						(plist-get val :start-ms)
						(plist-get (car prev) :stop-ms)
						(= (plist-get val :start-ms)
							 (1+ (plist-get (car prev) :stop-ms))))
					 (plist-put (car prev) :stop-ms (plist-get val :stop-ms))
					 prev)
					(t
					 (cons val prev))))
			 (cdr entry)
			 (list (car entry)))))
	 list))

(defun compile-media--format-audio (list &optional start-input)
  "Determine arguments for audio in LIST.
LIST is a plist of (:start-ms ... :stop-ms ... :source)
START-INPUT should have the numerical index for the starting input file."
  (when list
    (let ((silence-counter 0)
					(groups
           (mapcar (lambda (current)
										 (if (eq 'silence (plist-get (car current) :source))
												 (list
													:silence t
													:filter (format "aevalsrc=exprs=0:d=%.3fs" (apply '+ (mapcar (lambda (o) (/ (plist-get o :duration-ms) 1000.0)) current))))
											 ;; regular source
											 (list
												:input (list "-i" (plist-get (car current) :source))
												:filter (format "aselect='%s',asetpts='N/SR/TB'"
																				(mapconcat
																				 (lambda (o)
																					 (let ((start-s (and (plist-get o :start-ms) (/ (plist-get o :start-ms) 1000.0)))
																								 (stop-s (and (plist-get o :stop-ms) (/ (plist-get o :stop-ms) 1000.0))))
																						 (cond
																							((and start-s stop-s) (format "between(t,%.3f,%.3f)" start-s stop-s))
																							(start-s (format "gte(t,%.3f)" start-s))
																							(stop-s (format "lte(t,%.3f)" stop-s)))))
																				 current
																				 "+")))))
									 (compile-media--combine-times
										(compile-media--combine-sources
										 (compile-media--handle-padding list))))))
      (list
       :input
       (seq-mapcat (lambda (o) (plist-get o :input)) groups)
       :filter
       (concat
        (string-join (seq-map-indexed
                      (lambda (o i)
												(if (plist-get o :silence)
														(progn
															(setq silence-counter (1+ silence-counter))
															(format "%s[%s]"
																			(plist-get o :filter)
																			(if (> (length groups) 1)
																					(format "a%d" i)
																				"a")))
													(format "[%d:a]%s[%s]"
																	(- (+ (or start-input 0) i) silence-counter)
																	(plist-get o :filter)
																	(if (> (length groups) 1)
																			(format "a%d" i)
																		"a"))))
                      groups)
                     ";")
        (if (> (length groups) 1)
            (concat
             ";"
             (mapconcat (lambda (sink) (format "[a%d]" sink)) (number-sequence 0 (1- (length groups))) "")
             (format "concat=n=%d:v=0:a=1[a]" (length groups)))
          ""))
       :input-count (length groups)
       :output
       (list "-map:a" "[a]")))))

(defun compile-media--convert-timestamps (list)
  "Convert the timestamps in LIST."
  (mapc (lambda (o)
          (mapc (lambda (key)
                  (when (stringp (plist-get o key))
                    (setf (plist-get o key) (compile-media-string-to-msecs (plist-get o key)))))
                '(:start-ms :stop-ms :duration-ms :duration)))
        list))

(defun compile-media-get-args (sources output-file)
  "Return FFmpeg arguments for combining SOURCES into OUTPUT-FILE."
  (let* ((has-open-captions (plist-get (cadr (assoc 'subtitles sources)) :open-captions))
				 visual-args
         audio-args
				 visual-output)
    (mapc (lambda (track) (compile-media--convert-timestamps track)) sources)
    (setq visual-args (compile-media--format-visuals
											 (seq-filter (lambda (o) (member (car o)
																											 (if has-open-captions
																													 '(video text subtitles)
																												 '(video text))))
																	 sources)))
    (setq audio-args (compile-media--format-audio (cdr (assoc 'audio sources)) (plist-get visual-args :input-count)))
    (append
     (plist-get visual-args :input)
     (plist-get audio-args :input)
     ;; (when (and (assoc-default 'subtitles sources) (string-match "webm$" output-file) (not has-open-captions))
		 ;; 	 (seq-mapcat (lambda (f)
		 ;; 								 (list "-i" (plist-get f :source)))
		 ;; 							 (assoc-default 'subtitles sources)))
		 (when (delq nil
                 (list
                  (plist-get visual-args :filter)
                  (plist-get audio-args :filter)))
			 (list "-filter_complex" (string-join
																(delq nil
																			(list
																			 (plist-get visual-args :filter)
																			 (plist-get audio-args :filter)))
																";")))
     (plist-get visual-args :output)
     (plist-get audio-args :output)
     ;; (when (and (assoc-default 'subtitles sources) (string-match "webm$" output-file) (not has-open-captions))
     ;;   (list "-map:s"
		 ;; 				 (concat (number-to-string (+
		 ;; 																		(or (plist-get visual-args :input-count) 0)
		 ;; 																		(or (plist-get audio-args :input-count) 0)))
		 ;; 								 "?")))
     compile-media-ffmpeg-arguments
     (list "-y"
					 output-file)
     nil)))

(defun compile-media-get-command (sources output-file)
  "Return FFmpeg command for combining SOURCES into OUTPUT-FILE."
  (let ((temporary-files
         (string-join
          (seq-mapcat
           (lambda (track)
             (mapcar
              (lambda (entry) (shell-quote-argument (plist-get entry :source)))
              (seq-filter (lambda (entry) (plist-get entry :temporary))
                          (cdr track))))
           sources)
          " ")))
    (concat compile-media-ffmpeg-executable " "
            (mapconcat
						 (lambda (s) (format "\"%s\"" s))
						 (compile-media-get-args
              sources output-file)
						 " ")
            (if (> (length temporary-files) 0)
                (concat " && rm " temporary-files)
              ""))))

(defun compile-media-verify-video-keyframes (filename threshold throw-error)
  "Check FILENAME for premature ending.
Check if the ending of the video at FILENAME has the keyframes we're expecting.
THRESHOLD is the number of milliseconds to accept.
Return last keyframe time if it appears valid. Return nil if invalid.
If THROW-ERROR is non-nil, throw an error if invalid."
  (interactive (list (read-file-name "File: ") 10000 t))
  (let* ((duration (compile-media-get-file-duration-ms filename))
         (last-frames (compile-media-ffmpeg-get-keyframes-between filename (- duration threshold) duration)))
    (cond
     ((and last-frames (> (car (last last-frames)) (- duration threshold)))
      (car (last last-frames)))
     (throw-error (error "Video %s may have encoding issues, last keyframe %f before duration %s"
                         filename
                         (- duration
                            (or (car (last last-frames)) duration))
                         duration)))))

(defun compile-media-verify-video-frames (filename &optional threshold throw-error)
  "Check if the ending of the video at FILENAME has the frames we're expecting.
THRESHOLD is the number of milliseconds to accept, and it defaults to 3000.
Return last frame time if it appears valid, or nil if invalid.
If THROW-ERROR is non-nil, throw an error if invalid."
  (interactive (list (read-file-name "File: ") 3000 t))
  (let* ((threshold (or threshold 3000))
         (duration (compile-media-get-file-duration-ms filename))
         (last-frames (compile-media-ffmpeg-get-frames-between filename (- duration threshold) duration)))
    (cond
     ((and last-frames (> (car (last last-frames)) (- duration threshold)))
      (car (last last-frames)))
     (throw-error (error "Video %s may have encoding issues, last frame %f before duration %s"
                         filename
                         (- duration
                            (or (car (last last-frames)) duration))
                         duration)))))

(provide 'compile-media)
;;; compile-media.el ends here
