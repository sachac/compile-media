(require 'ert)
(require 'compile-media)
(ert-deftest compile-media--format-text-track ()
	(should
	 (equal
		(compile-media--format-text-track '(text (:text "Hello" :start-ms 0 :stop-ms 100))
																			"v")
		'(:filter "drawtext=fontcolor=white:x=5:y=5:fontsize=40:text=Hello:enable='between(t,0.000,0.100)'[v]")))
	(should
	 (equal
		(compile-media--format-text-track '((:text "Hello" :start-ms 0 :stop-ms 100)
																				(:text "world" :start-ms 100 :stop-ms 200))
																			"v")
		'(:filter "drawtext=fontcolor=white:x=5:y=5:fontsize=40:text=Hello:enable='between(t,0.000,0.100)',drawtext=fontcolor=white:x=5:y=5:fontsize=40:text=world:enable='between(t,0.100,0.200)'[v]"))))


(ert-deftest compile-media--format-audio ()
	"Tests `compile-media--format-audio'."
	(should
	 (equal
		(compile-media--format-audio
		 '((:start-ms 0 :stop-ms 999 :source "test.opus")
			 (:start-ms 1000 :stop-ms 1999 :source "test.opus")))
		'(:input ("-ss" 0.0 "-t" 1.999 "-i" "test.opus") :filter
						 "[0:a]anull[a]" :input-count 1 :output ("-map:a" "[a]"))))
	(should
	 (equal
		(compile-media--format-audio
		 '((:start-ms 0 :stop-ms 999 :source "test.opus")
			 (:start-ms 1000 :stop-ms 1599 :source "test.opus" :av-offset -100)))
		'(:input
			("-ss" 0.0 "-t" 0.999 "-i" "test.opus" "-ss" 1.1 "-t" 0.599 "-i"
			 "test.opus")
			:filter "[0:a]anull[a0];[1:a]anull[a1];[a0][a1]concat=n=2:v=0:a=1[a]"
			:input-count 2 :output ("-map:a" "[a]"))))
	(should
	 (equal
		(compile-media--format-audio
		 '((:start-ms 0 :stop-ms 999 :source "test.opus")
			 (:start-ms 1000 :stop-ms 1599 :source "test.opus" :av-offset 100)))
		'(:input
			("-ss" 0.0 "-t" 0.999 "-i" "test.opus" "-ss" 1.0 "-t" 0.499 "-i"
			 "test.opus")
			:filter
			"[0:a]anull[a0];[1:a]adelay=100:all=1[a1];[a0][a1]concat=n=2:v=0:a=1[a]"
			:input-count 2 :output ("-map:a" "[a]"))
		))
	(should
	 (equal
		(compile-media--format-audio
		 '((:start-ms 0 :stop-ms 999 :source "test.opus")
			 (:start-ms 1000 :stop-ms 1599 :source "test.opus" :av-offset 100)
			 (:start-ms 1600 :stop-ms 1699 :source "test.opus")))
		'(:input
			("-ss" 0.0 "-t" 0.999 "-i" "test.opus" "-ss" 1.0 "-t" 0.599 "-i"
			 "test.opus")
			:filter
			"[0:a]anull[a0];[1:a]adelay=100:all=1[a1];[a0][a1]concat=n=2:v=0:a=1[a]"
			:input-count 2 :output ("-map:a" "[a]"))
		))
	;; av-offset should be carried forward from previous cues.
	(should
	 (equal
		(compile-media--format-audio
		 '((:start-ms 0 :stop-ms 999 :source "test.opus")
			 (:start-ms 1000 :stop-ms 1599 :source "test.opus" :av-offset -100)
			 (:start-ms 1700 :stop-ms 1799 :source "test.opus")))
		'(:input
			("-ss" 0.0 "-t" 0.999 "-i" "test.opus" "-ss" 1.1 "-t" 0.599 "-i"
			 "test.opus" "-ss" 1.8 "-t" 0.099 "-i" "test.opus")
			:filter
			"[0:a]anull[a0];[1:a]anull[a1];[2:a]anull[a2];[a0][a1][a2]concat=n=3:v=0:a=1[a]"
			:input-count 3 :output ("-map:a" "[a]"))

		))
	(should
	 (equal
		(compile-media--format-audio
		 '((:start-ms 0 :stop-ms 999 :source "test.opus")
			 (:start-ms 1000 :stop-ms 1599 :source "test.opus" :av-offset 100)
			 (:start-ms 1600 :stop-ms 1799 :source "test.opus")))
		'(:input
			("-ss" 0.0 "-t" 0.999 "-i" "test.opus" "-ss" 1.0 "-t" 0.699 "-i"
			 "test.opus")
			:filter
			"[0:a]anull[a0];[1:a]adelay=100:all=1[a1];[a0][a1]concat=n=2:v=0:a=1[a]"
			:input-count 2 :output ("-map:a" "[a]"))))
	(should
	 (equal
		(compile-media--format-audio
		 '((:start-ms 0 :stop-ms 999 :source "test.opus")
			 (:start-ms 1000 :stop-ms 1599 :source "test.opus" :av-offset 100)
			 (:start-ms 1700 :stop-ms 1799 :source "test.opus")))
		'(:input
			("-ss" 0.0 "-t" 0.999 "-i" "test.opus"
			 "-ss" 1.0 "-t" 0.499 "-i" "test.opus"
			 "-ss" 1.6 "-t" 0.099 "-i" "test.opus")
			:filter
			"[0:a]anull[a0];[1:a]adelay=100:all=1[a1];[2:a]anull[a2];[a0][a1][a2]concat=n=3:v=0:a=1[a]"
			:input-count 3 :output ("-map:a" "[a]"))))
	;; If I have several positive syncs, they shouldn't add too much
	(should
	 (equal
		(compile-media--format-audio
		 '((:start-ms 0 :stop-ms 999 :source "test.opus")
			 (:start-ms 1000 :stop-ms 1599 :source "test.opus" :av-offset 300)
			 (:start-ms 1700 :stop-ms 2599 :source "test.opus" :av-offset 500)
			 (:start-ms 5000 :stop-ms 5199 :source "test.opus" :av-offset -100)
			 (:start-ms 5200 :stop-ms 5399 :source "test.opus" :av-offset -200)))
		'(:input
 ("-ss" 0.0 "-t" 0.999 "-i" "test.opus" "-ss" 1.0 "-t" 0.299 "-i"
	"test.opus" "-ss" 1.3 "-t" 0.799 "-i" "test.opus" "-ss" 5.1 "-t"
	0.199 "-i" "test.opus" "-ss" 5.4 "-t" 0.199 "-i" "test.opus")
 :filter
 "[0:a]anull[a0];[1:a]adelay=300:all=1[a1];[2:a]adelay=100:all=1[a2];[3:a]anull[a3];[4:a]anull[a4];[a0][a1][a2][a3][a4]concat=n=5:v=0:a=1[a]"
 :input-count 5 :output ("-map:a" "[a]")))))


(ert-deftest compile-media--format-open-captions ()
  "Tests `compile-media--format-open-captions'."
	(let* ((result (compile-media--format-open-captions
									'((:start-ms 1 :stop-ms 1000 :x 10 :y 10 :align "top left" :text "Hello world"))
									"captions"
									))
				 (text (with-temp-buffer
								 (insert-file-contents (car (plist-get result :temp-files)))
								 (buffer-string))))
		(should (string-match "\\an" text))
		(should (string-match "Hello world" text))
		(should (string-match "subtitles" (plist-get result :filter)))
		(should (string-match (regexp-quote (car (plist-get result :temp-files)))
													(plist-get result :filter)))
		(delete-file (car (plist-get result :temp-files)))))



(ert-deftest compile-media--combine-times ()
  "Tests `compile-media--combine-times'."
  (should (equal (compile-media--combine-times
									'((:start-ms 1 :stop-ms 999)
										(:start-ms 1000 :stop-ms 1999)))
								 '((:start-ms 1 :stop-ms 1999))))
	(should (equal (compile-media--combine-times
									'((:start-ms 1000 :stop-ms 2000)
										(:start-ms 2000 :stop-ms 3000)))
								 '((:start-ms 1000 :stop-ms 3000))))
	;; don't combine if we have different offsets
	(should (equal (compile-media--combine-times
									'((:start-ms 1000 :stop-ms 1999)
										(:start-ms 2000 :stop-ms 2999 :av-offset 200)
										(:start-ms 3000 :stop-ms 3999 :av-offset 300)))
								 '((:start-ms 1000 :stop-ms 1999)
									 (:start-ms 2000 :stop-ms 2999 :av-offset 200)
									 (:start-ms 3000 :stop-ms 3999 :av-offset 300))))
	;; combine if we have the same offset
	(should (equal (compile-media--combine-times
									'((:start-ms 1000 :stop-ms 1999)
										(:start-ms 2000 :stop-ms 2999 :av-offset 200)
										(:start-ms 3000 :stop-ms 3999 :av-offset 200)))
								 '((:start-ms 1000 :stop-ms 1999)
									 (:start-ms 2000 :stop-ms 3999 :av-offset 200)))))


(ert-deftest compile-media-ass-format-tags ()
  "Tests `compile-media-ass-format-tags'."
  (should
	 (equal (compile-media-ass-format-tags
					 '(:align "center center"))
					"\\an5"))
	(should (equal (compile-media-ass-format-tags
					 '(:font-size 20 :font-family "Arial" :align "center center"))
					"\\an5")))


(ert-deftest compile-media-expand-combined-sources ()
  "Tests `compile-media-expand-combined-sources'."
  (should (equal (compile-media-expand-combined-sources
                  '((combined (:source "test.webm" :original-start-ms 1000 :original-stop-ms 2000))
                    (combined (:source "test2.webm"))))
                 '((video
                    (:source "test.webm" :original-start-ms 1000
                             :original-stop-ms 2000))
                   (audio
                    (:source "test.webm" :original-start-ms 1000
                             :original-stop-ms 2000))
                   (video (:source "test2.webm"))
                   (audio (:source "test2.webm"))))))

(ert-deftest compile-media--process-intermediate-video ()
  "Tests `compile-media--process-intermediate-video'."
  (should
   (equal
    (let ((compile-media-output-video-width nil)
          (compile-media-output-video-height nil)
          (compile-media-output-video-fps nil))
      (compile-media--process-intermediate-video
       `((video (:source "test.webm")))
       "/tmp"
       t))
    '((:source
       "test.webm"
       :command
       ("ffmpeg" "-y" "-i" "test.webm"
        "-filter_complex"
        "[0:v]setpts=PTS-STARTPTS,scale[v]"
        "/tmp/video_0000.webm")
       :intermediate "/tmp/video_0000.webm")))))


(ert-deftest compile-media--prepare-static-image ()
  "Tests `compile-media--prepare-static-image'."
  (should (equal (compile-media--prepare-static-image
                  '(:source "test.png" :index 0 :duration-ms 2000.0))
                 '(:input
                   ("-loop" "1" "-t" "2.000" "-i" "test.png")
                   :filter "[0:v]scale[r0]"))))

(ert-deftest compile-media--prepare-video ()
  "Tests `compile-media--prepare-video'."
  (should
   (equal
    (let ((compile-media-output-video-width nil)
          (compile-media-output-video-height nil))
      (compile-media--prepare-video
       '(:source "test.webm" :index 0)))
    '(:input ("-i" "test.webm") :filter "[0:v]setpts=PTS-STARTPTS[r0]")))
  (should
   (equal
    (let ((compile-media-output-video-width nil)
          (compile-media-output-video-height nil))
      (compile-media--prepare-video
       '(:source "test.webm" :index 0
                 :original-start-ms 0
                 :original-stop-ms 6000
                 )))
    '(:input ("-i" "test.webm" "-t" "6.000") :filter "[0:v]setpts=PTS-STARTPTS[r0]"))))

(ert-deftest compile-media--format-visuals-for-a-single-track ()
  (let ((compile-media-output-video-width nil)
        (compile-media-output-video-height nil))
    (should
	   (equal
      (compile-media--format-visuals-for-a-single-track
		   '((:source "test.webm" :start-ms 0 :stop-ms 100 :video-duration 10 :duration 100))
		   "v")
		  '(:input ("-i" "test.webm") :filter
						   "[0:v]select='between(t,0.000,0.100)',setpts=(PTS-STARTPTS)*10.000,setpts=PTS-STARTPTS,scale[v]"
						   :input-count 1)))
    ;; text
    (should
	   (equal
      (compile-media--format-visuals-for-a-single-track
		   '((:source "test.webm" :start-ms "00:00:00.000" :stop-ms "00:00:00.100" :video-duration 10 :duration 100))
		   "v")
		  '(:input ("-i" "test.webm") :filter
						   "[0:v]select='between(t,0.000,0.100)',setpts=(PTS-STARTPTS)*10.000,setpts=PTS-STARTPTS,scale[v]"
						   :input-count 1)))
    (should
	   (equal
		  (compile-media--format-visuals-for-a-single-track
		   '((:source "test.webm" :start-ms 0 :stop-ms 100 :video-duration 100 :duration 100))
		   "v")
		  '(:input ("-i" "test.webm") :filter
						   "[0:v]select='between(t,0.000,0.100)',setpts=(PTS-STARTPTS)*1.000,setpts=PTS-STARTPTS,scale[v]"
						   :input-count 1)))
    (should
     (equal
      (compile-media--format-visuals-for-a-single-track '((:source "test.webm" :start-ms 0 :stop-ms 100 :video-duration 200 :duration 100)) "v")

      '(:input ("-i" "test.webm")
               :filter "[0:v]select='between(t,0.000,0.100)',setpts=(PTS-STARTPTS)*0.500,setpts=PTS-STARTPTS,scale[v]"
               :input-count 1)))
    (should
     (equal
      (compile-media--format-visuals-for-a-single-track
       '((:source "test.webm" :start-ms 0 :stop-ms 100 :video-duration 200 :duration 100)
         (:source "test2.webm" :video-duration 200 :duration 200))
       "v")
      '(:input ("-i" "test.webm" "-i" "test2.webm") :filter
               "[0:v]select='between(t,0.000,0.100)',setpts=(PTS-STARTPTS)*0.500,setpts=PTS-STARTPTS,scale[r0];[1:v]setpts=(PTS-STARTPTS)*1.000,setpts=PTS-STARTPTS,scale[r1];[r0][r1]concat=n=2:v=1:a=0[v]"
               :input-count 2)))))


(ert-deftest compile-media--format-visuals ()
	;; video by itself
	(should
   (equal
    (let ((compile-media-output-video-width nil)
          (compile-media-output-video-height nil))
      (compile-media--format-visuals
       '((video (:source "test.webm" :start-ms 0 :stop-ms 100 :video-duration 10 :duration 100)))
       "v"))
    '(:input
      ("-i" "test.webm")
      :filter "[0:v]select='between(t,0.000,0.100)',setpts=(PTS-STARTPTS)*10.000,setpts=PTS-STARTPTS,scale[v]"
      :output nil :input-count 1)))

	(should
   (equal
    (let ((compile-media-output-video-width 640)
          (compile-media-output-video-height 480))
      (compile-media--format-visuals
       '((video (:source "test.png" :duration-ms 1000))
         (video (:source "test2.png" :duration-ms 2000)))
       "v"))
    '(:input
      ("-loop" "1" "-t" "1.000" "-i" "test.png" "-loop"
       "1" "-t" "2.000" "-i" "test2.png")
      :filter
      "[0:v]scale=640:480:force_original_aspect_ratio=decrease,setsar=sar=1,pad=640:480:(ow-iw)/2:0+(oh-0-ih)/2[v-input-0];[1:v]scale=640:480:force_original_aspect_ratio=decrease,setsar=sar=1,pad=640:480:(ow-iw)/2:0+(oh-0-ih)/2[v-input-1];[v-input-0][v-input-1]overlay[v-overlaid-1]"
      :output nil :input-count 2)))
  (should
   (equal
    ;; Overlay these two videos
    (let ((compile-media-output-video-width nil)
          (compile-media-output-video-height nil))
      (compile-media--format-visuals
       '((video (:source "test.webm"))
         (video (:source "test2.webm")))
       "v"))
    '(:input
      ("-i" "test.webm" "-i" "test2.webm")
      :filter
      "[0:v]setpts=PTS-STARTPTS,scale[v-input-0];[1:v]setpts=PTS-STARTPTS,scale[v-input-1];[v-input-0][v-input-1]overlay[v-overlaid-1]"
      :output nil :input-count 2)))
  (should
   (equal
    ;; Overlay these two videos starting at a specific time
    (let ((compile-media-output-video-width nil)
          (compile-media-output-video-height nil))
      (compile-media--format-visuals
       '((video (:source "test.webm"))
         (video (:source "test2.webm" :output-start-ms 1000 :output-stop-ms 2000)))
       "v"))
    '(:input
      ("-i" "test.webm" "-i" "test2.webm")
      :filter "[0:v]setpts=PTS-STARTPTS,scale[v-input-0];[1:v]setpts=PTS-STARTPTS,scale[v-input-1];[v-input-0][v-input-1]overlay=enable='between(t,1.000,2.000)'[v-overlaid-1]"
      :output nil :input-count 2)))
  (should
   (equal
    ;; Overlay these three videos starting at a specific time
    (let ((compile-media-output-video-width nil)
          (compile-media-output-video-height nil))
      (compile-media--format-visuals
       '((video (:source "test.webm"))
         (video (:source "test2.webm" :output-start-ms 1000 :output-stop-ms 2000))
         (video (:source "test3.webm" :output-start-ms 3000 :output-stop-ms 4000)))
       "v"))
    '(:input
      ("-i" "test.webm" "-i" "test2.webm" "-i"
       "test3.webm")
      :filter
      "[0:v]setpts=PTS-STARTPTS,scale[v-input-0];[1:v]setpts=PTS-STARTPTS,scale[v-input-1];[v-input-0][v-input-1]overlay=enable='between(t,1.000,2.000)'[v-overlaid-1];[2:v]setpts=PTS-STARTPTS,scale[v-input-2];[v-overlaid-1][v-input-2]overlay=enable='between(t,3.000,4.000)'[v-overlaid-2]"
      :output nil :input-count 3)))
  (should
   (equal
    ;; Overlay with a picture
    (let ((compile-media-output-video-width 1280)
          (compile-media-output-video-height 720))
      (compile-media--format-visuals
       '((video (:source "test.webm"))
         (video (:source "test2.png" :output-start-ms 1000 :output-stop-ms 5000)))
       "v"))
    '(:input
      ("-i" "test.webm" "-loop" "1" "-i" "test2.png")
      :filter "[0:v]setpts=PTS-STARTPTS,scale=1280:720:force_original_aspect_ratio=decrease,setsar=sar=1,pad=1280:720:(ow-iw)/2:0+(oh-0-ih)/2[v-input-0];[1:v]scale=1280:720:force_original_aspect_ratio=decrease,setsar=sar=1,pad=1280:720:(ow-iw)/2:0+(oh-0-ih)/2[v-input-1];[v-input-0][v-input-1]overlay=enable='between(t,1.000,5.000)'[v-overlaid-1]"
      :output nil :input-count 2)))
  (should
   (equal
    ;; Overlay with a picture
    (let ((compile-media-output-video-width 1280)
          (compile-media-output-video-height 720))
      (compile-media--format-visuals
       '((video (:source "test.webm"))
         (video (:source "test2.png" :output-start-ms 1000 :output-stop-ms 5000
                         :output-pos (100 200 300 400))))
       "v"))
    '(:input
      ("-i" "test.webm" "-loop" "1" "-i" "test2.png")
      :filter "[0:v]setpts=PTS-STARTPTS,scale=1280:720:force_original_aspect_ratio=decrease,setsar=sar=1,pad=1280:720:(ow-iw)/2:0+(oh-0-ih)/2[v-input-0];[1:v]scale=1280:720:force_original_aspect_ratio=decrease,setsar=sar=1,pad=1280:720:(ow-iw)/2:0+(oh-0-ih)/2[v-input-1];[v-input-0][v-input-1]overlay=enable='between(t,1.000,5.000)'[v-overlaid-1]"
      :output nil :input-count 2))))

(ert-deftest compile-media--input-seq ()
	(expect (compile-media--input-seq 0 5 "v") :to-equal "[v0][v1][v2][v3][v4]"))

(ert-deftest compile-media--overlay ()
	(should
	 (string= (compile-media--overlay '("vt0" "vt1") "v")
						"[vt0][vt1]overlay[v]"))
	(should (string= (compile-media--overlay '("vt0" "vt1" "vt2") "v")
									 "[vt0][vt1]overlay[vt1-overlaid],[vt1-overlaid][vt2]overlay[v]"))
	(should
	 (string=
		(compile-media--overlay (compile-media--input-seq-as-list 0 4 "vt") "v")
		"[vt0][vt1]overlay[vt1-overlaid],[vt1-overlaid][vt2]overlay[vt2-overlaid],[vt2-overlaid][vt3]overlay[v]")))


(ert-deftest compile-media--between ()
	(expect (compile-media--between '(:start-ms 0 :stop-ms 100))
					:to-equal "between(t,0.000,0.100)")
	(expect (compile-media--between '(:stop-ms 100))
					:to-equal "lte(t,0.100)")
	(expect (compile-media--between '(:start-ms 100))
					:to-equal "gte(t,0.100)"))

(ert-deftest compile-media--format-trim ()
  (should
   (equal (compile-media--format-trim '((20 30) (40 50)))
          "1-between(t,0.020,0.030)-between(t,0.040,0.050)")))
