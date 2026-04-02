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
	 (equal (compile-media--format-visuals '((video (:source "test.webm" :start-ms 0 :stop-ms 100 :video-duration 10 :duration 100)))
																				 "v")
					'(:input ("-i" "test.webm") :filter
									 "[0:v]select='between(t,0.000,0.100)',setpts=(PTS-STARTPTS)*10.000[v]"
									 :output ("-map:v" "[v]") :input-count 1)))
	;; text by itself
	(should
	 (equal (compile-media--format-visuals
					 '((text (:text "Hello" :start-ms 0 :stop-ms 100)
									 (:text "world" :start-ms 100 :stop-ms 200)))
					 "v")
					'(:input ("-f" "lavfi" "-i" "color=size=1280x720:color=black") :filter
				"drawtext=fontcolor=white:x=5:y=5:fontsize=40:text=Hello:enable='between(t,0.000,0.100)',drawtext=fontcolor=white:x=5:y=5:fontsize=40:text=world:enable='between(t,0.100,0.200)'[v]"
				:output ("-map:v" "[v]") :input-count 1)))
	;; video + text should chain together
	(should
	 (equal (compile-media--format-visuals
					 '((video (:source "test.webm" :start-ms 0 :stop-ms 100 :video-duration 10 :duration 100)
										(:source "test2.webm" :video-duration 100 :duration 100))
						 (text (:text "Hello" :start-ms 0 :stop-ms 100)
									 (:text "world" :start-ms 100 :stop-ms 200)))
					 "v")
					'(:input ("-i" "test.webm" "-i" "test2.webm") :filter
				"[0:v]select='between(t,0.000,0.100)',setpts=(PTS-STARTPTS)*10.000[r0];[1:v]setpts=(PTS-STARTPTS)*1.000[r1];[r0][r1]concat=n=2:v=1:a=0[v-video];[v-video]drawtext=fontcolor=white:x=5:y=5:fontsize=40:text=Hello:enable='between(t,0.000,0.100)',drawtext=fontcolor=white:x=5:y=5:fontsize=40:text=world:enable='between(t,0.100,0.200)'[v]"
				:output ("-map:v" "[v]") :input-count 2)
					)))

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
