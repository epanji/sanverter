(cl:in-package #:sanverter-test)

(defun suite-tests ()
  (run! 'subrip-suite)
  (run! 'subass-suite))

(defun pr-string (input &optional (index 1))
  (with-output-to-string (stream)
    (print-subrip input index stream)))

(defun pa-string (input)
  (with-output-to-string (stream)
    (let ((dialogue (nth-value 1 (parse-subass input))))
      (claraoke:print-script dialogue stream))))

(def-suite subrip-suite :description "Sanverter subrip test suite.")
(def-suite subass-suite :description "Sanverter subass test suite.")

(in-suite subrip-suite)

(test basic-dialogue-for-subrip
  (let ((str0 (format nil "1~%00:01:00,000 --> 00:01:05,000~%Hello world!~2%"))
        (str1 (format nil "00:01:00,000 --> 00:01:05,000~%Hello world!"))
        (str2 (format nil "00:01:00,000 --> 00:00:05,000~%Hello world!"))
        (str3 (format nil "00:01:00.000-->00:01:05.000~%Hello world!"))
        (str4 (format nil "00:01:00.000        -->        00:01:05,000~%Hello world!"))
        (str5 (format nil "00:01:00,000 --> 00:01:05,000 :x 100 :y 100~%Hello world!")))
    (is (string= str0 (pr-string str0)))
    (is (string= str0 (pr-string str1)))
    (is (string= str0 (pr-string str2)))
    (is (string= str0 (pr-string str3)))
    (is (string= str0 (pr-string str4)))
    (is (string= str0 (pr-string str5)))))

(test style-dialogue-for-subrip
  (let ((1str0 (format nil "1~%00:01:00,000 --> 00:01:05,000~%<font color=\"#FFFF00\">Hello <u>world!~%<b>Hello <i>world!</i></b></u></font>~2%"))
        (1str1 (format nil "1~%00:01:00,000 --> 00:01:05,000~%<font color=#ffff00>Hello <u>world!~%<b>Hello <i>world!~2%"))
        (1str2 (format nil "1~%00:01:00,000 --> 00:01:05,000~%<font color=#ff0>Hello <u>world!~%<b>Hello <i>world!~2%"))
        (2str0 (format nil "1~%00:01:00,000 --> 00:01:05,000~%<font color=\"#FFFF00\">Hello <u>world!~%<b>Hello </font><i>world!</i></b></u>~2%"))
        (2str1 (format nil "1~%00:01:00,000 --> 00:01:05,000~%<font color=#ff0>Hello <u>world!~%<b>Hello </font><i>world!~2%"))
        (2str2 (format nil "1~%00:01:00,000 --> 00:01:05,000~%<font color=#ff0>Hello <u>world!~%</b><b>Hello </font></i><i>world!~2%")))
    (is (string= 1str0 (pr-string 1str0)))
    (is (string= 1str0 (pr-string 1str1)))
    (is (string= 1str0 (pr-string 1str2)))
    (is (string= 2str0 (pr-string 2str0)))
    (is (string= 2str0 (pr-string 2str1)))
    (is (string= 2str0 (pr-string 2str2)))))

(test html-character-for-subrip
  (let ((1str0 (format nil "1~%00:01:00,000 --> 00:01:05,000~%Hello &quot;world!&quot;~2%"))
        (1str1 (format nil "1~%00:01:00,000 --> 00:01:05,000~%Hello &amp; world!~2%"))
        (1str2 (format nil "1~%00:01:00,000 --> 00:01:05,000~%Hello &apos;world&apos;!~2%"))
        (1str3 (format nil "1~%00:01:00,000 --> 00:01:05,000~%Hello&nbsp;world!~2%"))
        (1str4 (format nil "1~%00:01:00,000 --> 00:01:05,000~%Hello&lt;world!~2%"))
        (1str5 (format nil "1~%00:01:00,000 --> 00:01:05,000~%Hello&gt;world!~2%"))
        (2str0 (format nil "1~%00:01:00,000 --> 00:01:05,000~%Hello \"world!\"~2%"))
        (2str1 (format nil "1~%00:01:00,000 --> 00:01:05,000~%Hello & world!~2%"))
        (2str2 (format nil "1~%00:01:00,000 --> 00:01:05,000~%Hello 'world'!~2%"))
        (2str3 (format nil "1~%00:01:00,000 --> 00:01:05,000~%Hello world!~2%"))
        (2str4 (format nil "1~%00:01:00,000 --> 00:01:05,000~%Hello<world!~2%"))
        (2str5 (format nil "1~%00:01:00,000 --> 00:01:05,000~%Hello>world!~2%")))
    (is (string= 2str0 (pr-string 1str0)))
    (is (string= 2str1 (pr-string 1str1)))
    (is (string= 2str2 (pr-string 1str2)))
    (is (string= 2str3 (pr-string 1str3)))
    (is (string= 2str4 (pr-string 1str4)))
    (is (string= 2str5 (pr-string 1str5)))))


(in-suite subass-suite)

(test basic-dialogue-for-subass
  (let ((str0 (format nil "Dialogue: 0,0:01:00.00,0:01:05.00,Default,,0,0,0,,Hello world!~2%"))
        (str1 "Dialogue: 0,1:,1:5,Default,,0,0,0,,Hello world!")
        (str2 "Dialogue: 0,1:,1:5,,,0,0,0,,Hello world!")
        (str3 "Dialogue: ,1:,1:5,,,,,,,Hello world!")
        (str4 "Dialogue:,1:,1:5,,,,,,,Hello world!"))
    (is-true (typep (parse-subass str0) 'claraoke-subtitle:subtitle))
    (is (string= str0 (pa-string str0)))
    (is (string= str0 (pa-string str1)))
    (is (string= str0 (pa-string str2)))
    (is (string= str0 (pa-string str3)))
    (is (string= str0 (pa-string str4)))))

(test style-dialogue-for-subass
  (let ((1str0 (format nil "Dialogue: 0,0:01:00.00,0:01:05.00,Default,,0,0,0,,{\\1c&H00FFFF&}Hello {\\u1}world!\\N{\\b1}Hello {\\i1}world!~2%"))
        (1str1 "Dialogue: 0,1:,1:5,,,,,,,{\\1c&H00FFFF&}Hello {\\u1}world!\\N{\\b1}Hello {\\i1}world!")
        (2str0 (format nil "Dialogue: 0,0:01:00.00,0:01:05.00,Default,,0,0,0,,{\\1c&H00FFFF&}Hello {\\u1}world!\\N{\\b1}Hello {\\1c\\i1}world!~2%"))
        (2str1 "Dialogue: 0,1:,1:5,,,,,,,{\\1c&H00FFFF&}Hello {\\u1}world!\\N{\\b1}Hello {\\1c\\i1}world!")
        (2str2 "Dialogue: 0,1:,1:5,,,,,,,{\\c&H00FFFF&}Hello {\\u1}world!\\N{\\b1}Hello {\\c\\i1}world!"))
    (is (string= 1str0 (pa-string 1str0)))
    (is (string= 1str0 (pa-string 1str1)))
    (is (string= 2str0 (pa-string 2str0)))
    (is (string= 2str0 (pa-string 2str1)))
    (is (string= 2str0 (pa-string 2str2)))))

