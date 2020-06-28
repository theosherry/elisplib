(defun my-html-shebang ()
  "Fill current buffer with minimum tags needed for html page."
  (interactive)
  (let ((shebang "<!DOCTYPE html>
<html lang=\"en\">
  <head>
    <meta charset=\"utf-8\">
    <title>title</title>
    <link rel=\"stylesheet\" href=\"style.css\">
    <script src=\"script.js\"></script>
  </head>
  <body>
    <!-- page content -->
  </body>
</html>"))
    (insert shebang)))

(defun my-svg-shebang ()
  "Insert svg markup."
  (interactive)
  (let ((shebang "<svg version=\"1.1\"
  baseProfile=\"full\"
  width=\"320\" height=\"320\"
  xmlns=\"http://www.w3.org/2000/svg\">
</svg>"))
    (insert shebang)))


(defun my-html-format ()
  "Rudimentary html formatting."
  (interactive)
  (sgml-pretty-print 0 (buffer-size))
  (indent-region 0 (buffer-size)))

(provide 'my-html-library)
