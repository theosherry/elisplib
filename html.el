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

(provide 'my-html-shebang)