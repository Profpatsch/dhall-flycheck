; inspired by https://github.com/mmark-md/flycheck-mmark/blob/a11563dcb9ed48f71274e0c6eb9e76b65d44bf40/flycheck-mmark.el#L44

(defun flycheck-dhall-parse-errors (output checker buffer)
  "Decode dhall parse errors in JSON format decoding OUTPUT.
CHECKER is the checker used, BUFFER is the buffer that is being
checked."
  (let ((json-array-type 'list))
    (unless (string-empty-p output)
      (mapcar
       (lambda (err)
         (flycheck-error-new
          :checker checker
          :buffer  buffer
          :filename (cdr (assoc 'filename err))
          :line     (cdr (assoc 'line   err))
          :column   (cdr (assoc 'column err))
          :message  (cdr (assoc 'message   err))
          :level    'error))
       (json-read-from-string output)))))

(flycheck-define-checker dhall
  "A checker for the dhall configuration language.

See URL `https://dhall-lang.org'."
  :command ("dhall-flycheck" source)
  :error-parser flycheck-dhall-parse-errors
  :modes dhall-mode
)
(add-to-list 'flycheck-checkers 'dhall)

