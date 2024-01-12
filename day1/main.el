(let ((bags (with-temp-buffer
	       (progn (insert "((")
		      (insert-file-contents "input")
		      (while (re-search-forward "^$" nil t)
			(replace-match ")(" nil nil))
		      (end-of-buffer)
		      (insert "))")
		      (goto-char 0)
		      (read (current-buffer))))))
  (cl-reduce #'max
	     (mapcar (lambda (bag) (cl-reduce #'+ bag)) bags))
  )


(let ((bags (with-temp-buffer
	       (progn (insert "((")
		      (insert-file-contents "input")
		      (while (re-search-forward "^$" nil t)
			(replace-match ")(" nil nil))
		      (end-of-buffer)
		      (insert "))")
		      (goto-char 0)
		      (read (current-buffer))))))
  (cl-reduce #'+
	     (seq-take
	      (sort (mapcar (lambda (bag) (cl-reduce #'+ bag)) bags) #'>)
	      3)
	     )
  )
