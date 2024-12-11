(defun clean-string (str)
  (string-trim '(#\Space #\Tab #\Return) str))

(defun split (line &optional (delimiter #\,))
  (let ((result nil)
        (start 0))
    (loop for i = (position delimiter line :start start)
          do (if i
                 (progn
                   (push (clean-string (subseq line start i)) result)
                   (setf start (1+ i)))
                 (progn
                   (push (clean-string (subseq line start)) result)
                   (return))))
    (nreverse result)))

(defun hash-table-keys (table)
  (let ((keys '()))
    (maphash (lambda (key value)
               (push key keys))
             table)
    keys))

(defun read-csv-to-hash-tables (filename)
  (with-open-file (stream filename :direction :input)
    (let* ((header (split (clean-string (read-line stream))))
           (hash-tables nil))
      (do ((line (read-line stream nil 'eof) (read-line stream nil 'eof)))
          ((eq line 'eof) hash-tables)
        (let ((values (split (clean-string line)))
              (hash-table (make-hash-table :test #'equal)))
          (loop for key in header
                for value in values
                do (setf (gethash key hash-table) value))
          (push hash-table hash-tables)))
      (nreverse hash-tables))))

(defun select (filename &optional filter-fn)
  (lambda (&rest filters)
    (let* ((table (read-csv-to-hash-tables filename))
           (filtered-table table))
      (when filters
        (setf filtered-table
              (remove-if-not
               (lambda (row)
                 (every (lambda (filter)
                          (let* ((key (car filter))
                                 (values (cdr filter)))
                            (if (listp values)
                                (member (gethash key row) values :test #'equal)
                                (equal (gethash key row) values))))
                        filters))
               table)))
      (when filter-fn
        (setf filtered-table (remove-if-not filter-fn filtered-table)))
      filtered-table)))

(defun write-hash-table-to-csv (data filename)
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (when data
      (let ((keys (hash-table-keys (first data))))
        (format stream "~{~A~^,~}~%" keys)
        (dolist (record data)
          (format stream "~{~A~^,~}~%"
                  (mapcar (lambda (key) (gethash key record)) keys)))))))

(defun print-table (records &optional keys)
  (when records
    (let* ((keys (or keys (hash-table-keys (first records))))
           (column-width 20))
      (dolist (key keys)
        (format t "~vA" column-width key))
      (format t "~%~A~%" (make-string (* column-width (length keys)) :initial-element #\-))
      (dolist (record records)
        (dolist (key keys)
          (format t "~vA" column-width (gethash key record)))
        (format t "~%"))))
  (format t "~%~%"))

(defun hash-table-to-alist (hash-table)
  (let ((result '()))
    (maphash (lambda (key value)
               (push (cons key value) result))
             hash-table)
    result))

(defun test-reading-data ()
  (format t "All data from specialties.csv:~%")
  (print-table (funcall (select "/Users/admin/Desktop/University/ItFP/lab5/specialties.csv"))
               '("id" "name" "field"))

  (format t "All data from articles.csv:~%")
  (print-table (funcall (select "/Users/admin/Desktop/University/ItFP/lab5/articles.csv"))
               '("id" "title" "author" "specialty_id" "year"))

  (format t "Filter from articles.csv by author(John Doe):~%")
  (print-table (funcall (select "/Users/admin/Desktop/University/ItFP/lab5/articles.csv"
                                     (lambda (row)
                                       (string= (gethash "author" row) "John Doe"))))
               '("id" "title" "author" "specialty_id" "year"))

  (format t "Filter from articles.csv by year > 2022:~%")
  (print-table (funcall (select "/Users/admin/Desktop/University/ItFP/lab5/articles.csv"
                                     (lambda (row)
                                       (let ((year (gethash "year" row)))
                                         (> (parse-integer year) 2022)))))
               '("id" "title" "author" "specialty_id" "year")))

(defun test-write-data-to-csv-file ()
  (format t "Filter from articles.csv by year = 2022 or year = 2023, and write to output.csv:~%")
  (let ((filtered (funcall (select "/Users/admin/Desktop/University/ItFP/lab5/articles.csv"
                                   (lambda (row)
                                     (member (gethash "year" row) '("2022" "2023") :test #'equal))))))
    (if filtered
        (progn
          (print-table filtered '("id" "title" "author" "specialty_id" "year"))
          (write-hash-table-to-csv filtered "/Users/admin/Desktop/University/ItFP/lab5/output.csv"))
        (format t "No data found for the given filter.~%")))

    (format t "All data from output.csv:~%")
    (print-table (funcall (select "/Users/admin/Desktop/University/ItFP/lab5/output.csv"))
               '("id" "title" "author" "specialty_id" "year")))

(defun test-hash-table-to-alist ()
  (format t "Convert records from a hash-table to associative lists:~%")
  (let* ((hash-table (make-hash-table :test 'equal))
         (expected-alist '(("id" . "1")
                           ("title" . "Deep Learning Basics")
                           ("author" . "John Doe")
                           ("specialty_id" . "1")
                           ("year" . "2023"))))

    (setf (gethash "id" hash-table) "1")
    (setf (gethash "title" hash-table) "Deep Learning Basics")
    (setf (gethash "author" hash-table) "John Doe")
    (setf (gethash "specialty_id" hash-table) "1")
    (setf (gethash "year" hash-table) "2023")
    
    (let ((generated-alist (hash-table-to-alist hash-table)))
      (if (equal (sort (copy-list expected-alist) #'string< :key #'car) 
                 (sort (copy-list generated-alist) #'string< :key #'car))
          (format t "The result is correct: ~a~%" generated-alist)
          (format t "The result is not correct. Expected: ~a, but got: ~a~%" expected-alist generated-alist)))))

(defun test-check-database ()
  (format t "Start testing database functions~%")
  (test-reading-data)
  (test-write-data-to-csv-file)
  (test-hash-table-to-alist)
  (format t "End of tests~%"))

(test-check-database)