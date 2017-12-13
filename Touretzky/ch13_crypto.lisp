13.9

(setf crypto-text
      '("zj ze kljjls jf slapzi ezvlij pib kl jufwxuj p hffv jupi jf"
	"enlpo pib slafml pvv bfwkj"))

a.

(setf *encipher-table* (make-hash-table))

(setf *decipher-table* (make-hash-table))

b.

(defun make-substitution (code clear)
  (setf (gethash code *decipher-table*) clear)
  (setf (gethash clear *encipher-table*) code))

(make-substitution #\a #\y)

(gethash #\a *decipher-table*)

c.

(defun undo-substitution (code clear)
  (setf (gethash code *decipher-table*) nil)
  (setf (gethash clear *encipher-table*) nil))

(undo-substitution 'a `y)
  
d.

(defun clearh ()
  (clrhash *decipher-table*)
  (clrhash *encipher-table*))
  
(clear)

e.

;;this is practice

(defun decipher-string (enc-str)
  (let ((len (length enc-str)))
    (setf new-str (make-string len :initial-element #\Space))
    (dotimes (i len new-str)
      (if (null (gethash (aref enc-str i) *decipher-table*)) (setf (aref new-str i) #\ )
	  (setf (aref new-str i) (gethash (aref enc-str i) *decipher-table*))))))

(defun decipher-string (string)
  (do* ((len (length string))
	(new-string (make-string len :initial-element #\Space))
	(i 0 (+ i 1)))
       ((equal i len) new-string)
    (let* ((char (aref string i))
	   (new-char (gethash char *decipher-table*)))
      (when new-char
	(setf (aref new-string i) new-char)))))



(decipher-string "a aa x ax x a")

f.

(defun show-line ()
  (let* ((txt (car crypto-text))
	 (dec (decipher-string txt)))
    (format t "~&~A" txt)
    (format t "~%~A" dec)))

(defun show-line (line)
  (format t "~%~A~%~A~%"
	  line
	  (decipher-string line)))

g.

(defun show-text ()
  (format t "~&--------------------")
  (dolist (line crypto-text)
    (show-line line))
  (format t "~&--------------------"))

(show-text crypto-text)

h.

(defun get-first-char (obj)
  (char-downcase
   (char (format nil "~A" obj) 0)))

i.

(defun read-letter ()
  (let ((r (read)))
    (if (member r '(end undo)) r
	(get-first-char r))))

j.

(defun sub-letter (code)
  (let ((dec (gethash code *decipher-table*)))
    (when dec (format t "~&~S has already been deciphered to ~S!" code dec)
	  (return-from sub-letter nil))
    (format t "~&What does ~S decipher to? " code))
	  (let ((clear (read-letter)))
	    (cond ((and (characterp clear)
			(not (gethash clear *encipher-table*)))
		   (make-substitution code clear))
		  (t (format t "~&~S has already been " clear)
		     (format t "enciphered to ~S!" (gethash clear *encipher-table*))))))

k.

(defun undo-letter ()
  (format t "~&Undo which letter? ")
  (let* ((code (read-letter))
	 (clear (gethash code *decipher-table*)))
    (cond ((not (characterp code)) (format t "~&Invalid input."))
	  (clear (undo-substitution code clear))
	  (t (format t "~&'~A' has not been deciphered!" code)))))
    

l.

(defun solve (crypt)
  (show-text)
  (format t "~&Substitute which letter? ")
  (let ((code (read-letter)))
    (unless (equal code 'end)
      (cond ((characterp code) (sub-letter code))
	    ((equal code 'undo) (undo-letter)))
      (solve crypt))))

(defun solve ()
  (do* ((code nil))
       ((equal code 'end))
    (show-text)
    (format t "~&Substitute which letter? ")
    (setf code (read-letter))
    (cond ((characterp code) (sub-letter code))
	  ((equal code 'undo) (undo-letter))
	  ((equal code 'end) t)
	  (t (format t "~&Invalid inpet.")))))
