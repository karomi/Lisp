;; Touretzky book - Chapter 14 problems

(defmacro ppmx (form)
  "Pretty prints the macro expansion of FORM."
  `(let* ((exp1 (macroexpand-1 ',form))
	  (exp (macroexpand exp1))
	  (*print-circle* nil))
    (cond ((equal exp exp1)
	   (format t "~&Macro expansion:")
	   (pprint exp))
	  (t (format t "~&First step of expansion:")
	     (pprint exp1)
	     (format t "~%~%Final expansion:")
	     (pprint exp)))
    (format t "~%~%")
    (values)))

14.3

(defmacro set-nil (var)
  (list 'setq var (list 'null var)))

(defmacro set-nil (var)
  (list 'setq var nil))

14.4

(defmacro simple-rotatef (a b)
  `(let ((a1 ,a)
	 (b1 ,b))
     (setq ,a b1)
     (setq ,b a1)))

14.5

(defmacro set-mutual (a b)
  `(progn (setq ,a ',b)
	  (setq ,b ',a)))

14.6

(defmacro variable-chain (&rest var)
  `(progn
     ,@(do ((a var (rest a))
	    (end nil))
	   ((null (rest a)) (reverse end))
	 (push `(setf ,(first a) ',(second a))
	       end))))

(defstruct (node (:print-function print-node))
  (name nil)
  (inputs nil)
  (outputs nil))

(defun print-node (node stream depth)
  (format stream "#<Node ~A>" (node-name node)))

(defstruct (arc (:print-function print-arc))
  (from nil)
  (to nil)
  (label nil)
  (action nil))

(defun print-arc (arc stream depth)
  (format stream "#<ARC ~A / ~A / ~A>"
	  (node-name (arc-from arc))
	  (arc-label arc)
	  (node-name (arc-to arc))))

(defvar *nodes*)
(defvar *arcs*)
(defvar *current-node*)

(defun initialize ()
  (setf *nodes* nil)
  (setf *arcs* nil)
  (setf *current-node* nil))

(defun add-node (name)
  (let ((new-node (make-node :name name)))
    (setf *nodes* (nconc *nodes* (list new-node)))
    new-node))

(defmacro defnode (name)
  `(add-node ',name))

(defun find-node (name)
  (or (find name *nodes* :key #'node-name)
      (error "No node name ~A exists." name)))

(defun add-arc (from-name label to-name action)
  (let* ((from (find-node from-name))
	 (to (find-node to-name))
	 (new-arc (make-arc :from from
			    :label label
			    :to to
			    :action action)))
    (setf *arcs* (nconc *arcs* (list new-arc)))
    (setf (node-outputs from)
	  (nconc (node-outputs from)
		 (list new-arc)))
    (setf (node-inputs to)
	  (nconc (node-inputs to)
		 (list new-arc)))
    new-arc))

(defmacro defarc (from label to &optional action)
  `(add-arc ',from ',label ',to ',action))

(defun one-transition ()
  (format t "~&State ~A. Input: "
	  (node-name *current-node*))
  (let* ((ans (read))
	 (arc (find ans
		    (node-outputs *current-node*)
		    :key #'arc-label)))
    (unless arc
      (format t "~&No arc from ~A has label ~A.~%"
	      (node-name *current-node*) ans)
      (return-from one-transition nil))
    (let ((new (arc-to arc)))
      (format t "~&~A" (arc-action arc))
      (setf *current-node* new))))

(defun fsm (&optional (starting-point 'start))
  (setf *current-node* (find-node starting-point))
  (do ()
      ((null (node-outputs *current-node*)))
    (one-transition)))

;; before running fsm
(initialize)
(load "c:/Users/michael/Code/Lisp/ch/chap14.lisp")

14.8

Writing macros with side-effects is unwise because 
macro expansion can occur at any time when executing
a program. If a macro is compiled, the result on
execution may be whatever value is returned by
the macro instead of the side-effect.

14.9

Common Lisp special functions:

block		catch		[compiler-let]
declare		eval-when	flet
function	go			if
labels		let			let*
macrolet	multiple-value-call multiple-value-prog1
progn		progv		quote
return-form	setq		tagbody
the			throw		unwind-protect

14.10

10 to 100% faster after compilation

14.11

a.

(defun compile-arc (arc)
  (let ((a (arc-action arc)))
    `((equal this-input ',(arc-label arc))
      (format t "~&~A" ,a)
      (,(node-name (arc-to arc))
	(rest input-syms)))))

b.

(defun compile-node (node)
  (let ((name (node-name node))
	(arc-clauses
	 (mapcar #'compile-arc
		 (node-outputs node))))
    `(defun ,name (input-syms
		   &aux (this-input
			 (first input-syms)))
       (cond ((null input-syms) ',name)
	     ,@arc-clauses
	     (t (format t
			"~&There is no arc from ~A with label ~S"
			',name this-input))))))

c.

(defun compile-machine ()
  (progn ,@(mapcar #'compile-node *nodes*)))
