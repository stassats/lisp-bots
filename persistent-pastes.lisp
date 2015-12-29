(in-package :lisppaste)

;;;; Pastes

(defclass paste ()
  ((parent-paste :initarg :parent-paste :initform nil :accessor paste-parent-paste)
   (number :initarg :number :initform 0 :accessor paste-number)
   (user :initarg :user :initform "" :accessor paste-user)
   (title :initarg :title :initform "" :accessor paste-title)
   (contents :initarg :contents :initform "") ;; Accessor defined below
   (universal-time :initarg :universal-time :initform 0 :accessor paste-universal-time)
   (annotations :initarg :annotations :initform nil :accessor paste-annotations)
   (annotation-counter :initarg :annotation-counter :initform 0 :accessor paste-annotation-counter)
   (channel :initarg :channel :initform "" :accessor paste-channel)
   (colorization-mode :initarg :colorization-mode :initform "" :accessor paste-colorization-mode)
   (maybe-spam :initarg :maybe-spam :initform nil :accessor paste-maybe-spam-p)
   (is-unicode :initarg :is-unicode :initform :true :accessor paste-is-unicode-p)
   (deletion-requested :initarg :deletion-requested :initform nil :accessor paste-deletion-requested)
   (deletion-requested-email :initarg :deletion-requested-email :initform nil :accessor paste-deletion-requested-email)
   (expiration-time :initarg :expiration-time :initform nil :accessor paste-expiration-time)))


;;;; Serialization / deserialization

;;; Serialization is done to XML via s-xml's LXML representation.
;;; There are two sets of serialize / deserialize functions.  The
;;; lower-level functions are simple-serialize-with-type and
;;; simple-deserialize-with-type, which work with strings, keywords,
;;; integers and NIL.  The higher-level functions are
;;; serialize-object-slots and deserialize-object-slots, which work
;;; with CLOS instances whose slot values to be stored are
;;; representable via the lower-level functions.
;;;
;;; The SLOTS argument to serialize-object-slots and
;;; deserialize-object-slots is a list of (name reader writer) tuples
;;; where:
;;;
;;;   NAME is the name of the slot as it is known in LXML.
;;;
;;;   READER is a function of one argument that, given an object being
;;;   serialized, returns the value to use for the slot.
;;;
;;;   WRITER is a function of two arguments that, given a deserialized
;;;   value and an object being deserialized, sets the slot value.
;;;
;;; These are designed to be used with standard CLOS accessors, which
;;; is to say #'reader and #'(setf reader).
;;;
;;; During deserialization, if any slot in SLOTS is not found in the
;;; LXML input it is written to the object as NIL.

(defun simple-serialize-with-type (obj)
  "Return an LXML entity corresponding to the serialized form of the
serializable object OBJ."
  (etypecase obj
    (string `(:|string| ,obj))
    (keyword `(:|keyword| ,(symbol-name obj)))
    (integer `(:|integer| ,(prin1-to-string obj)))
    (null :|null|)))

(defun simple-deserialize-with-type (form)
  "Return the deserialized form of the serialized LXML entity FORM."
  (cond ((eq form :|null|)
	 nil)
	((eq form :|string|)
	 "")
	(t
	 (ecase (car form)
	   (:|string| (second form))
	   (:|keyword| (intern (second form) :keyword))
	   (:|integer| (parse-integer (second form)))))))

(defun serialize-object-slots (object slots &key root)
  "Return an LXML tree representing the serialized form of OBJECT as
described by SLOTS.  ROOT is the name for the root element of the
tree, and defaults to a lowercase keyword of the name of the class of
OBJECT."
  (list* (or root
             (intern (string-downcase (symbol-name (class-name (class-of object)))) :keyword))
         (mapcar #'(lambda (slot)
                     (destructuring-bind (name reader writer) slot
		       (declare (ignore writer))
		       (list name
			     (simple-serialize-with-type (funcall reader object)))))
                 slots)))

(defun deserialize-object-slots (list class slots)
  "Return a new instance of CLASS with slots described in SLOTS having
been initialized by accessors from the serialized form in the LXML
tree LIST."
  (let ((object (make-instance class)))
    (mapc #'(lambda (slot)
              (destructuring-bind (name reader writer) slot
		(declare (ignore reader))
		(funcall writer
			 (simple-deserialize-with-type
			  (or (second (assoc name (cdr list))) :|null|))
			 object)))
          slots)
    object))


;;;; Paste contents accessor

(defmethod (setf paste-contents) (contents paste)
  ;; This is currently only used in deserialization, so always setting
  ;; a weak pointer here shouldn't break anything.
  (setf (slot-value paste 'contents)
	(sb-ext:make-weak-pointer contents))
  contents)

(defun reload-paste-contents (paste)
  "Given PASTE (either a root paste or an annotation), reload its
contents from the copy in the storage file on disk.  Returns the
reloaded paste contents."
  (labels ((find-tag-contents (lxml tag)
	     (simple-deserialize-with-type (second (assoc tag (cdr lxml)))))
	   (set-contents-from (lxml)
	     (setf (paste-contents paste)
		   (find-tag-contents lxml :|contents|))))
    (let ((root-paste (or (paste-parent-paste paste) paste)))
      (with-open-file (s (paste-xml-file root-paste) :direction :input)
	(let ((lxml (s-xml:parse-xml-dom s :lxml)))
	  (if (eq paste root-paste)
	      (set-contents-from lxml)
	      (loop for ann-lxml = (s-xml:parse-xml-dom s :lxml)
		    while ann-lxml
		    when (= (paste-number paste)
			    (find-tag-contents ann-lxml :|number|))
		    return (set-contents-from ann-lxml)
		    finally (error "Unable to recover lost paste ~A annotation ~A contents"
				   (paste-number root-paste) (paste-number paste)))))))))

(defmethod paste-contents (paste)
  (let ((contents (slot-value paste 'contents)))
    (if (sb-ext:weak-pointer-p contents)
	(multiple-value-bind
	      (data valid)
	    (sb-ext:weak-pointer-value contents)
	  (if valid
	      data
	      (reload-paste-contents paste)))
	contents)))

(defun weaken-paste-contents (paste)
  "Allow the paste-contents of PASTE to be discarded by GC."
  (let ((contents (slot-value paste 'contents)))
    (unless (sb-ext:weak-pointer-p contents)
      (setf (slot-value paste 'contents)
	    (sb-ext:make-weak-pointer contents)))))


;;;; Paste list access

(defun paste-expired-p (paste)
  (and (paste-expiration-time paste)
       (<= (paste-expiration-time paste)
	   (get-universal-time))))

(defun find-expired-paste (number)
  (let ((found (find number *pastes* :key #'paste-number)))
    (when (and found (paste-expired-p found))
      found)))

(defun find-paste (number)
  (let ((found (find number *pastes* :key #'paste-number)))
    (unless (and found (paste-expired-p found))
      found)))

(defun find-pastes-like/title (paste)
  (remove (paste-title (find-paste paste))
	  *pastes* :key #'paste-title :test-not #'equal))

(defun find-pastes-like/user (paste)
  (remove (paste-user (find-paste paste))
	  *pastes* :key #'paste-user :test-not #'equal))

(defun list-pastes (&key starting-from limit in-channel maybe-spam deletion-requested)
  "Return a list of pastes matching certain criteria, sorted
descending by paste number.

STARTING-FROM may be an inclusive upper bound for paste number.

LIMIT may be a maximum number of pastes to return.

IN-CHANNEL may name a single channel worth of pastes to return or be
T, indicating all actual channels (as opposed to \"None\").

If MAYBE-SPAM is true, only returns pastes which maybe-spam-p.

If DELETION-REQUESTED is true, only returns pastes whose deletion has
been requested."
  (let ((source-paste-list (if starting-from
			       (member starting-from *pastes*
				       :key #'paste-number)
			       *pastes*)))
    (loop with count = 0
	  for paste in source-paste-list
	  until (and limit (>= count limit))
	  when (and
		(not (paste-expired-p paste))
		(or (not in-channel)
		    (if (eql in-channel t)
			(not (string-equal "None" (paste-channel paste)))
			(string-equal in-channel (paste-channel paste))))
		(or (not maybe-spam)
		    (paste-maybe-spam-p paste))
		(or (not deletion-requested)
		    (paste-deletion-requested paste)))
	  do (incf count)
	  and collect paste)))

(defun count-pastes (&rest args &key starting-from in-channel maybe-spam
		      deletion-requested)
  (declare (ignore starting-from in-channel))
  "Return the number of pastes matching certain criteria.

STARTING-FROM may an inclusive upper bound for paste number.

IN-CHANNEL may name a single channel worth of pastes to count or be T,
indicating all actual channels (as opposed to \"None\").

If MAYBE-SPAM is true, only count pastes which maybe-spam-p.

If DELETION-REQUESTED is true, only count pastes whose deletion has
been requested."
  ;; XXX: This is the cheap-hack implementation, just to get the
  ;; interface defined.
  (length (apply #'list-pastes args)))

(defun count-pastes-by-channel (&key (time nil))
  "Returns an association list of channel to number of pastes in the
channel, sorted by number of pastes."
  (let ((channel-hash (make-hash-table :test #'equalp))
	(now (get-universal-time)))
    (loop for paste in *pastes*
	  do (when (or (not time)
		       (< (- now (paste-universal-time paste)) time))
	       (incf (gethash (paste-channel paste) channel-hash 0))))
    (sort (loop for channel being each hash-key of channel-hash
		using (hash-value count)
		collect (cons channel count)) #'> :key #'cdr)))

(defun reset-paste-index ()
  "Reset the paste index to the empty state (no pastes)."
  (setf *pastes* nil)
  (setf *paste-counter* 0))

(defun add-paste-to-index (paste)
  "Add existing paste PASTE to the paste index."
  (with-paste-lock
    (setf *paste-counter* (max (paste-number paste) *paste-counter*))
    (push paste *pastes*)))

(defun remove-paste-from-index (paste)
  "Remove PASTE from the paste index."
  (with-paste-lock
   (setf *pastes* (remove paste *pastes*))))

(defun add-new-paste-to-index (paste)
  "Add a new paste, PASTE, to the paste index, setting its
paste-number, incrementing the paste-counter, writing the paste to
disk, etc."
  (with-paste-lock
    (setf (paste-number paste) *paste-counter*)
    (push paste *pastes*)
    (paste-write-xml-to-file paste)
    (weaken-paste-contents paste)))

;; ANNOTATE-PASTE is roughly analogous to ADD-NEW-PASTE-TO-INDEX in
;; intent, and has a parallel structure, but operates in terms of a
;; parent-paste's annotation list rather than the paste index.
(defun annotate-paste (root-paste annotation)
  "Add a new annotation, ANNOTATION, to ROOT-PASTE, setting the
annotation's paste-number, incrementing ROOT-PASTE's annotation
counter, writing the annotation to disk, etc."
  (with-paste-lock
    (setf (paste-number annotation)
          (incf (paste-annotation-counter root-paste)))
    (setf (paste-parent-paste annotation) root-paste)
    (push annotation (paste-annotations root-paste))
    (write-new-annotation root-paste annotation)
    (weaken-paste-contents annotation)))


;;; Serialization / deserialization of pastes

(defparameter *paste-serialization-slots*
  `((:|number| ,#'paste-number ,#'(setf paste-number))
    (:|user| ,#'paste-user ,#'(setf paste-user))
    (:|title| ,#'paste-title ,#'(setf paste-title))
    (:|contents| ,#'paste-contents ,#'(setf paste-contents))
    (:|universal-time| ,#'paste-universal-time ,#'(setf paste-universal-time))
    (:|channel| ,#'paste-channel ,#'(setf paste-channel))
    (:|colorization-mode| ,#'paste-colorization-mode ,#'(setf paste-colorization-mode))
    (:|maybe-spam| ,#'paste-maybe-spam-p ,#'(setf paste-maybe-spam-p))
    (:|is-unicode| ,#'paste-is-unicode-p ,#'(setf paste-is-unicode-p))
    (:|deletion-requested| ,#'paste-deletion-requested ,#'(setf paste-deletion-requested))
    (:|deletion-requested-email| ,#'paste-deletion-requested-email ,#'(setf paste-deletion-requested-email))
    (:|expiration-time| ,#'paste-expiration-time ,#'(setf paste-expiration-time))))

(defun paste-lxml (paste &key root)
  (serialize-object-slots paste *paste-serialization-slots*
                          :root root))

(defun lxml-paste (lxml)
  (deserialize-object-slots lxml 'paste *paste-serialization-slots*))

(defun paste-xml-file (paste)
  "Return a pathname to the storage file for the \"root-paste
designator\" PASTE."
  (let ((paste-number (if (numberp paste)
			  paste
			  (paste-number (or (paste-parent-paste paste)
					    paste)))))
    ;; If we want to break up the paste storage into subdirectories,
    ;; return (or (probe-file <this-pathname>)
    ;; <subdirectory-paste-pathname>).  This allows for transparent
    ;; relocation of the pastes over time, with all new pastes going
    ;; to the right place from the get-go and all old pastes movable
    ;; without having to take the system down.
    (merge-pathnames (make-pathname :name (prin1-to-string paste-number)
				    :type "xml")
		     *paste-path*)))

(defun paste-write-xml* (paste stream)
  (write-string "<?xml version=\"1.0\"?>" stream)
  (write-string (s-xml:print-xml-string (paste-lxml paste) :pretty t) stream)
  (mapc #'(lambda (ann)
            (write-string (s-xml:print-xml-string (paste-lxml ann :root :|annotation|) :pretty t)
                          stream))
        (paste-annotations paste)))

(defun paste-write-xml (paste stream)
  (write-string "<?xml version=\"1.0\"?>" stream)
  (write-string (s-xml:print-xml-string (list* :|paste-with-annotations| (paste-lxml paste)
					       (mapcar (lambda (ann)
							 (paste-lxml ann :root :|annotation|))
						       (paste-annotations paste))) :pretty t) stream))

(defun paste-write-xml-to-file (paste)
  "Write the root paste for PASTE and its annotations to its storage
file."
  ;; FIXME: If there is an error during writing, such as running out
  ;; of disk space, the storage file contents will be invalid.
  ;; Ideally, we'd like to have the original contents of the storage
  ;; file still available at that point, only to be overwritten when
  ;; the new version of the file is entirely on-disk.  The way to do
  ;; this is to write to a separate file and then to delete the old
  ;; file and rename the new one into place once the write has
  ;; succeeded.  This would also mean we could eliminate the KLUDGE
  ;; below, as the old file would remain until the new one was
  ;; completely written.
  (let ((xml-file (paste-xml-file paste)))
    (ensure-directories-exist xml-file)
    ;; KLUDGE: Can't write directly to file, as if the paste contents
    ;; were discarded we need to be able to read from the file.  To
    ;; compensate, write to a string-stream, then write the buffer to
    ;; the file.
    (with-output-to-string (s)
      (paste-write-xml* (or (paste-parent-paste paste) paste) s)
      (with-open-file (f xml-file
			 :direction :output
			 :if-exists :supersede)
	(write-sequence (get-output-stream-string s) f))))
  (values))

(defun write-new-annotation (paste ann)
  (with-open-file (s (paste-xml-file paste) :direction :output :if-exists :append)
    (write-string (s-xml:print-xml-string (paste-lxml ann :root :|annotation|) :pretty t)
                  s)))

(defun read-paste-xml-from-file (file)
  (handler-bind ((error (lambda (c)
                          (warn "Error reading paste ~S:~%~A"
                                file c)
                          (return-from read-paste-xml-from-file))))
    (with-open-file (s file :direction :input)
      (let ((paste (lxml-paste (s-xml:parse-xml-dom s :lxml))))
        (cond ((paste-number paste)
               (add-paste-to-index paste)
               (setf (paste-annotation-counter paste) 0)
               (loop for ann-lxml = (s-xml:parse-xml-dom s :lxml)
                     while ann-lxml
                     do (let ((ann (lxml-paste ann-lxml)))
                          (push ann (paste-annotations paste))
                          (setf (paste-parent-paste ann) paste)
                          (setf (paste-annotation-counter paste)
                                (max (paste-annotation-counter paste)
                                     (paste-number ann))))))
              (t
               (warn "Paste file ~A is malformed." file)))))))

(defun read-xml-pastes ()
  (with-paste-lock
    (reset-paste-index)
    (mapc #'read-paste-xml-from-file
          (sort (directory (make-pathname :name :wild
                                          :type "xml"
                                          :defaults *paste-path*))
                #'< :key #'(lambda (e)
                             (parse-integer (pathname-name e) :junk-allowed t))))))

;; This function occasionally comes in handy when changing the paste
;; format on the live server (usually in recovering from a mistake).
;; It takes forever to run and if you're low on disk space will likely
;; fail catastrophically.  Try not to need to use it.
(defun write-all-xml-pastes ()
  "Recreate the contents of all on-disk paste storage files from the
in-memory copy."
  (mapc #'paste-write-xml-to-file (list-pastes))
  (values))


;;;; Paste manipulation

(defun paste-display-url (paste)
  "Return a string of the display URL for PASTE, including a suitable
anchor if PASTE is an annotation."
  (let ((parent-paste (paste-parent-paste paste)))
    (merge-url (full-url *display-paste-url*)
               (if parent-paste
                   (format nil "~A#~A"
                           (paste-number parent-paste)
                           (paste-number paste))
                   (prin1-to-string (paste-number paste))))))

(defun paste-short-url (paste)
  "Return a string of the short URL for PASTE, including a suitable
anchor if PASTE is an annotation."
  (let ((parent-paste (paste-parent-paste paste)))
    (concatenate 'string
		 *short-paste-url*
		 (if parent-paste
		     (format nil "~36R/~36R"
			     (paste-number parent-paste)
			     (paste-number paste))
		     (format nil "~36R" (paste-number paste))))))

(defun disable-paste (paste)
  (rename-file (paste-xml-file paste)
               (make-pathname :defaults (paste-xml-file paste)
                              :type "disabled")))

(defun kill-paste (number)
  (let ((paste (if (typep number 'paste)
		   number
		   (find-paste number))))
    (when paste
      (remove-paste-from-index paste)
      (disable-paste paste))))

(defun kill-paste-annotations (number)
  (setf (paste-annotations (find-paste number))
        nil)
  (paste-write-xml-to-file (find-paste number)))

(defun kill-paste-annotation (number ann)
  (let ((paste (find-paste number)))
    (setf (paste-annotations paste)
          (remove ann (paste-annotations paste) :key #'paste-number))
    (paste-write-xml-to-file paste)))


;;;; Paste creation

(defun irc-announce-paste (paste)
  ;; Shouldn't this be dependent on #-lisppaste-no-irc ?
  (let ((channel (paste-channel paste))
	(parent-paste (paste-parent-paste paste)))
    (unless (string-equal channel "None")
      (irc-notify channel
		  (format nil "~A ~:[pasted~;~:*annotated #~A~] \"~A\" at ~A"
			  (paste-user paste)
			  (and parent-paste (paste-number parent-paste))
			  (paste-title paste)
			  (paste-display-url paste))))))

(defun make-new-paste (annotate &rest keys &key user title contents
		       channel colorization-mode expiration-time)
  (let ((paste (apply #'make-instance 'paste
		      :universal-time (get-universal-time)
		      keys)))
    (if annotate
	(annotate-paste annotate paste)
	(add-new-paste-to-index paste))
    (irc-announce-paste paste)
    paste))
