(defpackage :webutils.simple-serialized-classes (:use :cl) (:nicknames :ssc)
            (:export :register-instance :delete-instance
                     :serialize-instance :deserialize-for-key
                     :deserialize-for-file
                     :find-instance-by-key :load-store-for-class
                     :serialize-file-name-for-key :key-of-instance
                     :serializable-name-of-key
                     :class-name-of-instance
                     :define-simple-serialized-class
                     :register-key :shallow-copy-instance
                     :note-registered-instance :note-deleted-instance
                     :simple-serialized-class :store-of-class
                     :slot-to-accessor :class-has-slot-p))
(in-package :webutils.simple-serialized-classes)
(webutils::export-all :webutils.simple-serialized-classes)

(defgeneric register-instance (instance &key no-serialize))
(defgeneric delete-instance (instance))
(defgeneric serialize-instance (instance))
(defgeneric deserialize-for-file (file class))
(defgeneric deserialize-for-key (key class))
(defgeneric find-instance-by-key (key class &key find-by-loading))
(defgeneric load-store-for-class (class))
(defgeneric serialize-file-name-for-key (key class))
(defgeneric key-of-instance (instance))
(defgeneric serializable-name-of-key (key class))
(defgeneric class-name-of-instance (instance))
(defgeneric register-key (key class))
(defgeneric shallow-copy-instance (instance))
(defgeneric slot-to-accessor (slot class))
(defgeneric class-has-slot-p (slot class))

;;; Generic store protocol:
;;; add-instance-to-store: takes current value of store, must return new value of store
;;; remove-instance-from-store: ditto
;;; find-instance-in-store: takes current value of store and key, returns instance
;;; map-instances-in-store: maps a function across the store as if by mapc; returns store
(defgeneric add-instance-to-store (store-type current-store-value key instance))
(defgeneric remove-instance-from-store (store-type current-store-value key instance))
(defgeneric find-instance-in-store (store-type current-store-value key))
(defgeneric map-instances-in-store (store-type current-store-value function))

(defmethod add-instance-to-store ((store-type (eql 'list)) current-store-value key instance)
  (declare (ignore key))
  (cons instance current-store-value))
(defmethod remove-instance-from-store ((store-type (eql 'list)) current-store-value key instance)
  (declare (ignore instance))
  (remove key current-store-value :key #'key-of-instance :test #'equalp))
(defmethod find-instance-in-store ((store-type (eql 'list)) current-store-value key)
  (find key current-store-value :key #'key-of-instance :test #'equalp))
(defmethod map-instances-in-store ((store-type (eql 'list)) current-store-value function)
  (mapc function current-store-value))

(defmethod add-instance-to-store ((store-type (eql 'hash-table)) current-store-value key instance)
  (setf (gethash key current-store-value) instance)
  current-store-value)
(defmethod remove-instance-from-store ((store-type (eql 'hash-table)) current-store-value key instance)
  (declare (ignore instance))
  (remhash key current-store-value)
  current-store-value)
(defmethod find-instance-in-store ((store-type (eql 'hash-table)) current-store-value key)
  (gethash key current-store-value))
(defmethod map-instances-in-store ((store-type (eql 'hash-table)) current-store-value function)
  (maphash (lambda (key value)
             (declare (ignore key))
             (funcall function value)) current-store-value)
  current-store-value)

(defgeneric note-registered-instance (instance))
(defgeneric note-deleted-instance (instance))

(defmethod note-registered-instance (instance))

(defmethod note-deleted-instance (instance))

(defclass simple-serialized-class () ())

;; This method is provided to the user for use when deserializing an object
(defmethod register-key (key class)
  nil)

;; This default method handles the case where there are objects not of
;; the serializable type on that type's store list.
(defmethod key-of-instance (instance)
  nil)

;; This default method errors when a simple-serialized-class has no
;; :key-slot and no supplied method
(defmethod key-of-instance ((instance simple-serialized-class))
  (error "No method supplied for KEY-OF-INSTANCE on class ~A." (class-of instance)))

(defmethod serializable-name-of-key ((key string) class)
  (declare (ignore class))
  key)

(defmethod serializable-name-of-key ((key integer) class)
  (declare (ignore class))
  (prin1-to-string key))

(defvar *class-store-places* nil)

(defmacro store-of-class (class)
  (or (cdr (assoc class *class-store-places*))
      (error "No store found yet for class ~A!" class)))

(defmethod print-object :around ((object simple-serialized-class) stream)
  (when (find-package "SSC-TEMP")
    (delete-package "SSC-TEMP"))
  (let ((normal-printed
         (with-output-to-string (s)
           (with-standard-io-syntax
             (let ((*print-readably* nil))
               (call-next-method object s)))))
        (temp-package (make-package "SSC-TEMP")))
    (unwind-protect
         (let ((*package* temp-package))
           (with-standard-io-syntax
             (let ((*print-circle* t))
               (format stream "#.(#| ~A |# ~S ~S '~S ~S ~S)"
                       normal-printed
                       'find-instance-by-key
                       (key-of-instance object)
                       (class-name-of-instance object)
                       :find-by-loading t))))
      (delete-package temp-package))))

(defun canonicalize-to-readable-form (object)
  (if (typep object 'string)
      (map 'string
           #'(lambda (char)
               (if (typep char 'base-char)
                   char
                   #\?)) object)
      object))

(defmacro define-simple-serialized-class (class-name (&rest superclasses) (&rest all-slots)
                                          &rest all-keys)
  (let* ((key-slot-name (second (assoc :key-slot all-keys)))
         (path-name (gensym))
         (store (or (second (assoc :store all-keys))
                    (error "Store name not supplied!")))
         (store-directory (or (second (assoc :store-directory all-keys))
                              (error "Store directory not supplied!")))
         (store-type (or (second (assoc :store-type all-keys))
                         'list))
         (managed-slots (cdr (assoc :managed-slots all-keys)))
         (all-slot-names (set-difference (mapcar #'car all-slots) managed-slots)))
    (setf (cdr (or (assoc class-name *class-store-places*)
                   (car (push (cons class-name nil) *class-store-places*))))
          store)
    (mapc (lambda (slot)
            (when (getf (cdr slot) :reader)
              (error "~A can't be supplied as a slot option in a serializable class; use ~A instead."
                     :reader :accessor))
            (when (getf (cdr slot) :writer)
              (error "~A can't be supplied as a slot option in a serializable class; use ~A instead."
                     :writer :accessor))
            (unless (getf (cdr slot) :accessor)
              (error "An accessor must be supplied for slot ~A."
                     (car slot)))) all-slots)
    `(progn
      (setf (cdr (or (assoc ',class-name *class-store-places*)
                     (car (push (cons ',class-name nil) *class-store-places*))))
       ',store)
       (defvar ,path-name (merge-pathnames ,store-directory))
       (defclass ,class-name (,@superclasses simple-serialized-class)
         (,@all-slots)
         ,@(remove-if (lambda (e)
                        (member e '(:store :store-directory :key-slot :managed-slots :store-type)))
                      all-keys :key #'car))
       (defmethod class-name-of-instance ((instance ,class-name))
         ',class-name)
       ,@(when key-slot-name
               `((defmethod key-of-instance ((instance ,class-name))
                   (slot-value instance ',key-slot-name))))
       (defmethod class-has-slot-p (slot (class (eql ',class-name)))
         (member slot '(,@(loop for slot in all-slots collect (car slot)))))
       (defmethod slot-to-accessor (slot (class (eql ',class-name)))
         (ecase slot
           ,@(loop for slot in all-slots
                for accessor = (getf (cdr slot) :accessor)
                when accessor
                collect `(,(car slot) ',accessor))))
       (defmethod register-instance ((instance ,class-name) &key no-serialize)
         (let ((key (key-of-instance instance)))
           (when (find-instance-by-key key ',class-name)
             (error "An instance of ~A with key ~A has already been defined!"
                    ',class-name (key-of-instance instance)))
           (setf ,store (add-instance-to-store ',store-type ,store key instance)))
         (unless no-serialize
           (serialize-instance instance))
         (note-registered-instance instance)
         instance)
       (defmethod delete-instance ((instance ,class-name))
         (setf ,store (remove-instance-from-store ',store-type ,store (key-of-instance instance) instance))
         (rename-file (serialize-file-name-for-key (key-of-instance instance) ',class-name)
                      (merge-pathnames (make-pathname :type "disabled")
                                       (serialize-file-name-for-key (key-of-instance instance) ',class-name)))
         (note-deleted-instance instance))
       (defmethod serialize-instance ((instance ,class-name))
         (ensure-directories-exist (serialize-file-name-for-key (key-of-instance instance) ',class-name))
         (with-open-file (file (serialize-file-name-for-key (key-of-instance instance) ',class-name)
                               :direction :output :if-exists :supersede)
           (with-standard-io-syntax
             (let ((*package* ,(symbol-package class-name))
                   (*print-circle* t))
               (write
                (list ',class-name
                      ,@(loop for slot in all-slot-names
                           collect `',slot
                           collect `(canonicalize-to-readable-form (slot-value instance ',slot))))
                :stream file)))))
       (defmethod serialize-file-name-for-key (key (class (eql ',class-name)))
         (declare (special ,path-name))
         (merge-pathnames (make-pathname :name (serializable-name-of-key key class)
                                         :type "ssc")
                          ,path-name))
       (defmethod deserialize-for-file (filename (class (eql ',class-name)))
         (with-open-file (file filename
                               :direction :input)
           (let* ((*package* ,(symbol-package class-name))
                  (list (read file)))
             (assert (eql (car list) ',class-name))
             (let ((instance (make-instance ',class-name)))
               ,@(loop for slot in all-slot-names
                    collect `(setf (slot-value instance ',slot)
                                   (getf (cdr list) ',slot)))
               (unless (find-instance-by-key (key-of-instance instance) class)
                 (register-key (key-of-instance instance) class)
                 (register-instance instance :no-serialize t))))))
       (defmethod deserialize-for-key (key (class (eql ',class-name)))
         (deserialize-for-file (serialize-file-name-for-key key ',class-name) class))
       (defmethod find-instance-by-key (key (class (eql ',class-name)) &key find-by-loading)
         (let ((result (find-instance-in-store ',store-type ,store key))) 
           (when (and (not result) find-by-loading)
             (deserialize-for-key key class)
             (setf result (find-instance-by-key key class)))
           result))
       (defmethod load-store-for-class ((class (eql ',class-name)))
         (declare (special ,path-name))
         (ensure-directories-exist ,path-name)
         (let ((path (make-pathname :name :wild :type "ssc" :defaults ,path-name)))
           (loop for file in (directory path)
              do (deserialize-for-file file class))))
       (defmethod shallow-copy-instance ((instance ,class-name))
         (let ((new (allocate-instance (find-class ',class-name))))
           ,@(loop for slot in all-slots
                collect `(setf (slot-value new ',(car slot))
                               (slot-value instance ',(car slot))))
           new))
       (find-class ',class-name))))
