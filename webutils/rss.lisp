(defpackage :webutils.rss (:use :araneida :net.telent.date :webutils.xml-mixed-mode :cl)
            (:export :generate-rss :rss-entry))
(in-package :webutils.rss)
(webutils::export-all :webutils.rss)

(defun rss-1 (title link time description)
  (<item>
     (<title> title)
     (<link> link)
     (<pubDate> (universal-time-to-rfc-date time))
     (<description> description)))

(defun send-rss (request title link description bodies)
  (format (request-stream request)
            "<?xml version=\"1.0\"?>~%")
  (with-xml-output-to-stream (request-stream request)
    (<rss version="2.0">
          (<channel>
           (<title> title)
           (<link> link)
           (<description> description)
           (<language> "en")
           bodies))))

(defun rss-entry (&key title link time description)
  (declare (ignore title link time description))
  (error "rss-entry called outside flet!"))

(defmacro generate-rss ((request title link description)
                        &body body)
  (let ((entries (gensym)))
    `(let (,entries)
       (flet ((rss-entry (&key title link time description)
                (push (rss-1 title link time description) ,entries)))
         ,@body
         (send-rss ,request ,title ,link ,description (reverse ,entries))))))