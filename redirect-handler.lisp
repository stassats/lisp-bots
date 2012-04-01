(in-package :lisppaste)

(defclass redirector-handler (araneida:handler) ())

(defmethod araneida:handle-request-response ((handler redirector-handler) method request)
  (araneida:request-redirect request
                             (araneida:merge-url *paste-external-url*
                                                 (araneida:request-unhandled-part request))))

(araneida:install-handler
 (araneida:http-listener-handler *paste-listener*)
 (make-instance 'redirector-handler)
 (araneida:urlstring *old-url*) nil)
