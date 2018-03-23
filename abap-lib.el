;;; sap-abap-lib.el --- sap abap server lib          -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Marvin Qian

;; Author: Marvin Qian <qianmarv@gmail.com>
;; Keywords: SAP, ABAP, CDS

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

;;; Dev Log
;;; TODO Read Project List & Select Project & Read Server Information & Read User/Password

(require 'request)


(defcustom abap-workspace-dir
  "~/ws/ABAP"
  "ABAP Workspace Directory"
  :type 'string
  :group 'abap-mode)

;; (defcustom abap-store-logon-data
;;   t
;;   "Whether Save Logon Data: User/Pass/Client"
;;   :type 'boolean
;;   :group 'abap-mode)

(defcustom abap-query-list-max-result
  "51"
  "Object Query List Maximum Result"
  :type 'string
  :group 'abap-mode)

(defvar abaplib--token nil
  "ABAP token used for authentication.")

(defvar abaplib--client nil
  "ABAP system client used for login.")

;; (defvar abaplib-system-ID nil
;;   "ABAP system client used for login.")
(defvar abaplib--service-url nil
  "The address of the abap server host")


(defvar abaplib--project-name nil
  "Current ABAP Project")

(defvar abaplib--project-dir nil
  "ABAP Project Directory")

(defvar abaplib--project-config-dir nil
  "ABAP Project Configuration Directory")


(defun abaplib-setup-project ()
  "Setup ABAP Project"
  (interactive
   (let* ((system-id (read-string "System ID: "))
          (server-address (read-string "Server Address: "))
          (http-port (read-string "ICM HTTP Port: ")))
     (unless (file-directory-p abap-workspace-dir)
       (make-directory abap-workspace-dir))
     (setq abaplib--project-dir (format "%s/%s" abap-workspace-dir system-id))
     (setq abaplib--project-config-dir (format "%s/.abap/" abaplib--project-dir))
     (setq abaplib--project-name system-id)
     ;; (setq abaplib-system-ID system-id)
     (if (file-directory-p abaplib--project-dir)
         (error "Project %s already exist!" system-id)
       (let ((abap-config-file (format "%s/server.ini" abaplib--project-config-dir)))
         (make-directory abaplib--project-dir)
         (make-directory abaplib--project-config-dir)
         (write-region
          (concat
           (format "Server=%s\n" server-address)
           (format "HttpPort=%s\n" http-port))
          nil
          abap-config-file)
         (setq abaplib--service-url (format "http://%s:%s/" server_address http_port))
         (message "Project Initialized Successfully!")
         nil
         ))
     )))

(defun abaplib-get-service-uri (service_name object_name)
  (cond
   ((string= service_name "query-object")
    (format "/sap/bc/adt/repository/informationsystem/search?operation=quickSearch&query=%s&maxResults=%s" object_name abap-query-list-max-result))
   ((string= service_name "get-program-metadata")
    (format "/sap/bc/adt/programs/programs/%s" object_name))
   ((string= service_name "get-program-source")
    (format "/sap/bc/adt/programs/programs/%s/source/main" object_name))
   ))

(defun abaplib-get-project-list ()
  (let ((files (directory-files abap-workspace-dir)))
    (seq-remove ;; Support Emacs 25.1 Or Above Only
     (lambda (file)
       (cond ((string= file "."))
             ((string= file ".."))
             ((not (file-directory-p (expand-file-name file abap-workspace-dir)))))
       )
     files))
  )

(defun abaplib-login ()
  "Login into ABAP Server as user USERNAME with PASSWORD and CLIENT
After a successful login, store the authentication token in `abaplib--token'.
  TODO Do real login and store cookies"
  (interactive
   (let ((username (upcase (read-string "Username: ")))
         (password (read-passwd "Password: "))
         (client   (read-string "Client: "  )))
     (let* ((trial-token `("Authorization" .
                          ,(format "Basic %s"
                                   (base64-encode-string (concat username ":" password)))))
            (logon-uri "/sap/bc/adt/core/discovery")
            (logon-status (request-response-symbol-status
                           (request
                            (concat  abaplib--service-url
                                     (replace-regexp-in-string "^/*" "" logon-uri))
                            :sync t
                            :headers (list abaplib--token)
                            :params (list (cons "sap-client" client))
                            ))))

       (if (eq logon-status 'success)
           (progn
             (setq abaplib--token trial-token)
             (setq abaplib--client client)
             (message "Logon successfully!")
             nil)
         (error "Logon Failed!")
         nil)
       ))))

(defun abaplib-select-project ()
  "Select Existing Project"
  (interactive)
  (let ((project_name (completing-read "Select Project: " (abaplib-get-project-list))))
    (unless (string= project_name abaplib--project-name)
      (setq abaplib--token nil)
      (let* ((abap-config-dir (expand-file-name (format "%s/.abap" project_name) abap-workspace-dir))
             (abap-config-file (expand-file-name "server.ini" abap-config-dir)))
        ;; Read server information
        (with-temp-buffer
          (insert-file-contents abap-config-file)
          (let ((server_address)
                (http_port))
            (mapcar
             (lambda (item)
               (let* ((config (split-string item "=" t))
                      (config_id (car config))
                      (config_value (car (cdr config))))
                 (cond ((string= config_id "Server") (setq server_address config_value))
                       ((string= config_id "HttpPort") (setq http_port config_value)))
                 ))
             (split-string (buffer-string) "\n" t))
            (unless (and server_address http_port)
              (error "Not a valid project!"))
            (cond
             ((string= http_port "80") (setq abaplib--service-url (format "http://%s/" server_address)))
             ((string= http_port "443") (setq abaplib--service-url (format "https://%s/" server_address)))
             (t (setq abaplib--service-url (format "http://%s:%s/" server_address http_port))))
            (setq abaplib--project-dir (format "%s/%s" abap-workspace-dir project_name))
            (setq abaplib--project-config-dir (format "%s/.abap/" abaplib--project-dir))
            (setq abaplib--project-name project_name)
            ))
        )
      )
    (helm-open-dired abaplib--project-dir))
  )

(defun abaplib-service-call (api success &rest args)
  "Invoke corresponding service API."
  (unless abaplib--project-name
    (call-interactively 'abaplib-select-project))
  (unless abaplib--token ;; Try to logon for the first time
    (call-interactively 'abaplib-login))

  (append (request-response-data
           (apply #'request (if (string-match "^http[s]*://" api) api
                              (concat (replace-regexp-in-string "/*$" "/" abaplib--service-url)
                                      (replace-regexp-in-string "^/*" "" api)))
                  :sync (not success)
                  :headers (append (list abaplib--token) (cl-getf args :headers))
                  :status-code '((304 . (lambda (&rest _) (message "Source Not Modified")))
                                 (401 . (lambda (&rest _) (error "Not Authorized"))))
                  :params `((sap-client . ,abaplib--client))
                  :success success
                  :error  (lambda (&key error-thrown &allow-other-keys &rest _)
                            (message "Got error: %S" error-thrown))
                  ;; :complete (lambda (&rest _) (message "Complete" ))
                  args))
          nil)
  )

;; (append (request-response-data
;;          (apply #'request (if (string-match "^http[s]*://" api) api
;;                             (concat (replace-regexp-in-string "/*$" "/" abaplib--service-url)
;;                                     (replace-regexp-in-string "^/*" "" api)))
;;                 :sync nil
;;                 :headers `(,abaplib--token ("Content-Type" . "application/json"))
;;                 :params `((sap-client . ,abaplib--client))
;;                 :parser 'buffer-string
;;                 :complete (lambda (&rest_) (message "Call \"%s\" complete" api))
;;                 args))
;;         nil)
;; )

;; 0. Query List
;; 1. Pull 
;; 2. Check Syntax
;; 3. Save to Server
;; 3.1 Lock
;; 4. Activate At Server

;; Supported Object Type
;;;; DCLS => Access Control
;;;; DDLS => Data Definition

;; ------------------------------------------Process----------------------------------------

;; Query Object List From Server
;;;; GET /sap/bc/adt/repository/informationsystem/objecttypes?maxItemCount=999&name=*&data=usedByProvider
;;;; GET /sap/bc/adt/repository/informationsystem/search?operation=quickSearch&query=I_CNSLDTNINTCORECNCL%2A&maxResults=51

;; Start to Edit: Send Lock Post
;;;; POST /sap/bc/adt/programs/programs/zmq_abap_test01?_action=LOCK&accessMode=MODIFY
;;;; Get LOCK_HANDLE

;; Check Syntax In Server
;;;; POST /sap/bc/adt/checkruns?reporters=abapCheckRun
;;;; Before Submit to Server, Need base64 encode source and wrap into posting xml
;;;; After Saved to Server, 
;;;; 1. Active Version
;;;; 2. Unsaved Version
;;;; 3. Saved in server version

;; Save Source In Server
;;;; PUT /sap/bc/adt/programs/programs/zmq_abap_test01/source/main?lockHandle=tdOnBGpU37goVbesZZSK6aK%2FG%2FA%3D HTTP/1.1

;; Activate Source
;; 1) Unlock
;;;; POST /sap/bc/adt/programs/programs/zmq_abap_test01?_action=UNLOCK&lockHandle=tdOnBGpU37goVbesZZSK6aK%2FG%2FA%3D HTTP/1.1
;; 2) Activate
;;;; POST /sap/bc/adt/activation?method=activate&preauditRequested=true HTTP/1.1

(defun abaplib-sourcecode-parser()
  (progn
    (goto-char (point-min))
    (while (re-search-forward "\r" nil t)
      (replace-match ""))
    (buffer-string))
  )
(defun abaplib-xml-parser()
  (libxml-parse-xml-region (point-min) (point-max)))



(defun abaplib-request-object-list (object-name)
  (xml-get-children
   (abaplib-service-call
    (abaplib-get-service-uri "query-object" (format "%s%%2A" object-name))
    nil
    :parser 'abaplib-xml-parser)
   'objectReference))

(defun abaplib-convert-object-plain-list (object-list)
  (mapcar (lambda (obj)
            (let* ((attrs       (xml-node-attributes obj))
                   (type        (cdr (assq 'type        attrs)))
                   (name        (cdr (assq 'name        attrs)))
                   (packageName (cdr (assq 'packageName attrs)))
                   (description (cdr (assq 'description attrs)))
                   )
              (list (format "%-8s%-31s%s" type name description))))
          object-list))

(defun abaplib-pull-abap-object ()
  (interactive
   (let* ((object-name (read-string "Enter Search String: "))
          (object-list (abaplib-request-object-list object-name))
          (object-plain-list (abaplib-convert-object-plain-list object-list))
          (selected-object (split-string
                            (completing-read "Maching Items: " object-plain-list)
                            " "
                            t))
          (object-type (car selected-object))
          (object-name (car (cdr selected-object))))

     (cond ((string= object-type "PROG/P" ) (abaplib-prog-get-source object-name))
           ((string= object-type "DDLS/DL") (message "TODO: Handle Retrieve CDS - Data Definition"))
           ((string= object-type "DDLS/DF") (message "TODO: Handle Retrieve CDS - Entity"))
           ((string= object-type "DCLS/DL") (message "TODO: Handle Retrieve CDS - Access Control"))
           ((string= object-type "CLAS/OC") (message "TODO: Handle Retrieve Class")))
     nil
     )
   ;; Retrieve Local Attribute File and Get ETag
   ;; Compose request call
   ;; Request to Server
   ;; Write File & Open File
   ))

(defun abaplib-prog-get-source (prog_name)
  ;; Retrieve metadata
  (abaplib-service-call
   (abaplib-get-service-uri "get-program-metadata" prog_name)
   (lambda (&rest data)
     (let ((prog_metadata (format "%s" (cl-getf data :data)))
           (file (format "%s/%s.prog.xml" abaplib--project-config-dir prog_name)))
       (write-region prog_metadata nil file)
       ))
   :parser 'abaplib-sourcecode-parser
   )
;; Retrieve source
(abaplib-service-call
 (abaplib-get-service-uri "get-program-source" prog_name)
 (lambda (&rest data)
   (let ((prog_source (format "%s" (cl-getf data :data)))
         (file (format "%s/%s.prog.abap" abaplib--project-dir prog_name)))
     (unless (string= prog_source "")
       (write-region prog_source nil file)
       nil
       )))
 :parser 'abaplib-sourcecode-parser
 ;; :headers (list '("If-None-Match" . "201704241108050011")
 ;;                '("Content-Type" . "plain/text"))
 ))

(defun abaplib-prog-check-syntax (prog_name)
  ())



(provide 'abaplib)
;;; abaplib.el ends here
