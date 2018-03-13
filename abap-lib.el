;;; sap-abap-lib.el --- sap abap server lib          -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Marvin Qian

;; Author: Marvin Qian <qianmarv@gmail.com>
;; Keywords: 

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

(require 'request)

(defcustom abap-url
  "http://your-abap-server:50000/"
  "The address of the abap server host"
  :type 'string
  :group 'abap)

(defcustom abap-workspace-dir
  "~/ws/ABAP/"
  "ABAP Workspace Directory"
  :type 'string
  :group 'abap)

(defcustom abap-query-list-max-result
  "10"
  "Object Query List Maximum Result"
  :type 'string
  :group 'abap)

(defvar abaplib-token nil
  "ABAP token used for authentication.")

(defvar abaplib-client nil
  "ABAP system client used for login.")

(defvar abaplib-system-ID nil
  "ABAP system client used for login.")

(defvar abaplib-project-dir nil
  "ABAP Project Directory")

(defvar abaplib-project-config-dir nil
  "ABAP Project Directory")


(defun setup-abap-project ()
  "Setup ABAP Project"
  (interactive
   (let* ((system-id (read-string "System ID: "))
          (server-address (read-string "Server Address: "))
          (http-port (read-string "ICM HTTP Port: ")))
     (unless (file-directory-p abap-workspace-dir)
       (make-directory abap-workspace-dir))
     (setq abaplib-project-dir (format "%s/%s" abap-workspace-dir system-id))
     (setq abaplib-project-config-dir (format "%s/.abap/" abaplib-project-dir))
     (if (file-directory-p abaplib-project-dir)
         (message "Project %s already exist!" system-id)
       (let ((abap-config-file (format "%s/system.ini" abaplib-project-config-dir)))
         (make-directory abaplib-project-dir)
         (make-directory abaplib-project-config-dir)
         (write-region
          (concat
           (format "Server=%s\n" server-address)
           (format "HttpPort=%s\n" http-port))
          nil
          abap-config-file
          )
         (Message "Project Initialized Successfully!")
         ))
     )))

(defun abaplib-get-service-uri (service-name &optional object-name)
  (cond
   ((string= service-name "query-object")
    (format "/sap/bc/adt/repository/informationsystem/search?operation=quickSearch&query=%s&maxResults=%s" object-name abap-query-list-max-result))
   ((string= service-name "get-program-metadata")
    (format "/sap/bc/adt/programs/programs/%s" object-name))
   ((string= service-name "get-program-source")
    (format "/sap/bc/adt/programs/programs/%s/source/main" object-name))
   ))

(defun abaplib-login (username password client)
  "Login into ABAP Server as user USERNAME with PASSWORD and CLIENT
After a successful login, store the authentication token in `abaplib-token'."
  (interactive
   (let ((username (read-string "Username: "))
         (password (read-string "Password: "))
         (client (read-string "Client: ")))
     (setq abaplib-token `("Authorization" . ,(format "Basic %s" (base64-encode-string (concat username ":" password)))))
     (setq abaplib-client client)
     ;; (list username password client)
     )))

(defun abaplib-service-call (api &optional callback &rest args)
  "Invoke corresponding service API."
  (unless abaplib-token
    (call-interactively 'abaplib-login))
  (append (request-response-data
           (apply #'request (if (string-match "^http[s]*://" api) api
                              (concat (replace-regexp-in-string "/*$" "/" abap-url)
                                      (replace-regexp-in-string "^/*" "" api)))
                  :sync (not callback)
                  :headers `(,abaplib-token ("Content-Type" . "application/json"))
                  :params `((sap-client . ,abaplib-client))
                  :parser 'buffer-string
                  :complete callback
                  args))
          nil))

;; (defun abaplib-get-program-source (source &optional callback &rest args)
;;   (abaplib-service-call
;;    (format "/sap/bc/adt/programs/programs/%s/source/main" source)
;;    callback
;;    args
;;    ))

(defun abaplib-pull-program (program_name)
  ;; Retrieve metadata
  (abaplib-service-call
   (abaplib-get-service-uri "get-program-metadata" programe_name)
   (lambda (&rest data)
     (let ((prog_metadata (format "%s" (cl-getf data :data)))
           (file (format "%s/%s.prog.xml" abaplib-project-config-dir programe_name)))
       ;; (get-buffer-create "*ABAP*")
       ;; (switch-to-buffer "*ABAP*")
       ;; (insert idata)
       (write-region prog_metadata nil file)
       )))

  ;; Retrieve source
  ;; (abaplib-service-call
  ;;  (abaplib-get-service-uri "get-program-source" programe_name)
  ;;  (lambda (&rest data)
  ;;    (let ((prog_metadata (format "%s" (cl-getf data :data)))
  ;;          (file (format "%s/%s.prog.abap" abaplib-project-dir programe_name)))
  ;;      ;; (get-buffer-create "*ABAP*")
  ;;      ;; (switch-to-buffer "*ABAP*")
  ;;      ;; (insert idata)
  ;;      (write-region prog_metadata nil file)
  ;;      )))
  )

(provide 'abaplib)
;;; abaplib.el ends here
