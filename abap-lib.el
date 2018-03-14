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

(defcustom abap-query-list-max-result
  "10"
  "Object Query List Maximum Result"
  :type 'string
  :group 'abap-mode)

(defvar abaplib-token nil
  "ABAP token used for authentication.")

(defvar abaplib-client nil
  "ABAP system client used for login.")

;; (defvar abaplib-system-ID nil
;;   "ABAP system client used for login.")
(defvar abaplib-service-url nil
  "The address of the abap server host")


(defvar abaplib-project-name nil
  "Current ABAP Project")

(defvar abaplib-project-dir nil
  "ABAP Project Directory")

(defvar abaplib-project-config-dir nil
  "ABAP Project Configuration Directory")


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
     (setq abaplib-project-name system-id)
     ;; (setq abaplib-system-ID system-id)
     (if (file-directory-p abaplib-project-dir)
         (message "Project %s already exist!" system-id)
       (let ((abap-config-file (format "%s/server.ini" abaplib-project-config-dir)))
         (make-directory abaplib-project-dir)
         (make-directory abaplib-project-config-dir)
         (write-region
          (concat
           (format "Server=%s\n" server-address)
           (format "HttpPort=%s\n" http-port))
          nil
          abap-config-file)
         (setq abaplib-service-url (format "http://%s:%s/" server_address http_port))
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
After a successful login, store the authentication token in `abaplib-token'."
  (interactive
   (let ((username (read-string "Username: "))
         (password (read-string "Password: "))
         (client   (read-string "Client: "  )))
     (progn
       (setq abaplib-token `("Authorization" . ,(format "Basic %s" (base64-encode-string (concat username ":" password)))))
       (setq abaplib-client client)
       nil)
     )))

(defun abaplib-select-project ()
  "Select Existing Project"
  (interactive
   (let ((project_name (completing-read "Select Project: " (abaplib-get-project-list))))
     (unless (string= project_name abaplib-project-name)
       (setq abaplib-token nil)
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
             (setq abaplib-service-url (format "http://%s:%s/" server_address http_port))
             ))
         (setq abaplib-project-dir (format "%s/%s" abap-workspace-dir project_name))
         (setq abaplib-project-config-dir (format "%s/.abap/" abaplib-project-dir))
         (setq abaplib-project-name project_name)
         nil
         )
       ))
   ))

(defun abaplib-service-call (api &optional callback &rest args)
  "Invoke corresponding service API."
  (unless abaplib-project-name
    (call-interactively 'abaplib-select-project))
  (unless abaplib-token
    (call-interactively 'abaplib-login))
  (append (request-response-data
           (apply #'request (if (string-match "^http[s]*://" api) api
                              (concat (replace-regexp-in-string "/*$" "/" abaplib-service-url)
                                      (replace-regexp-in-string "^/*" "" api)))
                  :sync (not callback)
                  :headers `(,abaplib-token ("Content-Type" . "application/json"))
                  :params `((sap-client . ,abaplib-client))
                  :parser 'buffer-string
                  :complete callback
                  args))
          nil))


(defun abaplib-pull-program (programe_name)
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
  (abaplib-service-call
   (abaplib-get-service-uri "get-program-source" programe_name)
   (lambda (&rest data)
     (let ((prog_metadata (format "%s" (cl-getf data :data)))
           (file (format "%s/%s.prog.abap" abaplib-project-dir programe_name)))
       ;; (get-buffer-create "*ABAP*")
       ;; (switch-to-buffer "*ABAP*")
       ;; (insert idata)
       (write-region prog_metadata nil file)
       )))
  )

(provide 'abaplib)
;;; abaplib.el ends here
