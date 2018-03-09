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

(defcustom abaplib-url
  "http://your-abap-server:50000/"
  "The address of the abap server host"
  :type 'string
  :group 'abaplib)

(defvar abaplib-token nil
  "ABAP token used for authentication.")

(defvar abaplib-client nil
  "ABAP system client used for login.")


(defun abaplib-login (username password client)
  "Login into ABAP Server as user USERNAME with PASSWORD and CLIENT
After a successful login, store the authentication token in `abaplib-token'."
  (interactive
   (let ((username (read-string "Username: "))
         (password (read-string "Password: "))
         (client (read-string "Client: ")))
     (setq abaplib-token `("Authorization" . ,(format "Basic %s" (base64-encode-string (concat username ":" password)))))
     (setq abaplib-client `("sap-client" . ,(format "%s" client)))
     ;; (list username password client)
     )))

(defun abaplib-service-call (api &optional callback &rest args)
  "Invoke corresponding service API."
  (unless abaplib-token
    (call-interactively 'abaplib-login))
  (append (request-response-data
           (apply #'request (if (string-match "^http[s]*://" api) api
                              (concat (replace-regexp-in-string "/*$" "/" abaplib-url)
                                      (replace-regexp-in-string "^/*" "" api)))
                  :sync (not callback)
                  :headers `(,abaplib-token ("Content-Type" . "application/json"))
                  :params `((sap-client . ,abaplib-client))
                  :parser 'buffer-string
                  :complete callback
                  args))
          nil))

(provide 'abaplib)
;;; abaplib.el ends here
