;;; abap-indention.el --- Indention functions    -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Marvin Qian

;; Author: Marvin Qian <qianmarv@gmail.com>
;; Keywords: abap-mode

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

(setq abap-keywords-open '("IF" "ELSE" "LOOP" "DO" "FORM" "CASE" "CLASS" "TRY" "CATCH" "METHOD" "BEGIN OF"))

(setq abap-keywords-close '("ENDIF" "ENDCLASS" "ENDMETHOD" "ENDTRY" "END" "ENDLOOP" "ENDFORM" "ENDCASE" "ENDDO" "END OF"))


(defun abap-delete-leading-space()
  " Delete leading SPACE / TAB"
  (let ((end (progn
               (back-to-indentation)
               (point)))
        (beg (progn
               (move-beginning-of-line nil)
               (point))))
    (delete-region beg end)
    )
  )

(defun abap-is-line-empty()
  "Check space line"
  ;; (beginning-of-line)
  (back-to-indentation)
  (if (looking-at "$")
      t
    nil
    ))

(defun abap-goto-prev-non-empty-line()
  "goto previous non empty line"
  (previous-line)
  (if (and (not (= 1 (point)))
           (abap-is-line-empty))
      (abap-goto-prev-non-empty-line)
    ))

(defun abap-get-prev-line-width ()
  "Get width of previous non empty line"
  (save-excursion
    (abap-goto-prev-non-empty-line)
    (current-column)))

(defun abap-calc-indent ()
  "Get width of previous non empty line"
  (save-excursion
    (back-to-indentation)
    ;; (beginning-of-line)
    ;; Close
    (let ((offset (if (looking-at (regexp-opt abap-keywords-close 'words))
                      (* -1 tab-width)
                    0)))
      (abap-goto-prev-non-empty-line)
      (if (looking-at (regexp-opt abap-keywords-open 'words))
      ;; (if (looking-at "METHOD ")
          (+ (current-column) tab-width offset)
        (+ (current-column) offset)
        )
      )))

(defun abap-indent-line ()
  "Indent ABAP Line"
  (let (
        (width tab-width)
        (indent (abap-calc-indent)))
    ;; (save-excursion
    (abap-delete-leading-space)
    (indent-to indent)))


(provide 'abap-indention)
;;; abap-indention.el ends here
