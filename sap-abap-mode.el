;;; sap-abap-indention.el --- Indention functions    -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Marvin Qian

;; Author: Marvin Qian <qianmarv@gmail.com>
;; Keywords: SAP ABAP

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
;; Major mode for SAP ABAP Language (ABAP = Advanced Business Application Programing)
;;==============================================================================
;; Special Thanks
;;
;; This program is developed on the basis of abap-mode developed by:
;;   hugo-dc (https://github.com/hugo-dc/abap-mode)
;;
;; And is inspired by:
;;   Vincent.Zhang <vincent.zhang@sap.com>
;;==============================================================================

;;; Code:

;;; Dev Log
;; FIXME Only when * is in the begining of the line should the line be comment line!

;; define keywords 
;; ABAP keywords
;; Refer to https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/abenabap_statements_overview.htm
(require 'sap-abap-indention)

(setq sap-abap-keywords
      '(
        ;; Introductory Statements for Programs
        "CLASS-POOL"
        "FUNCTION-POOL"
        "INTERFACE-POOL"
        "PROGRAM"
        "REPORT"
        "TYPE-POOL"

        ;; Modularization Statements
        ;;; Procedures
        "FUNCTION" "ENDFUNCTION"
        "METHOD" "ENDMETHOD"
        ;;; Dialog Modules
        "MODULE" "ENDMODULE"
        ;;; Event Blocks
        "AT LINE-SELECTION"
        "AT SELECTION-SCREEN"
        "AT USER-COMMAND"
        "END-OF-PAGE"
        "END-OF-SELECTION"
        "GET"
        "INITIALIZATION"
        "LOAD-OF-PROGRAM"
        "START-OF-SELECTION"
        "TOP-OF-PAGE"
        ;;; Source Code Modules
        "DEFINE" "END-OF-DEFINITION"
        "INCLUDE"

        ;; Declarative Statement
        ;;; Data Types and Data Objects
        "CONSTANTS" "DATA" "FIELD-SYMBOLS" "INCLUDE" "NODES" "STATICS" "TABLES" "TYPES"
        ;;; Classes and Interfaces
        "ALIASES"
        "CLASS" "ENDCLASS"
        "CLASS-DATA"
        "CLASS-EVENTS"
        "CLASS-METHODS"
        "EVENTS"
        "INTERFACE"
        "INTERFACES"
        "METHODS"
        "PRIVATE SECTION"
        "PROTECTED SECTION"
        "PUBLIC SECTION"
        ;;; Object Creation
        "CREATE DATA"
        "CREATE OBJECT"

        ;; Calling and Exiting Program Units
        ;;; Calling Programs
        "CALL TRANSACTION"
        "LEAVE TO TRANSACTION"
        "SUBMIT"
        ;;; Calling Processing Blocks
        "CALL FUNCTION"
        "CALL METHOD"
        "PERFORM"
        "RAISE EVENT"
        "SET HANDLER"
        "SET USER-COMMAND"
        ;;; Exiting Program Units
        "CHECK"
        "CONTINUE"
        "EXIT"
        "LEAVE PROGRAM"
        "REJECT"
        "RETURN"
        "STOP"

        ;; Program Flow Logic
        ;;; Control Structure
        "DO" "ENDDO"
        "CASE" "WHEN" "ENDCASE"
        "CASE TYPE OF" "WHEN TYPE" "ENDCASE"
        "IF" "ELSEIF" "ELSE" "ENDIF"
        "WHILE" "ENDWHILE"
        ;;; Program Interruption
        "WAIT UP TO"
        ;;; Exception Handling
        "RAISE" "RAISE EXCEPTION"
        "TRY" "CATCH" "CLEANUP" "ENDTRY"
        "RESUME"

        ;; Assignment
        ;;; Special Assignment
        "MOVE-CORRESPONDING"
        "UNPACK"
        ;;; Setting References
        "ASSIGN"
        "UNASSIGN"
        "GET REFERENCE"
        ;;; Initizalization
        "CLEAR"
        "FREE"

        ;; Processing Internal Data
        ;;; Calculation Statements
        "ADD"
        "DIVIDE"
        "MULTIPLY"
        "SUBTRACT"
        ;;; Character String and Byte String Processing
        "CONCATENATE"
        "CONDENSE"
        "CONVERT"
        "FIND"
        "GET BIT"
        "OVERLAY"
        "REPLACE"
        "SET BIT"
        "SHIFT"
        "SPLIT"
        "TRANSLATE"
        "WRITE TO"
        ;;; Date and Time Processing
        "CONVERT INTO TIME STAMP"
        "CONVERT TIME STAMP"
        "GET TIME"
        "GET TIME STAMP"
        ;;; Internal Tables
        "APPEND"
        "COLLECT"
        "DELETE"
        "FIND IN TABLE"
        "INSERT"
        "LOOP AT" "ENDLOOP"
        "LOOP AT GROUP" "ENDLOOP"
        "AT"
        "MODIFY"
        "READ TABLE"
        "REPLACE IN TABLE"
        "SORT"
        "SUM"
        ;;; Meshes
        "SET ASSOCIATION"
        ;;; Attributes of Data Objects
        "DESCRIBE"

        ;; Processing External Data
        ;;; Open SQL
        "CLOSE CURSOR"
        "DELETE"
        "FETCH NEXT CURSOR"
        "INSERT"
        "MODIFY"
        "OPEN CURSOR"
        "SELECT"
        "ENDSELECT"
        "UPDATE"
        ;;; Native SQL
        "EXEC SQL" "ENDEXEC"
        ;;; ABAP and HANA
        "CALL DATABASE PROCEDURE"
        ;;; Secondary Database Connections
        "COMMIT CONNECTION"
        "ROLLBACK CONNECTION"
        ;;; Data Clusters
        "DELETE"
        "EXPORT"
        "FREE MEMORY"
        "IMPORT"
        "IMPORT DIRECTORY"
        ;;; File Interface
        "CLOSE DATASET"
        "DELETE DATASET"
        "GET DATASET"
        "OPEN DATASET"
        "READ DATASET"
        "SET DATASET"
        "TRANSFER"
        "TRUNCATE DATASET"
        ;;; Data Consistency
        "AUTHORITY-CHECK"
        "COMMIT WORK"
        "ROLLBACK WORK"
        "SET UPDATE TASK LOCAL"

        ;; Program Parameters
        ;;; SAP Memory
        "GET PARAMETER"
        "SET PARAMETER"
        ;;; Language Environment
        "GET LOCALE"
        "SET COUNTRY"
        "SET LANGUAGE"
        "SET LOCALE"

        ;; Program Editing
        ;;; Testing and Checking Programs
        "ASSERT"
        "BREAK-POINT"
        "LOG-POINT"
        "GET RUN TIME"
        "SET RUN TIME"
        "SET RUN TIME"
        "TEST-SEAM"
        "END-TEST-SEAM"
        "TEST-INJECTION"
        "END-TEST-INJECTION"
        ;;; Dynamic Program Development
        "GENERATE SUBROUTINE POOL"
        "INSERT REPORT"
        "INSERT TEXTPOOL"
        "READ REPORT"
        "READ TEXTPOOL"
        "SYNTAX-CHECK"

        ;; ABAP Data and Communication Interfaces
        ;;; Remote Function Call
        "CALL FUNCTION DESTINATION"
        "RECEIVE"
        "WAIT FOR ASYNCHRONOUS TASKS"
        "WAIT FOR MESSAGING CHANNELS"
        ;;; ABAP and XML
        "CALL TRANSFORMATION"
        ;;; OLE Interface
        "CALL METHOD"
        "CREATE OBJECT"
        "FREE OBJECT"
        "GET PROPERTY"
        "SET PROPERTY"

        ;; User Dialogs
        ;;; Dynpros
        "CALL SCREEN"
        "CONTROLS"
        "EXIT FROM STEP-LOOP"
        "GET CURSOR"
        "GET PF-STATUS"
        "LEAVE [TO]"
        "LOOP AT SCREEN" "ENDLOOP"
        "MODIFY SCREEN"
        "REFRESH CONTROL"
        "SET CURSOR"
        "SET HOLD DATA"
        "SET PF-STATUS"
        "SET SCREEN"
        "SET TITLEBAR"
        "SUPPRESS DIALOG"
        ;;; Selection Screens
        "PARAMETERS"
        "SELECTION-SCREEN"
        "SELECT-OPTIONS"
        ;;; List
        "BACK"
        "DESCRIBE LIST"
        "FORMAT"
        "GET CURSOR"
        "HIDE"
        "LEAVE TO LIST-PROCESSING"
        "LEAVE LIST-PROCESSING"
        "MODIFY LINE"
        "NEW-LINE"
        "NEW-PAGE"
        "POSITION"
        "PRINT-CONTROL"
        "READ LINE"
        "RESERVE"
        "SCROLL LIST"
        "SET BLANK LINES"
        "SET CURSOR"
        "SET MARGIN"
        "SET PF-STATUS"
        "SET LEFT SCROLL-BOUNDARY"
        "SET TITLEBAR"
        "SKIP"
        "ULINE"
        "WINDOW"
        "WRITE"
        ;; Messages
        "MESSAGE"

        ;; Enhancements
        ;;; Source Code Enhancement
        "ENHANCEMENT" "ENDENHANCEMENT"
        "ENHANCEMENT-POINT"
        "ENHANCEMENT-SECTION" "END-ENHANCEMENT-SECTION"
        ;;; Enhancements Using BAdls
        "GET BADI"
        "CALL BADI"

        ;; Statements for Experts
        "INFOTYPES"
        "PROVIDE" "ENDPROVIDE"

        ;; Not Listed in Previous Section, But Somehow is Keyword
        ;; TODO Should be assembled in a regular expression form
        "FIELD-SYMBOL"
        "ASSIGNING"
        "EQ" "LE" "LT" "GT" "GE" "NE"
        "IS" "INITIAL" "BOUND" "FOUND"
        "AND" "OR" "NOT" "IN"
        "USING" "CHANGING"
        "VALUE" "INTO" "SINGLE"
        "WHERE" "ORDER BY" "ASCENDING" "DESCENDING" "HAVING"
        "TYPE" "OF" "LENGTH" "REF" "TO" "BY"
        "IMPLEMENTATION" "DEFINITION"
        "EXPORTING" "IMPORTING" "RETURNING" "EXCEPTIONS"
        "BEGIN" "END"
        "ADJACENT" "DUPLICATES" "FROM" "LINES"
        "WITH" "DEFAULT" "UNIQUE" "KEY"
        "TRANSPORTING" "NO FIELDS"
        "STANDARD" "SORTED" "HASHED" "TABLE"
        ))

;; (setq sap-abap-keywords '("REPORT" "DATA" "DATA:" "TYPE" "REF" "TYPES" "TABLES" "AT" "BEGIN" "OF" "TIMES" "PERFORM" "APPEND" "CLEAR" "TO" "CALL" "FUNCTION" "EXPORTING" "EXCEPTIONS" "SELECT" "UP" "FROM" "INTO" "CORRESPONDING" "FIELDS" "TABLE" "GT" "LT" "EQ" "LE" "GE" "INSERT" "INTO" "MODIFY" "WHEN" "USING" "LIKE" "CHANGING" "TYPE-POOLS" "ROWS" "INITIAL" "SIZE" "WITH" "HEADER" "LINE" "LINES" "WRITE" "ASSIGNING" "READ" "IMPORT" "EXPORT"  "IMPORTING" "PUBLIC" "FINAL" "DEFINITION" "CREATE PUBLIC" "PUBLIC SECTION" "CLASS-METHODS" "PROTECTED SECTION" "PRIVATE SECTION" "METHODS" "CONSTANTS" "VALUE" "NOT" "IS" "BOUND" "IMPLEMENTATION" "CHECK"))


;; (setq sap-abap-keywords (append sap-abap-keywords-open sap-abap-keywords-close sap-abap-keywords))

(setq sap-abap-types    '("C" "I" "F" "STRING" "X" "XSTRING" "N" "P" "ABAP_BOOL") )
(setq sap-abap-constants '("SPACE" "SY-" "ABAP_FALSE" "ABAP_TRUE"))
(setq sap-abap-events    '("START-OF-SELECTION" "AT SELECTION-SCREEN"))
(setq sap-abap-functions '("STRLEN" "CONCATENATE" "SPLIT" ))

;; Generate regex string for each category
(setq sap-abap-keywords-regexp  ( regexp-opt sap-abap-keywords  'words))
(setq sap-abap-type-regexp      ( regexp-opt sap-abap-types     'words))
(setq sap-abap-constants-regexp ( regexp-opt sap-abap-constants 'words))
(setq sap-abap-event-regexp     ( regexp-opt sap-abap-events    'words))
(setq sap-abap-functions-regexp ( regexp-opt sap-abap-functions 'words))

;; create the list for font-lock
(setq sap-abap-font-lock-keywords
      `(
        (,sap-abap-constants-regexp . font-lock-constant-face)
        (,sap-abap-event-regexp     . font-lock-builtin-face)
        (,sap-abap-functions-regexp . font-lock-function-name-face)
        (,sap-abap-keywords-regexp  . font-lock-keyword-face)
        (,sap-abap-type-regexp      . font-lock-type-face)
        ;; Order above matters, in general longer words first
        ))


;;;###autoload
(define-derived-mode sap-abap-mode prog-mode
  "ABAP Mode"
  ;; Code for syntax highlighting
  (setq-local font-lock-defaults '(sap-abap-font-lock-keywords nil t))
  (setq-local indent-line-function 'sap-abap-indent-line)
  (setq-local comment-start "*")
  (setq-local comment-style "plain")

  "Major mode for the ABAP Programming Language"
  (modify-syntax-entry ?' "\"")
  (modify-syntax-entry ?\" "<")
  (modify-syntax-entry ?*  "<")
  (modify-syntax-entry ?\n ">")
  (modify-syntax-entry ?_  "w")
  (modify-syntax-entry ?-  "w")
  (modify-syntax-entry ?|  "\"")
  )

;; clear memory
(setq sap-abap-keywords nil)
(setq sap-abap-types    nil)
(setq sap-abap-constants nil)
(setq sap-abap-events    nil)
(setq sap-abap-functions nil)

(setq sap-abap-keywords-regexp nil)
(setq sap-abap-type-regexp    nil)
(setq sap-abap-constants-regexp nil)
(setq sap-abap-event-regexp    nil)
(setq sap-abap-functions-regexp nil)

;; add the mode to the list
(provide 'sap-abap-mode)

;; Local Variables:
;; coding: utf-8
;; End:
