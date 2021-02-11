;;; kanborg.el --- A kanboard to org converter -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Vincent RAVERA
;;
;; Author: Vincent RAVERA <http://github.com/VincentRAVERA>
;; Maintainer: Vincent RAVERA
;; Created: janvier 20, 2021
;; Modified: janvier 20, 2021
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/VincentRAVERA/kanborg
;; Package-Requires: ((emacs 27.1) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;
;;; Commentary:
;;
;; For the swarm ...
;;
;;; Code:

; ----------------------------------------------------------------------
; Configuration options
; ----------------------------------------------------------------------

(setq kanborg/task-json-path "./task.json")
(setq kanborg/task-data nil)
(setq kanborg/task-org-file "./tasks.org")
(setq kanborg/columns-json-path "./columns.json")
(setq kanborg/columns-data nil)

; ----------------------------------------------------------------------
; Load needed packages
; ----------------------------------------------------------------------
;; (require 'request)
(require 'json)
(require 'org-element)
;; (require 'cl)


; ----------------------------------------------------------------------
; Get authentication token via API
; ----------------------------------------------------------------------

;; (defun kanborg/get-project (user pass url)
;;   (progn
;;     (request
;;      (concat url "jsonrpc.php")
;;      :type "POST"
;;      :parser 'json-read
;;      :data "jsonrpc=2.0&id=1&method=getMyProjects"
;;      ;; :username user
;;      ;; :password pass
;;      :headers '(("Content-Type" . "application/json")
;;                 ("Accept" . "application/json"))
;;      :sync t
;;      :success (function*
;;                 (lambda (&key data &allow-other-keys)
;;                   (message "Done: %s" (request-response-status-code data))
;;                   (setq myresponse data))))
;;     myresponse))

;; Ignore tls and certs
;; (advice-add 'gnutls-available-p :around #'ignore)
;;

;;; ----------------------------------------------------------------------
;;; Columns Processing
;;; ----------------------------------------------------------------------
;;
;; Columns name should be the todo-keyword
;; Columns mapping between the if and the name is stored in a global alist named:
;; `kanborg/columns-data'
;;

(defun kanborg/process-column (col)
  "Extract a collumn COL id to title."
  (let-alist col
      (list (string-to-number .id) .title)))

(defun kanborg/column-json-parse (&optional arg)
  "Parse the Kanboard columns Jsons with ARG is a path."
  (interactive)
  (if (equal nil arg)
      (kanborg/column-json-parse kanborg/columns-json-path)
    (with-temp-buffer
      (insert-file-contents arg)
      (goto-char 1)
      (let ((results (cdr (assq 'result (json-read)))))
        (setq kanborg/columns-data (mapcar 'kanborg/process-column results))))))

;;; ----------------------------------------------------------------------
;;; Tasks Processing
;;; ----------------------------------------------------------------------
;;
;; Tasks in kanboard should be a task in org
;;
;; Task data from an extracted json is stored on

(defun kanborg/task-json-parse (&optional arg)
  "Parse the Kanboard task Jsons with ARG is a path."
  (interactive)
  (if (equal nil arg)
      (kanborg/task-json-parse kanborg/task-json-path)
      (with-temp-buffer
        (insert-file-contents arg)
        (goto-char 1)
        (setq kanborg/task-data (json-read)))))

(defun kanborg/process-task (task)
  "Parse a json TASK data structure and outputs the Org data structure."
  (let ((id (cdr (assq 'id task)))
        (title (cdr (assq 'title task)))
        (description (cdr (assq 'description  task)))
        (state (car (alist-get (string-to-number (cdr (assq 'column_id task))) kanborg/columns-data)))
        (url (cdr (assq 'url task)))
        (deadline (cdr (assq 'date_due task)))
        (schedule (cdr (assq 'date_started task))))
    (kanborg/task-generate-org-elem
     id title description state url deadline schedule)))

(defun kanborg/task-generate-org-elem (id title description state url deadline schedule)
  "Template to generate an org data structure.

ID is the id of the Kanboard task,
TITLE is the title of the Kanboard task,
DESCRIPTION is the text of the Kanboard task,
STATE is the swimlane of the Kanboard task,
URL is a direct url to view the task,
DEADLINE if equals to '0' will not add a deadline block
SCHEDULE id equals to '0' will not add a schedule block"
  (let ((dead (kanborg/epoch-to-date deadline))
        (sced (kanborg/epoch-to-date schedule)))
    (org-element-create
     'org-data nil
     (org-element-create
      'headline (list :title title
                      :level 1
                      :todo-keyword state)
      (org-element-create 'planning
                          (list :deadline dead
                                :scheduled sced))
      (org-element-create 'property-drawer
                          `(:begin 0
                            :end 0
                            :contents-begin 0
                            :contents-end 0
                            :post-blank 0
                            :post-affiliated 0)
                          (org-element-create 'node-property
                                              (list :key "ID"
                                                    :value id))
                          (org-element-create 'node-property
                                              (list :key "URL"
                                                    :value url))
                          (org-element-create 'node-property
                                              (list :key "ORIGIN"
                                                    :value "KANBORG")))
      description))))

;;; ----------------------------------------------------------------------
;;; Conversions
;;; ----------------------------------------------------------------------

;; JSON -> ORG
(defun kanborg/conversion-json-to-org (&optional json-data)
  "Convert the `kanborg/task-data' from json to an org buffer.
Or JSON-DATA the data structure extarcted from the api."
  (if (equal nil json-data)
      kanborg/conversion-json-to-org kanborg/task-data
      (let ((tasks (cdr (assq 'result json-data)))
            (buffer (kanborg/tasks-get-org-buffer)))
        (with-current-buffer buffer
          (insert
           (org-element-interpret-data
            (mapcar 'kanborg/process-task tasks)))))))

;; ORG -> JSON

;;; ----------------------------------------------------------------------
;;; Utils
;;; ----------------------------------------------------------------------

;; BUFFER: org tasks buffer
(defun kanborg/tasks-get-org-buffer ()
  "Return the Buffer we are allowed to write into."
  (get-buffer-create "*KanBorg:Tasks:ORG*"))

;; BUFFER: json tasks buffer
(defun kanborg/task-get-json-buffer ()
  "Return the Buffer were the json tasks are."
  (if (equal nil kanborg/task-json-path)
      (get-buffer-create "KanBorg:Tasks:JSON")
    (find-file kanborg/task-json-path)))

;; BUFFER: json columns buffer
(defun kanborg/column-get-json-buffer ()
  "Return the Buffer were the json collumns are."
  (if (equal nil kanborg/column-json-path)
      (get-buffer-create "KanBorg:Column:JSON")
    (find-file kanborg/column-json-path)))

;; TIME: json -> org
(defun kanborg/epoch-to-date (epoch-string)
  "Convert the json EPOCH-STRING to org datastructure."
  (if (not (equal 0 (string-to-number epoch-string)))
      (org-timestamp-from-time (seconds-to-time (string-to-number epoch-string)))
    nil))

;; TIME: org  -> json
(defun kanborg/date-to-epoch (org-timestamp)
  "Convert the org ORG-TIMESTAMP to json epoch string."
  (number-to-string (time-to-seconds (org-timestamp-to-time org-timestamp))))


;;; ----------------------------------------------------------------------
;;; Updates
;;; ----------------------------------------------------------------------

;; Global: Pull
(defun kanborg/pull-and-compare ()
  "Spawns ediff between the file and a tempary buffer."
  (interactive)
  (if (equal nil kanborg/task-org-file)
      (message "Please specify kanborg/tasks-org-file to compare remote tasks and local.")
    (kanborg/conversion-json-to-org (kanborg/task-json-parse))
    (ediff-buffers (kanborg/tasks-get-org-buffer) (find-file kanborg/task-org-file))))

;; Unitary: Pull
(defun kanborg/pull-and-compare-at-point ()
  "Update task at point. Pops up a diff buffer."
  (let ((task-org (org-element-at-point)))
    task-org))

;; Unitary: Push
(defun kanborg/push-and-compare-at-point ()
  "Update task at point. Pops up a diff buffer."
  (let ((task-org (org-element-at-point)))
    task-org))

(provide 'kanborg)
;; ;;
;; ;;
;; ;; Samples
;; ;;
;; ;; Example to extract data from org
;; (save-excursion
;;   (with-current-buffer (get-buffer-create "*new*")
;;     (setq my-data (org-element-parse-buffer))))
;; ;; Sample self conctructed org
;; ;; https://emacs.stackexchange.com/questions/2869/turn-a-list-or-data-structure-into-an-org-document
;; (setq my-data '(headline (:title "MyTitle"
;;                           :level 1
;;                           :todo-keyword #("TODO"))
;;                          (property-drawer nil
;;                                           ((node-property (:key 31 :value "BloCked"))))
;;                          (#("Hello text"))))
;; ;; Example to insert data in a buffer
;; (save-excursion
;;   (with-current-buffer (get-buffer-create "*new*<3>")
;;     (insert (org-element-interpret-data my-data))))
;;
;; TESTs
;;
;; (setq kanborg/task-json-path "./ticket.json")
;; (setq kanborg/task-data nil)
;; (setq kanborg/columns-json-path "./columns.json")
;; (setq kanborg/columns-data nil)
;; (kanborg/column-json-parse)
;; (kanborg/task-json-parse)
;; (kanborg/conversion-json-to-org (kanborg/task-json-parse))
;; (setq mydata (kanborg/task-generate-org-elem 1991 "Toto" "Hello" "TODO" "http://example.com" "1611915616" "1611905616" ))


;;; kanborg.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
