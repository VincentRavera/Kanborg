
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

(setq kanborg/task-data nil)
(setq kanborg/columns-data nil)
(setq kanborg/project-id 7)
(setq kanborg/url "https://your-kan-board/")
(setq kanborg/user-token "user:token")
(setq kanborg/task-org-file "~/Documents/ORG/AGENDA/Work/kanb.org")

; ----------------------------------------------------------------------
; Load needed packages
; ----------------------------------------------------------------------
(require 'url)
(require 'cl-lib)
(require 'json)
(require 'org-element)
(require 'seq)
;; (require 'cl)


; ----------------------------------------------------------------------
; Communication functions
; ----------------------------------------------------------------------

(defun kanborg/headers ()
  "Return the header for kanboard requests."
  (cl-acons "Authorisation" kanborg/user-token
         (cl-acons "Accept" "*/*"
                (cl-acons "Content-Type" "application/json"
                       nil))))

(defun kanborg/get-columns (url)
  "Get the columns information from the URL."
  (let ((url-request-method "POST")
        (url-request-extra-headers (kanborg/headers))
        (url-debug t)
        (url-request-data (json-encode
                           (kanborg/columns-json-tempate kanborg/project-id))))
    (with-current-buffer (url-retrieve-synchronously (concat url "/jsonrpc.php"))
      (goto-char url-http-end-of-headers)
      (kanborg/column-json-parse (json-read)))))


(defun kanborg/columns-json-tempate (projectid)
  "Return the json object for columns, for the project PROJECTID."
  (cl-acons 'jsonrpc "2.0"
         (cl-acons 'id 1
                (cl-acons 'method "getColumns"
                       (cl-acons 'params (vector projectid)
                              nil)))))

(defun kanborg/get-tasks (url)
  "Get the tasks information from the URL."
  (when (equal nil kanborg/columns-data)
    (kanborg/get-columns url))
  (let ((url-request-method "POST")
        (url-request-extra-headers (kanborg/headers))
        (url-debug t)
        (url-request-data (json-encode
                           (kanborg/tasks-json-tempate kanborg/project-id))))
    (with-current-buffer (url-retrieve-synchronously (concat url "/jsonrpc.php"))
      (goto-char url-http-end-of-headers)

      (kanborg/task-json-parse (json-read)))))

(defun kanborg/tasks-json-tempate (projectid)
  "Return the json object for tasks, for the project PROJECTID."
  (cl-acons 'jsonrpc "2.0"
         (cl-acons 'id 1
                (cl-acons 'method "getAllTasks"
                       (cl-acons 'params
                              (cl-acons 'project_id projectid
                                     (cl-acons 'status_id 1 nil))
                              nil)))))

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

(defun kanborg/column-json-parse (json-columns)
  "Build columns from JSON-COLUMNS data structure."
  (let ((results (cdr (assq 'result json-columns))))
    (setq kanborg/columns-data (reduce
                                (lambda (a b) (append a b))
                                (mapcar 'kanborg/process-column results)))))

;;; ----------------------------------------------------------------------
;;; Tasks Processing
;;; ----------------------------------------------------------------------
;;
;; Tasks in kanboard should be a task in org
;;
;; Task data from an extracted json is stored on

(defun kanborg/task-json-parse (&optional json-tasks)
  "Build tasks from JSON-TASKS data structure."
      (kanborg/conversion-json-to-org (setq kanborg/task-data json-tasks)))

(defun kanborg/process-json-task (task)
  "Parse a json TASK data structure and outputs the Org data structure."
  ;; (setq toto task)
  (let ((id (cdr (assq 'id task)))
        (title (cdr (assq 'title task)))
        (description (cdr (assq 'description  task)))
        (state (plist-get kanborg/columns-data (string-to-number (cdr (assq 'column_id task)))))
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
      (decode-coding-string description 'utf-8)))))
      ;; description))))

(defun kanborg/process-org-task (org-task)
  "Parse an ORG-TASK data structure and outputs the JSON data structure."
  (let ((id (org-element-property :ID task))
        (title (org-element-property :title task))
        (description (cdr (assq 'description  task)))
        ;; Reverse plist-get with a equal
        (state (plist-get
                (reverse kanborg/columns-data)
                (car (seq-filter
                      (lambda (a) (equal a (org-element-property :todo-keyword task)))
                      kanborg/columns-data))))
        (url (cdr (assq 'url task)))
        (deadline (kanborg/date-to-epoch (org-element-property :deadline task)))
        (schedule (kanborg/date-to-epoch (org-element-property :scheduled task))))
    (kanborg/task-generate-json-elem
     id title description state url deadline schedule)
    )

  )
(defun kanborg/task-generate-json-elem (id title description state url deadline schedule)
  "Template to generate an org data structure.

ID is the id of the Kanboard task,
TITLE is the title of the Kanboard task,
DESCRIPTION is the text of the Kanboard task,
STATE is the swimlane of the Kanboard task,
URL is a direct url to view the task,
DEADLINE if equals to '0' will not add a deadline block
SCHEDULE id equals to '0' will not add a schedule block"
  )

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
            (mapcar 'kanborg/process-json-task tasks)))
          buffer))))

;; ORG -> JSON

(defun kanborg/conversion-org-to-json (&optional org-data)
  "Convert ORG-DATA the data structure extarcted from the api."
  )
;;; ----------------------------------------------------------------------
;;; Utils
;;; ----------------------------------------------------------------------

;; BUFFER: org tasks buffer
(defun kanborg/tasks-get-org-buffer ()
  "Return the Buffer we are allowed to write into."
  (let ((buffername "*KanBorg:Tasks:ORG*"))
    (kill-buffer (get-buffer-create buffername))
    (let ((buffer (get-buffer-create buffername)))
      (with-current-buffer buffer
        (org-mode))
      buffer)))

;; BUFFER: json tasks buffer
(defun kanborg/task-get-json-buffer ()
  "Return the Buffer were the json tasks are."
  (if (equal nil kanborg/task-json-path)
      (get-buffer-create "*KanBorg:Tasks:JSON*")
    (find-file kanborg/task-json-path)))

;; BUFFER: json columns buffer
(defun kanborg/column-get-json-buffer ()
  "Return the Buffer were the json collumns are."
  (if (equal nil kanborg/column-json-path)
      (get-buffer-create "*KanBorg:Column:JSON*")
    (find-file kanborg/column-json-path)))
;; URL-BUFFERS:
(defun kanborg/columns-url-callback (status)
  "Switch to the buffer returned by `url-retreive'.
The buffer contains the raw HTTP response sent by the server.
STATUS the response status."
  (message "Kanborg:GET:Collumns:%s" status)
  (rename-buffer "*KanBorg:Column:JSON*")
  (switch-to-buffer (current-buffer)))

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
    (let ((kanbuffer (kanborg/get-tasks kanborg/url)))
      (ediff-buffers kanbuffer (find-file kanborg/task-org-file)))))

;; TODO Unitary: Pull
(defun kanborg/pull-and-compare-at-point ()
  "Update task at point. Pops up a diff buffer."
  (let ((task-org (org-element-at-point)))
    task-org))

;; Unitary: Push
(defun kanborg/push-at-point ()
  "Push current task at point to the server."
  (interactive)
  (let ((task-org (org-element-at-point)))
    ;; Does not give enough information
    ;; Validation is a headline
    (if (not (equal 'headline (car task-org)))
        (message "Please redo at headline")
      (setq mdata
            (org-element-map (org-element-parse-buffer) 'headline
              (lambda (hl)
                (when (string= (encode-coding-string (org-element-property :ID hl) 'utf-8)
                               (encode-coding-string (org-element-property :ID task-org) 'utf-8))
                  hl))
              nil t)))
    ;; (kanborg/conversion-org-to-json org-data-full)
    ))

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
;; (kanborg/column-json-parse)
;; (kanborg/task-json-parse)
;; (kanborg/conversion-json-to-org (kanborg/task-json-parse))
;; (setq mydata (kanborg/task-generate-org-elem 1991 "Toto" "Hello" "TODO" "http://example.com" "1611915616" "1611905616" ))
;;
;; (kanborg/get-columns kanborg/url)
;; (kanborg/get-tasks kanborg/url)


;;; kanborg.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
