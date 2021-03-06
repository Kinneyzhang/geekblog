---
title: "Emacs Hack - 习惯记录与打卡的demo"
date: 2020-08-04
draft: false
categories: ["Happy Hacking Emacs"]
tags: ["elisp", "习惯"]
comment: false
---

想要培养一些好的习惯，想要不那么费力的变得更自律。

读了一些关于时间管理和习惯培养的书籍和文章，我意识到“仪式感”和“记录反思”的重要性。事实上，以自己喜欢的方式做记录是件很开心的事情，我已经连续80天写晨间日记和晚间总结。现在, 写日志已经变成了习惯。但我希望培养更多好的习惯，用我自己喜欢的方式(在emacs中)，所以有了gk-habit这个demo。

# 效果

详见 <https://github.com/Kinneyzhang/gkhabit>

# 思路

gk-habit使用emacsql-sqlite创建数据库，有两个表 "habit" 和 "record"。前者记录习惯及其相关参数，后者记录习惯打卡的数据。

habit表

-   create-time 习惯创建的时间
-   name 习惯名称
-   frequency-type 习惯打卡频率类型，包括“每天”、“每周几重复”，“一周几次”，“一月几次”
-   frequency-param 频率类型的参数，每天重复则为nil、每周几重复则用数字表示（134表示每周一三四）、一周或一月的打卡的次数。
-   period 习惯发生的时间时间段
-   remind-time 提醒打卡的时间
-   remind-string 提醒打卡时的文本
-   status 习惯的状态，active或archive，archive表示不再跟踪该习惯
-   archive-time 归档的时间

record表

-   create-time 记录创建的时间
-   habit 记录的习惯
-   status 习惯完成状态(DONE完成，MISS未完成)
-   comment 未完成的原因

# 代码

    ;;; habit record - gk-habit
    (require 'cl-lib)
    (require 'emacsql-sqlite)
    
    (defconst gkh-buffer "*Gk-Habit*")
    
    (defvar gkh-db (emacsql-sqlite3 "~/.emacs.d/gk-habit/habit.db"))
    
    (defvar gkh-file "~/.emacs.d/gk-habit/habit.org")
    
    (defvar gkh-record-status '("DONE" "MISS"))
    
    (defvar gkh-frequency-type '("everyday" "repeat" "by-week" "by-month"))
    
    (defvar gkh-period '("get-up" "morning" "noon" "afternoon" "evening" "before-sleep"))
    
    (defvar gkh-table-habit-column
      '("create-time" "name" "frequency" "frequency-param"
        "peroid" "remind-time" "remind-string" "status" "archive-time"))
    
    (defvar gkh-table-record-column
      '("create-time" "habit" "status" "comment"))
    
    (defun gkh-db-create-tables ()
      (interactive)
      "Create database tables of gk-habit."
      (emacsql gkh-db [:create-table habit
                                     ([(create-time string :primary-key :not-null)
                                       (name string :not-null :unique)
                                       (frequency-type string :not-null)
                                       (frequency-param)
                                       (period string :not-null)
                                       (remind-time)
                                       (remind-string)
                                       (status string :not-null)
                                       (archive-time string)])])
    
      (emacsql gkh-db [:create-table record
                                     ([(create-time string :primary-key :not-null)
                                       (habit string :not-null)
                                       (status string :not-null)
                                       (comment string)]
                                      (:foreign-key [habit] :references habit [name]
                                                    :on-delete :cascade))]))
    
    (defun gkh-db-drop-tables ()
      "Drop database tables of gk-habit."
      (interactive)
      (let* ((tables (emacsql gkh-db [:select name
                                              :from sqlite_master
                                              :where (= type 'table)]))
             (table (completing-read "Choose a table: " tables nil t)))
        (emacsql gkh-db `[:drop-table ,(intern table)])))
    
    (defun gkh--frequency-params (frequency-type)
      "Get the habit frequency parameters"
      (let (param)
        (cond
         ((string= frequency-type "everyday")
          (setq param nil))
         ((string= frequency-type "repeat")
          (setq param (completing-read "repeated day (exam. \"135\" means habit repeat on Monday, Wensday and Friday in every week.): " nil)))
         ((string= frequency-type "by-week")
          (setq param (completing-read "how many times a week: " nil)))
         ((string= frequency-type "by-month")
          (setq param (completing-read "how many times a month: " nil))))
        param))
    
    (defun gkh-init ()
      "Gk-habit initialize, create database and org tables."
      (interactive)
      (ignore-errors (gkh-db-create-tables))
      (gkh-org-table-draw))
    
    (defun gkh-new ()
      "Add a new habit"
      (interactive)
      (cl-block 'return
        (let* ((create-time (format-time-string "%Y-%m-%d %T"))
               (habit
                (let ((temp (completing-read "name of habit: " nil))
                      (habits (mapcar 'car (emacsql gkh-db [:select name :from habit
                                                                    :where (= status "Active")]))))
                  (if (member temp habits)
                      (cl-return-from 'return
                        (message "the habit '%s' already exist!" temp))
                    temp)))
               (frequency-type (completing-read "frequency of habit: " gkh-frequency-type nil t))
               (frequency-param (gkh--frequency-params frequency-type))
               (period  (completing-read "period of habit: " gkh-period nil t))
               (remind-time
                (let ((temp (completing-read "remind this habit at: " nil)))
                  (if (string= "" temp)
                      nil temp)))
               (remind-string
                (let ((temp (completing-read "habit remind sentence: " nil)))
                  (if (string= "" temp)
                      nil temp))))
          (emacsql gkh-db `[:insert :into habit
                                    :values ([,create-time ,habit ,frequency-type ,frequency-param ,period ,remind-time ,remind-string "Active" nil])])
          (gkh-org-table-draw)
          (message "Habit '%s' is added!" habit))))
    
    (defun gkh-record ()
      "Insert a habit redord in table."
      (interactive)
      (let* ((create-time (format-time-string "%Y-%m-%d %T"))
             (habit (completing-read "Choose a habit: "
                                     (emacsql gkh-db [:select [name] :from habit
                                                              :where (= status "Active")])))
             (status (completing-read "Is the habit done?" gkh-record-status nil t))
             (comment
              (when (string= "MISS" status)
                (completing-read "Reason why missed: " nil))))
        (emacsql gkh-db `[:insert-into record
                                       :values ([,create-time ,habit ,status ,comment])])
        (gkh-org-table-draw)
        (message "Habit '%s' is %s, record on %s, %s" habit status create-time comment)))
    
    (defun gkh-archive ()
      "Archive a habit"
      (interactive)
      (let* ((habits (emacsql gkh-db [:select name :from habit
                                              :where (= status "Active")]))
             (habit (completing-read "Choose a habit: " habits nil t)))
        (emacsql gkh-db `[:update habit
                                  :set [(= status "Archive") (= archive-time ,(format-time-string "%Y-%m-%d %T"))]
                                  :where (= name ,habit)])
        (gkh-org-table-draw)
        (message "habit %s has been archived!" habit)))
    
    (defun gkh-org-table-draw ()
      "Draw gk-habit database in org table."
      (interactive)
      (let* ((table-alist '(("habit" . gkh-table-habit-column)
                            ("record" . gkh-table-record-column))))
        (with-temp-file gkh-file
          (goto-char (point-min))
          (dotimes (i (length table-alist))
            (let* ((headline (car (nth i table-alist)))
                   (column-list (eval (cdr (nth i table-alist))))
                   (column-num (length column-list)))
              (insert (concat "* " headline " table\n"))
              (org-table-create (concat (format "%s" column-num) "x2"))
              (dotimes (j column-num)
                (org-table-next-field)
                (insert (nth j column-list)))
              (let ((items (emacsql gkh-db `[:select * :from ,(intern headline)])))
                (dotimes (m (length items))
                  (dotimes (n column-num)
                    (org-table-next-field)
                    (insert (format "%s" (nth n (nth m items)))))))
              (org-table-align)
              (forward-line 2)
              (end-of-line)
              (newline 2))))))
    
    (defun gkh-org-table-display ()
      "Display gk-habit org table in a bottom buffer."
      (interactive)
      (gkh-org-table-draw)
      (if (string= (buffer-name) gkh-buffer)
          (message "Already in the Gk Habit buffer."))
      (select-window
       (or (get-buffer-window gkh-buffer)
           (selected-window)))
      (with-current-buffer (get-buffer-create gkh-buffer)
        (org-mode)
        (read-only-mode -1)
        (erase-buffer)
        (insert (file-contents gkh-file))
        (valign-mode)
        (goto-char (point-min))
        (read-only-mode 1))
      (view-buffer gkh-buffer 'kill-buffer))
    
    (provide 'gk-habit)

# 计划

-   设置习惯打卡时间提醒
-   使用matplotlib库绘制习惯打卡统计图

