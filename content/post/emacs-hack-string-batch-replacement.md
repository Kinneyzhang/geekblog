---
title: "Emacs Hack - 批量替换字符串"
date: 2020-06-16
draft: false
categories: ["Happy Hacking Emacs"]
tags: ["elisp", "批量替换"]
comment: false
---

# 实际问题

不使用interactive function(M-%)如何批量替换文件中的字符串？应用场景为文字校验。

# 解决思路

字符串替换分三种情况：

1.  在当前buffer替换
2.  在指定文件中替换
3.  为指定目录下所有文件替换。

用于替换的pair有两种来源：

1.  `repl-string-list` 和 `repl-regexp-list` 变量
2.  `repl-file` 对应的文件。

字符串查找的方式有两种，分别对应两个函数：

1.  纯字符串 `(replall-string)`
2.  正则表达式 `(replall-regexp)`

优先使用列表变量的值，如果该变量值为nil则使用repl-file对应的文件中的值。其中文件中的pair只能用于 `replall-string` 函数。两个变量列表的值分别对应两个函数。repl-file的格式为每行一个pair，分别为被替换的字符串和用于替换的字符串，用空格隔开。

# 代码实现

主函数 `replall-string` 和 `replall-regexp` 可以选择替换类型。对于具体的情况也可以直接使用具体的替换函数。代码如下:

    (setq repl-string-list
          '(("old" "new")
            ("test" "测试")
            ("错误" "right")
            ("隔开你" "戈楷旎")))
    
    (setq repl-regexp-list
          '(("\\." "。")))
    
    (setq repl-file "~/replace.txt")
    
    (defun replall--read-pair-from-file ()
      (let ((repl-list '()))
        (with-temp-buffer
          (insert-file-contents repl-file)
          (goto-char (point-min))
          (while (< (point) (point-max))
            (setq repl-pair (split-string (thing-at-point 'line) "[ \f\t\n\r\v]+" t "[ \f\t\n\r\v]+"))
            (if (null repl-pair)
                (next-line)
              (next-line)
              (setq repl-list (append repl-list (list repl-pair))))))
        repl-list))
    
    (defun replall--get-repl-string-list ()
      (if (bound-and-true-p repl-string-list)
          repl-string-list
        (replall--read-pair-from-file)))
    
    (defun replall--get-repl-regexp-list ()
      (if (bound-and-true-p repl-regexp-list)
          repl-regexp-list
        (message "please set variable 'repl-regexp-list'!")))
    
    (defun replall--string (file lst)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (dolist (pair lst)
          (while (search-forward (car pair) nil t)
            (replace-match (cadr pair)))
          (goto-char (point-min)))
        (write-file file)))
    
    (defun replall--regexp (file lst)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (dolist (pair lst)
          (while (re-search-forward (car pair) nil t)
            (replace-match (cadr pair)))
          (goto-char (point-min)))
        (write-file file)))
    
    (defun replall-string-in-curr-buffer ()
      (interactive)
      (let ((curr-file (buffer-file-name (current-buffer)))
            (repl-list (replall--get-repl-string-list)))
        (replall--string curr-file repl-list)))
    
    (defun replall-regexp-in-curr-buffer ()
      (interactive)
      (let ((curr-file (buffer-file-name (current-buffer)))
            (repl-list (replall--get-repl-regexp-list)))
        (replall--regexp curr-file repl-list)))
    
    (defun replall-string-in-file (file repl)
      (interactive "fchoose a file to be processed: ")
      (let ((repl-list (replall--get-repl-string-list)))
        (replall--string file repl-list)))
    
    (defun replall-regexp-in-file (file repl)
      (interactive "fchoose a file to be processed: ")
      (let ((repl-list (replall--get-repl-regexp-list)))
        (replall--regexp file repl-list)))
    
    (defun replall--get-real-files-in-dir (dir)
      (let ((real-files)
            (files (directory-files dir)))
        (dolist (file files)
          (when (not (or (string= "." (substring file 0 1))
                         (string= "#" (substring file 0 1))
                         (string= "~" (substring file -1))))
            (push file real-files)))
        real-files))
    
    (defun replall-string-in-directory (dir)
      (interactive "Dchoose a directory to be processed: ")
      (let* ((repl-list (replall--get-repl-string-list))
             (real-files (replall--get-real-files-in-dir dir)))
        (dolist (file real-files)
          (replall--string (concat dir file) repl-list))))
    
    (defun replall-regexp-in-directory (dir)
      (interactive "Dchoose a directory to be processed: ")
      (let* ((repl-list (replall--get-repl-regexp-list))
             (real-files (replall--get-real-files-in-dir dir)))
        (dolist (file real-files)
          (replall--regexp (concat dir file) repl-list))))
    
    (defun replall-string (type)
      (interactive "sreplace string: 1.in current buffer  2.in a file  3.in a directory (input 1~3): ")
      (cond
       ((string= type "1")
        (replall-string-in-curr-buffer))
       ((string= type "2")
        (call-interactively #'replall-string-in-file))
       ((string= type "3")
        (call-interactively #'replall-string-in-directory))
       (t (message "please input 1~3!"))))
    
    (defun replall-regexp (type)
      (interactive "sreplace regexp: 1.in current buffer  2.in a file  3.in a directory (input 1~3): ")
      (cond
       ((string= type "1")
        (replall-regexp-in-curr-buffer))
       ((string= type "2")
        (call-interactively #'replall-regexp-in-file))
       ((string= type "3")
        (call-interactively #'replall-regexp-in-directory))
       (t (message "please input 1~3!"))))

