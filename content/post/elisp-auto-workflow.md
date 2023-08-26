---
title: "Emacs Lisp - 自动化工作流"
date: 2023-08-26
draft: false
categories: ["Happy Hacking Emacs"]
tags: ["elisp", "workflow"]
toc: false
comment: false
---

晚上睡觉前我会固定地做下面一系列的事情：check每日待办，写总结，记录习惯。这些事情都是在emacs中进行，涉及到不同的文件buffer，每天重复相同的操作都要反应一下打开什么文件、做什么，实在是无趣。本着不让大脑在琐碎事情上增加思考的原则，于是想到了实现“将一系列固定工作流自动化执行”的想法，这便有了 autoflow。

    (defvar autoflow-list nil)
    (defvar autoflow-curr-nth 0)
    (defvar autoflow-curr-flow nil)

    (defmacro define-autoflow (name &rest funcs)
      (unless (member name (mapcar #'car autoflow-list))
        `(push (append (list ,name) ',funcs) autoflow-list)))

    (defun autoflow-set-header-info ()
      (let* ((name autoflow-curr-flow)
             (funcs (autoflow-flows name)))
        (setq-local header-line-format
                    (format "Autoflow [%s] %s/%s"
                            name (1+ autoflow-curr-nth) (length funcs)))))

    (defun autoflow-flows (name)
      (cdr (assoc name autoflow-list)))

    (defun autoflow-start (&optional name)
      (interactive)
      (let* ((name (completing-read "Choose a autoflow: "
                                    autoflow-list nil t))
             (funcs (autoflow-flows name)))
        (setq autoflow-curr-flow name)
        (setq autoflow-curr-nth 0)
        (funcall (car funcs))
        (autoflow-set-header-info)))

    (defun autoflow-next ()
      (interactive)
      (when autoflow-curr-flow
        (cl-incf autoflow-curr-nth)
        (let* ((flow-name autoflow-curr-flow)
               (flow-funcs (autoflow-flows flow-name)))
          (if (< autoflow-curr-nth (length flow-funcs))
              (progn
                (funcall (nth autoflow-curr-nth flow-funcs))
                (autoflow-set-header-info))
            (message "autoflow %s over!" autoflow-curr-flow)
            (setq autoflow-curr-flow nil)
            (setq autoflow-curr-nth 0)
            (setq-local header-line-format nil)))))

    (global-set-key (kbd "C-c f s") #'autoflow-start)
    (global-set-key (kbd "C-c f n") #'autoflow-next)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (defun autoflow-testflow1 () (message "autoflow-testflow1"))
    (defun autoflow-testflow2 () (message "autoflow-testflow2"))
    (defun autoflow-testflow3 () (message "autoflow-testflow3"))
    (defun autoflow-testflow4 () (message "autoflow-testflow4"))

    (define-autoflow "daily routine"
      autoflow-testflow1
      autoflow-testflow2
      autoflow-testflow3
      autoflow-testflow4)

    (provide 'autoflow)
