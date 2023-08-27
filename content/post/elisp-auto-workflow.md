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
      `(progn
         (if-let ((match (assoc ,name autoflow-list)))
             (unless (equal (cdr match) ',funcs)
               (setcdr match ',funcs))
           (push (append (list ,name) ',funcs) autoflow-list))
         autoflow-list))

    (defun autoflow-set-header-info ()
      (let* ((name autoflow-curr-flow)
             (funcs (autoflow-flows name)))
        (setq-local header-line-format
                    (format "Autoflow %s/%s [%s] "
                            (1+ autoflow-curr-nth) (length funcs) name))))

    (defun autoflow-flows (name)
      (cdr (assoc name autoflow-list)))

    (defun autoflow--curr-func (nth funcs)
      "Return the current applying function as a list."
      (if-let* ((func (nth nth funcs))
                (_ (functionp func)))
          (list func)
        func))

    (defun autoflow--next ()
      (cl-incf autoflow-curr-nth)
      (let* ((flow-name autoflow-curr-flow)
             (flow-funcs (autoflow-flows flow-name)))
        (if (< autoflow-curr-nth (length flow-funcs))
            (progn
              (setq-local header-line-format nil)
              (apply (autoflow--curr-func autoflow-curr-nth flow-funcs))
              (autoflow-set-header-info))
          (message "autoflow %s over!" autoflow-curr-flow)
          (setq autoflow-curr-flow nil)
          (setq autoflow-curr-nth 0)
          (setq-local header-line-format nil))))

    ;;;###autoload
    (defun autoflow-start (&optional name)
      (interactive)
      (if autoflow-curr-flow
          (autoflow--next)
        (let* ((flow-name (completing-read "Choose a autoflow: "
                                      autoflow-list nil t))
               (flow-funcs (autoflow-flows flow-name)))
          (setq autoflow-curr-flow flow-name)
          (setq autoflow-curr-nth 0)
          (apply (autoflow--curr-func autoflow-curr-nth flow-funcs))
          (autoflow-set-header-info))))

    (global-set-key (kbd "C-c n n") #'autoflow-start)
