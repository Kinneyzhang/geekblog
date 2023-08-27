---
title: "Emacs Lisp - autoflow自动化工作流"
date: 2023-08-26
draft: false
categories: ["Happy Hacking Emacs"]
tags: ["elisp", "workflow"]
toc: false
comment: false
---

晚上睡觉前我会固定地做下面一系列的事情：检查每日待办，写总结，记录习惯。这些事情都是在emacs中进行，存放在不同的文件中。我发现每次当我做其中一件时，都要反应一下需要打开什么文件，这就很烦。本着不让大脑在重复的事情上增加思考的原则，想到了实现“将一系列固定工作流自动化执行”的想法，这便有了`autoflow.el`。

# 源码

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

    ;; (global-set-key (kbd "C-c n n") #'autoflow-start)

# 使用

使用 `define-autoflow` 宏定义一个自动流程。该宏的第一个参数是流程的名字，剩下的参数需要提供一系列的函数。`M-x autoflow-start` 命令选择事先定义好的一个流程后开始执行，每做完一件事情，继续 `M-x autoflow-start`，直到整个工作流结束。

比如下面定义了一个我每晚睡前总结的工作流:

    (define-autoflow "summary routine"
      (para-daily-page-today) ;; 1.检查今日计划
      (para-find-habit) ;; 2.记录习惯完成情况
      (para-find-summary)) ;; 3.写今日总结

