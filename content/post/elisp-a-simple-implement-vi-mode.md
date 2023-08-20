---
title: "Emacs Lisp - 一个极简的(vi)模式编辑实现"
date: 2023-08-20
draft: false
categories: ["elisp"]
tags: ["vi"]
comment: false
---

偶然看到了一个命令的文档:

<pre>(suppress-keymap MAP &optional NODIGITS)

Make MAP override all normally self-inserting keys to be undefined.
Normally, as an exception, digits and minus-sign are set to make prefix args,
but optional second arg NODIGITS non-nil treats them like other chars.</pre>

一瞬间，我意识到，这个命令可以用来实现类似vi的模式编辑！准确的说是，基于输入字符按键的命令模式。于是有了下面的代码，一个极简的模式编辑的实现。一行配置：`(myvi-mode 1)` 即可开启使用。

    (defvar myvi-mode-type nil
      "Mode type of myvi, which should be
    one of these symbols: control, insert.")

    (defvar myvi-mode-map
      (let ((map (make-sparse-keymap))) map)
      "Keymap of `myvi-mode'.")

    (defvar myvi-insert-mode-map
      (let ((map (make-sparse-keymap))) map)
      "Keymap of `myvi-insert-mode'.")

    (defvar myvi-control-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map "<escape>" #'myvi-switch)
        (define-key map "i" #'myvi-switch)
        (define-key map "j" #'next-line)
        (define-key map "k" #'previous-line)
        (define-key map "h" #'backward-char)
        (define-key map "l" #'forward-char)
        (define-key map "w" #'forward-word)
        (define-key map "b" #'backward-word)
        (define-key map "G" #'forward-page)
        (define-key map "gg" #'backward-page)
        (define-key map "o" #'other-window)
        (define-key map "dd" #'kill-region)
        (define-key map "yy" #'kill-ring-save)
        (define-key map "p" #'yank)
        (define-key map "v" #'set-mark-command)
        ;; 在这里任意定义你想要的按键
        ;; ..............................
        map)
      "Keymap of `myvi-control-mode'.")

    ;;;; switch

    (defun myvi-refresh-cursor ()
      (let ((wins (window-list)))
        (with-selected-window (selected-window)
          (dolist (win wins)
            (select-window win)
            (when cursor-type
              (if myvi-control-mode
                  (setq cursor-type 'box)
                (setq cursor-type '(bar . 2))))))))

    (defun myvi-switch ()
      (interactive)
      (cond
       ((eq myvi-mode-type 'control) (myvi-insert-mode 1))
       ((eq myvi-mode-type 'insert) (myvi-control-mode 1))))

    ;;;; Minor modes

    (define-minor-mode myvi-mode
      "Minor mode for myvi."
      :keymap myvi-mode-map
      :global t
      (if myvi-mode
          (progn
            (myvi-control-mode 1)
            (add-hook 'window-configuration-change-hook 'myvi-refresh-cursor)
            (global-set-key (kbd "<escape>") #'myvi-switch))
        (myvi-control-mode -1)
        (myvi-insert-mode -1)
        (remove-hook 'window-configuration-change-hook 'myvi-refresh-cursor)
        (global-unset-key (kbd "<escape>"))))

    (define-minor-mode myvi-insert-mode
      "Minor mode for myvi insert mode."
      :keymap myvi-insert-mode-map
      :global t
      (when myvi-insert-mode
        (myvi-control-mode -1)
        (myvi-refresh-cursor)
        (setq myvi-mode-type 'insert)))

    (define-minor-mode myvi-control-mode
      "Minor mode for myvi control mode."
      :keymap myvi-control-mode-map
      :global t
      (when myvi-control-mode
        (myvi-insert-mode -1)
        (suppress-keymap myvi-control-mode-map)
        (myvi-refresh-cursor)
        (setq myvi-mode-type 'control)))
