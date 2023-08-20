---
title: "Emacs Lisp - 不打断工作流的通用 capture 功能"
date: 2023-08-21
draft: false
categories: ["elisp"]
tags: ["capture"]
comment: false
---

当我们在emacs中进行一项日常工作，一个灵感跑到脑子里。这时，我们通常的做法是：打开自己用来记录灵感的文件，记录想法，切回正在编辑文件，继续当前工作... 尽管在emacs中我们有许多的方法快速打开那个“灵感文件”，比如设置bookmark等，但这个过程仍然容易打断我们的工作流。每一次的buffer切换，光标移动都意味着我们需要为这次临时的记录，多一次选择，多付出一些精力。

这让我开始思考，如何在保留当前编辑区域的情况下，快速记录临时的想法呢。事实上，org-capture 的思路是不错的。但是 org-capture 和 org-mode 绑定，我们需要一个更加通用的 capture 功能，支持任何格式，任意方式的记录。于是便有了 my-capture。

# 代码

    (defvar my-capture-return-winconf nil)
    (defvar my-capture-buffer "*My Capture*")
    (defvar my-capture-target-alist nil)
    (defvar my-capture-target-key nil)

    (defvar my-capture-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-c C-s") #'my-capture-save)
        (define-key map (kbd "C-c C-c") #'my-capture-finalize)
        (define-key map (kbd "C-c C-k") #'my-capture-cancel)
        map))

    (defun my-capture--parse-target (target-info)
      (pcase target-info
        ((pred stringp) (cons target-info 'append))
        ((pred consp)
         (cond
          ((functionp (car target-info)) (apply (car target-info) (cdr target-info)))
          ((stringp (car target-info)) target-info)))))

    (defun my-capture--write ()
      (let* ((target-key my-capture-target-key)
             (content (buffer-substring (point-min) (point-max)))
             (target-info-lst (cdr (assoc target-key my-capture-target-alist))))
        (mapcar (lambda (target)
                  (let* ((file-pos-cons (my-capture--parse-target target))
                         (file (car file-pos-cons))
                         (point-or-symbol (cdr file-pos-cons))
                         point)
                    (with-current-buffer (find-file-noselect file)
                      (save-excursion
                        (pcase point-or-symbol
                          ('prepend (setq point (point-min)))
                          ('append (setq point (point-max)))
                          (_ (setq point point-or-symbol)))
                        (goto-char (or point (point-max)))
                        (insert content)))))
                target-info-lst)))

    (defun my-capture-save ()
      "Save content to file and continue capturing."
      (interactive)
      (my-capture--write)
      (erase-buffer))

    (defun my-capture-finalize ()
      "Finalize current capture"
      (interactive)
      (my-capture--write)
      (set-window-configuration my-capture-return-winconf)
      (kill-buffer my-capture-buffer))

    (defun my-capture-cancel ()
      "Cancel current capture."
      (interactive)
      (set-window-configuration my-capture-return-winconf)
      (kill-buffer my-capture-buffer))

    (define-minor-mode my-capture-mode
      "Minor mode for quick capture."
      :keymap my-capture-mode-map
      (if my-capture-mode
          (setq-local header-line-format
                      (substitute-command-keys
                       (format "\\<my-capture-mode-map>Capture %s, save `\\[my-capture-save]', finish \
    `\\[my-capture-finalize]', cancel `\\[my-capture-cancel]'." my-capture-target-key)))
        (setq-local header-line-format nil)))

    (defun my-capture ()
      "Pop up a side window to capture text."
      (interactive)
      (let ((keyword (completing-read "Choose a capture type: " my-capture-target-alist nil t)))
        (setq my-capture-return-winconf (current-window-configuration))
        (display-buffer-in-side-window (get-buffer-create my-capture-buffer) nil)
        (select-window (get-buffer-window my-capture-buffer))
        (erase-buffer)
        (setq-local my-capture-target-key keyword)
        (my-capture-mode 1)))

# 配置

    (setq my-capture-target-alist
          '(("fleeting-note" (my-capture-note "Fleeting notes"))
            ("permanent-note" (my-capture-note "Permanent notes"))
            ("literature-note" (my-capture-note "Literature notes"))
            ("project-note" (my-capture-note "Project notes"))
            ("misc-note" (my-capture-note "Misc"))
            ("todo" (my-capture-todo))
            ("inbox" (my-capture-inbox))))

    (defvar my-capture-note-file "~/PARA/RESOURCE/Emacs/pkgs/capture/zknote.org")
    (defvar my-capture-todo-file "~/PARA/RESOURCE/Emacs/pkgs/capture/zktodo.org")
    (defvar my-capture-inbox-file "~/PARA/RESOURCE/Emacs/pkgs/capture/zkinbox.org")

    (defun my-capture-note (&optional type)
      "TYPE should be one of Fleeting notes, Permanent notes,
    Literature notes, Project notes."
      (let ((file my-capture-note-file)
            (timestamp (format-time-string "** %Y-%m-%d %H:%M:%S"))
            point)
        (with-current-buffer (find-file-noselect file)
          (save-excursion
            (goto-char (point-min))
            (if type
                (when (re-search-forward type nil t)
                  (end-of-line)
                  (insert "\n" timestamp "\n")
                  (setq point (point)))
              (when (re-search-forward "MISC" nil t)
                (end-of-line)
                (insert "\n" timestamp "\n")
                (setq point (point))))))
        (cons file point)))

    (defun my-capture-todo (&optional month-num)
      (let ((file my-capture-todo-file)
            (month (if month-num
                       (concat (format-time-string "%Y-")
                               (string-pad month-num 2 ?0 t))
                     (format-time-string "%Y-%m")))
            point)
        (with-current-buffer (find-file-noselect file)
          (save-excursion
            (goto-char (point-min))
            (when (re-search-forward (concat "^* " month) nil t)
              (insert "\n** TODO ")
              (setq point (point)))))
        (cons file point)))

    (defun my-capture-inbox ()
      (let ((file my-capture-inbox-file)
            point)
        (with-current-buffer (find-file-noselect file)
          (save-excursion
            (goto-char (point-min))
            (insert "\n")
            (setq point (point-min))))
        (cons file point)))


# demo gif
<img class="gk-single-img" src="/image/my-capture-demo.gif">
