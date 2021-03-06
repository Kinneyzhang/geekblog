---
title: Emacs Workflow - 文件管理
date: 2020-08-27
draft: false
categories: ["Happy Hacking Emacs"]
tags: ["文件管理"]
comment: false
---

**Geekinney** 说: "文件管理"是每个操作系统最基本的功能。当我们使用计算机时，不可避免的要和文件打交道，比如：文件的创建、重命名、拷贝、移动、删除、搜索&#x2026; 通常，我们通过鼠标来完成这些操作。但你有没有想过，这种鼠标点击和拖动的操作实际是非常低效的，我们在鼠标上浪费了大量的时间！

此刻，Linux用户加入了会话:“我们不用鼠标，我们用命令行。” 诚然，Linux命令行确实比鼠标高效很多。但你有没有想过存在一种更加便捷的方式， **结合了GUI的直观和TUI的高效，将每种命令换成一个或简单的几个按键来完成** ？这就是我要介绍的emacs文件管理系统。

在阅读下文之前，请先在 `.emacs.d` 目录下的 `elisp` 文件夹中创建 `init-filemanage.el` 文件, 我们将用这个文件来存储所有与文件管理相关的配置。

# 基础

## 文件目录结构

Emacs中浏览文件目录结构主要使用内置的 `dired-mode` 及其拓展 `dired-x` 。

由于是内置的package，无需额外安装。在emacs中直接通过按键 `C-x d` (调用 `dired` 命令)后输入要打开的目录后回车，即可打开对应的目录结构。默认情况下，emacs将当前buffer的所在的目录作为打开的目录。在改变目录路径时可以使用 `TAB` 键补全。

或者，通过按键 `C-x C-j` (调用 `dired-jump` 命令，这是dired-x的功能)直接打开当前buffer文件所在的目录。dired目录结构如下图：

![img](/image/dired-mode.png)

### 文件导航

打开目录结构后，使用 `n/p` 或 `C-n/C-p` 或方向键都可以在文件和目录间移动。在目录上按回车键会打开新的目录，在文件上按回车键会打开该文件。在 `..` 上按回车键可以跳转到上级目录。除了这种方式，还可以按 `C-x C-j` 回到上级目录。其他的按键操作：

-   `SPC`: 向下移动一行
-   `^`: 跳转到上一目录级
-   `<`: 移到上一个目录行，跳过文件行
-   `>`: 移到下一个目录行，跳过文件行
-   `j`: 移到指定文件行

### 文件浏览

-   `A`: 按照正则搜索文件，列出搜索结果
-   `v`: 以view-mode(只读)浏览文件内容
-   `o`: 在另一个窗口打开文件或目录
-   `i`: 在当前窗口插入子目录
-   `s`: 对列表按名字或日期排序

一些文件在emacs中的浏览体验不是很好，可以使用系统默认程序浏览文件，将下面这段代码(来自xah-emacs)加入 `init-filemanage.el` 文件中：

    (defun xah-open-in-external-app (&optional @fname)
      "Open the current file or dired marked files in external app.
    The app is chosen from your OS's preference.
    When called in emacs lisp, if @fname is given, open that.
    URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
    Version 2019-11-04"
      (interactive)
      (let* (($file-list
              (if @fname
                  (progn (list @fname))
                (if (string-equal major-mode "dired-mode")
                    (dired-get-marked-files)
                  (list (buffer-file-name)))))
             ($do-it-p (if (<= (length $file-list) 5)
                           t
                         (y-or-n-p "Open more than 5 files? "))))
        (when $do-it-p
          (cond
           ((string-equal system-type "windows-nt")
            (mapc
             (lambda ($fpath)
               (w32-shell-execute "open" $fpath))
             $file-list))
           ((string-equal system-type "darwin")
            (mapc
             (lambda ($fpath)
               (shell-command
                (concat "open " (shell-quote-argument $fpath))))
             $file-list))
           ((string-equal system-type "gnu/linux")
            (mapc
             (lambda ($fpath) (let ((process-connection-type nil))
                                (start-process "" nil "xdg-open" $fpath)))
             $file-list))))))
    
    (define-key dired-mode-map (kbd "<C-return>") 'xah-open-in-external-app)

使用方法：按键 `C-return` 调用这个命令可以使用系统默认应用打开dired中光标所在文件或被标记的多个文件。

### 文件选择

选择多个文件的目的是批量操作，比如批量移动、批量删除等。选择文件的操作我们称为标记(mark)，相关按键如下：

-   `m`: 标记光标所在的文件或目录，并将光标下移一行
-   `DEL`: 删除上一行标记，并将光标上移一行
-   `u`: 取消光标所在文件或目录的标记
-   `U`: 取消所有文件或目录的标记
-   `t`: 反选所有文件或目录的标记
-   `#`: 标记所有以 `#` 结尾的emacs临时文件
-   `~`: 标记所有以 `～` 结尾的emacs自动备份文件
-   `d`: 给文件或目录加“待删除”标记(按'x'执行删除)

### 文件操作

按下快捷键时，dired优先操作有mark标记的文件，多个标记则为批量操作，没有标记则只对当前光标下的文件操作。常用的操作有：

-   `+`: 创建子目录
-   `C`: 拷贝文件或目录
-   `R`: 重命名/移动 文件或目录
-   `D`: 直接删除文件或目录
-   `x`: 删除带有“待删除”标记(d)的文件或目录
-   `c`: 压缩文件，默认可使用的后缀有 .zip, .tar.gz, .tar.bz2, .tar.xz, .tar.zst
-   `Z`: 使用gzip压缩或解压缩文件
-   `g`: 刷新dired buffer

## 文件侧边栏

Emacs中浏览文件侧边栏目录树的插件有好几个，比较常用的有 内置的speedbar、[neotree](https://github.com/jaypei/emacs-neotree)、[treemacs](https://github.com/Alexander-Miller/treemacs) 等。这里我们介绍neotree。Neotree侧边栏效果如下图：

![img](/image/neotree.png)

### 安装

将如下安装代码粘贴到 `init-filemanage.el` 文件中:

    (use-package neotree
      :ensure t
      :init (setq neo-window-fixed-size nil
                  neo-theme (if (display-graphic-p) 'icons 'arrow))
      :bind (("<f8>" . neotree-toggle)))

### 使用

-   `<f8>`: 打开neotree
-   `p, n`: 文件目录间上下移动
-   `SPC/RET/TAB`: 这三个快捷键都可以打开文件或展开目录
-   `U`: 跳转到上一级目录
-   `g`: 刷新
-   `H`: 显示或隐藏 隐藏文件(dotfiles)
-   `O`: 打开目录下的所有目录结构
-   `A`: 最大化/最小化neotree窗口
-   `C-c C-n`: 创建文件或目录(以"/"结尾)
-   `C-c C-d`: 删除文件或目录
-   `C-c C-r`: 重命名文件后目录
-   `C-c C-c`: 设置当前目录为展示的根目录
-   `C-c C-p`: 复制文件或目录

## 文件Tab栏

文件的Tab栏用于快速切换最近打开的文件，我们介绍 [centaur-tabs](https://github.com/ema2159/centaur-tabs) 。centaur-tabs效果如下图：

![img](/image/tabs.png)

### 安装

将如下安装代码粘贴到 `init-filemanage.el` 文件中:

    (use-package centaur-tabs
      :ensure t
      :config
      (setq centaur-tabs-style "bar"
            centaur-tabs-height 22
            centaur-tabs-set-icons t
            centaur-tabs-plain-icons t
            centaur-tabs-gray-out-icons t
            centaur-tabs-set-close-button t
            centaur-tabs-set-modified-marker t
            centaur-tabs-show-navigation-buttons t
            centaur-tabs-set-bar 'left
            centaur-tabs-cycle-scope 'tabs
            x-underline-at-descent-line nil)
      (centaur-tabs-headline-match)
      ;; (setq centaur-tabs-gray-out-icons 'buffer)
      ;; (centaur-tabs-enable-buffer-reordering)
      ;; (setq centaur-tabs-adjust-buffer-order t)
      (centaur-tabs-mode t)
      (setq uniquify-separator "/")
      (setq uniquify-buffer-name-style 'forward)
      (defun centaur-tabs-buffer-groups ()
        "`centaur-tabs-buffer-groups' control buffers' group rules.
     Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
     All buffer name start with * will group to \"Emacs\".
     Other buffer group by `centaur-tabs-get-group-name' with project name."
        (list
         (cond
          ((ignore-errors
             (and (string= "*xwidget" (substring (buffer-name) 0 8))
                  (not (string= "*xwidget-log*" (buffer-name)))))
           "Xwidget")
          ((or (string-equal "*" (substring (buffer-name) 0 1))
               (memq major-mode '(magit-process-mode
                                  magit-status-mode
                                  magit-diff-mode
                                  magit-log-mode
                                  magit-file-mode
                                  magit-blob-mode
                                  magit-blame-mode
                                  )))
           "Emacs")
          ((derived-mode-p 'prog-mode)
           "Editing")
          ((derived-mode-p 'dired-mode)
           "Dired")
          ((memq major-mode '(helpful-mode
                              help-mode))
           "Help")
          ((memq major-mode '(org-mode
                              org-agenda-clockreport-mode
                              org-src-mode
                              org-agenda-mode
                              org-beamer-mode
                              org-indent-mode
                              org-bullets-mode
                              org-cdlatex-mode
                              org-agenda-log-mode
                              diary-mode))
           "OrgMode")
          (t
           (centaur-tabs-get-group-name (current-buffer))))))
      :hook
      (dashboard-mode . centaur-tabs-local-mode)
      (term-mode . centaur-tabs-local-mode)
      (calendar-mode . centaur-tabs-local-mode)
      (org-agenda-mode . centaur-tabs-local-mode)
      (helpful-mode . centaur-tabs-local-mode)
      :bind
      ("C-c b" . centaur-tabs-backward)
      ("C-c n" . centaur-tabs-forward)
      ("C-c m" . centaur-tabs-forward-group)
      ("C-c v" . centaur-tabs-backward-group))

### 使用

-   `C-c n`: 切换到下一个tab
-   `C-c b`: 切换到上一个tab
-   `C-c v`: 切换到上一个分组
-   `C-c m`: 切换到下一个分组

## 文件(内容)搜索

### 搜索当前文件内容

在当前文件中快速搜索内容，使用 `swiper` ，将下面代码粘贴到 `init-filemanage.el` 中：

    (use-package swiper
      ;; 快捷搜索
      :ensure nil
      :bind (("C-s" . swiper)))

按键 `C-s` 就可以搜索内容啦。

### 搜索最近访问的文件

定位最近访问的文件最快的方法就是切换buffer，使用 `ivy-switch-buffer` 或 `counsel-switch-buffer` 。区别是，后者在buffer选项间移动时会实时的显示buffer的内容。读者可以尝试一下这两个命令，然后选择自己喜欢的方式绑定到快捷键。我用 `ivy-switch-buffer` 。将下面的代码粘贴到init-filemanage.el文件中：

    (global-set-key (kbd "C-x b") 'ivy-switch-buffer)

这段按键绑定的代码应该不难理解吧，相信你也应该知道怎么修改来使用另一个命令。哈哈，其实emacs-lisp也没有那么难啦～

当然也可以在之前ivy的配置中使用bind参数来绑定快捷键，就像前面的centaur-tabs。我们后面也将使用这种方式。

### 搜索经常访问的文件

对于经常需要访问的文件，使用 `bookmark` ，下次访问时直接从bookmark列表打开。将下面的代码粘贴到 `init-filemanage.el` 文件中：

    (use-package bookmark
      :ensure nil
      :bind (("C-x r m" . bookmark-set)
             ("C-x r d" . bookmark-delete)
             ("C-x r j" . bookmark-jump)))

按键 `C-x r m` 将当前文件加入bookmark，默认名称为文件名，也可以自己重命名。按键 `C-x r d` 选择一个bookmark删除。按键 `C-x r j` 选择一个bookmark打开。或者，使用 `counsel-bookmark` ，结合了创建和跳转bookmark两个功能，代码见下文。

### 搜索当前目录下文件内容

在当前目录内按照文件的内容查找，使用 `counsel-rg` 。由于该命令通过 `rg` 来实现搜索，所以在使用前需要先安装命令行工具 `rg(ripgrep)` 。MacOS下直接使用homebrew安装(其他系统请使用各自的包管理器安装)：

    $ brew install ripgrep


### 搜索当前目录下文件

在当前目录下按照文件名称查找文件，使用 `counsel-fzf` 。由于该命令通过 `fzf` 来实现搜索，所以在使用前需要先安装命令行工具 `fzf` 。

    $ brew install fzf


### 搜索当前git仓库下文件

在当前git代码仓库中查找文件，使用 `counsel-git` 。当前文件若不在git仓库中则无法使用该命令。

以上三种搜索方式，基本可以满足在emacs中快速定位和查找文件的需求。将下面的代码粘贴到 `init-filemanage.el` 文件中，相应快捷键的使用不再赘述。

    (use-package counsel
      :ensure t
      :bind (("M-x" . counsel-M-x)
             ("C-x C-f" . counsel-find-file)
             ("C-c c t" . counsel-load-theme)
             ("C-c c b" . counsel-bookmark)
             ("C-c c r" . counsel-rg)
             ("C-c c f" . counsel-fzf)
             ("C-c c g" . counsel-git)))

额外地, `C-c c t` 用于切换内置主题。

# 附加

## Dired额外功能

与文件系统相关的操作：

-   `H`: 建立硬链接
-   `S`: 建立软链接
-   `G`: 改变文件Group
-   `O`: 改变文件Owner
-   `M`: 改变文件权限
-   `P`: 打印

Dired中除了使用标记来批量操作文件外，还可以使用正则表达式。正则操作的快捷键一般以 `%` 开头。

-   `% m`: 标记正则匹配的文件
-   `% d`: 给正则匹配的文件添加“待删除标记”(按键'x'执行删除)
-   `% g`: 根据正则表达式，搜索所有文件的内容，标记内容中有正则匹配的文件
-   `% u`: 所有标记的文件名称转化为大写
-   `% l`: 所有标记的文件名称转化为小写

对于已经标记的文件，我们可以不用打开文件，而对多个文件的内容进行搜索或替换操作。

-   `A`: 根据正则表达式搜索已标记的文件的内容，并列出所有匹配行
-   `Q`: 对标记的文件逐一进行正则替换，按键 "y" 替换，按键 "n" 跳过

dired还可以通过调用外部命令来操作文件。

-   `!`: 以同步的方式调用shell命令来操作文件，命令运行的工作目录就是dired的当前目录
-   `&`: 以异步的方式调用shell命令来操作文件,命令运行的工作目录就是dired的当前目录

## Dired美化

嫌dired-mode颜色太单调？下面我们就给dired变个妆：

    (use-package diredfl
      :ensure t
      :config (diredfl-global-mode t))
    
    (use-package all-the-icons-dired
      :ensure t
      :config
      (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

   all-the-icons-dired 会自动安装其依赖的 `all-the-icons` package，安装成功后通过按键 `M-x all-the-icons-install-fonts` 安装必要的字体(此操作需要科学上网)。
没办法科学上网的同学可以直接将 [字体](https://github.com/domtronn/all-the-icons.el/tree/master/fonts) 下载到本地后添加到系统的字体目录中，如何添加自行搜索。

美化后的效果：

![img](/image/beautiful-dired.png)

# 结语

断断续续，花了一周的时间梳理文件管理的工作流。希望大家能够积极留言，说说有哪些对于新手难以理解的地方，有哪些其他的文件管理方面的需求。有不合理的地方也敬请指出，我将根据大家的反馈不断完善文章的内容。

At last, happy hacking emacs!

