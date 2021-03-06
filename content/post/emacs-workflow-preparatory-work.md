---
title: Emacs Workflow - 准备工作
date: 2020-06-25
draft: false
categories: ["Happy Hacking Emacs"]
comment: false
---

众所周知，emacs以学习曲线陡峭著称，所以在开始介绍workflow前，得做些准备工作。这篇文章主要介绍emacs的安装、基础概念、基础配置和使用emacs的基础知识。

# 安装

Emacs的安装非常简单，各个平台的安装方法详见[官网](https://www.gnu.org/software/emacs/)。我使用的是MacOS，通过homebrew安装 [emacs-plus](https://github.com/d12frosted/homebrew-emacs-plus) 27，支持xwidget webkit。安装代码如下：

    $ brew tap d12frosted/emacs-plus
    $ brew install emacs-plus@27 --with-xwidgets

MacOS Homebrew 的安装方法见 [brew.sh](https://brew.sh) 。

# 快捷键

Emacs和其他编辑器的相比一个重要的优势就是全键盘操作。所以在使用emacs之前，先通过按键 `C-h t` 查看内置的快捷键教程是一个较好的开端。下面对基本快捷键做简要介绍：

Emacs的快捷键都是组合键，由前缀键加上字母或数字组成。常见的前缀键有：

-   `M` 对应 `Alt` 键
-   `C` 对应 `Control` 键
-   `S` 对应 `Shift` 键

形如 `C-x C-f` 的快捷键表示按住 `Ctrl` 键时按下字母 `X` , 再按住 `Ctrl` 键同时按下字母 `F` 。

下面是一些常用快捷键：

<table>


<colgroup>
<col  class="org-left">

<col  class="org-left">

<col  class="org-left">

<col  class="org-left">
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">快捷键</th>
<th scope="col" class="org-left">功能</th>
<th scope="col" class="org-left">快捷键</th>
<th scope="col" class="org-left">功能</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left"><code>C-x C-c</code></td>
<td class="org-left">关闭Emacs</td>
<td class="org-left"><b>编辑</b></td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left"><code>C-g</code></td>
<td class="org-left">取消当前按键输入</td>
<td class="org-left"><code>C-@</code> 或 <code>C-SPC</code></td>
<td class="org-left">设置mark</td>
</tr>


<tr>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left"><code>M-w</code></td>
<td class="org-left">复制</td>
</tr>


<tr>
<td class="org-left"><b>移动</b></td>
<td class="org-left">&#xa0;</td>
<td class="org-left"><code>C-w</code></td>
<td class="org-left">剪切</td>
</tr>


<tr>
<td class="org-left"><code>C-p</code></td>
<td class="org-left">光标上移</td>
<td class="org-left"><code>C-y</code></td>
<td class="org-left">粘贴</td>
</tr>


<tr>
<td class="org-left"><code>C-n</code></td>
<td class="org-left">光标下移</td>
<td class="org-left"><code>C-x h</code></td>
<td class="org-left">全选</td>
</tr>


<tr>
<td class="org-left"><code>C-b</code></td>
<td class="org-left">光标左移</td>
<td class="org-left"><code>C-x C-q</code></td>
<td class="org-left">切换只读/编辑模式</td>
</tr>


<tr>
<td class="org-left"><code>C-f</code></td>
<td class="org-left">光标右移</td>
<td class="org-left"><code>C-/</code></td>
<td class="org-left">撤销上一步操作</td>
</tr>


<tr>
<td class="org-left"><code>C-a</code></td>
<td class="org-left">光标移到行首</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left"><code>C-e</code></td>
<td class="org-left">光标移到行尾</td>
<td class="org-left"><b>缓冲区</b></td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left"><code>M-b</code></td>
<td class="org-left">光标前移一个单词</td>
<td class="org-left"><code>C-x b</code></td>
<td class="org-left">切换buffer</td>
</tr>


<tr>
<td class="org-left"><code>M-f</code></td>
<td class="org-left">光标后移一个单词</td>
<td class="org-left"><code>C-x k</code></td>
<td class="org-left">关闭buffer</td>
</tr>


<tr>
<td class="org-left"><code>C-v</code></td>
<td class="org-left">向下翻页</td>
<td class="org-left"><code>C-x C-b</code></td>
<td class="org-left">查看所有打开的缓冲区</td>
</tr>


<tr>
<td class="org-left"><code>M-v</code></td>
<td class="org-left">向上翻页</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left"><code>C-l</code></td>
<td class="org-left">光标移到屏幕上/中/下部</td>
<td class="org-left"><b>窗口</b></td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left"><code>C-x [</code></td>
<td class="org-left">光标跳到文首</td>
<td class="org-left"><code>C-x 2</code></td>
<td class="org-left">在下面分割一个窗口</td>
</tr>


<tr>
<td class="org-left"><code>C-x ]</code></td>
<td class="org-left">光标跳到文末</td>
<td class="org-left"><code>C-x 3</code></td>
<td class="org-left">在右边分割一个窗口</td>
</tr>


<tr>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left"><code>C-x 0</code></td>
<td class="org-left">关闭当前窗口</td>
</tr>


<tr>
<td class="org-left"><b>文件</b></td>
<td class="org-left">&#xa0;</td>
<td class="org-left"><code>C-x 1</code></td>
<td class="org-left">关闭其它窗口</td>
</tr>


<tr>
<td class="org-left"><code>C-x C-f</code></td>
<td class="org-left">打开(创建)文件</td>
<td class="org-left"><code>C-x o</code></td>
<td class="org-left">依次切换窗口</td>
</tr>


<tr>
<td class="org-left"><code>C-x C-s</code></td>
<td class="org-left">保存文件</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
</tr>
</tbody>
</table>

# 最小配置

Emacs使用Emacs-Lisp作为配置语言，通过添加配置可以使emacs符合个人的使用习惯和实现各种功能。通常，emacs的配置文件放置在用户根目录的 `.emacs.d` 文件夹中。其中的 `init.el` 作为配置的入口文件。

下面提供了一段最小配置(参考 [better-defaults](https://github.com/technomancy/better-defaults) )，直接将它复制粘贴到 `init.el` 文件中后，重启emacs。

      ;;; better defaults
    (unless (or (fboundp 'helm-mode) (fboundp 'ivy-mode))
      (ido-mode t)
      (setq ido-enable-flex-matching t)) ;; 使用ido补全
    
    (unless (eq window-system 'ns)
      (menu-bar-mode -1)) ;; 禁用菜单栏
    (when (fboundp 'tool-bar-mode)
      (tool-bar-mode -1)) ;; 禁用工具栏
    (when (fboundp 'scroll-bar-mode)
      (scroll-bar-mode -1)) ;; 禁用垂直滚动条
    (when (fboundp 'horizontal-scroll-bar-mode)
      (horizontal-scroll-bar-mode -1)) ;; 禁用水平滚动条
    
    (require 'uniquify)
    (setq uniquify-buffer-name-style 'forward)
    
    ;; https://www.emacswiki.org/emacs/SavePlace
    (save-place-mode 1) ;; 保存光标文位置
    
    (global-set-key (kbd "C-s") 'isearch-forward)
    (global-set-key (kbd "C-r") 'isearch-backward)
    (global-set-key (kbd "C-c C-/") 'comment-or-uncomment-region) ;; 代码块注释和反注释
    
    (show-paren-mode 1) ;; 显示括号匹配
    (setq-default indent-tabs-mode nil)
    (setq save-interprogram-paste-before-kill t
          apropos-do-all t
          mouse-yank-at-point t
          require-final-newline t
          load-prefer-newer t
          ediff-window-setup-function 'ediff-setup-windows-plain)
    
    (require 'dired-x)
    (delete-selection-mode 1) ;; 选择后插入，删除原字符。
    (recentf-mode 1) ;; 保存最近访问
    (global-auto-revert-mode 1) ;; 自动加载更新内容
    (fset 'yes-or-no-p 'y-or-n-p) ;; 使用 'y/n' 代替 'yes/no'
    (setq custom-file (concat user-emacs-directory "custom.el"))
    (setq inhibit-startup-message t) ;; 禁止启动信息
    (setq ring-bell-function 'ignore) ;; 禁止发出声音警告
    (setq make-backup-files nil) ;; 不允许备份
    (setq auto-save-default nil  ;; 不允许默认自动保存
          auto-save-silent t))   ;; 自动保存时不显示消息
    (setq scroll-step 1 scroll-margin 3 scroll-conservatively 10000) ;; 连续滚动
    (setq confirm-kill-emacs
          (lambda (prompt) (y-or-n-p-with-timeout "Whether to quit Emacs:" 10 "y"))) ;; 防误操作退出
    (setq dired-recursive-deletes 'always
          dired-recursive-copies 'always) ;; 全部递归拷贝、删除文件夹中的文件

# 包管理

Emacs的package(也就是我们通常说的"包"或"插件")可以为emacs拓展丰富多样的功能。为了能够使用这些package，需要配置获取package的源。在init.el的最后加上以下代码：

    (setq package-enable-at-startup nil)
    (setq package-archives '(("gnu" . "http://mirrors.cloud.tencent.com/elpa/gnu/")
                             ("melpa" . "http://mirrors.cloud.tencent.com/elpa/melpa/")))

配置好源后，按键 `M-x list-packages` 可以查看所有已发布的package。按键 `M-x package-install` 后输入package名字可以直接安装，同理使用 `package-delete` 删除。使用package，需要先在配置文件中写入 `(require '<package-name>)` 这个过程相当于导入(import)。再加上必要的自定义配置便可使用该package所有的功能。 

以上的包管理方案由emacs内置的 `package.el` 提供。但内置的不一定是最好的。因此，有一些package专门提供了更加灵活、自动化的包管理方案。常用的有 `use-package` `quelpa` `straight` `el-get` 等，我使用的是 [use-package](https://github.com/jwiegley/use-package) 结合 `git submodule` 。下面的代码用于初始化 use-package ，加入init.el结尾。

    (unless (package-installed-p 'use-package)
      (package-refresh-contents)
      (package-install 'use-package))

写完配置代码后，在最后一个括号后面按键 `C-x C-e` (eval-last-sexp)即可执行配置，安装package。也可以重启emacs，再次打开时emacs会自动加载所有配置。

# 配置管理

值得注意的是，我们将上面的配置代码统统写入了init.el文件中。可以预见，当安装许多package时，配置代码将会增多，init.el的内容会变得复杂无比，难以阅读和维护。我们需要一种合理的组织配置文件的方式。

解决方法是将每一种workflow的配置代码写在单独的文件中，然后在init.el中引入该文件。操作如下：

1.  在.emacs.d文件夹下创建elisp文件夹。
2.  在init.el中添加代码 `(add-to-list 'load-path (concat user-emacs-directory "elisp"))` 。
3.  在elisp文件夹下创建 `init-better.el` ，将“最小配置”的代码粘贴进去。
4.  在 `init-better.el` 最后加上代码 `(provide 'init-better)` 。
5.  在 `init.el` 最后加上代码 `(require 'init-better)`

根据字面意思也不难理解：步骤2的代码将elisp文件夹下的所有文件加入配置加载路径；步骤4的provide提供文件名，使其可以被引入；步骤5的require引入了该文件。这样我们就将最小配置的代码引入到init.el中了。以后的各种workflow我们也将使用这种方式来组织配置文件。

# 实用package

介绍一些对于新手实用的package，直接将下面的配置粘贴到elisp文件夹下的 `init-utils.el` 文件中。

    (use-package super-save
      ;; 自动保存，用于替换默认的自动保存
      :ensure t
      :config
      (super-save-mode +1)
      (setq super-save-auto-save-when-idle t))
    
    (use-package which-key
      ;; emacs按键提示
      :ensure t
      :config
      (which-key-mode))
    
    (use-package ivy
      ;; emacs补全框架
      :ensure t
      :init
      (setq ivy-use-virtual-buffers t
            enable-recursive-minibuffers t)
      :config
      (ivy-mode 1))

同理，在配置init-utils.el文件结尾加上 `(provide 'init-utils)` ，然后在init.el中引入 `(require 'init-utils)` 。

# emacs主题

选择一个简洁、美观的主题不仅可以缓解眼睛疲劳，还可以提高使用emacs的效率。emacs的主题分为亮色和暗色两种，我的使用习惯是白天使用亮色主题，晚上使用暗色主题。也可以选择喜欢的第三方主题安装。我最喜欢的亮色主题是leuven(内置)，暗色主题是dracula(第三方)。

# 全部配置代码

我建议你按照教程的步骤，一步步拷贝、粘贴、执行代码。这个过程中，你会了解到如何从零配置一个功能强大的emacs编辑器，如何像搭积木一样通过添加配置文件使emacs充满无限的可能性。所有的配置代码我也会放在 [emacs-workflow-config](https://github.com/Kinneyzhang/emacs-workflow-config) 这个代码仓库，读者可以直接把它克隆到 `.emacs.d` 文件夹下使用。

# 结语

如何配置一个舒适易用的emacs环境是一个大话题，有很多非常nice的package，但考虑到这大多数是与“提高编程的体验”相关，并不是Emacs Workflow的重点。所以，这一篇中我只介绍一个最小配置和部分实用(必要)的package，更多的优化配置不多讲解。好啦，以上就是使用emacs前的准备工作，接下来就可以愉快的学习各种工作流啦！
