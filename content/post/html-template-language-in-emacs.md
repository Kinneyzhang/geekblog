---
title: 基于Emacs-Lisp的HTML模版库
date: 2020-05-10
draft: false
categories: ["Happy Hacking Emacs"]
tags: ["elisp", "html"]
comment: false
---

[English Document](https://github.com/Kinneyzhang/pp-html/blob/master/README.org)

# 介绍

[pp-html](https://github.com/Kinneyzhang/pp-html) 是使用elisp开发的html模版语言。它借鉴了部分 [Liquid template language](https://shopify.github.io/liquid/) 的设计思路，包括对象，标签和过滤器三部分。通过书写elisp的S表达式，读者可以快速便捷地构建简单html片段或复杂的html页面。其中 `:include` 和 `:extend` (包含和继承) 标签使得模块化构建html成为可能，提高了代码重用率。


# 安装

克隆此代码仓库:

    $ git clone https://github.com/Kinneyzhang/pp-html.git <path-to-pp-html>

安装相关依赖: [dash](https://github.com/magnars/dash.el), [s](https://github.com/magnars/s.el) 和 [web-mode](https://github.com/fxbois/web-mode)。

然后在emacs配置中添加如下两行:

    (add-to-list 'load-path "<path-to-pp-html>")
    (require 'pp-html)


# 使用


## 基础

pp-html使用elisp的S表达式来构建html代码，使用 `pp-html` 函数对S表达式求值。下面有一些简单的例子，便于理解基本的使用方法。


### 单个S表达式

单个S表达式的构成为: `(html标签 多个属性键值对 内容)` 。

其中html标签是必须的，其余可缺省。属性键值对的书写语法为Plist形式。特别地是，对CSS选择器设置了语法糖，用 "." 表示 "class"，"@" 表示 "id"，其值为符号后面的内容。对于无值属性(例如 async)有两种写法：1. (:async nil) 2. 直接写:async，要求不能是最后一个属性。

    (pp-html '(a "content"))
    (pp-html '(a @id .class))
    (pp-html '(a :id "id" :class "class"))
    (pp-html '(a .test :href "url" :target "_blank" "content"))
    (pp-html '(link :async :rel "stylesheet" :href "url" :async nil))

    <a>content</a>
    <a id="id" class="class"></a>
    <a id="id" class="class"></a>
    <a class="test" href="url" target="_blank">content</a>
    <link async rel="stylesheet" href "url" async/>


### 并列S表达式

并列的多个S表达式直接用括号包含。

    (pp-html
     '((div .div1 "div-content")
       (p "paragraph")
       (a :href "url" "a-content")
       (img :src "path")
       (ul .org-ul
           (li "1")
           (li "2")
           (li "3"))))

    <div class="div1">div-content</div>
    <p>paragraph</p>
    <a href="url">a-content</a>
    <img src="path"/>
    <ul class="org-ul">
      <li>1</li>
      <li>2</li>
      <li>3</li>
    </ul>


### 嵌套S表达式

在同一html标签的S表达式内代表该元素的子元素，否则为兄弟元素。

    (pp-html
     '(div .container
           (div .row
                (div .col-8
                     (p "paragraph 1"))
                (div .col-4
                     (p "paragraph 2")))))

    <div class="container">
      <div class="row">
        <div class="col-8">
          <p>paragraph 1</p>
        </div>
        <div class="col-4">
          <p>paragraph 2</p>
        </div>
      </div>
    </div>


## 对象

对象告诉pp-html在页面何处显示对象的值。它包括三类：变量求值、对象属性求值和函数求值。可以使用 `pp-html-eval` 函数获取对象的值。


### 变量求值

变量求值的基本语法是在变量前加上"$"符号。

    (let ((var1 "happy hacking emacs"))
      (pp-html-eval '$var1))

    happy hacking emacs

变量可应用于S表达式的任何部分。

    (let ((url "https://geekinney.com/")
          (name "戈楷旎"))
      (pp-html '(a :href $url $name)))

    <a href="https://geekinney.com/">戈楷旎</a>


### 对象属性求值

**特别地，对于Plist对象使用"."来获取属性值。**

    (let ((site '(:name "戈楷旎" :domain "geekinney.com" :author "Geekinney")))
      (pp-html '(div .site-info
                     (p $site.name)
                     (p $site.domain)
                     (p $site.author))))

    <div class="site-info">
      <p>戈楷旎</p>
      <p>geekinney.com</p>
      <p>Geekinney</p>
    </div>


### 函数求值

函数求值的S表达式语法为 ($ <function> <args&#x2026;>), 函数的参数也可写成变量形式。

    (let ((var1 "happy")
          (var2 " hacking"))
      (pp-html-eval '($ concat $var1 $var2 " emacs")))

    happy hacking emacs

函数可嵌套调用，或直接写，两种写法等价。

    (let ((var1 "now")
          (var2 " is ")
          (now '(current-time)))
      (pp-html-eval '($ concat ($ upcase $var1) $var2 ($ format-time-string "%Y-%m-%d" $now)))
      (pp-html-eval '($ concat (upcase $var1) $var2 (format-time-string "%Y-%m-%d" $now))))

    NOW is 2020-05-10
    NOW is 2020-05-10

同理，函数也可用于S表达式的任何部分，这样pp-html就可以任意使用elisp丰富强大的函数库了。


## 标签

标签为模版创造了逻辑和流程控制，它用冒号表示并且放在S表达式的第一个位置: (:tag &#x2026;)。标签分为5类：

-   变量定义
-   流程控制
-   迭代
-   代码块


### 变量定义

**assign**

定义变量，相当于elisp的let或setq。

    (pp-html
     '((:assign str1 "happy"
                str2 "hacking"
                str3 "emacs")
       (p ($ concat $str1 " " $str2 " " $str3))))

    <p>happy hacking emacs</p>


### 流程控制

**if**

如果条件为真执行第一个代码块，否则执行第二个

    (pp-html
     '((:assign bool nil)
       (:if $bool (p "true")
            (p "false"))))

    <p>false</p>

**unless**

和if相反，如果条件为假，执行第一个代码块，否则执行第二个。

    (pp-html
     '((:assign bool nil)
       (:unless $bool (p "true")
            (p "false"))))

    <p>true</p>

**cond**

执行每一个分支，直到条件满足，执行满足条件的代码块。

    (pp-html
     '((:assign case "case3")
       (:cond
        ($ string= $case "case1") (p "case1 branch")
        ($ string= $case "case2") (p "case2 branch")
        ($ string= $case "case3") (p "case3 branch")
        t (p "default branch"))))

    <p>case3 branch</p>


### 迭代

**for**

for循环

    (pp-html
     '((:assign editors ("vim" "emacs" "vscode"))
       (ul
        (:for editor in $editors
              (li :id $editor $editor)))))

    <ul>
      <li id="vim">vim</li>
      <li id="emacs">emacs</li>
      <li id="vscode">vscode</li>
    </ul>


### 代码块

**include**

在一个代码块中包含另一个代码块。

    (setq block1
          '(p "block1 content"
              (a :href "url" "content")))
    
    (setq block2
          '(div .block2
                (p "block2 content")
                (:include $block1)))
    
    (pp-html block2)

    <div class="block2">
      <p>block2 content</p>
      <p>
        block1 content
        <a href="url">content</a>
      </p>
    </div>

**extend** 和 **block**

代码块继承。如果新代码块重写了 `:block` 标签之间的内容，覆盖原代码块对应的部分，其余保持不变。

    (setq base-block '(p .base
                         (:block block-name (span "base content")))
          extend-block1 '(:extend $base-block
                                  (:block block-name))
          extend-block2 '(:extend $base-block
                                  (:block block-name
                                          (span "extended content"))))
    (pp-html
     '((div "extend the default"
            (:include $extend-block1))
       (div "extend with new"
            (:include $extend-block2))))

    <div>
      extend the default
      <p class="base">
        <span>base content</span>
      </p>
    </div>
    <div>
      extend with new
      <p class="base">
        <span>extended content</span>
      </p>
    </div>


## 过滤器

过滤器的语法形式为 (/ <value> <:filter args> &#x2026;)。过滤器作用于<value>，可以有参数，也可以没有。


### 自定义过滤器

pp-html支持自定义过滤器，使用 `pp-html-define-filter` 函数，它有两个参数：过滤器名称和过滤函数。例：

    (pp-html-define-filter :add 'pp-html-filter-add)
    (defun pp-html-filter-add (value arg)
      "Add a value to a number"
      (let ((arg (if (stringp arg)
                     (string-to-number arg)
                   arg)))
        (+ value arg)))


### 内置过滤器

**abs**: 取绝对值

    (pp-html-eval '(/ -5 :abs)) ;; => 5

**add**: 加上一个数

    (pp-html-eval '(/ 4 :add 5)) ;; => 9

**append**: 结合两个列表

    (let ((list1 '(1 2 3))
          (list2 '(5 6 7)))
      (pp-html-eval '(/ $list1 :append $list2))) ;; => (1 2 3 5 6 7)

**capitalize**: 第一个单词首字母大写

    (pp-html-eval '(/ "happy hacking emacs!" :capitalize)) ;; => Happy hacking emacs!

**compact**: 删除列表中所有的nil

    (let ((lst '(nil 1 2 nil 3 4 nil)))
      (pp-html-eval '(/ $lst :compact))) ;; => (1 2 3 4)

**concat**: 字符串连接

    (let ((str1 "happy hacking ")
          (str2 "emacs"))
      (pp-html-eval '(/ $str1 :concat $str2))) ;; => happy hacking emacs

**default**: 不是nil或空字符串，设为默认值

    (let ((str1 "")
          (str2 "new value")
          (lst1 '(1 2 3))
          (lst2 nil))
      (pp-html-eval '(/ $str1 :default "default value")) ;; => default value
      (pp-html-eval '(/ $str2 :default "default value")) ;; => new value
      (pp-html-eval '(/ $lst1 :default (4 5 6))) ;; => (1 2 3)
      (pp-html-eval '(/ $lst2 :default (4 5 6))) ;; => (4 5 6)
      )

**escape**: html特殊字符转义

    (pp-html-eval '(/ "Have you read 'James & the Giant Peach'?" :escape)) ;; => Have you read &apos;James &amp; the Giant Peach&apos;?

**join**: 使用分隔符连接列表中字符串

    (let ((lst '("happy" "hacking" "emacs")))
      (pp-html-eval '(/ $lst :join "-"))) ;; => happy-hacking-emacs

&#x2026; **More useful filters are on the way!**


## 综合

综合以上语法的例子: 

    (setq assign-vars
          '(:assign name "geekinney blog"
                    description "Emacs is a lifestyle :-) And happy hacking emacs!"
                    menus ((:path "/" :name "Index")
                           (:path "/archive" :name "Archive")
                           (:path "/category" :name "Category")
                           (:path "/about" :name "About"))
                    comment-p t
                    comment-type "disqus"
                    valine-block (p "this is valine block")
                    disqus-block (p "this is disqus block")))
    (setq header-block
          '(header @topheader
                   (a @logo :href "/" $name)
                   (p .description $description)))
    
    (setq menu-block
          '(nav @topmenu
                (:for menu in $menus
                      (a :href $menu.path $menu.name))))
    
    (setq article-block
          '(article
            (p ($ concat "Function: the site name is " ($ upcase $name)))
            (p (/ "Filter: the site name is " :concat (/ $name :capitalize)))
            (p (/ ("happy" "hacking" "emacs") :join " " :capitalize :concat "!"))))
    
    (setq comment-block
          '(div @comment
                (:if comment-p
                     (:cond
                      ($ string= $comment-type "valine") (:include $valine-block)
                      ($ string= $comment-type "disqus") (:include $disqus-block)
                      t nil)
                     (p "The comment is closed!"))))
    
    (setq side-block
          '(aside @sidebar
                  (:block side-block
                          (p "this is base sidebar"))))
    
    (setq footer-block
          '(:block footer-block
                   (footer
                    (p "this is base footer."))))
    
    (setq base-block
          '((:include $assign-vars)
            (body
             (div .container
                  (div .row
                       (div .col-12
                            (:include $header-block)))
                  (div .row
                       (div .col-12
                            (:include $menu-block)))
                  (div .row
                       (div .col-12 .col-sm-12 .col-md-8 .col-lg-8
                            (:include $article-block)
                            (:include $comment-block))
                       (div .col-md-4 .col-lg-4
                            (:include $side-block)))
                  (div .row
                       (div .col-12
                            (:include $footer-block)))))))
    
    (pp-html
     '(:extend $base-block
               (:block side-block
                       (p "this is extended sidebar"))
               (:block footer-block)))

    <body>
      <div class="container">
        <div class="row">
          <div class="col-12">
            <header id="topheader">
              <a id="logo" href="/">geekinney blog</a>
              <p class="description">Emacs is a lifestyle :-) And happy hacking emacs!</p>
            </header>
          </div>
        </div>
        <div class="row">
          <div class="col-12">
            <nav id="topmenu">
              <a href="/">Index</a>
              <a href="/archive">Archive</a>
              <a href="/category">Category</a>
              <a href="/about">About</a>
            </nav>
          </div>
        </div>
        <div class="row">
          <div class="col-12 col-sm-12 col-md-8 col-lg-8">
            <article>
              <p>Function: the site name is GEEKINNEY BLOG</p>
              <p>Filter: the site name is Geekinney blog</p>
              <p>Happy hacking emacs!</p>
            </article>
            <div id="comment">
              <p>this is disqus block</p>
            </div>
          </div>
          <div class="col-md-4 col-lg-4">
            <aside id="sidebar">
              <p>this is extended sidebar</p>
            </aside>
          </div>
        </div>
        <div class="row">
          <div class="col-12">
            <footer>
              <p>this is base footer.</p>
            </footer>
          </div>
        </div>
      </div>
    </body>


# 说明


## 预览调试

`pp-html-test` 函数可以在view buffer中预览生成的格式化html。 `pp-html-parse` 函数可以查看解析完所有逻辑标签后的S表达式。这两个函数便于调试代码。


## XML支持

pp-html还额外支持生成xml。与html不同，xml没有单元素(img,link&#x2026;)，所以更简单。使用方法为设置 `pp-html` 函数的第二个参数为t。


## 结合OrgMode

在Org文件中，使用带参数的emacs-lisp代码块可以在Org或HTML中生成elisp代码对应的HTML。例如：

1.当导出Org文件时，生成一个有红色背景div的html页面。

    #+BEGIN_SRC emacs-lisp :results value html :exports results
    (pp-html '(div :style "background-color:red;" "content"))
    #+END_SRC
    
    #+RESULTS:
    #+begin_export html
    <div style="background-color:red;">content</div>
    #+end_export

2.当导出Org文件时，生成包含 `<div style="background-color:red;">content</div>` 代码的html页面。

    #+BEGIN_SRC emacs-lisp :wrap src html :exports results
    (pp-html '(div :style "background-color:red;" "content"))
    #+END_SRC
    
    #+RESULTS:
    #+begin_src html
    <div style="background-color:red;">content</div>
    #+end_src

关于OrgMode导出代码块的参数设置参考 [Working-with-Source-Code](https://orgmode.org/org.html#Working-with-Source-Code) 。


## 构建博客

我的 [个人博客](https://geekinney.com/) 就是基于 `pp-html` 构建的，我将构建博客的代码组织成了emacs包: `geekblog` ，目前处理代码优化整理阶段，敬请关注 [我的Github](https://github.com/Kinneyzhang) 或博客。

# 鸣谢

pp-html是我写的第一个emacs包。由于是新手，开发过程断断续续持续了一个多月的时间，其间遇到了许多的技术难题。特别感谢 [Emacs-China社区](https://emacs-china.org) 的同学们答疑解惑。

此package可能有不成熟的地方，希望读者诸君、emacs大牛批评指正。关于package功能的拓展和集成，也可以给我提建议(issue或博客留言)。

如果你觉得我的工作对你有所帮助，欢迎 [Star](https://github.com/Kinneyzhang/pp-html) 此代码仓库！
