---
title: "Emacs Lisp 编程总结"
date: 2019-11-12
draft: false
categories: ["Happy Hacking Emacs"]
tags: ["elisp", "学习笔记"]
comment: false
---

# lisp介绍

Lisp（历史上拼写为LISP）是具有悠久历史的计算机编程语言家族，有独特和完全括号的前缀符号表示法。起源于公元1958年，是现今第二悠久而仍广泛使用的高端编程语言。只有FORTRAN编程语言比它更早一年。Lisp编程语族已经演变出许多种方言。现代最著名的通用编程语种是Clojure、Common Lisp和Scheme。

Lisp最初创建时受到阿隆佐·邱奇的lambda演算的影响，用来作为计算机程序实用的数学表达。因为是早期的高端编程语言之一，它很快成为人工智能研究中最受欢迎的编程语言。在计算机科学领域，Lisp开创了许多先驱概念，包括：树结构、自动存储器管理、动态类型、条件表达式、高端函数、递归、自主（self-hosting）编译器、读取﹣求值﹣输出循环（英语：Read-Eval-Print Loop，REPL）。

"LISP"名称源自“列表处理器”（英语：LISt Processor）的缩写。列表是Lisp的主要数据结构之一，Lisp编程代码也同样由列表组成。因此，Lisp程序可以把源代码当作数据结构进行操作，而使用其中的宏系统，开发人员可将自己定义的新语法或领域专用的语言，嵌入在Lisp编程中。

代码和数据的可互换性为Lisp提供了立即可识别的语法。所有的Lisp程序代码都写为S-表达式或以括号表示的列表。函数调用或语义形式也同样写成列表，首先是函数或操作符的名称，然后接着是一或多个参数：例如，取三个参数的函数f即为（f arg1 arg2 arg3）。

Lisp语言的主要现代版本包括Common Lisp, Scheme，Racket以及Clojure。1980年代盖伊·史提尔二世编写了Common Lisp试图进行标准化，这个标准被大多数解释器和编译器所接受。还有一种是编辑器Emacs所派生出来的Emacs Lisp（而Emacs正是用Lisp作为扩展语言进行功能扩展）非常流行，并创建了自己的标准。


# Elisp


## 概览


### 运行emacs-lisp的几种方式

<table>


<colgroup>
<col  class="org-left">

<col  class="org-left">

<col  class="org-left">
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">key</th>
<th scope="col" class="org-left">command</th>
<th scope="col" class="org-left">description</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">`C-x C-e`</td>
<td class="org-left">`eval-last-sexp`</td>
<td class="org-left">在S表达式结尾运行，在minibuffer显示结果</td>
</tr>


<tr>
<td class="org-left">`C-j`</td>
<td class="org-left">`eval-print-last-sexp`</td>
<td class="org-left">在S表达式结尾运行，打印运行结果</td>
</tr>


<tr>
<td class="org-left">`M-:`</td>
<td class="org-left">`eval-expression`</td>
<td class="org-left">在minibuffer输入命令并执行</td>
</tr>


<tr>
<td class="org-left">`M-x ielm`</td>
<td class="org-left">`ielm`</td>
<td class="org-left">使用IELM解释器运行代码</td>
</tr>
</tbody>
</table>


### 创建命令（interactive函数）

    ;; example
    (defun buffer/insert-filename ()
      "Insert file path of current buffer at current point"
      (interactive)
      (insert (buffer-file-name (current-buffer))))


### emacs探索

<table>


<colgroup>
<col  class="org-left">

<col  class="org-left">

<col  class="org-left">
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">key</th>
<th scope="col" class="org-left">command</th>
<th scope="col" class="org-left">description</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">`C-h k`</td>
<td class="org-left">`describe-key`</td>
<td class="org-left">运行命令后，继续按键，查看此时按键绑定的函数</td>
</tr>


<tr>
<td class="org-left">`C-h b`</td>
<td class="org-left">`describe-bindings`</td>
<td class="org-left">在\*Help\*界面搜索 `Major Mode Bindings:` 可以查看所有与当前major mode相关的按键。</td>
</tr>


<tr>
<td class="org-left">`C-h f`</td>
<td class="org-left">`describe-function`</td>
<td class="org-left">查看函数文档及详细代码</td>
</tr>
</tbody>
</table>


## elisp编程的基本设置

三个有用的pcakage:

-   rainbow-delimiters: 不同颜色区分不同层级的括号
-   paredit: 检查括号匹配
-   company: elisp代码补全


## 基本运算


### 算术

    ELISP> (+ 20 30)
    50
    ELISP> (- 100 80)
    20
    ELISP> (+ 1 2 3 4 5 6)
    21
    ELISP> (* 1 2 3 4 5 6)
    720
    ELISP> (/ 1 100)
    0
    
    ELISP> (> 10 1) ;; ?? 10 > 1
    t
    ELISP> (< 2 8) ;; ?? 2 < 8
    t
    ELISP> (< 8 2) ;; ?? 8 < 2
    nil
    
    ELISP> (= 2 2)
    t
    ELISP> (= 2 4)
    nil
    
    ELISP> (/= 2 2)
    nil
    ELISP> (exp -1)
    0.36787944117144233
    ELISP> (log 10)
    2.302585092994046
    ELISP> (sin pi)
    1.2246467991473532e-16
    ELISP> (cos pi)
    -1.0
    ELISP> (tan (/ pi 2))
    1.633123935319537e+16
    ELISP>


### 比较

    ;;;; Compare Numbers
    ELISP> (= 2 (+ 1 1))
    t
    
    ;;; Compare Symbols and Numbers
    ELISP> (eq 1 1)
    t
    ELISP> (eq 1 2)
    nil
    ELISP>
    
    ELISP> (eq 'x 'x)
    t
    ELISP>
    
    ;;; Compare Elements of a List
    ELISP> (equal (list 1 2 3 4) (list 1 2 3 4))
    t
    
    ;;; Compare Strings
    ELISP> (string= "hello" "hello")
    t


### 列表

    ELISP> '(10 20 30 40)
    (10 20 30 40)
    
    ELISP> '(10 203 40 "hello" () ("empty" 65))
    (10 203 40 "hello" nil
        ("empty" 65))


## 类型判断和Literals


### Emacs Literals

    ;;; Numbers
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ELISP> 1e3
    1000.0
    
    ;;; String
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ELISP> "Hello World Emacs Literals"
    "Hello World Emacs Literals"
    ELISP>
    
    ;;; Symbol
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ELISP> 'this-a-symbol
    this-a-symbol
    
    ELISP> 'vector->list
    vector->list
    
    ELISP> 'symbol?
    symbol\?
    ELISP>
    
    ;; Boolean t and nil
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ELISP> t
    t
    ELISP> nil
    nil
    ELISP>
    
     ;;; Everything that is not "nil" is true:
     ;;-----------------------------------------
    ELISP> (if t "It is true (not nil)" "It is false (it is nil)")
    "It is true (not nil)"
    ELISP>
    ELISP> (if 100e3 "It is true (not nil)" "It is false (it is nil)")
    "It is true (not nil)"
    ELISP> (if '(a b c d)  "It is true (not nil)" "It is false (it is nil)")
    "It is true (not nil)"
    ELISP>
    
    ELISP> (if nil  "It is true (not nil)" "It is false (it is nil)")
    "It is false (it is nil)"
    ELISP>
    
    ;;; Pair / Cons Cell
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ELISP> '(a . b)
    (a . b)
    
    ELISP> '(a . 2999)
    (a . 2999)
    
    ;;; List
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ELISP> '(1 2 3 (3 4) (5 6 (+ 3 4)) 10 'a 'b "hello" )
    (1 2 3
       (3 4)
       (5 6
          (+ 3 4))
       10 'a 'b "hello")
    
    ELISP> '(+ 1 2 3 4 5)
    (+ 1 2 3 4 5)
    
    ELISP> '(cos 10)
    (cos 10)
    
    ;;; Vectors
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ELISP> [1 2 3 4 (+ 1 2 3 54)]
    [1 2 3 4
       (+ 1 2 3 54)]


### 基本类型判断

<table>


<colgroup>
<col  class="org-left">

<col  class="org-left">

<col  class="org-left">

<col  class="org-left">
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Type</th>
<th scope="col" class="org-left">Predicate</th>
<th scope="col" class="org-left">Literal</th>
<th scope="col" class="org-left">Description</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">Nil</td>
<td class="org-left">null</td>
<td class="org-left">nil '()</td>
<td class="org-left">Test if argument is nil</td>
</tr>


<tr>
<td class="org-left">Numbers</td>
<td class="org-left">numberp</td>
<td class="org-left">100, 200e3</td>
<td class="org-left">Test if it is number.</td>
</tr>


<tr>
<td class="org-left">String</td>
<td class="org-left">stringp</td>
<td class="org-left">"hello"</td>
<td class="org-left">Test if it is string</td>
</tr>


<tr>
<td class="org-left">Symbol</td>
<td class="org-left">symbolp</td>
<td class="org-left">'sym :keyworkd</td>
<td class="org-left">Test if it is a symbol.</td>
</tr>


<tr>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">Atom</td>
<td class="org-left">atom</td>
<td class="org-left">'x "h" :key 200</td>
<td class="org-left">Everything that is not a list or pair is an atom.</td>
</tr>


<tr>
<td class="org-left">List</td>
<td class="org-left">listp</td>
<td class="org-left">'(1 2 x y)</td>
<td class="org-left">Test if it is a list</td>
</tr>


<tr>
<td class="org-left">Pair</td>
<td class="org-left">consp</td>
<td class="org-left">'(a . 200)</td>
<td class="org-left">Test if it is a pair (cons cell)</td>
</tr>


<tr>
<td class="org-left">Vector</td>
<td class="org-left">vectorp</td>
<td class="org-left">[1 200 'sym]</td>
<td class="org-left">Test if it is a vector</td>
</tr>
</tbody>
</table>

<table>


<colgroup>
<col  class="org-left">

<col  class="org-left">
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Object</th>
<th scope="col" class="org-left">Predicate</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">Buffer</td>
<td class="org-left">bufferp</td>
</tr>


<tr>
<td class="org-left">Window</td>
<td class="org-left">windowp</td>
</tr>


<tr>
<td class="org-left">Frame</td>
<td class="org-left">framep</td>
</tr>


<tr>
<td class="org-left">Process</td>
<td class="org-left">processp</td>
</tr>
</tbody>
</table>

    ELISP> (null nil)
    t
    ELISP>
    ELISP> (null '())
    t
    
    ELISP> (null 10)
    nil
    
    ELISP> (atom 10)
    t
    ELISP> (atom '(a . b))
    nil
    ELISP> (atom "hello world")
    t
    ELISP>
    
    ELISP> (bufferp (current-buffer))
    t
    ELISP> (bufferp (selected-window))
    nil
    ELISP> (windowp (selected-window))
    t
    ELISP>


### 获取对象类型

    ELISP> (type-of (current-buffer))
    buffer
    ELISP>
    ELISP> (type-of (selected-window))
    window
    ELISP>
    
    ELISP> (equal 'buffer (type-of (current-buffer)))
    t
    ELISP> (equal 'buffer (type-of (selected-window)))
    nil
    ELISP>


## 变量定义

    ;;; Constants
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ELISP> (defconst zsh-shell "/usr/bin/zsh")
    zsh-shell
    
    ELISP> zsh-shell
    "/usr/bin/zsh"
    ELISP>
    
    ;;; Define a variable
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;;;; Set is not used very much
    ;;
    ELISP> (set 'avar "hello world")
    "hello world"
    
    ELISP> avar
    "hello world"
    ELISP>
    
    ;;;;; The most used command for assignment is setq
    ;;
    ELISP> (setq x 10)
    10
    
    ELISP> (setq avar "hello world")
    "hello world"
    
    ELISP> x
    10
    
    ELISP> avar
    "hello world"
    ELISP>
    
    ELISP> (setq my-list '(10 20 30 40))
    (10 20 30 40)
    
    ELISP> my-list
    (10 20 30 40)
    
    ;;; Multiple Assignment
    ;;
    ELISP> (setq a 10 b 20 c "Emacs")
    "Emacs"
    ELISP> a
    10
    ELISP> b
    20
    ELISP> c
    "Emacs"
    ELISP>
    
    ;; Dynamic Scoping  (Local Variables)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ELISP> (let ((x 1) (y 10)) (+ (* 4 x) (* 5 y)) )
    54
    ELISP> x
        ** Eval error **  Symbol's value as variable is void: x
    ELISP> y
        ** Eval error **  Symbol's value as variable is void: y
    ELISP>


## 函数定义


### 定义简单函数

语法: (defun <function name> (<parameters>) (<body>))

    ELISP> (defun afunction (a b c) (+ a b c))
    afunction
    
    ELISP> (afunction 10 20 30)
    60
    
    ELISP> (defun myfun () (message "Hello Emacs"))
    myfun
    ELISP> (myfun)
    "Hello Emacs"
    ELISP>
    
    
    ELISP>
    ELISP> (defun signum (n)
         (cond ((> n 0) 1 )
    	   ((< n 0) -1)
    	   (0)))
    signum
    ELISP> (signum 10)
    1
    ELISP> (signum 0)
    0
    ELISP> (signum -23)
    -1
    ELISP>
    
    
    ELISP> (defun factorial (n)
         (if (= n 0)
    	 1
    	 (* n (factorial (- n 1)))))
    factorial
    
    ELISP> (factorial 5)
    120
    ELISP


### 匿名函数/Lambda函数

语法: (lambda (<parameters>) (<body>))

    ELISP> (lambda (x) (+ x 3))
    (lambda
      (x)
      (+ x 3))
    
    ;;; Applying Lambda Functions
    ;;
    
    ELISP> ((lambda (x) (+ x 3)) 4)
    7
    ELISP> (funcall (lambda (x) (+ x 3)) 4)
    7
    ELISP>
    
    ;;; Storing Lambda Function in Variable
    ;;
    ;;
    
    ELISP> (defvar add3 (lambda (x) (+ x 3)))
    add3
    
    
    ELISP> add3
    (lambda
      (x)
      (+ x 3))
    
    ELISP> (funcall add3 10)
    13
    
    ELISP> (add3 10)
        ** Eval error **  Symbol's function definition is void: add3
    
    ELISP> (funcall #'add3 10)
        ** Eval error **  Symbol's function definition is void: add3
    ELISP>
    
    ;;; Passing Lambda Function to functions
    ;;
    ELISP> (mapcar (lambda (x) (+ x 3))  '(1 2 3 4 5))
    (4 5 6 7 8)


### 函数作为参数

语法: (caller-function #'<function-1> #'<function-1> arg1 arg2 &#x2026;)

在函数内部，使用 `funcall` 调用函数作为参数

    ELISP> (mapcar log '(1 10 100 1000))
        ** Eval error **  Symbol's value as variable is void: log
    
    
    ELISP> (mapcar #'log10 '(1 10 100 1000))
    (0.0 1.0 2.0 3.0)
    
    (defun sum-fun (f1 f2 x)
      (+ (funcall f1 x) (funcall f2 x)))
    
    ELISP> (sum-fun #'log #'exp 3)
    21.18414921185578
    ELISP>
    
    ELISP> (+ (log 3) (exp 3))
    21.18414921185578
    ELISP>
    
    ELISP> (sum-fun (lambda (x) (* 3 x))
    	(lambda (x) (* 4 x))
    	5)
    35
    ELISP>
    
    ELISP> (defun 1+ (x) (+ 1 x))
    1+
    ELISP> (defun 3* (x) (* 3 x))
    3*
    
    ELISP> (sum-fun #'1+  #'3* 4)
    17
    ELISP>
    
    ELISP> (sum-fun #'1+  (lambda (x) (* 3 x)) 4)
    17
    ELISP>


### 多参函数

    (defun sum (&rest numbers)
      (apply #'+ numbers))
    
    ELISP> (sum 1 2 3 4 5 6)
    21
    
    
    ELISP> (apply #'sum '(1 2 3 5 6))
    17
    
    ELISP> (apply #'sum (list 1 2 3 5 (+ 6 5 2)))
    24
    
    ELISP> (apply #'sum '())
    0
    
    ELISP> (apply #'sum nil)
    0
    
    ELISP> (sum nil)
        ** Eval error **  Wrong type argument: number-or-marker-p, ni
    
    ;;----------------------------------
    
    (defun sum-prod (a &rest xs)
      (* a (apply #'+ xs)))
    
    
    ELISP> (sum-prod 3 1 2 3 4 5)
    45
    
    ELISP> (sum-prod 1 1 2 3 4 5)
    15


### 可选参数函数

    (defun test-optional (a &optional b)
      (list a b))
    
    ELISP> (test-optional 10 20)
    (10 20)
    
    ELISP> (test-optional 10 )
    (10 nil)
    
    ;--------------------------------;
    
    (defun test-optional2 (a b &optional b c d e)
      (list :a a :b b :c c :d d :e e))
    
    ELISP> (test-optional2 0 1 2 3 4 5 )
    (:a 0 :b 2 :c 3 :d 4 :e 5)
    
    
    ELISP> (test-optional2 0 1 2 3 4  )
    (:a 0 :b 2 :c 3 :d 4 :e nil)
    
    ELISP> (test-optional2 0 1 2 3   )
    (:a 0 :b 2 :c 3 :d nil :e nil)
    
    ELISP> (test-optional2 0 1 2    )
    (:a 0 :b 2 :c nil :d nil :e nil)
    
    ELISP> (test-optional2 0 1  )
    (:a 0 :b nil :c nil :d nil :e nil)
    
    ELISP> (test-optional2 0 1)
    (:a 0 :b nil :c nil :d nil :e nil)
    
    ;--------------------------------;
    
    (defun test-optional-default-b (a &optional b)
      (if b
          (list a b)
          (list a "b is null")))
    
    ELISP> (test-optional-default-b 1 2)
    (1 2)
    
    ELISP> (test-optional-default-b 1)
    (1 "b is null")
    
    ELISP> (test-optional-default-b 1 nil)
    (1 "b is null")


### 含属性列表参数函数

    (defun make-shell-interface (&rest params)
      "
      Create a shell interface.
    
      Possible parameters:
    
        :name      Name of shell
        :type      ['sh, 'bash, ...]
        :path      Path to program
        :buffer    Name of buffer
    
      "
      (let
           ((name   (plist-get params :name ))
    	(type   (plist-get params :type))
    	(path   (plist-get params :path))
    	(buffer (plist-get params :buffer)))
        (list
         (cons 'name buffer)
         (cons 'type type)
         (cons 'path path)
         (cons 'buffer buffer))))
    
    
    ELISP> (make-shell-interface :name "pylaucher" :path "/usr/bin/python" :type 'sh :buffer "pyshell")
    ((name . "pyshell")
     (type . sh)
     (path . "/usr/bin/python")
     (buffer . "pyshell"))
    
    ELISP> (make-shell-interface :name "pylaucher" :path "/usr/bin/python" :type 'sh)
    ((name)
     (type . sh)
     (path . "/usr/bin/python")
     (buffer))
    
    ELISP> (make-shell-interface :name "pylaucher" :path "/usr/bin/python" :type 'bash)
    ((name)
     (type . bash)
     (path . "/usr/bin/python")
     (buffer))
    
    ELISP> (make-shell-interface :name "pylaucher" :path "/usr/bin/python")
    ((name)
     (type)
     (path . "/usr/bin/python")
     (buffer))
    
    ELISP> (make-shell-interface :name "pylaucher" )
    ((name)
     (type)
     (path)
     (buffer))
    
    ELISP> (make-shell-interface  )
    ((name)
     (type)
     (path)
     (buffer))
    
    ELISP> (make-shell-interface :buffer "pyshell"  :path "/usr/bin/python" :type 'sh :name "pylaucher")
    ((name . "pyshell")
     (type . sh)
     (path . "/usr/bin/python")
     (buffer . "pyshell"))


### Closures

elisp方言默认不支持closure，所以下面的代码不会像Scheme或Common Lisp一样执行。

参考：

-   [EmacsWiki: Lexical Binding](https://www.emacswiki.org/emacs/LexicalBinding)
-   [EmacsWiki: Dynamic Binding Vs Lexical Binding](https://www.emacswiki.org/emacs/DynamicBindingVsLexicalBinding)
-   [Emacs Lisp Readable Closures « null program](https://nullprogram.com/blog/2013/12/30/)
-   [https://www.jamesporter.me/2013/06/14/emacs-lisp-closures-exposed.html](https://www.jamesporter.me/2013/06/14/emacs-lisp-closures-exposed.html)
-   [Technical Dresese: A brief demonstration of emacs new lexical bindings](http://technical-dresese.blogspot.com/2011/04/brief-demonstration-of-emacs-new.html)

        (defun make-adder (x)
              (lambda (y) (+ x y)))
            
            
            ELISP>
            ELISP> (make-adder 3)
            (lambda
              (y)
              (+ x y))
            
            ELISP> ((make-adder 3) 4)
                ** Eval error **  Invalid function: (make-adder 3)
            ELISP> (funcall (make-adder 3) 4)
                ** Eval error **  Symbol's value as variable is void: x
            ELISP> (map (make-adder 3) '(1 2 3 4 5))
                ** Eval error **  Symbol's value as variable is void: x
            ELISP>

支持closure的代码：

    (setq lexical-binding t)
    
    (defun make-adder (x)
      (lambda (y) (+ x y)))
    
    ELISP> (make-adder 3)
    (closure
     ((x . 3)
      t)
     (y)
     (+ x y))
    
    ELISP> ((make-adder 3) 4)
        ** Eval error **  Invalid function: (make-adder 3)
    ELISP>
    
    ELISP> (funcall (make-adder 3) 4)
    7
    ELISP>
    
    ELISP> (mapcar (make-adder 3) '(1 2 3 4 5))
    (4 5 6 7 8)
    
    
    ;;;; Sometimes is better to create macro rather than a higher order function
    
    
    (defmacro make-sum-fun (f1 f2)
      `(lambda (x) (+ (,f1 x) (,f2 x))))
    
    ELISP>
    ELISP> (funcall (make-sum-fun sin cos) 3)
    -0.8488724885405782
    ELISP>
    ELISP> (make-sum-fun sin cos)
    (closure
     (t)
     (x)
     (+
      (sin x)
      (cos x)))
    
    ELISP> (map (make-sum-fun sin cos) '(1 2 3 4 5))
    (1.3817732906760363 0.4931505902785393 -0.8488724885405782 -1.4104461161715403 -0.6752620891999122)

在 `~/.emacs.d/init.el` 中添加如下配置以支持closure.

    (setq lexical-binding t)


## 列表操作

参考：

-   <https://www.fincher.org/tips/Languages/Emacs.shtml>

        ;; Defining a List
            ;;
            ;; An emacs list can contain elements of almost any type.
            ;;
            ELISP> '( "a" 2323 "b" 21.2323 "hello" "emacs" nil () (34 134) '(+ 2 3 5))
            ("a" 2323 "b" 21.2323 "hello" "emacs" nil nil
             (34 134)
             '(+ 2 3 5))
            
            ELISP> (quote (1 3 3 4 5))
            (1 3 3 4 5)
            
            ;;;;; Empty List
            ;;
            ELISP> nil
            nil
            ELISP> '()
            nil
            ELISP>
            
            ;; Length of a list
            ELISP> (length '(1 2 3 4 5 6))
            6
            ELISP>
            
            
            ;; nth element of a list
            ;;
            ELISP> (nth 0 '(0 1 2 3 4 5))
            0
            ELISP> (nth 2 '(0 1 2 3 4 5))
            2
            ELISP> (nth 5 '(0 1 2 3 4 5))
            5
            ELISP> (nth 10 '(0 1 2 3 4 5))
            nil
            ELISP>
            
            
            ;; Membership test
            ;; member returns null if the element is not member of the list
            ;;
            ELISP> (member 2 '(0 1 2 3 4 5))
            (2 3 4 5)
            
            ELISP> (member 10 '(0 1 2 3 4 5))
            nil
            ELISP>
            
            ;; Position of list element (prior to emacs 24.4)
            ;;
            ELISP> (position 7 '(5 6 7 8))
            2
            
            ELISP> (position 17 '(5 6 7 8))
            nil
            ELISP>
            
            ;; Position of list element (emacs 24.4 or later)
            ;;
            ELISP> (cl-position 7 '(5 6 7 8))
            2
            
            ELISP> (cl-position 17 '(5 6 7 8))
            nil
            ELISP>
            
            ;; cdr
            ;;
            ;; Removes first element of the list, returns the list tail.
            ;;
            ELISP> (cdr '(1 2 3 4 5))
            (2 3 4 5)
            
            ;; car
            ;;
            ;; Returns the first list element
            ;;
            ELISP> (car '(1 2 3 4 5))
            1
            ELISP>
            
            
            ;; cons
            ;;
            ;; List constructor
            ;;
            ELISP> (cons 10 '(1 2 3 4))
            (10 1 2 3 4)
            
            ELISP> (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 '())))))
            (1 2 3 4 5)
            
            ;; Last element of a list
            ;;
            ;;
            ELISP> (car (last '(1 2 3 4 5)))
            5
            ELISP>
            
            
            ;; Reverse a list
            ;;
            ELISP> (reverse '(1 2 3 4 5))
            (5 4 3 2 1)
            
            
            ;; Append lists
            ;;
            ;; Note: nil also means an empty list
            ;;
            ELISP> (append '(1 2) '( "a" "b" "c" "d"))
            (1 2 "a" "b" "c" "d")
            
            ELISP> (append '(1 2) nil '( "a" "b" "c" "d") nil)
            (1 2 "a" "b" "c" "d")
            
            
            
            ;; Filter list elements given a predicate function
            ;;
            ;;
            ELISP> (remove-if-not (lambda (x) (> x 2)) '(1 2 3 4 5 6 7 8 9 10))
            (3 4 5 6 7 8 9 10)
            
            ;; Test if list is empty
            ;;
            ELISP> (null '(1 2 3 4 5))
            nil
            ELISP> (null '())
            t
            ELISP> (null nil)
            t
            ELISP>
            
            ;; Drop the firsts n elements of a list
            ;;
            ;;
            ELISP> (nthcdr 2 '(1 2 3 4))
            (3 4)
            
            ELISP> (nthcdr 3 '(1 2 3 4))
            (4)
            
            ELISP> (nthcdr 13 '(1 2 3 4))
            nil
            ELISP>
            
            ;; Delete an element of a list
            ;;
            ;;
            ELISP> (delq 1 '(1 2 3 4))
            (2 3 4)
            
            
            ELISP> (delq 10 '(1 2 3 4))
            (1 2 3 4)
            
            ;; It doesn't work to delete sublists
            ;;
            ELISP> (delq (5) '(1 2 (5) 3 4))
                ** Eval error **  Invalid function: 5
            ELISP> (delq '(5) '(1 2 (5) 3 4))
            (1 2
               (5)
               3 4)
            
            ELISP> (delete '(5) '(1 2 (5) 3 4))
            (1 2 3 4)
            
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            
            ;; Convert Vector to List
            ;;
            ;;
            ELISP> (coerce [1 2 3] 'list)
            (1 2 3)
            
            ;; Convert List to Vector
            ;;
            ELISP> (coerce '(1 2 3) 'vector)
            [1 2 3]
            
            ELISP> (number-sequence 0 10 2)
            (0 2 4 6 8 10)
            
            ELISP> (number-sequence 9 4 -1)
            (9 8 7 6 5 4)
            
            
            ;; Modify list variables.
            ;;
            ELISP> alist
            (a b c d e)
            
            ELISP> (push 'f alist)
            (f a b c d e)
            
            ELISP> alist
            (f a b c d e)
            
            ELISP> (pop alist)
            f
            
            ELISP> alist
            (a b c d e)
            
            ELISP> (pop alist)
            a
            ELISP> alist
            (b c d e)
            
            ELISP>


## 关联列表和属性列表


### 概览

关联列表是一系列cons对，这里我可以称作 `clist` 或者 由两个元素组成的列表的集合，可以称为 `alist`

**关联列表类型：clist**

键: a, x, 2 and 4 值: b, y, 3 and (1 2 3 4 5)

    ELISP> '((a . b) (x . y) (2 . 3) (4 . (1 2 3 4 5)))
    ((a . b)
     (x . y)
     (2 . 3)
     (4 1 2 3 4 5)
    
    ELISP> (cons 'a 'b)
    (a . b)
    
    ELISP> (cons 'a (cons 'b (cons 'c nil)))
    (a b c)

**关联列表类型：alist**

    ELISP> '((a  b) (x  y) (2  3) (4  (1 2 3 4 5)))
    ((a b)
     (x y)
     (2 3)
     (4
      (1 2 3 4 5)))
    
    ELISP> (list (list 'a 'b) (list 'x 'y) (list 2 3) (list 2 '(1 2 3 4 5)))
    ((a b)
     (x y)
     (2 3)
     (2
      (1 2 3 4 5)))

`alist` 不像 `clist` 有歧义。

**属性列表：Plist**

属性列表是连续的键值对集合，它的优势是括号少和可读性高。

    '(:key1 value1 :key2 value2 :key3 1002.23 :key4 (a b c d e))
    
    ELISP> '(:key1 value1 :key2 value2 :key3 1002.23 :key4 (a b c d e))
    (:key1 value1 :key2 value2 :key3 1002.23 :key4
           (a b c d e))
    
    ;;; It is more useful in configuration files
    
    (
    :key1  value1
    :key2  value2
    :key3  value3
    :key4  (a b c d e )
    )


### 关联列表/Alist

    ELISP> (setq dict
    '((pine . cones)
     (oak . acorns)
     (maple . seeds)))
    ((pine . cones)
     (oak . acorns)
     (maple . seeds))
    
    ELISP> dict
    ((pine . cones)
     (oak . acorns)
     (maple . seeds))
    
    ;; Get a cell associated with a key
    ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ELISP>
    ELISP> (assoc 'oak dict)
    (oak . acorns)
    
    ELISP> (assoc 'wrong dict)
    nil
    
    ;; Get a Key
    ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ELISP> (car (assoc 'oak dict))
    oak
    ELISP> (cdr (assoc 'oak dict))
    acorns
    ELISP>
    
    
    ELISP> (car (assoc 'oak dict))
    oak
    ELISP>
    
    ;; Get all keys
    ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ELISP> (mapcar #'car dict)
    (pine oak maple)
    
    ;; Get all values
    ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ELISP> (mapcar #'cdr dict)
    (cones acorns seeds)

例：过滤多个键

    ELISP> (defvar language-list
      '(
       ("io" . ((:command . "io")
    	     (:description . "Run IO Language script")))
        ("lua" . ((:command . "lua")
    	      (:description . "Run Lua script")))
        ("groovy" . ((:command . "groovy")
    		 (:description . "Run Groovy")))
        ("scala" . ((:command . "scala")
    		(:cmdopt . "-Dfile.encoding=UTF-8")
    		(:description . "Run Scala file with scala command")))
    
        ("haml" . ((:command . "haml")
    	       (:exec    . "%c %o %s")
    	       (:description . "Convert HAML to HTML")))
        ("sass" . ((:command . "sass")
    	       (:exec    . "%c %o --no-cac")))
     ))
    language-list
    
    
    ELISP> (assoc  "scala"  language-list )
    ("scala"
     (:command . "scala")
     (:cmdopt . "-Dfile.encoding=UTF-8")
     (:description . "Run Scala file with scala command"))
    
    ELISP> (assoc  "lua"  language-list )
    ("lua"
     (:command . "lua")
     (:description . "Run Lua script"))
    
    ELISP> (assoc  "wrong"  language-list )
    nil
    
    ELISP> (assoc ':command (assoc  "scala"  language-list ))
    (:command . "scala")
    
    ELISP> (cdr (assoc ':command (assoc  "scala"  language-list )))
    "scala"
    ELISP>
    
    ELISP> (assoc ':description (assoc  "scala"  language-list ))
    (:description . "Run Scala file with scala command")
    
    ELISP> (cdr (assoc ':description (assoc  "scala"  language-list )))
    "Run Scala file with scala command"
    ELISP>
    
    ELISP> (mapcar 'car language-list)
    ("io" "lua" "groovy" "scala" "haml" "sass")
    
    ELISP> (mapcar 'cdr language-list)
    (((:command . "io")
      (:description . "Run IO Language script"))
     ((:command . "lua")
      (:description . "Run Lua script"))
     ((:command . "groovy")
      (:description . "Run Groovy"))
     ((:command . "scala")
      (:cmdopt . "-Dfile.encoding=UTF-8")
      (:description . "Run Scala file with scala command"))
     ((:command . "haml")
      (:exec . "%c %o %s")
      (:description . "Convert HAML to HTML"))
     ((:command . "sass")
      (:exec . "%c %o --no-cac")))
    
    ELISP>
    
    ELISP> (mapcar (lambda (x) (
    			     list
    			     (car x)
    			     (cdr x)
    			     ))
    			    language-list)
    (("io"
      ((:command . "io")
       (:description . "Run IO Language script")))
     ("lua"
      ((:command . "lua")
       (:description . "Run Lua script")))
     ("groovy"
      ((:command . "groovy")
       (:description . "Run Groovy")))
     ("scala"
      ((:command . "scala")
       (:cmdopt . "-Dfile.encoding=UTF-8")
       (:description . "Run Scala file with scala command")))
     ("haml"
      ((:command . "haml")
       (:exec . "%c %o %s")
       (:description . "Convert HAML to HTML")))
     ("sass"
      ((:command . "sass")
       (:exec . "%c %o --no-cac"))))
    
    ELISP>
    
    ELISP> (mapcar (lambda (x) (
         list
         (car x)
         (assoc ':command       (cdr x))
         (assoc ':cmdopt        (cdr x))
         (assoc ':description   (cdr x))
         ))
        language-list)
    
    (("io"
      (:command . "io")
      nil
      (:description . "Run IO Language script"))
     ("lua"
      (:command . "lua")
      nil
      (:description . "Run Lua script"))
     ("groovy"
      (:command . "groovy")
      nil
      (:description . "Run Groovy"))
     ("scala"
      (:command . "scala")
      (:cmdopt . "-Dfile.encoding=UTF-8")
      (:description . "Run Scala file with scala command"))
     ("haml"
      (:command . "haml")
      nil
      (:description . "Convert HAML to HTML"))
     ("sass"
      (:command . "sass")
      nil nil))
    
    ELISP>
    
    
    ELISP> (mapcar (lambda (x) (
    	 list
    	 (car x)
    	 (cdr (assoc ':command   (cdr x)))
    	 (cdr (assoc ':cmdopt       (cdr x)))
    	 (cdr (assoc ':description   (cdr x)))
    	 ))
    
    	language-list)
    (("io" "io" nil "Run IO Language script")
     ("lua" "lua" nil "Run Lua script")
     ("groovy" "groovy" nil "Run Groovy")
     ("scala" "scala" "-Dfile.encoding=UTF-8" "Run Scala file with scala command")
     ("haml" "haml" nil "Convert HAML to HTML")
     ("sass" "sass" nil nil))
    
    ELISP>
    
    ELISP> (defun get-value (alist key) (cdr (assoc key alist)))
    get-value
    ELISP> (get-value language-list "scala")
    ((:command . "scala")
     (:cmdopt . "-Dfile.encoding=UTF-8")
     (:description . "Run Scala file with scala command"))
    
    ELISP> (get-value language-list "lua")
    ((:command . "lua")
     (:description . "Run Lua script"))
    
    ELISP>
    ELISP> (get-value language-list "0")
    nil
    ELISP>
    
    
    ELISP> (defun get-key-value (alist key field)
    		(cdr (assoc  field  (cdr (assoc key alist))  )))
    get-key-value
    ELISP>
    ELISP> (get-key-value language-list "scala" ':description)
    "Run Scala file with scala command"
    ELISP>
    
    ELISP> (get-key-value language-list "scala" ':command)
    "scala"
    ELISP>


### 属性列表

    ELISP> (defvar plst (list :buffer (current-buffer) :line 10 :pos 2000))
    plst
    
    ELISP>
    ELISP> (plist-get plst :line)
    10
    
    ELISP> (plist-get plst :pos)
    2000
    
    ELISP> (plist-get plst :buffer)
    #<buffer *ielm*>
    ELISP>
    
    ELISP>
    ELISP> (plist-get plst :buffdfds)
    nil
    ELISP>
    
    ELISP> (plist-member plst :buffer)
    (:buffer #<buffer *ielm*> :line 10 :pos 2000)
    
    ELISP> (plist-member plst :bufferasd)
    nil
    ELISP>
    
    ELISP> (plist-put plst :winconf (current-window-configuration))
    (:buffer #<buffer *ielm*> :line 10 :pos 2000 :winconf #<window-configuration>)
    
    ELISP> plst
    (:buffer #<buffer *ielm*> :line 10 :pos 2000 :winconf #<window-configuration>)
    
    ELISP>


### 转换Alist成Plist和vice-versa

    ;; Alist to plist
    (defun plist->alist (plist)
      (if (null plist)
          '()
          (cons
           (list (car plist) (cadr plist))
           (plist->alist (cddr plist)))))
    
    ELISP> (plist->alist (list :x 10 :y 20 :name "point"))
    ((:x 10)
     (:y 20)
     (:name "point"))
    
    ;;; Converts association list to plist
    (defun alist->plist (assocl)
      (if (null assocl)
          '()
        (let
        ((hd (car assocl))
         (tl (cdr assocl)))
          (cons (car hd)
    	(cons (cadr hd)
    	  (alist->plist tl))))))
    
    ;;; Converts plist to clist (List of cons pairs)
    (defun plist->clist (plist)
      (if (null plist)
          '()
          (cons
           (cons (car plist) (cadr plist))
          (plist->clist (cddr plist)))))
    
    ELISP> (plist->clist (list :x 10 :y 20 :name "point"))
    ((:x . 10)
     (:y . 20)
     (:name . "point"))
    
    ;; Separates a property list into two lists of keys and values.
    ;;
    (defun plist->kv (plist)
      (let ((alist (plist->alist plist)))
        (cons
         (mapcar #'car alist)
         (mapcar #'cdr alist))))
    
    ELISP> (setq al (plist->alist (list :x 10 :y 20 :name "point")))
    ((:x 10)
     (:y 20)
     (:name "point"))
    
    ELISP> (alist->plist al)
    (:x 10 :y 20 :name "point")
    
    ELISP>
    
    (setq keylist
        '("M-i"  'previous-line
          "M-j"  'backward-char
          "M-k"  'next-line
          "M-l"  'forward-char))
    
    
    ELISP> (setq kv (plist->kv keylist))
    (("M-i" "M-j" "M-k" "M-l")
     ('previous-line)
     ('backward-char)
     ('next-line)
     ('forward-char))
    
    ELISP> (car kv)
    ("M-i" "M-j" "M-k" "M-l")
    
    ELISP> (cdr kv)
    (('previous-line)
     ('backward-char)
     ('next-line)
     ('forward-char))
    
    ELISP>


## 字符串

    ;; Split String
    
    ELISP> (split-string "  two words ")
    ("two" "words")
    
    ELISP>
    
    ELISP> (split-string "o\no\no" "\n" t)
    ("o" "o" "o")
    
    ELISP> (split-string "Soup is good food" "o*" t)
    ("S" "u" "p" " " "i" "s" " " "g" "d" " " "f" "d")
    
    ELISP>
    
    ;; Format String
    
    ELISP> (format-time-string "%Y/%m/%d %H:%M:%S" (current-time))
    "2015/06/26 06:10:04"
    ELISP>
    ELISP>
    
    
    ;; Concatenate Strings
    
    ELISP> (concat "The " "quick brown " "fox.")
    "The quick brown fox."
    ELISP>
    
    ELISP> (mapconcat 'identity '("aaa" "bbb" "ccc") ",")
    "aaa,bbb,ccc"
    ELISP> (split-string "aaa,bbb,ccc" ",")
    ELISP> (split-string "aaa,bbb,ccc" ",")
    ("aaa" "bbb" "ccc")
    
    ;; String Width
    
    ELISP> (string-width "hello world")
    11
    ELISP>
    ELISP> (substring "Freedom Land" 0 5)
    "Freed"
    ELISP>
    ELISP> (string-match "ce" "central park")
    0
    ELISP> (string-match "gt" "central park")
    nil
    ELISP>
    
    
    ;;;;; Misc
    
    ELISP> (make-string 5 ?x)
    "xxxxx"
    ELISP> (make-string 5 ?a)
    "aaaaa"
    ELISP> (make-string 5 ?r)
    "rrrrr"
    ELISP> (make-string 15 ?r)
    "rrrrrrrrrrrrrrr"
    ELISP>

**elisp符号/字符串转换**

    ; Convert a symbol to string
    ELISP> (symbol-name 'wombat)
    "wombat"
    
    ; Convert a String to Symbol
    ELISP> (intern "wombat")
    wombat

**读取字符串中的S表达式**

    ELISP> (read-from-string
    	    "(
    	       (POINT1  (X  10.2323)  (Y   20.2323))
    	       (POINT2  (x  0.2)          (Y 923.23))
    	       (POINT3  (x -10.5)       (Y 78,23))
    	     )")
    (((POINT1
       (X 10.2323)
       (Y 20.2323))
      (POINT2
       (x 0.2)
       (Y 923.23))
      (POINT3
       (x -10.5)
       (Y 78
          (\, 23))))
     . 174)
    
    ELISP>


## 符号

    ;;; Convert a string to symbol
    
    ELISP> (intern "a-symbol")
    a-synmbol
    ELISP> (symbolp (intern "a-symbol"))
    t
    ELISP>
    
    ;;; Convert a symbol to a string
    
    ELISP> (symbol-name 'symbol)
    "symbol"
    ELISP>
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ELISP> (setq sym '(1 2 3 4 5))
    (1 2 3 4 5)
    
    ELISP> sym
    (1 2 3 4 5)
    
    ;;; Test if variable is defined
    ELISP> (boundp 'sym)
    t
    ELISP>
    
    ;;; Test if variable sym is a symbol
    ELISP> (symbolp sym)
    nil
    
    ;;; Test if the symbol sym is a symbol
    ELISP> (symbolp 'sym)
    t
    ELISP>
    
    ;; Get symbol as string
    ;;
    ELISP> (symbol-name 'sym)
    "sym"
    
    ;; Get value from a symbol
    ;;
    ELISP> (symbol-value 'sym)
    (1 2 3 4 5)
    
    ELISP> (symbol-function 'sym)
    nil
    
    ELISP> (symbol-plist 'sym)
    nil
    
    ;;-------------------------;;
    
    ELISP> (defun func (x y) (+ (* 3 x) (* 4 y)))
    func
    
    ELISP> (func 10 2)
    38
    ELISP>
    
    ;;; Check if function is defined
    ;;
    ELISP> (fboundp 'func)
    t
    ELISP> (fboundp 'sym)
    nil
    ELISP>
    
    
    ELISP> (symbol-name 'func)
    "func"
    
    ELISP> (symbol-value 'func)
        ** Eval error **  Symbol's value as variable is void: func
    ELISP> (symbol-function 'func)
    (lambda
      (x y)
      (+
       (* 3 x)
       (* 4 y)))
    
    ELISP> (symbol-plist 'func)
    nil
    ELISP>
    
    ;;; Function Source Code
    
    ELISP> (symbol-function #'func)
    (lambda
      (x y)
      (+
       (* 3 x)
       (* 4 y)))
    
    
    ;; Test if function is an elisp primitive
    
    ELISP> (subrp (symbol-function 'goto-char))
    t
    ELISP>


## 类型转换

**类型查询**

    ELISP> (type-of 1000)
    integer
    
    ELISP> (type-of 1000.3434)
    float
    ELISP>
    
    ELISP> (type-of "lisp")
    string
    
    ELISP> (type-of '(1 2 3 4 5))
    cons
    ELISP> (type-of (list 'cos 'sin 1 2 3 4 5))
    cons
    ELISP>
    
    ELISP> (type-of [1 2 3 4])
    vector
    
    ELISP> (type-of 'elisp-mode-map)
    symbol
    ELISP>
    
    ELISP> (type-of #'cos)
    symbol
    ELISP>

**类型判断**

    ;; Test if it is a number
    ;;-----------------------------------
    
    ELISP> (numberp 1000)
    t
    ELISP> (numberp 10e4)
    t
    ELISP> (numberp '(1 2 3 4))
    nil
    ELISP> (numberp "hello world")
    nil
    ELISP>
    
    
    ;; Test if it is a string
    ;;-----------------------------------
    
    ELISP> (stringp "Emacs")
    t
    ELISP> (stringp '(1 2 3 4))
    nil
    ELISP>
    
    ;; Test if ti is a symbol
    ;;------------------------------------
    ELISP> (symbolp 'emacs)
    t
    ELISP> (symbolp #'emacs)
    t
    ELISP> (symbolp "something")
    nil
    ELISP> (symbolp 10000)
    nil
    ELISP>
    
    
    ;; Test if it is a list
    ;;-----------------------------------
    
    ELISP> (listp '(1 2 3 4))
    t
    ELISP> (listp [1 2 3 4])
    nil
    ELISP> (listp "hello world")
    nil
    ELISP>
    
    
    ;; Test if it is a vector
    ;;-----------------------------------
    
    ELISP> (vectorp ["Lisp" "Emacs" "Scheme" "Clojure"])
    t
    ELISP>
    ELISP> (vectorp '(1 2 3))
    nil
    ELISP> (vectorp "lisp")
    nil
    ELISP>

**数字/字符串转换**

    ELISP>
    ELISP> (number-to-string 1000)
    "1000"
    
    ELISP> (string-to-number "200")
    200
    ELISP>
    ELISP>

**符号/字符串转换**

    ELISP> (symbol-name 'my-symbol)
    "my-symbol"
    
    ELISP> (symbol-name :my-symbol)
    ":my-symbol"
    ELISP>
    
    ELISP> (intern "some-symbol")
    some-symbol

**S表达式/字符串转换**

-   read: 解析S表达式

        ELISP>
            ELISP> (setq raw "(:x 10 :y 20 :z 30 :w \"hello world\")")
            "(:x 10 :y 20 :z 30 :w \"hello world\")"
            ELISP>
            ELISP> (read raw)
            (:x 10 :y 20 :z 30 :w "hello world")
            
            ELISP> (plist-get (read raw) :x)
            10
            ELISP> (plist-get (read raw) :w)
            "hello world"
            ELISP>

        -   prin1-to-string: 序列化S表达式

            ELISP> (setq sexp '(:x 10 :y 20 :z 30 :w "hello world"))
            (:x 10 :y 20 :z 30 :w "hello world")
            
            ELISP> sexp
            (:x 10 :y 20 :z 30 :w "hello world")
            
            ELISP> (prin1-to-string sexp)
            "(:x 10 :y 20 :z 30 :w \"hello world\")"
            ELISP>


## 求值

**S表达式求值**

    ELISP> (eval '(+ 1 2 3 4 5))
    15
    ELISP>
    
    
    ELISP> '(defun func1(x)(* 10 x))
    (defun func1
        (x)
      (* 10 x))
    
    ELISP>
    
    ELISP> '((+ 1 3) (* 4 5) (- 8 9))
    ((+ 1 3)
     (* 4 5)
     (- 8 9))
    
    ELISP> (eval '(defun func1(x)(* 10 x)))
    func1
    ELISP> (func1 5)
    50
    ELISP>
    
    
    ELISP> (mapcar 'eval '((+ 1 3) (* 4 5) (- 8 9)))
    (4 20 -1)

**字符串求值**

    ELISP> (defun eval-string (str) (eval (read str)))
    eval-string
    
    ELISP> (eval-string "(+ 1 2 3 4 5 6)")
    21
    ELISP>
    
    ELISP> (eval-string "(defun func2(x)(* 10 x)))")
    func2
    ELISP> (func2 6)
    60
    ELISP>

**S表达式格式化为字符串**

    ELISP> (setq sexp1 '(+ 1 (* 2 3)))
    (+ 1
       (* 2 3))
    
    ELISP> (eval sexp1)
    7
    
    ELISP> (format "%S" sexp1)
    "(+ 1 (* 2 3))"
    ELISP>

**Elisp中的求值命令**

<table>


<colgroup>
<col  class="org-left">

<col  class="org-left">
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">命令</th>
<th scope="col" class="org-left">功能</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">M-x eval-defun</td>
<td class="org-left">函数求值</td>
</tr>


<tr>
<td class="org-left">M-x eval-region</td>
<td class="org-left">区域内表达式求值</td>
</tr>


<tr>
<td class="org-left">M-x eval-buffer</td>
<td class="org-left">buffer内表达式求值</td>
</tr>


<tr>
<td class="org-left">M-x eval-expression</td>
<td class="org-left">输入框输入求值</td>
</tr>


<tr>
<td class="org-left">M-x load-file</td>
<td class="org-left">文件加载</td>
</tr>
</tbody>
</table>


## Defalias

内置宏 `defalias` 可以为emaca函数定义简短的名字。

参考：[Emacs: Use Alias for Fast M-x](http://ergoemacs.org/emacs/emacs_alias.html)

    ELISP> (require 'cl)
    cl
    ELISP>
    
    ELISP> (defalias 'map 'mapcar)
    map
    ELISP> (map (lambda (x) (* 3 x)) (list 1 2 3 4 5 6))
    (3 6 9 12 15 18)
    
    ELISP> (defalias 'filter 'remove-if-not) ;; remove-if-not comes from "cl" library
    filter
    
    ;;; Filter all buffers bounded to a file
    ;;
    ELISP> (filter #'buffer-file-name (buffer-list))
    (#<buffer README.org> #<buffer Projects.wiki.org> #<buffer Index.wiki.org> #<buffer settings.org> #<buffer project.org>)
    
    ;;; Reject all buffers which are not bounded to a file
    ELISP> (reject #'buffer-file-name (buffer-list))
    (#<buffer *ielm*> #<buffer *Help*> #<buffer  *Minibuf-1*> #<buffer emacs> #<buffer *scratch*> ..)
    
    ;;; The command M-x org-html-export-to-htm will export this document (README.org) to html
    ;;  the command M-x org2html will do so too.
    ;;
    (defalias #'org2html #'org-html-export-to-html)
    
    ;;
    ;;  It is also useful to create more convenient names for Emacs API
    ;; in a namsepace-like fashion that makes easier to find functions and
    ;; autocomplete functions, for instance:
    ;;
    (defalias 'file/extension         'file-name-extension)
    (defalias 'file/extension-sans    'file-name-sans-extension)
    (defalias 'file/path-expand       'expand-file-name)
    (defalias 'file/filename          'file-name-nondirectory)
    (defalias 'file/path-relative     'file-relative-name)
    (defalias 'file/rename            'rename-file)
    (defalias 'file/delete            'delete-file)
    (defalias 'file/copy              'copy-file)
    
    ;;; To find the documentation of a function group defined in this fashion
    ;; Enter M-x apropos  and then type file/
    (apropos "file/")
    
    ELISP> (set-buffer "README.org")
    #<buffer README.org>
    ELISP> (buffer-file-name)
    "/home/tux/PycharmProjects/emacs/README.org"
    ELISP> (file/basename (buffer-file-name))
    "README"
    ELISP> (file/extension (buffer-file-name))
    "org"
    ELISP> (file/filename (buffer-file-name))
    "README.org"
    ELISP>


## 控制结构


### Conditional Statement

**If Else 语句**

    ;;
    ;; Any value different from nil or '() is true, otherwise false.
    ;;
    
    ;; True
    ;;
    ELISP> (if t 5 6)
    5
    
    ELISP> (if 10 5 6)
    5
    
    ELISP> (if 0 5 6)
    5
    
    ;; False
    ELISP> (if nil 5 6)
    6
    
    ELISP> (if '() 5 6)
    6
    
    
    ;; Inverting Predicate
    ;;
    ELISP> (if (not t) 5 6)
    6
    
    ELISP> (if (not nil) 5 6)
    5
    
    
    ELISP> (if (< 5 10)  (message "less than 10") (message "greater or equal to 10") )
    "less than 10"
    
    ELISP> (if (< 30 10)  (message "less than 10") (message "greater or equal to 10") )
    "greater or equal to 10"
    ELISP>
    
    ;;; If else with multiple statements
    
    ELISP> (setq x 10)
    10
    
    ELISP> (if (> x 5)
           ;; Then Statement
           (progn
    
    	 (message "Positive Number")
    	 (print "Greater than five")
    	 (split-window-vertically)
    	 78 ;;  Return Value
    	)
         ;; Else Statement
         (progn
           (print "Less than five")
           (split-window-horizontally)
           12 ;;  Return Value
         ))
    
    "Greater than five"
    
    78
    ELISP>

**When语句**

    ELISP> (when t 3)
    3
    
    ELISP> (when nil 3)
    nil
    
    
    ELISP> (setq x 5)
    5
    
    ELISP> (when (> x 3)
    	 (message "Less than 3"))
    "Less than 3"
    ELISP>
    
    ELISP> (setq x 1)
    1
    
    ELISP> (when (> x 3)
    	 (message "Less than 3"))
    nil
    ELISP>
    
    
    ;;;;; When with Multiple Statements
    
    ELISP> (setq x 10)
    10
    
    ELISP> (when (> x 7)
         (progn
           (message "Greater than 7 OK.")
           (message "Print message 2")
           (split-window-horizontally)
          ))
    
     #<window 8 on *ielm*>
    ELISP>


### Cond - Case Switch

    ELISP> (setq a 3)       ;; a = 3
    3
    ELISP>
    
    ELISP> (cond
    	((evenp a) a)       ;; if   (a % 2 == 0)    ==> a
    	((> a 7) (/ a 2))   ;; elif (a > 7)         ==> a/2
    	((< a 5) (- a 1))   ;; elif (a < 5)         ==> a-1
    	(t 7)               ;; else                 ==> 7
    	)
    2
    ELISP>


### CL-Case - Case Switch

    (defun test-cl-case (operation x y)
      (cl-case operation
        (:mul (* x y))
        (:add (+ x y))
        (:sub (- x y))
        (:div (/ x y))
        (otherwise nil)))
    
    ELISP> (test-cl-case :mul 2 10)
    20
    
    ELISP> (test-cl-case :sub 10 2)
    8
    
    ELISP> (test-cl-case :add 10 2)
    12
    ELISP> (test-cl-case :div 10 2)
    5
    
    ELISP> (test-cl-case 'dummy 20 10)
    nil


### 循环

**Dolist**

    ELISP> (dolist (h '(a b c)) (print h))
    
    a
    
    b
    
    c
    
    nil
    
    ELISP> (dolist (x '(1 2 3)) (print (* 2 x)))
    
    2
    
    4
    
    6
    
    nil
    ELISP>
    
    ELISP> (dolist (x '(1 2 3))
    	 (dolist (y '(a b))
    	    (print (list x y))))
    (1 a)
    
    (1 b)
    
    (2 a)
    
    (2 b)
    
    (3 a)
    
    (3 b)
    
    nil
    ELISP>

**Dotimes**

    ELISP> (dotimes (i 3) (print i))
    
    0
    
    1
    
    2
    
    nil
    ELISP
    
    ELISP> (dotimes (i 3) (print (* 2 i)))
    
    0
    
    2
    
    4
    
    nil
    ELISP>

**Loop**

最好使用 `map` 和 `filter` 代替 `loops` , 详见 Functional Programming

    ELISP> (setq a 4)
    4
    
    ELISP> (loop
    	(setq a (+ a 1))
    	(when (> a 7) (return a)))
    8
    
    ELISP> a
    8
    ELISP>
    
    ELISP> (loop
       (setq a (- a 1))
       (when (< a 3) (return)))
    nil
    ELISP> a
    2
    ELISP>

**Loop Collecting / Summing / For**

    ELISP> (loop for i from 1 to 10 collecting i)
    (1 2 3 4 5 6 7 8 9 10)
    
    ELISP> (loop for i from 1 to 10 collecting (* 3 i))
    (3 6 9 12 15 18 21 24 27 30)
    
    ELISP> (loop for x from 1 to 10 summing (expt x 2))
    385
    
    ELISP> (loop for x from 1 to 10 collecting (* 2 x))
    (2 4 6 8 10 12 14 16 18 20)
    
    ELISP> (loop for x from 1 to 10 summing (* 2 x))
    110
    ELISP>
    
    ELISP> (apply #'+ '(2 4 6 8 10 12 14 16 18 20))
    110
    
    ELISP> (loop for i below 10 collecting i)
    (0 1 2 3 4 5 6 7 8 9)
    
    ELISP>  (loop for x in '(1 2 3)
          do (print x) )
    
    1
    
    2
    
    3
    
    nil
    
    (loop
           for x in '(a b c)
           for y in '(1 2 3 4 5 6)
           collect (list x y))
    ((a 1)
     (b 2)
     (c 3))
    
    ELISP> (loop for (a b) in '((x 1) (y 2) (z 3))
          collect (list b a))
    ((1 x)
     (2 y)
     (3 z))
    
    ELISP> (loop for i upto 20
          if (oddp i)
    	collect i into odds
          else
    	collect i into evens
          finally (return (values evens odds)))
    ((0 2 4 6 8 10 12 14 16 18 20)
     (1 3 5 7 9 11 13 15 17 19))

**Do Loop**

    (do (variable-definition*)
        (end-test-form result-form*)
      statement*)

    (do
       ;; Variables Definitions
       ((i 0 (1+ i)))
    
       ;; Test form
        ((>= i 4))
    
      ;; Statement form
      (print i))
    
    0
    
    1
    
    2
    
    3
    nil
    
    ;; Fibbonaci Computing Loop
    ;;
    (do ((n 0 (1+ n))
         (cur 0 next)
         (next 1 (+ cur next)))
        ((= 10 n) cur))
    55


### 函数式编程

[Dash](https://github.com/magnars/dash.el.git) 是emacs经常使用的函数式编程库。

-   Map and Filter

**Mapcar / Equivalent to map**

    ELISP> (defun my-fun (x) (* x 10))
    my-fun
    ELISP>
    
    ELISP> (mapcar 'my-fun '(1 2 3 5 6))
    (10 20 30 50 60)
    
    ELISP> (mapcar 'capitalize '("hello" "world" "emacs"))
    ("Hello" "World" "Emacs")
    
    ;;  Anonymous Functions
    ;;
    ELISP> (mapcar (lambda (x) (* x x))   '(1 2 3 4 5 6))
    (1 4 9 16 25 36)
    
    
    ELISP> (setq anon (lambda (x) (* x x)))
    (lambda
      (x)
      (* x x))
    
    ELISP> (mapcar anon '(1 2 3 4 5 6))
    (1 4 9 16 25 36)

**Filter**

    ELISP> (null nil)
    t
    ELISP> (null 23)
    nil
    ELISP>
    
    ;; Equivalent to  Haskell idiom:
    ;;
    ;; > filter predicate list
    ;;
    ELISP> (remove-if-not 'null '(1 2 3 nil 5 6 nil nil ))
    (nil nil nil)
    
    ;; Equivalent to Haskell idiom:
    ;;
    ;;   > filter (\x -> not (predicate x)) list
    ;;
    ;; a more apropriate name would be reject
    ;;
    ELISP> (remove-if 'null '(1 2 3 nil 5 6 nil nil ))
    (1 2 3 5 6)
    
    
    
    ELISP> (defun range (step start stop)
      (if (> start stop)
          nil
          (cons start (range step (+ step start) stop))
    
      );; End If
    );; End range
    
    ELISP> (range 1 0 10)
    (0 1 2 3 4 5 6 7 8 9 10)
    
    ELISP> (range 2 0 20)
    (0 2 4 6 8 10 12 14 16 18 20)
    
    
    ELISP> (remove-if (lambda (x) (= (% x 2) 0)) (range 1 0 20))
    (1 3 5 7 9 11 13 15 17 19)
    
    ELISP> (remove-if-not (lambda (x) (= (% x 2) 0)) (range 1 0 20))
    (0 2 4 6 8 10 12 14 16 18 20)
    
    
    ELISP> (remove-if (lambda (x) (= (% x 3) 0)) (range 1 0 20))
    (1 2 4 5 7 8 10 11 13 14 16 17 19 20)
    
    ELISP> (remove-if-not (lambda (x) (= (% x 3) 0)) (range 1 0 20))
    (0 3 6 9 12 15 18)
    
    ELISP>

-   匿名函数/lambda函数

        ELISP> (lambda (x)(* x 10))
            (lambda
              (x)
              (* x 10))
            
            ELISP>
            
            ELISP> (funcall (lambda (x)(* x 10)) 5)
            50
            ELISP>
            
            ELISP> (setq my-lambda (lambda (x) (+ (* x 10) 5))) ;; 10 * x + 5
            (lambda
              (x)
              (+
               (* x 10)
               5))
            
            ELISP> (funcall my-lambda 10)
            105
            ELISP> (mapcar my-lambda '(1 2 3 4 5))
            (15 25 35 45 55)
            
            
            ELISP>  (setq double (function (lambda (x) (+ x x)) ))
            (lambda
              (x)
              (+ x x))
            
            ELISP> (funcall double 22)
            44
            ELISP>
            
            
            ;;
            ;; Apply a function to a list of arguments
            ;;
            ;;;;;;;;;;;
            
            ELISP> (apply #'+ '(1 2 3 4 5))
            15
            ELISP>
            
            ELISP>
            ELISP> (defun f (x y z) (+ (* 10 x) (* -4 y) (* 5 z)))
            f
            ELISP> (f 2 3 5)
            33
            
            ELISP> (apply 'f '(2 3 5))
            33
            
            
            ELISP> (mapcar (lambda (x) (apply 'f x)) '( (2 3 5) (4 5 6) (8 9 5)))
            (33 50 69)
            
            
            
            ;; Create Higher Order Functions
            ;;
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

-   Function Composition ????

        ELISP> ;; ID: f0c736a9-afec-3e3f-455c-40997023e130
            (defun compose (&rest funs)
              "Return function composed of FUNS."
              (lexical-let ((lex-funs funs))
                (lambda (&rest args)
                  (reduce 'funcall (butlast lex-funs)
            	      :from-end t
            	      :initial-value (apply (car (last lex-funs)) args)))))
            	      compose
            
            ELISP> (funcall (compose 'prin1-to-string 'random* 'exp) 10)
            "4757.245739507558"
            ELISP>

-   Interactive Functions

        (defun some-interactive-function ()
               "Documentation"
              (interactive)
              ...)

-   List Recursive Functions

**Map**

    (defun map (fun xs)
      (if (null xs)
          '()
        (cons (funcall fun (car xs))
          (map fun (cdr xs)))))
    
    ELISP> (map #'buffer-name (buffer-list))
    ("*ielm*" "*scratch*" " *Minibuf-1*" "*Backtrace*" "*eshell*" "sclj.import.scm" "*Messages*" "*GNU Emacs*" " *Minibuf-0*" " *code-conversion-work*" " *Echo Area 0*" " *Echo Area 1*" "*Shell Command Output*" "*Completions*")
    
    ELISP>

**Filter**

    (defun filter (fun xs)
      (if (null xs)
          '()
        (let ((hd (car xs))
          (tl (cdr xs)))
          (if (funcall fun hd)
          (cons hd (filter fun tl))
        (filter fun tl)))))
    
    (defun odd? (x) (zerop (% x 2)))
    
    ELISP> (filter #'odd? '(1 2 3 4 5 6))
    (2 4 6)

**Take**

    (defun take (n xs)
      (if (or (null xs) (zerop n))
          '()
        (cons (car xs)
              (take (- n 1) (cdr xs)))))
    
    
    ELISP> (take 5 '(a b c d e f g h i j))
    (a b c d e)
    
    ELISP> (take 10 '(a b c d e f g h i j))
    (a b c d e f g h i j)
    
    ELISP> (take 200 '(a b c d e f g h i j))
    (a b c d e f g h i j)
    
    ELISP> (take 0 '(a b c d e f g h i j))
    nil
    ELISP> (take 10 '())
    nil
    ELISP>

**Drop**

    (defun drop (n xs)
      (if (or (null xs) (zerop n))
          xs
        (drop (- n 1)  (cdr xs))))
    
    ELISP> (drop 3 '(a b c d e f g h i j))
    (d e f g h i j)
    
    ELISP> (drop 4 '(a b c d e f g h i j))
    (e f g h i j)
    
    ELISP> (drop 25 '(a b c d e f g h i j))
    nil
    ELISP>

**Map-apply**

    (defun map-apply (fun xss)
      (mapcar (lambda (xs) (apply fun xs)) xss))
    
    ELISP> (map-apply #'fxyz '((1 2 3) (3 4 5) (2 3 1)))
    (17 35 20)
    
    ELISP> (fxyz 1 2 3)
    17
    ELISP> (fxyz 3 4 5)
    35
    ELISP> (fxyz 2 3 1)
    20
    ELISP>

**Zip**

    (defun zip (&rest xss)
      (if (null (car xss))
          '()
        (cons
         (mapcar #'car xss)
         (apply #'zip (mapcar #'cdr xss)))))
    
    ELISP> (zip (list 1 2 3 4) '(a b c d) '(x y z w))
    ((1 a x)
     (2 b y)
     (3 c z)
     (4 d w))

**Zipwith**

    (defun zipwith (f &rest xss)
      (map-apply f (apply #'zip xss)))
    
    ELISP> (zipwith #'f '(1 2 3) '(4 5 6) '(3 6 8))
    (23 40 53)
    
    ELISP> (f 1 4 3)
    23
    
    ELISP> (f 2 5 6)
    40
    
    ELISP> (f 3 6 8)
    53
    ELISP>

**Foldr**

    ;;           f :: x -> acc -> acc
    ;; foldr :: (a -> b -> b) -> b -> [a] -> b
    ;; foldr :: (x -> acc -> acc) -> acc -> [x] -> acc
    ;; foldr f z []     = z
    ;; foldr f z (x:xs) = f x (foldr f z xs)
    ;;
    ;;  x = (car xss) , xs = (cdr xss)
    (defun foldr (f acc xss)
      (if (null xss)
          ;; foldr f z []     = z
          acc
        ;; foldr f z (x:xs) = f x (foldr f z xs)
        (funcall f (car xss)
                 (foldr f acc (cdr xss)))))
    
    ELISP> (foldr (lambda (a b) (+ (* 10 b) a)) 0 '(1 2 3 4 5))
    54321
    ELISP>
    
    ELISP> (foldr #'+ 0 '(1 2 3 4 5))
    15
    ELISP>

**Foldl**

    ;; foldl :: (b -> a -> b) -> b -> [a] -> b
    ;; foldl f z []     = z
    ;; foldl f z (x:xs) = foldl f (f z x) xs
    (defun foldl (f acc xss)
      (if (null xss)
          acc
        (foldl f (funcall f acc (car xss)) (cdr xss))))
    
    ELISP> (foldl (lambda (a b) (+ (* 10 a) b)) 0 '(1 2 3 4 5))
    12345
    ELISP>

**Map Pairs**

    (defun map-pair (func xs)
      (mapcar (lambda (x) (cons x (funcall func x))) xs))
    
    ELISP> (map-pair #'1+ '(1 2 3 4))
    ((1 . 2)
     (2 . 3)
     (3 . 4)
     (4 . 5))
    
    ELISP> (map-pair #'log10 '(1 10 100 1000 10000))
    ((1 . 0.0)
     (10 . 1.0)
     (100 . 2.0)
     (1000 . 3.0)
     (10000 . 4.0))
    
    (defun buffer-mode (buffer-or-string)
      "Returns the major mode associated with a buffer."
      (with-current-buffer buffer-or-string
        major-mode))
    
    ELISP> (map-pair #'buffer-mode (buffer-list))
    ((#<buffer *ielm*> . inferior-emacs-lisp-mode)
     (#<buffer *scratch*> . lisp-interaction-mode)
     (#<buffer *Backtrace*> . debugger-mode)
     (#<buffer *GNU Emacs*> . fundamental-mode)
     (#<buffer  *Minibuf-1*> . minibuffer-inactive-mode)
     (#<buffer  *Minibuf-0*> . minibuffer-inactive-mode)
     (#<buffer *Messages*> . messages-buffer-mode)

**Map pairs xy**

    (defun map-xypair (func-x func-y xs)
      (mapcar
       (lambda (x)
         (cons (funcall func-x x) (funcall func-y x)))
       xs))
    
    ELISP> (map-xypair #'buffer-name #'buffer-mode (buffer-list))
    (("*ielm*" . inferior-emacs-lisp-mode)
     ("*scratch*" . lisp-interaction-mode)
     ("*Backtrace*" . debugger-mode)
     ("*GNU Emacs*" . fundamental-mode)
     (" *Minibuf-1*" . minibuffer-inactive-mode)
     (" *Minibuf-0*" . minibuffer-inactive-mode)
     ("*Messages*" . messages-buffer-mode)
     (" *code-conversion-work*" . fundamental-mode)
     (" *Echo Area 0*" . fundamental-mode)
     (" *Echo Area 1*" . fundamental-mode)
     (" *http www.httpbin.org:80*" . fundamental-mode)
     (" *http www.httpbin.org:80*-820734" . fundamental-mode)
     (" *http www.httpbin.org:80*-914099" . fundamental-mode)
     (" *http www.httpbin.org:80*-945998" . fundamental-mode)
     ("*Help*" . help-mode)
     ("*Completions*" . completion-list-mode))

**Juxt**

    ELISP> (juxt #'buffer-name #'buffer-mode)
    (lambda
      (x)
      (list
       ((funcall #'buffer-name x)
        (funcall #'buffer-mode x))))
    
    
    ELISP> (funcall (juxt #'buffer-file-name  #'buffer-name #'buffer-mode) (current-buffer))
    (nil "*ielm*" inferior-emacs-lisp-mode)
    
    ELISP> (mapcar (juxt #'buffer-name #'buffer-file-name #'buffer-mode) (buffer-list))
    (("*ielm*" nil inferior-emacs-lisp-mode)
     ("*scratch*" nil lisp-interaction-mode)
     ("passgen.py" "/home/tux/bin/passgen.py" python-mode)
     (".bashrc" "/home/tux/.bashrc" sh-mode)
     (" *Minibuf-1*" nil minibuffer-inactive-mode)
     ("init.el" "/home/tux/.emacs.d/init.el" emacs-lisp-mode)
     ("*Backtrace*" nil debugger-mode)
     ("*GNU Emacs*" nil fundamental-mode)
     (" *Minibuf-0*" nil minibuffer-inactive-mode)
     ("*Messages*" nil messages-buffer-mode)
     (" *code-conversion-work*" nil fundamental-mode)
     (" *Echo Area 0*" nil fundamental-mode)
     (" *Echo Area 1*" nil fundamental-mode)
     (" *http www.httpbin.org:80*" nil fundamental-mode)
     (" *http www.httpbin.org:80*-820734" nil fundamental-mode)
     (" *http www.httpbin.org:80*-914099" nil fundamental-mode)
     (" *http www.httpbin.org:80*-945998" nil fundamental-mode)
     ("*Help*" nil help-mode)
     ("*Completions*" nil completion-list-mode))

**Map Juxt**

    (defmacro map-juxt (xs_f xs)
      `(mapcar (juxt ,@xs_f) ,xs))
    
    
    ELISP> (map-juxt (#'buffer-name #'buffer-file-name #'buffer-mode) (buffer-list))
    (("*ielm*" nil inferior-emacs-lisp-mode)
     ("*scratch*" nil lisp-interaction-mode)
     ("passgen.py" "/home/tux/bin/passgen.py" python-mode)
     (".bashrc" "/home/tux/.bashrc" sh-mode)
     (" *Minibuf-1*" nil minibuffer-inactive-mode)
     ("init.el" "/home/tux/.emacs.d/init.el" emacs-lisp-mode)
     ("*Backtrace*" nil debugger-mode)
     ("*GNU Emacs*" nil fundamental-mode)
     (" *Minibuf-0*" nil minibuffer-inactive-mode)
     ("*Messages*" nil messages-buffer-mode)
     ...

**Lambda Function Macro**

    (defmacro $f (f &rest params)
      `(lambda ($) (,f ,@params)))
    
    
    ELISP> ($f - 10 $)
    (lambda
      ($)
      (- 10 $))
    
    ELISP> ($f * (+ 3 $) 5)
    (lambda
      ($)
      (*
       (+ 3 $)
       5))
    
    ELISP> (funcall ($f * (+ 3 $) 5) 10)
    65
    ELISP> (mapcar  ($f * (+ 3 $) 5) '(1 2 3 4 5))
    (20 25 30 35 40)
    
    ELISP>
    ELISP> (mapcar  ($f list (1+ $) (1- $) (log10 $)) '(1 10 100 1000))
    ((2 0 0.0)
     (11 9 1.0)
     (101 99 2.0)
     (1001 999 3.0))

**Partial Application**

    (defmacro $c (f  &rest params)
      `(lambda (__x) (,f ,@params __x)))
    
    ELISP> (defun f (x y z) (+ (* 3 x) (* 2 y) (* 4 z)))
    f
    ELISP> (f 1 2 3)
    19
    ELISP> ($c f 1 2)
    (lambda
      (__x)
      (f 1 2 __x))
    
    ELISP> (mapcar ($c f 1 2) '(1 2 3 4 5))
    (11 15 19 23 27)
    
    ELISP> (mapcar ($c + 1 2) '(1 2 3 4 5))
    (4 5 6 7 8)
    
    ELISP>


## Structures

    ELISP> (defstruct account id name balance)
    account
    ELISP> (make-account :id 3434 :name "John" :balance 1000.34)
    [cl-struct-account 3434 "John" 1000.34]
    
    ELISP> (setq user1 (make-account :id 3434 :name "John" :balance 1000.34))
    [cl-struct-account 3434 "John" 1000.34]
    
    ELISP> (account-name user1)
    "John"
    
    ELISP> (account-id user1)
    3434
    
    ELISP> (account-balance user1)
    1000.34
    
    ;; Test if input is an account object
    ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ELISP> (account-p user1)
    t
    ELISP>
    
    ;; Change Field
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ELISP> (defun withdraw (accc amount)
             (setf (account-balance acc) (- (account-balance acc) amount)))
    withdraw
    
    ELISP> (withdraw user1 300)
    700.34
    ELISP> user1
    [cl-struct-account 3434 "John" 700.34]
    
    ELISP> (withdraw user1 500)
    200.34000000000003
    ELISP> user1
    [cl-struct-account 3434 "John" 200.34000000000003]
    
    ELISP>
    
    ;; Build structure from a list of parameters
    ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ELISP> (defun build-account (id name balance)
             (make-account :id id :name name  :balance balance))
    build-account
    
    ELISP> (build-account 3434 "O' Neil" 35434.23)
    [cl-struct-account 3434 "O' Neil" 35434.23]
    
    ELISP> (apply 'build-account '(3434 "O' Neil" 35434.23))
    [cl-struct-account 3434 "O' Neil" 35434.23]
    
    ELISP>
    
    ELISP> (mapcar (lambda (params) (apply 'build-account params))
                   '(
                     (34423 "O' Neil" 23.2323)
                     (1023  "John Edwards" 1002323.23)
                     (92323 "Mr. Dummy"  2323241.2323)
                     (8723  "John Oliver" 9823)
                     ))
    ([cl-struct-account 34423 "O' Neil" 23.2323]
     [cl-struct-account 1023 "John Edwards" 1002323.23]
     [cl-struct-account 92323 "Mr. Dummy" 2323241.2323]
     [cl-struct-account 8723 "John Oliver" 9823])
    
    ELISP>
    
    ELISP> (defun build-accounts-from-list (list-of-params)
             (mapcar (lambda (params) (apply 'build-account params)) list-of-params))
    build-accounts-from-list
    ELISP>
    
    ELISP> (setq accounts (build-accounts-from-list
                           '(
                             (34423 "O' Neil" 23.2323)
                             (1023  "John Edwards" 1002323.23)
                             (92323 "Mr. Dummy"  2323241.2323)
                             (8723  "John Oliver" 9823)
                             )))
    ([cl-struct-account 34423 "O' Neil" 23.2323]
     [cl-struct-account 1023 "John Edwards" 1002323.23]
     [cl-struct-account 92323 "Mr. Dummy" 2323241.2323]
     [cl-struct-account 8723 "John Oliver" 9823])
    
    ELISP> accounts
    ([cl-struct-account 34423 "O' Neil" 23.2323]
     [cl-struct-account 1023 "John Edwards" 1002323.23]
     [cl-struct-account 92323 "Mr. Dummy" 2323241.2323]
     [cl-struct-account 8723 "John Oliver" 9823])
    
    ELISP> (mapcar #'account-id accounts)
    (34423 1023 92323 8723)
    
    ELISP>
    
    ELISP>
    ELISP> (mapcar #'account-name accounts)
    ("O' Neil" "John Edwards" "Mr. Dummy" "John Oliver")
    
    ELISP>
    
    
    ELISP> (mapcar #'account-balance accounts)
    (23.2323 1002323.23 2323241.2323 9823)
    
    ELISP>


# 宏和元编程


## Quasi-quote

    ;;;; Quasiquote
    
    > `(the product of 3 and 4 is ,(* 3 4))
    (the product of 3 and 4 is 12)
    
    > `("the product of 3 and 4 is" ,(* 3 4))
    ("the product of 3 and 4 is" 12)
    
    > `("the value of (exp 3) is " ,(exp 3) "the value of (sqrt 100) is" ,(sqrt 100))
    ("the value of (exp 3) is " 20.085536923187668 "the value of (sqrt 100) is" 10.0)
    
    > `(a ,a b ,b c ,c d ,d)
    (a 10 b 20 c my-symbol d "a string")
    
    > `((a . ,a) (b . ,b) (c . ,c) (d . ,d))
    ((a . 10)
     (b . 20)
     (c . my-symbol)
     (d . "a string"))
    
    > (setq xs '(sym1 sym2 sym3))
    (sym1 sym2 sym3)
    
    > xs
    (sym1 sym2 sym3)
    
    > `(xs ,xs)
    (xs
     (sym1 sym2 sym3))
    
    > `(xs ,@xs)
    (xs sym1 sym2 sym3)
    
    > `(if (< ,a ,b) ,(+ a 4) ,d)
    (if
        (< 10 20)
        14 "a string")
    
    > (eval `(if (< ,a ,b) ,(+ a 4) ,d))
    14
    >
    
    > (eval `(if (> ,a ,b) ,(+ a 4) ,d))
    "a string"
    
    ;;------------------
    
    > (setq xlist '(1 2 3 4))
    (1 2 3 4)
    
    > (setq ylist '(a b c d e))
    (a b c d e)
    
    > `(xs ,xlist ys ,ylist)
    (xs
     (1 2 3 4)
     ys
     (a b c d e))
    
    > `(xs ,@xlist ys ,@ylist)
    (xs 1 2 3 4 ys a b c d e)


## 宏

**定义lambda函数语法糖:λ**

    (defmacro λ (args body)
      `(lambda ,args ,body))
    
    ELISP> (λ (x) (+ x 3))
    (lambda
      (x)
      (+ x 3))
    ELISP> (mapcar (λ (x) (+ x 3)) '(1 2 3 4 5 6))
    (4 5 6 7 8 9)

**Set variable to nil**

    (defmacro nil! (var)
      `(setq ,var nil))
    
    ELISP> (setq x 10)
    10
    ELISP> x
    10
    ELISP>
    
    ELISP> (nil! x)
    nil
    ELISP> x
    nil
    ELISP>
    
    ELISP> (nil! z)
    nil
    ELISP> z
    nil
    ELISP>

**Create Clojure def, defn and fn special forms**

    (defmacro fn (args body)
      `(lambda ,args ,body))
    
    (defmacro def (name value)
      `(setq ,name ,value))
    
    (defmacro defn (name args body)
      `(defun ,name ,args ,body))
    
    ELISP> (fn (x) (* x x))
    (lambda
      (x)
      (* x x))
    
    ELISP> (mapcar (fn (x) (* x x)) '(1 2 3 4 5))
    (1 4 9 16 25)
    
    ELISP> (def x 1000)
    1000
    ELISP> x
    1000
    ELISP>
    
    ELISP> (defn f (x y z) (+ (* 3 x) (* -4 y) (* 5 z)))
    f
    ELISP> (f 4 5 6)
    22
    ELISP>

&#x2026;&#x2026;


# Emacs API


## Emacs术语

<table>


<colgroup>
<col  class="org-left">

<col  class="org-left">
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Emacs Terminology</th>
<th scope="col" class="org-left">Description</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">Point</td>
<td class="org-left">Cursor position, number of characters from beggining of the buffer to current cursor position.</td>
</tr>


<tr>
<td class="org-left">Buffer</td>
<td class="org-left">Place where the user edit something. Not all buffers are bound to a file.</td>
</tr>


<tr>
<td class="org-left">Mark</td>
<td class="org-left">Beginning of the selected area.</td>
</tr>


<tr>
<td class="org-left">Region</td>
<td class="org-left">Selected area/ text</td>
</tr>


<tr>
<td class="org-left">Frame</td>
<td class="org-left">The current window of emacs</td>
</tr>


<tr>
<td class="org-left">Windows</td>
<td class="org-left">Each frame can be split in sections that Emacs documentation calls windows</td>
</tr>


<tr>
<td class="org-left">Fill</td>
<td class="org-left">Word Wrap</td>
</tr>


<tr>
<td class="org-left">Yank</td>
<td class="org-left">Copy</td>
</tr>


<tr>
<td class="org-left">Kill Region</td>
<td class="org-left">Cut</td>
</tr>


<tr>
<td class="org-left">Kill Ring</td>
<td class="org-left">Clipboard</td>
</tr>


<tr>
<td class="org-left">Kill Buffer</td>
<td class="org-left">Close Buffer</td>
</tr>


<tr>
<td class="org-left">Mode Line</td>
<td class="org-left">Status Bar</td>
</tr>


<tr>
<td class="org-left">Font Locking</td>
<td class="org-left">Syntax Coloring</td>
</tr>
</tbody>
</table>

[Ben's Journal: 11 Concepts The Emacs Newbie Should Master](http://www.blogbyben.com/2011/04/10-concepts-emacs-newbie-should-master.html)


## Emacs API

**API对象**

-   Buffer
-   Temporary Buffer
-   Modes
-   Mode Hooks
-   Mode Map
-   Window
-   Frame
-   Point
-   Process
-   Network Process
-   Minibuffers


## Buffers


### Buffer Attributes

    (buffer-list)
    (current-buffer)
    (mapcar #'buffer-name (buffer-list))
    (mapcar #'buffer-file-name (buffer-list))
    (kill-buffer "init.el")
    (get-buffer "*scratch*")

**列出打开文件**

    (defun opened-files ()
      "list all opened file in current session"
      (interactive)
      (remove-if 'null (mapcar 'buffer-file-name (buffer-list))))
    
    (opened-files)

**创建新buffer**

    ;;
    ;;
    ;; This function returns a buffer named  buffer-or-name.
    ;; The buffer returned does not become the current
    ;; buffer—this function does not change which buffer is current.
    ;;
    
    ELISP> (get-buffer-create "foobar")
    #<buffer foobar>
    ELISP>
    
    ;;
    ;;  Divide the screen in two windows, and switch to the new buffer
    ;;  window
    ;;
    ELISP> (switch-to-buffer-other-window "foobar")
    #<buffer foobar>
    ELISP>
    
    ;; Clean Current Buffer
    ;;
    ELISP> (erase-buffer)
    nil
    ELISP>
    
    ;;  Edit another buffer and go back to the old buffer
    ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ELISP> (defun within-buffer (name function)
             (let (curbuff (current-buffer))
               (switch-to-buffer name)
               (funcall function)
               (switch-to-buffer current-buffer)
               ))
    
    ELISP> (within-buffer "foobar" (lambda () (insert "dummy")))
    #<buffer *ielm*>
    ELISP>
    ELISP> (lambda (x)(* x 10))
    (lambda
      (x)
      (* x 10))
    
    ;;;; Translated from: http://d.hatena.ne.jp/rubikitch/20100201/elispsyntax
    ;;
    ELISP> ;; test-buffer Create a buffer named, to write a variety of content
    (with-current-buffer (get-buffer-create "test-buffer")
      ;; Empty the contents of the buffer
      (erase-buffer)
      ;; /tmp/foo.txt Make the contents inserted
      (insert-file-contents "/etc/fstab")
      ;; Insert a string
      (insert "End\n")
      ;; Write the contents of a buffer to a file
      (write-region (point-min) (point-max) "/tmp/bar.txt"))
    nil
    ELISP>


### Buffer Mode

**Show Buffers Mode**

    ELISP> (defun buffer-mode (buffer-or-string)
             "Returns the major mode associated with a buffer."
             (with-current-buffer buffer-or-string
               major-mode))
    buffer-mode
    
    ELISP> (mapcar (lambda (b)(
                               let
                               (
                                (name (buffer-name b))
                                (type   (buffer-mode (buffer-name b)))
                                )
                               (list name type)
                               ))
                   (buffer-list))
    (("*ielm*" inferior-emacs-lisp-mode)
     ("*SPEEDBAR*" speedbar-mode)
     (" *Minibuf-1*" minibuffer-inactive-mode)
     ("*scratch*" emacs-lisp-mode)
     ("test3.ml" tuareg-mode)
     ("*Help*" help-mode)
     ("*Messages*" messages-buffer-mode)
     ("sbet.ml" tuareg-mode)
     (" *Minibuf-0*" minibuffer-inactive-mode)
     ("test.el" emacs-lisp-mode)
     ...


### Get Buffer Contents / Selection / Line

**Get Buffer Content as String**

    ELISP> (defun buffer-content (name)
             (with-current-buffer name
               (buffer-substring-no-properties (point-min) (point-max))))
    buffer-content
    ELISP>
    
    ELISP> (buffer-content "test3.ml")
    "\n\nlet rec prodlist = function \n    | [] ... "

**Get Selected text in current buffer as string**

    (defun get-selection ()
      "Get the text selected in current buffer as string"
      (interactive)
      (buffer-substring-no-properties (region-beginning) (region-end))
      )

**Get current line in current buffer**

    (defun get-current-line ()
      (interactive)
      "Get current line, where the cursor lies in the current buffer"
      (replace-regexp-in-string "[\n|\s\t]+$" "" (thing-at-point 'line t))
      )


### Search and Replace in the entire Buffer

    (defun replace-regexp-entire-buffer (pattern replacement)
      "Perform regular-expression replacement throughout buffer."
      (interactive
       (let ((args (query-replace-read-args "Replace" t)))
         (setcdr (cdr args) nil)    ; remove third value returned from query---args
         args))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward pattern nil t)
          (replace-match replacement))))


## Point, Region, Line and Buffer


### Point

**Point**

<table>


<colgroup>
<col  class="org-left">

<col  class="org-left">
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Function</th>
<th scope="col" class="org-left">Description</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">(point)</td>
<td class="org-left">Current cursor position</td>
</tr>


<tr>
<td class="org-left">(point-min)</td>
<td class="org-left">Minimum cursor position in current buffer. (always returns 1)</td>
</tr>


<tr>
<td class="org-left">(point-max)</td>
<td class="org-left">Maximum cursor position in current buffer.</td>
</tr>


<tr>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">(line-beginning-position)</td>
<td class="org-left">Point of the beginning of current line.</td>
</tr>


<tr>
<td class="org-left">(line-end-position)</td>
<td class="org-left">Point of the end of current line.</td>
</tr>


<tr>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">(region-beginning)</td>
<td class="org-left">Position of the beginning current region (selected text).</td>
</tr>


<tr>
<td class="org-left">(region-end)</td>
<td class="org-left">Position of the end current region.</td>
</tr>


<tr>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">(bounds-of-thing-at-point <thing>)</td>
<td class="org-left">Returns the cons pair '(beginning . end) position of thing at point.</td>
</tr>
</tbody>
</table>

**Point Interface Functions**

<table>


<colgroup>
<col  class="org-left">

<col  class="org-left">
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Function</th>
<th scope="col" class="org-left">Description</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">(goto-char <point>)</td>
<td class="org-left">Move the cursor to a given point.</td>
</tr>


<tr>
<td class="org-left">(insert <string>)</td>
<td class="org-left">Insert text at current point.</td>
</tr>


<tr>
<td class="org-left">(buffer-substring [pmin] [pmax])</td>
<td class="org-left">Returns the text with properties between the points <pmin> and <pmax>.</td>
</tr>


<tr>
<td class="org-left">(buffer-substring-no-properties [pmin] pmax])</td>
<td class="org-left">Returns the text without properties between the points.</td>
</tr>


<tr>
<td class="org-left">(delete-region [pmin] [pmax])</td>
<td class="org-left">Deletes the text between <pmin> and <pmax>.</td>
</tr>
</tbody>
</table>

    > (point)
    99696
    
    > (point-min)
    1
    
    
    > (point-max)
    185623
    
    >  (line-beginning-position)
    99774
    
    >  (line-end-position)
    99804
    
    > (buffer-substring-no-properties
       (line-beginning-position)
       (line-end-position))
    
    (defun delete-line ()
      (interactive)
      (delete-region  (line-beginning-position)  (line-end-position)))
    
    (defun delete-region ()
      (interactive)
      (delete-region  (region-beginning) (region-end)))
    
    (defun insert-end-of-buffer ()
      (interactive)
    
      ;; Save Current Cursor Position
      ;; and go back to initial positon when
      ;; finish this block
      (save-excursion
        (goto-char (point-max)) ;;; Go to end of buffer
        (insert "Testing insert end of buffer")
        ))


### Thing at Point API

???


## Message / Output

    (message "Hello world")
    (message-box "Time for a break.\nDrink some coffee")


## Files, Directories and Path


### Basic Functions

    ;; Get and Set current directory
    
    ELISP> (pwd)
    "Directory /home/tux/tmp/"
    
    ELISP> (cd "/etc/")
    "/etc/"
    
    ELISP> (pwd)
    "Directory /etc/"
    ELISP>
    
    
    ELISP> (file-name-directory "/etc/hosts")
    "/etc/"
    
    ;; Expand File Name
    ;;
    ELISP> (expand-file-name "~/")
    "/home/tux/"
    ELISP> (expand-file-name ".")
    "/home/tux/tmp"
    ELISP> (expand-file-name "..")
    "/home/tux"
    ELISP>
    
    
    ;;;;; Create a Directory
    ;;;
    ELISP> (mkdir "dummy")
    nil
    ELISP> (mkdir "dummy")
    ** Eval error **  File exists: /home/tux/dummy
    ELISP>
    
    ;;; List Directory
    ;;;;
    ;;;
    ELISP> (directory-files "/home/tux/PycharmProjects/Haskell/")
    ("." ".." ".git" ".gitignore" ".idea" "LICENSE" "Make" "Makefile"
     "README.back.md" "README.html" "README.md" "Test.html" "build.sh" "clean.sh"
     "codes" "dict.sh" "haskell" "ocaml" "papers" "tags" "tmp")


### File Name Components

    ELISP> (file-name-directory "/usr/bin/env")
    "/usr/bin/"
    ELISP>
    
    ELISP> (file-name-nondirectory "/usr/bin/env")
    "env"
    ELISP>
    
    
    ELISP> (file-name-base "/home/foo/zoo1.c")
    "zoo1"
    ELISP> (file-name-base "/home/foo/zoo1.c.back")
    "zoo1.c"


### Read / Write file to a string

**Read File**

    ELISP> (defun file-contents (filename)
      (interactive "fFind file: ")
      (with-temp-buffer
        (insert-file-contents filename) ;; 先将文件内容插入临时buffer，再读取内容
        (buffer-substring-no-properties (point-min) (point-max))))
    
    ELISP> (file-contents "/proc/filesystems")
    "nodev  sysfs\nnodev    rootfs\nnodev   ramfs\nnodev
    bdev\nnodev proc\nnodev cgroup\nnode ...

**Write to File**

    ELISP> (append-to-file "hello world" nil "/tmp/hello.txt")
    nil
    
    ELISP> (file-contents "/tmp/hello.txt")
    "hello world"
    ELISP>


## Window Functions


### Basic Window Functions

    (split-window-horizontally)
    (split-window-vertically)
    (delete-other-windows)
    (switch-to-buffer-other-window "init.el")
    (delete-window)
    (make-frame)
    (frame-list)
    (delete-frame)


### Manipulate Buffer in Another Window

<http://caiorss.github.io/Emacs-Elisp-Programming/Elisp_Programming.html#sec-3-9-2>


### Window Configuration

    (current-window-configuration)
    (setq w (current-window-configuration))
    w
    (set-window-configuration w)

    ;; Screen Resolution
    
    ELISP> (x-display-pixel-width)
    1366
    
    ELISP> (x-display-pixel-height)
    768
    ELISP>
    ELISP>
    
    ;; Resize and Set Emacs Windows position
    ;;
    ;; From: http://uce.uniovi.es/tips/Emacs/mydotemacs.html#sec-41
    ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ELISP> (defun resize-frame ()
      "Set size"
      (interactive)
      (set-frame-width (selected-frame) 100)
      (set-frame-height (selected-frame) 28)
      (set-frame-position (selected-frame) 0 1))
    resize-frame
    ELISP>
    
    ELISP> (resize-frame)
    t
    ELISP>


## OS Interface


### Find the current operating system

<table>


<colgroup>
<col  class="org-left">

<col  class="org-left">
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Value</th>
<th scope="col" class="org-left">Description</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">gnu</td>
<td class="org-left">GNU Hurd system.</td>
</tr>


<tr>
<td class="org-left">gnu/linux</td>
<td class="org-left">GNU/Linux system.</td>
</tr>


<tr>
<td class="org-left">gnu/kfreebsd</td>
<td class="org-left">GNU system with a FreeBSD kernel.</td>
</tr>


<tr>
<td class="org-left">darwin</td>
<td class="org-left">Darwin (GNU-Darwin, Mac OS X)</td>
</tr>


<tr>
<td class="org-left">ms-dos</td>
<td class="org-left">MS-DOS application.</td>
</tr>


<tr>
<td class="org-left">windows-nt</td>
<td class="org-left">native W32 application.</td>
</tr>


<tr>
<td class="org-left">cygwin</td>
<td class="org-left">compiled using the Cygwin library</td>
</tr>
</tbody>
</table>

    system-type
    system-configuration


### Date and Time

    (current-time)
    (insert (format-time-string "%Y-%m-%d")) ;; 2019-12-15
    (insert (format-time-string "%H:%M:%S")) ;; 16:11:04
    (format-time-string "%d/%m/%Y %H:%M:%S" (current-time))


### Call External Commands or Apps

    (call-process "mpd")
    (shell-command-to-string "pwd")


### Environment Variables

    (getenv "PATH")
    (split-string (getenv "PATH") ":")
    (dolist (e (split-string  (getenv "PATH") ":")) (princ (format "%s\n" e)))
    exec-path
    (getenv "HOME")
    (setenv "JAVA_HOME" "/usr/local/java")
    
    system-type
    (eq system-type 'gnu/linux)()
    (dolist (e process-environment) (princ (format "%s\n" e)))


### Process Management

    (process-list)
    (get-process "merlin")
    (mapcar 'process-name (process-list))
    
    ;;;; Buffer Process
    (process-command (get-process "vterm"))
    (process-id (get-process "vterm"))
    (process-buffer (get-process "vterm"))
    (buffer-name (process-buffer (get-process "vterm")))
    (mapcar (lambda (p) (buffer-name (process-buffer p))) (process-list))
    (display-buffer (process-buffer (get-process "vterm")))
    
    ;;;; Start Asyncronous Process
    ;;  Start the process named py, with the buffer named pybff
    ;;  using the command python, /usr/bin/python (on linux)
    (start-process "py" "pybff" "python")
    ;; End the process named py
    (process-send-eof "py")
    
    (process-send-string "py" "print 'Hello world'\n")
    
    ;;;; Get Multiple Fields
    
    (mapcar
     (lambda (p)(list
                 p
                 (process-name p)
                 (process-command p)
                 (list (process-buffer p) (buffer-name (process-buffer p)))
                 (process-id p)
                 (process-status p)
                 ))
     (process-list))


## Interfaces


### Creating Quick Access Menu

    (require 'easymenu)
    
    (easy-menu-define djcb-menu global-map "Utils"
      '("Utils"
        ("Shells" ;; submenu
         ["Ielm   - Emacs Lisp Shell"       (ielm)]
         ["Eshell - Emacs Buitin Shell"    (eshell)]
         ["Native Shell "                  (shell)]
         ["---------------------" nil]
         ["Edit .bashrc" (find-file  "~/.bashrc")]
         ["Edit .profile" (find-file "~/.profile")]
         ["Edit .Xresources" (find-file "~/.Xresources")]
         ["Edit .xsessin"    (find-file "~/.xsession")]
         ["See all GNU MAN pages" ( info)]
         ["See a specific Man Page" (woman)]
    
         );; End of shells menu
    
        ("Emacs /Elisp"  ;; submenu
    
         ["Ielm   - Emacs Lisp Shell"  (ielm)]
         ["Eval buffer"   (eval-buffer) ]
         ["---------------------" nil]
    
         ["Edit  init.el" (find-file  user-init-file)]
         ["Reload init.el" (load-file user-init-file)]
         ["Open .emac.d dir" (find-file "~/.emacs.d")]
         ["List packages"     (list-packages)]
         ["Install package"   (package-install)]
    
         ) ;; End of Emacs / Elisp submenu
        )) ;; End of Custom Menu


### Add Icons to toolbar

<http://caiorss.github.io/Emacs-Elisp-Programming/Elisp_Programming.html#sec-3-11-2>


## Timer


### run-with-timer

    ;;; (run-with-timer SECS REPEAT FUNCTION &rest ARGS)
    (run-with-timer 5 nil
                    (lambda () (message-box "happy hacking emacs!")))
    
    (defun cofee-wait ()
      (interactive)
      (let ((minutes 3))
        (run-with-timer (* 60 minutes)  nil
                        (lambda () (message-box "Coffee done"))
                        )
        (message "Waiting for the cofee")
        ))


## Emacs Modes


### Mode Association with Files

    ;; 列出所有和拓展名相关的mode
    auto-mode-alist
    ;; 列出与一个mode相关的所有拓展名
    (remove-if-not
     (lambda (al) (equal (cdr al) 'web-mode)) auto-mode-alist)
    ;; 为一个mode关联拓展名
    (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))


### Lisp Routines to introspect modes??

    (defun show-doc (function)
      (princ (documentation function)))
    
    (defun mode/show ()
      "  Returns all modes associated with files
    
         To query the file extesions associated with a mode
         use:
             > (mode/ftypes 'markdown-mode)
    
         for example.
      "
      (dolist (m (remove-if #'listp
                            (mapcar #'cdr auto-mode-alist))) (print m)))
    
    (defun mode/ftypes (mode)
      "
      Get all file extension associated with a mode.
    
      Usage:
    
      ELISP> (get-mode-ftypes 'markdown-mode)
      ((\"\\.md\\'\" . markdown-mode)
      (\"\\.text\\'\" . markdown-mode)
      (\"\\.markdown\\'\" . markdown-mode)
    
      "
      (remove-if-not
       (lambda (al)
         (equal (cdr al) mode))
       auto-mode-alist))
    
    ELISP> (mode/ftypes 'clojure-mode)
    (("\\(?:build\\|profile\\)\\.boot\\'" . clojure-mode)
     ("\\.\\(clj\\|dtm\\|edn\\)\\'" . clojure-mode))
    
    ELISP> (mode/ftypes 'scheme-mode)
    (("\\.\\(scm\\|stk\\|ss\\|sch\\)\\'" . scheme-mode)
     ("\\.scm\\.[0-9]*\\'" . scheme-mode)
     ("\\.oak\\'" . scheme-mode))
    
    ELISP> (show-doc #'mode/ftypes)
    
    Get all file extension associated with a mode.
    
    Usage:
    
    ELISP> (get-mode-ftypes 'markdown-mode)
    (("\.md\'" . markdown-mode)
     ("\.text\'" . markdown-mode)
     ("\.markdown\'" . markdown-mode))


### Mode Specific Key Bindings

    (define-key emacs-lisp-mode-map (kbd "<f5>")
      (lambda () (interactive) (message "Hello world")))
    (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)


## Special Variables

    emacs-major-version
    load-path
    window-system
    system-type
    system-configuration
    shell-file-name
    user-full-name
    user-mail-address
    user-init-file
    user-emacs-directory
    exec-directory


## Network

<http://caiorss.github.io/Emacs-Elisp-Programming/Elisp_Programming.html#sec-3-15>


# 正则表达式


## Emacs Regex

**Special characters**

<table>


<colgroup>
<col  class="org-left">

<col  class="org-left">
</colgroup>
<tbody>
<tr>
<td class="org-left">.</td>
<td class="org-left">any character (but newline)</td>
</tr>


<tr>
<td class="org-left">\*</td>
<td class="org-left">previous character or group, repeated 0 or more time</td>
</tr>


<tr>
<td class="org-left">+</td>
<td class="org-left">previous character or group, repeated 1 or more time</td>
</tr>


<tr>
<td class="org-left">?</td>
<td class="org-left">previous character or group, repeated 0 or 1 time</td>
</tr>


<tr>
<td class="org-left">^</td>
<td class="org-left">start of line</td>
</tr>


<tr>
<td class="org-left">$</td>
<td class="org-left">end of line</td>
</tr>


<tr>
<td class="org-left">[…]</td>
<td class="org-left">any character between brackets</td>
</tr>


<tr>
<td class="org-left">[^..]</td>
<td class="org-left">any character not in the brackets</td>
</tr>


<tr>
<td class="org-left">[a-z]</td>
<td class="org-left">any character between a and z</td>
</tr>


<tr>
<td class="org-left">\\</td>
<td class="org-left">prevents interpretation of following special char</td>
</tr>


<tr>
<td class="org-left">\\</td>
<td class="org-left">or</td>
</tr>


<tr>
<td class="org-left">\w</td>
<td class="org-left">word constituent</td>
</tr>


<tr>
<td class="org-left">\b</td>
<td class="org-left">word boundary</td>
</tr>


<tr>
<td class="org-left">\sc</td>
<td class="org-left">character with c syntax (e.g. \s- for whitespace char)</td>
</tr>


<tr>
<td class="org-left">&#xa0;</td>
<td class="org-left">start\end of group</td>
</tr>


<tr>
<td class="org-left">\\<</td>
<td class="org-left">\\> start\end of word</td>
</tr>


<tr>
<td class="org-left">\\\`</td>
<td class="org-left">\\' start\end of buffer</td>
</tr>


<tr>
<td class="org-left">\\1</td>
<td class="org-left">string matched by the first group</td>
</tr>


<tr>
<td class="org-left">\n</td>
<td class="org-left">string matched by the nth group</td>
</tr>


<tr>
<td class="org-left">\\{3\\}</td>
<td class="org-left">previous character or group, repeated 3 times</td>
</tr>


<tr>
<td class="org-left">\\{3,\\}</td>
<td class="org-left">previous character or group, repeated 3 or more times</td>
</tr>


<tr>
<td class="org-left">\\{3,6\\}</td>
<td class="org-left">previous character or group, repeated 3 to 6 times</td>
</tr>
</tbody>
</table>

**POSIX Character classes**

<table>


<colgroup>
<col  class="org-left">

<col  class="org-left">
</colgroup>
<tbody>
<tr>
<td class="org-left">[:digit:]</td>
<td class="org-left">digit, same as [0-9]</td>
</tr>


<tr>
<td class="org-left">[:upper:]</td>
<td class="org-left">letter in uppercase</td>
</tr>


<tr>
<td class="org-left">[:space:]</td>
<td class="org-left">whitespace character, as defined by the syntax table</td>
</tr>


<tr>
<td class="org-left">[:xdigit:]</td>
<td class="org-left">hexadecimal digit</td>
</tr>


<tr>
<td class="org-left">[:cntrl:]</td>
<td class="org-left">control character</td>
</tr>


<tr>
<td class="org-left">[:ascii:]</td>
<td class="org-left">ascii character</td>
</tr>
</tbody>
</table>

**Syntax Classes**

<table>


<colgroup>
<col  class="org-left">

<col  class="org-left">

<col  class="org-left">

<col  class="org-left">
</colgroup>
<tbody>
<tr>
<td class="org-left">\s-</td>
<td class="org-left">whitespace character</td>
<td class="org-left">\s/</td>
<td class="org-left">character quote character</td>
</tr>


<tr>
<td class="org-left">\sw</td>
<td class="org-left">word constituent</td>
<td class="org-left">\s$</td>
<td class="org-left">paired delimiter</td>
</tr>


<tr>
<td class="org-left">\s\_</td>
<td class="org-left">symbol constituent</td>
<td class="org-left">\s'</td>
<td class="org-left">expression prefix</td>
</tr>


<tr>
<td class="org-left">\s.</td>
<td class="org-left">punctuation character</td>
<td class="org-left">\s<</td>
<td class="org-left">comment starter</td>
</tr>


<tr>
<td class="org-left">\s(</td>
<td class="org-left">open delimiter character</td>
<td class="org-left">\s></td>
<td class="org-left">comment starter</td>
</tr>


<tr>
<td class="org-left">\s)</td>
<td class="org-left">close delimiter character</td>
<td class="org-left">\s!</td>
<td class="org-left">generic comment delimiter</td>
</tr>


<tr>
<td class="org-left">\s"</td>
<td class="org-left">string quote character</td>
<td class="org-left">\s</td>
<td class="org-left">generic string delimiter</td>
</tr>


<tr>
<td class="org-left">\s\\</td>
<td class="org-left">escape character</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
</tr>
</tbody>
</table>

**Emacs X Perl Regex**

<table>


<colgroup>
<col  class="org-left">

<col  class="org-left">

<col  class="org-left">
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Emacs Regex</th>
<th scope="col" class="org-left">Perl Regex</th>
<th scope="col" class="org-left">Description</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">&#xa0;</td>
<td class="org-left">( )</td>
<td class="org-left">Capture group</td>
</tr>


<tr>
<td class="org-left">\\{ \\}</td>
<td class="org-left">{ }</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">\s-</td>
<td class="org-left">\s</td>
<td class="org-left">White space</td>
</tr>


<tr>
<td class="org-left">\\1, \\2, \\3, \\4</td>
<td class="org-left">$1, $2, $3</td>
<td class="org-left">Result of capture: search, replace.</td>
</tr>


<tr>
<td class="org-left">[ ]</td>
<td class="org-left">[ ]</td>
<td class="org-left">Character class</td>
</tr>


<tr>
<td class="org-left">[0-9] or [:digit:]</td>
<td class="org-left">\d</td>
<td class="org-left">Digit from 0 to 9</td>
</tr>


<tr>
<td class="org-left">\b</td>
<td class="org-left">\b</td>
<td class="org-left">Word boundary</td>
</tr>


<tr>
<td class="org-left">\w</td>
<td class="org-left">\w</td>
<td class="org-left">Word character</td>
</tr>
</tbody>
</table>


## Regex Commands

<table>


<colgroup>
<col  class="org-left">

<col  class="org-left">
</colgroup>
<tbody>
<tr>
<td class="org-left">C-M-s</td>
<td class="org-left">incremental forward search matching regexp</td>
</tr>


<tr>
<td class="org-left">C-M-r</td>
<td class="org-left">incremental backward search matching regexp</td>
</tr>
</tbody>
</table>

**Buffer Commands**

<table>


<colgroup>
<col  class="org-left">

<col  class="org-left">
</colgroup>
<tbody>
<tr>
<td class="org-left">M-x replace-regexp</td>
<td class="org-left">replace string matching regexp</td>
</tr>


<tr>
<td class="org-left">M-x query-replace-regexp</td>
<td class="org-left">same, but query before each replacement</td>
</tr>


<tr>
<td class="org-left">M-x align-regexp</td>
<td class="org-left">align, using strings matching regexp as delimiters</td>
</tr>


<tr>
<td class="org-left">M-x highlight-regexp</td>
<td class="org-left">highlight strings matching regexp</td>
</tr>


<tr>
<td class="org-left">M-x grep</td>
<td class="org-left">call unix grep command and put result in a buffer</td>
</tr>


<tr>
<td class="org-left">M-x lgrep</td>
<td class="org-left">user-friendly interface to the grep command</td>
</tr>


<tr>
<td class="org-left">M-x rgrep</td>
<td class="org-left">recursive grep</td>
</tr>


<tr>
<td class="org-left">M-x dired-do-copy-regexp</td>
<td class="org-left">copy files with names matching regexp</td>
</tr>


<tr>
<td class="org-left">M-x dired-do-rename-regexp</td>
<td class="org-left">rename files matching regexp</td>
</tr>


<tr>
<td class="org-left">M-x find-grep-dired</td>
<td class="org-left">display files containing matches for regexp with Dired</td>
</tr>
</tbody>
</table>

**Line Commands**

<table>


<colgroup>
<col  class="org-left">

<col  class="org-left">

<col  class="org-left">
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Command (M-x command)</th>
<th scope="col" class="org-left">Alias</th>
<th scope="col" class="org-left">Description</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">keep-lines</td>
<td class="org-left">delete-non-matching-lines</td>
<td class="org-left">Delete all lines except those containing matches</td>
</tr>


<tr>
<td class="org-left">flush-lines</td>
<td class="org-left">delete-matching-lines</td>
<td class="org-left">Delete lines containing matches</td>
</tr>


<tr>
<td class="org-left">highlight-lines-matching-regexp</td>
<td class="org-left">hi-lock-line-face-buffer</td>
<td class="org-left">Highlight lines matching regexp</td>
</tr>


<tr>
<td class="org-left">occur</td>
<td class="org-left">list-matching-lines</td>
<td class="org-left">Show lines containing a match</td>
</tr>


<tr>
<td class="org-left">multi-occur</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">Show lines in all buffers containing a match</td>
</tr>


<tr>
<td class="org-left">how-many</td>
<td class="org-left">count-matches</td>
<td class="org-left">Count the number of strings matching regexp</td>
</tr>
</tbody>
</table>


## Regex Functions


### match-string


### match-end


### match-beginning


### re-search


### re-search-forward


### replace-string-in-regexp


### replace-string


## Build regex interactively

M-x re-builder

M-x query-replace-regexp


## Emacs Regex rx-notation

    (require 'rx)
    
    ;;  (rx <patterns>)
    
    ELISP> (rx digit)
    "[[:digit:]]"
    
    ELISP> (rx-to-string '(or "foo" "bar"))
    "\\(?:\\(?:bar\\|foo\\)\\)"

<table>


<colgroup>
<col  class="org-left">

<col  class="org-left">

<col  class="org-left">
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Description</th>
<th scope="col" class="org-left">rx notation</th>
<th scope="col" class="org-left">Emacs regex</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">Beginning of Line</td>
<td class="org-left">bol</td>
<td class="org-left">^</td>
</tr>


<tr>
<td class="org-left">End of Line</td>
<td class="org-left">eol</td>
<td class="org-left">$</td>
</tr>


<tr>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">Begining of String</td>
<td class="org-left">bos</td>
<td class="org-left">\\\\\`</td>
</tr>


<tr>
<td class="org-left">End of String</td>
<td class="org-left">eos</td>
<td class="org-left">`\\'`</td>
</tr>


<tr>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">Beginning of Word</td>
<td class="org-left">bow</td>
<td class="org-left">\\\\<</td>
</tr>


<tr>
<td class="org-left">End of Word</td>
<td class="org-left">eow</td>
<td class="org-left">\\\\></td>
</tr>


<tr>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">Digit 0 to 9</td>
<td class="org-left">digit</td>
<td class="org-left">\lbr\lbr:digit:\rbr\rbr</td>
</tr>


<tr>
<td class="org-left">Hexadecimal digit</td>
<td class="org-left">hex</td>
<td class="org-left">\lbr\lbr:xdigit:\rbr\rbr</td>
</tr>


<tr>
<td class="org-left">Match ascii character</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">Match anything lower case</td>
<td class="org-left">lower</td>
<td class="org-left">\lbr\lbr:lower:\rbr\rbr</td>
</tr>


<tr>
<td class="org-left">Match anything upper case</td>
<td class="org-left">upper</td>
<td class="org-left">\lbr\lbr:upper:\rbr\rbr</td>
</tr>


<tr>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">word</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">\sw</td>
</tr>
</tbody>
</table>

*example*

    ELISP> (require 'rx)
    rx
    
    ELISP> (rx (+ digit))
    "[[:digit:]]+"
    
    ELISP> (rx digit (+ digit))
    "[[:digit:]][[:digit:]]+"
    
    ELISP> (rx bol (+ digit) eol)
    "^[[:digit:]]+$"
    
    ELISP> (rx (zero-or-more digit))
    "[[:digit:]]*"
    
    ELISP> (rx (one-or-more digit))
    "[[:digit:]]+"
    
    ELISP> (rx (or "cat" "rat" "dog"))
    "\\(?:cat\\|dog\\|rat\\)"
    
    ;; (replace-regexp-in-string REGEXP REP STRING
    ;;      &optional FIXEDCASE LITERAL SUBEXP START)
    
    ELISP> (replace-regexp-in-string
              (rx (or "cat" "rat" "dog"))
              ""
              "cat cata rat rat dograt dog cat2334 23rat2")
    " a     2334 232"
    
    ;; Replaces only in the beggining of line
    ;;
    ELISP>  (replace-regexp-in-string
              (rx bol (or "cat" "rat" "dog"))
              ""
              "cat cata rat rat dograt dog cat2334 23rat2")
    " cata rat rat dograt dog cat2334 23rat2"
    
    
    ELISP>  (replace-regexp-in-string
              (rx bow (or "cat" "rat" "dog") eow)
              ""
              "cat cata rat rat dograt dog cat2334 23rat2")
    " cata   dograt  cat2334 23rat2"
    
    ELISP>  (rx bow (or "cat" "rat" "dog") eow)
    "\\<\\(?:cat\\|dog\\|rat\\)\\>"
    
    ;;  Removes all whitespaces
    ;;
    ELISP>  (replace-regexp-in-string
              (rx (* whitespace))
              ""
              "cat cata rat rat dograt dog cat2334 23rat2")
    
    "catcataratratdogratdogcat233423rat"
    
    ELISP>  (replace-regexp-in-string
              (rx (* whitespace))
              ""
              "cat cata rat rat dograt dog cat2334 23rat2")
    
    "catcataratratdogratdogcat233423rat"
    
    ;; Capture group
    ;;
    ELISP>  (replace-regexp-in-string
              (rx (submatch bow (or "cat" "rat" "dog") eow))
              "(\\1)"
              "cat cata rat rat dograt dog cat2334 23rat2")
    
    "(cat) cata (rat) (rat) dograt (dog) cat2334 23rat2"
    
    ELISP> (rx (submatch bow (or "cat" "rat" "dog") eow))
    "\\(\\<\\(?:cat\\|dog\\|rat\\)\\>\\)"

# Color Scheme

<http://caiorss.github.io/Emacs-Elisp-Programming/Elisp_Programming.html#sec-5>

# Key Bindings

<http://caiorss.github.io/Emacs-Elisp-Programming/Elisp_Programming.html#sec-6>
