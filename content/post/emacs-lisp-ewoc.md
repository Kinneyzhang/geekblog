---
title: "Emacs Lisp - ewoc使用介绍"
date: 2020-08-27
draft: false
categories: ["Happy Hacking Emacs"]
tags: ["elisp", "ewoc"]
comment: false
---

# 介绍

Ewoc 是 Emacs's Widget for Object Collections的简写，它可以根据lisp对象的结构绘制和更新buffer的文本，就像MVC设计模式中的视图层。其中，生成的buffer的文本分为三个部分：特定的头部文本，lisp对象代表的数据元素的文本，特定的底部文本。一个ewoc对象的信息包含在以下内容中：

-   用于产生文本的buffer
-   buffer中文本的起始位置
-   头部和底部文本的内容
-   一个双向链接的结点链，每一个结点包含：
    -   一个数据元素（单个lisp对象）
    -   结点链中上一个和下一个结点的链接
-   一个将数据元素的文本插入到buffer中的打印函数

使用 `ewoc-create` 来定义一个ewoc对象，然后用其他的ewoc函数构建结点的结构并在buffer中显示。一旦在buffer中显示了文本，便可使用其他函数负责buffer的光标位置和结点间的数据通信。

让结点包含数据元素就像给变量设置值。通常结点包含数据元素的行为发生在将结点加入到ewoc对象过程中。可以通过下面两个函数获取和设置数据元素的值：

    (ewoc-data node)
    ⇒ value
    
    (ewoc-set-data node new-value)
    ⇒ new-value

也可以使用lisp对象(list or vector)或其他结构的索引作为数据元素的值。

当数据改变时，buffer中文本会相应的更新。使用 `ewoc-update` 函数更新所有结点，或者用 `ewoc-invalidate` ，使用 `ewoc-map` 函数给所有的结点增加条件判断。同时，可以使用 `ewoc-delete` 或 `ewoc-filter` 函数删除无效的结点并设置新的结点。删除一个结点的同时会删掉该结点在buffer中展示的文本。

# 函数

这一部分的术语中, *ewoc* 和 *node* 分别代表前面提到的两种结构，而 *data* 代表作为数据元素的lisp对象。三者的关系是：ewoc包含node，node包含data。下面详细介绍ewoc的每个函数。

— **ewoc-create** *pretty-printer &optional header footer nosep*

创建并返回一个不包含结点的ewoc对象。 *pretty-printer* 是包含一个数据元素参数的函数，该数据元素的文本值将被插入到buffer中。(使用 `insert` 函数插入，不要用 `insert-before-markers` ，因为它对ewoc包的内部机制有干扰)

通常，该函数会自动在头部文本，底部文本和每一个node的文本后插入新的一行。设置 *nosep* 参数为 non-nil 可以不插入新行，这在需要将整个ewoc的文本显示在一行时很有用。ewoc创建时会在当前buffer下维护文本，所以在调用创建函数前要先切换到目标buffer。

— **ewoc-buffer** *ewoc*

返回维护ewoc文本的buffer。

— **ewoc-get-hf** *ewoc*

返回一个由头部文本和底部文本构成的 cons cell (header . footer)。

— **ewoc-enter-first** *ewoc data*  
— **ewoc-enter-last** *ewoc data*

分别在ewoc的结点链的开头和结尾添加包含数据元素的新结点。

— **ewoc-enter-before** *ewoc node data*  
— **ewoc-enter-after** *ewoc node data*

分别在指定结点的前面和后面添加一个包含数据元素的新结点。

— **ewoc-prev** *ewoc node*  
— **ewoc-next** *ewoc node*

分别返回指定结点的前一个和后一个结点。

— **ewoc-nth** *ewoc n*

返回以0开始的索引n的结点，n为负值时从最后开始索引。n超出范围时返回nil。

— **ewoc-data** *node*

获取并返回node包含的数据元素。

— **ewoc-set-data** *node*

设置node包含的数据元素值为data。

— **ewoc-locate** *ewoc &optional pos guess*

确定并返回包含光标位置的结点。如果ewoc没有结点，返回nil。如果 *pos* 在第一个结点之前，返回第一个结点；在最后一个结点之后，返回最后一个结点。可选参数 *guess* 是有可能在pos附近的结点，它不会改变函数的结果但会让函数执行的更快。

— **ewoc-location** *node*

返回结点的起始位置。

— **ewoc-goto-prev** *ewoc arg*  
— **ewoc-goto-next** *ewoc arg*

分别移动光标到上arg个或下arg个结点。如果已经在第一个结点或ewoc为空, *ewoc-goto-prev* 不移动光标并返回nil，同样 *ewoc-goto-next* 不超过最后一个结点。除了以上特殊情况，函数返回移动到的结点。

— **ewoc-goto-node** *ewoc node*

移动光标到结点的起始位置。

— **ewoc-refresh** *ewoc*

重新生成ewoc的文本。执行过程是：删除header和footer之间的文本，然后为每一个结点依次调用 pretty-printer 函数生成文本。

— **ewoc-invalidate** *ewoc &rest nodes*

和 `ewoc-refresh` 类似，但只更新 *nodes* 列表内的结点而不是所有结点。

— **ewoc-delete** *ewoc &rest nodes*

删除所有 *nodes* 列表内的结点。

— **ewoc-filter** *ewoc predicate &rest args*

为ewoc中的每一个数据元素调用 *predicate* 函数，删除断言为nil的结点。 *args* 是传递给断言函数的参数。

— **ewoc-collect** *ewoc predicate &rest args*

为ewoc中的每一个数据元素调用 *predicate* 函数，返回断言为non-nil的元素列表。元素在列表中的顺序和buffer中一致。 *args* 是传递给断言函数的参数。

— **ewoc-map** *map-function ewoc &rest args*

为ewoc中的每一个数据元素调用 *map-function* , 更新map函数返回为non-nil的结点。 *args* 是传递给map函数的参数。

# 例子

下面是使用ewoc实现的显示颜色组成的例子，颜色组成由buffer中的三个整数组成的向量表示。

    (setq colorcomp-ewoc nil
          colorcomp-data nil
          colorcomp-mode-map nil
          colorcomp-labels ["Red" "Green" "Blue"])
    
    (defun colorcomp-pp (data)
      (if data
          (let ((comp (aref colorcomp-data data)))
            (insert (aref colorcomp-labels data) "\t: #x"
                    (format "%02X" comp) " "
                    (make-string (ash comp -2) ?#) "\n"))
        (let ((cstr (format "#%02X%02X%02X"
                            (aref colorcomp-data 0)
                            (aref colorcomp-data 1)
                            (aref colorcomp-data 2)))
              (samp " (sample text) "))
          (insert "Color\t: "
                  (propertize samp 'face
                              `(foreground-color . ,cstr))
                  (propertize samp 'face
                              `(background-color . ,cstr))
                  "\n"))))
    
    (defun colorcomp (color)
      "Allow fiddling with COLOR in a new buffer.
         The buffer is in Color Components mode."
      (interactive "sColor (name or #RGB or #RRGGBB): ")
      (when (string= "" color)
        (setq color "green"))
      (unless (color-values color)
        (error "No such color: %S" color))
      (switch-to-buffer
       (generate-new-buffer (format "originally: %s" color)))
      (kill-all-local-variables)
      (setq major-mode 'colorcomp-mode
            mode-name "Color Components")
      (use-local-map colorcomp-mode-map)
      (erase-buffer)
      (buffer-disable-undo)
      (let ((data (apply 'vector (mapcar (lambda (n) (ash n -8))
                                         (color-values color))))
            (ewoc (ewoc-create 'colorcomp-pp
                               "\nColor Components\n\n"
                               (substitute-command-keys
                                "\n\\{colorcomp-mode-map}"))))
        (set (make-local-variable 'colorcomp-data) data)
        (set (make-local-variable 'colorcomp-ewoc) ewoc)
        (ewoc-enter-last ewoc 0)
        (ewoc-enter-last ewoc 1)
        (ewoc-enter-last ewoc 2)
        (ewoc-enter-last ewoc nil)))

通过定义改变 colorcomp-data 的值，完成选择过程和按键绑定，这个例子可以拓展成一个颜色选择的组件(MVC模式中的模型)。

    (defun colorcomp-mod (index limit delta)
      (let ((cur (aref colorcomp-data index)))
        (unless (= limit cur)
          (aset colorcomp-data index (+ cur delta)))
        (ewoc-invalidate
         colorcomp-ewoc
         (ewoc-nth colorcomp-ewoc index)
         (ewoc-nth colorcomp-ewoc -1))))
    
    (defun colorcomp-R-more () (interactive) (colorcomp-mod 0 255 1))
    (defun colorcomp-G-more () (interactive) (colorcomp-mod 1 255 1))
    (defun colorcomp-B-more () (interactive) (colorcomp-mod 2 255 1))
    (defun colorcomp-R-less () (interactive) (colorcomp-mod 0 0 -1))
    (defun colorcomp-G-less () (interactive) (colorcomp-mod 1 0 -1))
    (defun colorcomp-B-less () (interactive) (colorcomp-mod 2 0 -1))
    
    (defun colorcomp-copy-as-kill-and-exit ()
      "Copy the color components into the kill ring and kill the buffer.
         The string is formatted #RRGGBB (hash followed by six hex digits)."
      (interactive)
      (kill-new (format "#%02X%02X%02X"
                        (aref colorcomp-data 0)
                        (aref colorcomp-data 1)
                        (aref colorcomp-data 2)))
      (kill-buffer nil))
    
    (setq colorcomp-mode-map
          (let ((m (make-sparse-keymap)))
            (suppress-keymap m)
            (define-key m "i" 'colorcomp-R-less)
            (define-key m "o" 'colorcomp-R-more)
            (define-key m "k" 'colorcomp-G-less)
            (define-key m "l" 'colorcomp-G-more)
            (define-key m "," 'colorcomp-B-less)
            (define-key m "." 'colorcomp-B-more)
            (define-key m " " 'colorcomp-copy-as-kill-and-exit)
            m))

# 参考

[GNU Emacs Lisp Reference Manual - Abstract-Display](https://www.gnu.org/software/emacs/manual/html_mono/elisp.html#Abstract-Display)

