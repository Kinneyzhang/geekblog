---
title: "一个关于抽象的思想实验"
date: 2024-05-29
draft: true
categories: ["计算机科学"]
tags: ["SICP","emacs"]
comment: false
---

Q: 存在一个序列 '(1 2 3 4 5), 求解对其中的每一个元素加1得到的新的序列。

最朴素的想法是通过遍历，对其中的每一个元素加1后，收集到新的序列中，下面是emacs-lisp的实现：
```emacs-lisp
(let (lst)
  (dolist (e '(1 2 3 4 5))
    (setq lst (append lst (list (1+ e)))))
  lst)
```

现在让我们开始抽象！

CASE1: 将“加1”的操作抽象为**任意一般的操作**，将指定的序列'(1 2 3 4 5)抽象为**任意序列**。于是我们得到**“对一个序列中的元素执行同一个操作”**这样普遍的行为。这就是emacs-lisp中的 `mapcar` 函数，它可以对 SEQUENCE 中的元素，执行 FUNCTION 操作，得到一个新的序列。

于是我们可以使用内置的 mapcar 函数来实现我们第一版的 `mymap` 函数:

```emacs-lisp
(defun mymap (function sequence)
  (mapcar function sequence))
```

```(mymap '1+ '(1 2 3 4 5)) ;; (2 3 4 5 6)=> ```

让我们继续抽象!

CASE2: 上述的 `mymap` 的特点是操作序列中的**所有**元素，**而“所有”是“部分”的一种特殊情况**。于是可以抽象出更加一般的情况：对一个序列中的任意部分元素执行同一操作。我们如何确定满足我们需求的“部分元素”？只需让它满足一个判断函数，即：只操作序列中满足一个判断函数的部分的元素，并收集所有的值形成新的序列。

PRED 断言函数是可选的，这样便可以兼容CASE1。

```emacs-lisp
(defun mymap (function sequence &optional pred)
  (mapcar (lambda (el)
            (if (and pred (funcall pred el))
                (funcall function el)
              el))
          sequence))
```

例子：只对序列中的奇数加一：
```(mymap '1+ '(1 2 3 4 5) 'oddp) ;; => (2 2 4 4 6)```

值得一提的是：如果我们传入 identity 函数 (Return the ARGUMENT unchanged)，便可退化为 CASE1。这也说明了，CASE1 是 CASE2 的特殊情况，CASE2 是对 CASE1 的进一步抽象。

```(mymap '1+ '(1 2 3 4 5) 'identity) ;; => (2 3 4 5 6)```

继续抽象

CASE3: 上述的 `mymap` 在对部分元素执行完操作后，会收集所有的元素，形成新的列表。同理，这里我们也可以将“所有元素”抽象为“满足条件的部分元素”。


