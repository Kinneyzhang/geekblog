---
title: "Emacs Hack - 分割文件相似内容"
date: 2020-06-28
draft: false
categories: ["Happy Hacking Emacs"]
tags: ["elisp"]
comment: false
---

# 实际问题

将一个文件中的相似块分割成若干个文件。比如下面这个例子：

    第一回: 灵根育孕源流出 心性修持大道生
      这是第一段内容.....
      这是第二段内容.....
      这是第三段内容.....
      更多...
    
    第二回: 悟彻菩提真妙理 断魔归本合元神
      这是第一段内容.....
      这是第二段内容.....
      这是第三段内容.....
      更多...
    
    第三回: 四海千山皆拱伏 九幽十类尽除名
      这是第一段内容.....
      这是第二段内容.....
      这是第三段内容.....
      更多...

假设有一个长篇小说txt文件，每一回有相似的结构。要解决的问题是：如何将每一回分割成单独的文件？文件名是每一回的标题，文件的内容就是每一回的内容。

# 解决思路

解决这种实际的问题，我的思路一般是：用人的思维找到直观的解决办法。然后，将人的思维转换成对应的代码逻辑，比如：运用什么控制语句？使用什么数据结构？涉及什么特殊算法？等等。

首先我们从人的思维出发考虑如何解决这个问题。最直接的方法是：根据开头“第一回:”这几个字确定这一回的标题，然后确定接下来直到“第二回:”之间的部分为第一回的内容。然后将第一回的内容写到以标题命名的文件中。接下里，以此类推，直到最后没有内容。

接下来，思考如何将这个过程转化成代码的逻辑。

显然，这里是循环判断的逻辑，循环的范围是从第一行到最后一行。分析过程如下：

1.  搜索第一行，是标题行，记录标题并移到下一行。
2.  搜索这一行，不是标题行，记录行首的位置start。判断下一行是否是标题行，不是则移到下一行。
3.  重复过程2，直到当前行的下一行是标题行时，记录本行行首的位置end。
4.  将start和end之间的内容写到以标题命名的文件中。移到下一行。
5.  重复上面的过程1,2,3,4。直到光标到达最后。

# 代码实现

根据上面的分析过程，有如下的代码:

    (defun my-split-file (file)
      (interactive "fchoose a file: ")
      (let ((filedir "~/test-dir/")
            filename start end content)
        (with-current-buffer (get-buffer-create "*split-file*")
          (insert-file-contents file)
          (goto-char (point-min))
          (while (< (point) (point-max))
            (if (search-forward-regexp "^第.+回:" (line-end-position) t)
                (progn 
                  (setq filename (string-trim (thing-at-point 'line))) 
                  (next-line)
                  (beginning-of-line)
                  (setq start (point))) ;; 获取内容的起始位置start
              (save-excursion ;; 保存光标的位置：只判断下一行的情况，不改变实际要操作的光标。
                (next-line)
                (beginning-of-line)
                (when (or (search-forward-regexp "^第.+回:" (line-end-position) t)
                          (= (point) (point-max)))
                  (previous-line)
                  (end-of-line)
                  (setq end (point)) ;; 获取内容的结尾位置end
                  (setq content (buffer-substring-no-properties start end)) ;; 获取start和end之间的内容。
                  (with-temp-file (concat filedir filename ".txt")
                    (insert content)) ;; 写文件
                  ))
              (next-line)
              (beginning-of-line))))))

