---
title: "实用文本处理函数"
date: 2023-08-19
draft: false
categories: ["elisp"]
comment: false
---

# Table of Contents

1.  [搜索](#org75943d5)
2.  [替换](#org06705ff)
3.  [行操作](#org1a7b191)
4.  [块分割](#org33d2a55)

总结了一些自己平时用的实用的文本操作的函数。


<a id="org75943d5"></a>

# 搜索

1.  根据正则 REGEXP 搜索文本 STRING，返回匹配的数量
```
    (defun tps-search-count (string regexp)
      (let ((start 0)
            (count 0))
        (save-match-data
          (while (string-match regexp string start)
            (cl-incf count)
            (setq start (match-end 0))))
        count))
    
    (tps-search-count "Treat a man as he is and will remain as he is; treat a man as he can and should be, he shall become as he can and should be." "he")
    ;; 5
```
2.  根据正则 REGEXP 搜索文本 STRING，返回所有匹配的位置信息
```
    (defun tps-search (string regexp)
      "Return the matched data after searching STRING by REGEXP."
      (let ((start 0)
            match-seq)
        (save-match-data
          (while (string-match regexp string start)
            (setq match-seq (append match-seq (list (match-data))))
            (setq start (match-end 0))))
        match-seq))
    
    (tps-search "Treat a man as he is and will remain as he is; treat a man as he can and should be, he shall become as he can and should be." "is")
    ;; ((18 20) (43 45))
```
3.  根据正则 REGEXP 搜索文本 STRING，返回指定位置 NTH 的匹配项的位置信息。NTH 从1开始正数，-1开始倒数。
```
    (defun tps-search-nth (string regexp nth)
      (let* ((count (tps-search-count string regexp))
             (list (tps-search string regexp))
             (i (if (< nth 0)
                    (+ count nth)
                  (1- nth))))
        (nth i list)))
    
    (tps-search-nth "Treat a man as he is and will remain as he is; treat a man as he can and should be, he shall become as he can and should be." "he" 3)
    ;; (62 64)
    (tps-search-nth "Treat a man as he is and will remain as he is; treat a man as he can and should be, he shall become as he can and should be." "he" -2)
    ;; (84 86)
```
4.  根据正则 REGEXP 搜索文本 STRING，返回匹配项前后分别符合 BEFORE, AFTER 正则的结果的位置信息
```
    (defun string-match--before (string point regexp)
      (save-match-data
        (with-temp-buffer
          (insert string)
          (goto-char (1+ point))
          (looking-back (or regexp "") 0))))
    
    (defun string-match--after (string point regexp)
      (save-match-data
        (with-temp-buffer
          (insert string)
          (goto-char (1+ point))
          (looking-at (or regexp "")))))
    
    (defun tps-search-around (string regexp before after)
      (let ((list (tps-search string regexp)))
        (seq-filter (lambda (pair)
                      (and (string-match--before string (car pair) (or before ""))
                           (string-match--after string (cadr pair) (or after ""))))
                    list)))
    
    (tps-search-around
     "Treat a man as he is and will remain as he is; treat a man as he can and should be, he shall become as he can and should be."
     "he" nil nil)
    ;; ((15 17) (40 42) (62 64) (84 86) (103 105))
    
    (tps-search-around
     "Treat a man as he is and will remain as he is; treat a man as he can and should be, he shall become as he can and should be."
     "he" "man as " " can")
    ;; ((62 64))

```
<a id="org06705ff"></a>

# 替换

1.  根据正则 REGEXP 搜索文本 STRING，替换所有匹配项为 REPL
```
    (defun tps-replace (string regexp repl)
      (let ((start 0))
        (save-match-data
          (while (string-match regexp string start)
            (setq string (replace-match repl nil nil string))
            (setq start (+ (match-beginning 0) (length repl)))))
        string))
    
    (tps-replace
     "Treat a man as he is and will remain as he is; treat a man as he can and should be, he shall become as he can and should be."
     "he" "HE")
    ;; Treat a man as HE is and will remain as HE is; treat a man as HE can and should be, HE shall become as HE can and should be.
```
2.  根据正则 REGEXP 搜索文本 STRING，替换第 NTH 个匹配项为 REPL。NTH 从1开始正数，-1开始倒数。
```
    (defun tps-replace-nth (string regexp repl nth)
      (let ((count (tps-search-count string regexp))
            (start 0)
            (i 0))
        (save-match-data
          (while (string-match regexp string start)
            (when (= i (if (< nth 0)
                           (+ count nth)
                         (1- nth)))
              (setq string (replace-match repl nil nil string)))
            (cl-incf i)
            (setq start (+ (match-beginning 0) (length repl)))))
        string))
    
    (tps-replace-nth
     "Treat a man as he is and will remain as he is; treat a man as he can and should be, he shall become as he can and should be." "he" "He" 3)
    ;; Treat a man as he is and will remain as he is; treat a man as He can and should be, he shall become as he can and should be.
    
    (tps-replace-nth
     "Treat a man as he is and will remain as he is; treat a man as he can and should be, he shall become as he can and should be." "he" "He" -1)
    ;; Treat a man as he is and will remain as he is; treat a man as he can and should be, he shall become as He can and should be.
```
3.  根据正则 REGEXP 搜索文本 STRING，替换匹配项前后分别符合 BEFORE, AFTER 正则的结果为 REPL。
```
    (defun tps-replace-around (string regexp repl before after)
      (let ((start 0))
        (save-match-data
          (while (string-match regexp string start)
            (when (and (string-match--before string (match-beginning 0) before)
                       (string-match--after string (match-end 0) after))
              (setq string (replace-match repl nil nil string)))
            (setq start (+ (match-beginning 0) (length repl)))))
        string))
    
    (tps-replace-around
     "Treat a man as he is and will remain as he is; treat a man as he can and should be, he shall become as he can and should be."
     "he" "HE" "as " " can")
    ;; Treat a man as he is and will remain as he is; treat a man as HE can and should be, he shall become as HE can and should be.
```
4.  根据 PAIRS 批量替换文本 STRING。PAIRS 是一个搜索项和替换项的列表。
```
    (defun tps-batch-replace (string pairs)
      (dolist (pair pairs)
        (setq string (tps-replace string (car pair) (cadr pair))))
      string)
    
    (tps-batch-replace
     "Treat a man as he is and will remain as he is; treat a man as he can and should be, he shall become as he can and should be."
     '(("he" "she") ("man" "woman")))
    ;; Treat a woman as she is and will remain as she is; treat a woman as she can and should be, she shall become as she can and should be.
```
5.  根据正则 REGEXP 搜索文本 STRING，将匹配项按照替换列表 REPL-SEQ 循环替换为新的文本列表。如果 SEPARATOR 不为空，将文本列表用 SEPARATOR 连接成字符串。对于数字类型的替换，支持语法糖，比如字符串 "{01..03}" 对应列表 ("01" "02" "03")。
```
    (defun tps--loop-parse (string)
      (save-match-data
        (when-let* ((_ (string-match "{[0-9]+\\.\\.[0-9]+}" string))
                    (pattern (match-string 0 string))
                    (pair (split-string (string-trim pattern "{" "}") "\\.\\." t "[ ]*"))
                    (from-len (length (car pair)))
                    (new-seq (mapcar (lambda (number)
                                       (string-pad (number-to-string number) from-len ?0 'start))
                                     (number-sequence (string-to-number (car pair))
                                                      (string-to-number (cadr pair))))))
          (save-match-data
            (string-match "{[0-9]+\\.\\.[0-9]+}" string)
            (mapcar (lambda (newtext)
                      (replace-match newtext nil nil string))
                    new-seq)))))
    
    (defun tps-replace-loop (string regexp repl-seq &optional separator)
      (let* ((repl-seq (if (stringp repl-seq)
                           (tps--loop-parse repl-seq)
                         repl-seq))
             (str-lst (mapcar (lambda (repl)
                                (tps-replace string regexp repl))
                              repl-seq)))
        (if separator
            (string-join str-lst separator)
          str-lst)))
    
    (tps-replace-loop "select * from jd.base_table_01;" "01" '("01" "02" "03") "\n")
    ;; select * from base_table_01;
    ;; select * from base_table_02;
    ;; select * from base_table_03;
    
    (tps-replace-loop
     "select count(1) from jd.base_table_01;" "base_table_01" "newtable_{01..04}" "\n")
    ;; select count(1) from jd.newtable_01;
    ;; select count(1) from jd.newtable_02;
    ;; select count(1) from jd.newtable_03;
    ;; select count(1) from jd.newtable_04;

```
<a id="org1a7b191"></a>

# 行操作

1.  获取所行文本 STRING 的第 COL-NUM 列。如果不指定 COL-SEP, ROW-SEP, 默认的列分隔符是 \`tps-default-col-sep', 默认的行分隔符是 \`tps-default-row-sep'。
```
    (defvar tps-default-row-sep "\n")
    (defvar tps-default-col-sep "[ ]+")
    (defun tps-get-column (string col-num &optional col-sep row-sep)
      (let* ((row-sep (or row-sep tps-default-row-sep))
             (col-sep (or col-sep tps-default-col-sep))
             (rows (split-string string row-sep t))
             (max-col-num (apply #'max (mapcar
                                        (lambda (row-str)
                                          (length (split-string row-str col-sep t)))
                                        rows)))
             (list (mapcar (lambda (row-str)
                  (let* ((row-col-lst (split-string row-str col-sep t))
                         (col-num (if (< col-num 0)
                                      (+ (1+ max-col-num) col-num)
                                    col-num)))
                    (nth (1- col-num) row-col-lst)))
                           rows))
             (not-all-nil (seq-some (lambda (data)
                                      (not (null data)))
                                    list)))
        (when not-all-nil
          (string-join list row-sep))))
    
    (tps-get-column
     "happy hacking emacs.
    happy hacking vim.
    happy hacking vscode.
    happy hacking atom." 3)
    ;; emacs.
    ;; vim.
    ;; vscode.
    ;; atom.
    
    (tps-get-column
     "1,2,3,4
    5,6,7,8
    9,10,11,12" 2 ",")
    ;; 2
    ;; 6
    ;; 10
```
2.  将多行文本 STRING 用 CONCAT-SEP 连接起来。如果 ROW-SEP 为空，默认使用 \`tps-default-row-sep' 分割行。
```
    (defvar tps-default-row-sep "\n")
    (defun tps-concat-row (string concat-sep &optional row-sep)
      (let* ((row-sep (or row-sep tps-default-row-sep))
             (rows (split-string string row-sep)))
        (string-join rows concat-sep)))
    
    (tps-concat-row
     "happy hacking emacs
    happy hacking vim
    happy hacking vscode
    happy hacking atom"
     ", ")
    ;; happy hacking emacs, happy hacking vim, happy hacking vscode, happy hacking atom
```

<a id="org33d2a55"></a>

# 块分割

按照正则 REGEXP 分割文本 STRING 为多个"块"的列表。EXCLUDE 不为空时，分割的块不包含分隔符本身; SEPARATOR 不为空时，将列表用 SEPARATOR 连接为字符串。
```
    (defun split-block (string regexp &optional exclude separator)
      (let ((start 0)
            result)
        (while (string-match regexp string start)
          (let* ((lst (match-data 0))
                 (beg (nth 0 lst))
                 (end (nth 1 lst))
                 block)
            (when exclude
              (setq beg (1+ end)))
            (if (string-match regexp string end)
                (setq end (1- (nth 0 (match-data 0))))
              (setq end (length string)))
            (setq block (substring string beg end))
            (setq result (append result (list block)))
            (setq start end)))
        (if separator
            (string-join result separator)
          result)))
    
    (split-block
      "--------
    happy hacking emacs1
    happy hacking vim1
    happy hacking vscode1
    --------
    happy hacking emacs2
    happy hacking vim2
    happy hacking vscode2
    --------
    happy hacking emacs3
    happy hacking vim3
    happy hacking vscode3"
      "^--+")
    
    ;; ("--------
    ;; happy hacking emacs1
    ;; happy hacking vim1
    ;; happy hacking vscode1" "--------
    ;; happy hacking emacs2
    ;; happy hacking vim2
    ;; happy hacking vscode2" "--------
    ;; happy hacking emacs3
    ;; happy hacking vim3
    ;; happy hacking vscode3")
```
