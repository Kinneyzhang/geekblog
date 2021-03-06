---
title: "Emacs Hack - 通过列表数据创建表格"
date: 2020-08-19
draft: false
categories: ["Happy Hacking Emacs"]
tags: ["elisp", "orgmode"]
comment: false
---

# 问题描述

OrgMode内置的创建表格的函数是 `org-table-create` , 传入"列数x行数"参数即可生成特定行数、列数的表格。这种交互函数在编写org文档时很实用，但在代码中却显得鸡肋。因为在代码中，我们通常希望表格和数据可以一起生成，而不是手动添加数据。

我在折腾 gk-habit.el 时，就产生了这样的需求：生成习惯的月度打卡视图。就像下面这个样子:

<table>


<colgroup>
<col  class="org-left">

<col  class="org-left">

<col  class="org-left">

<col  class="org-left">

<col  class="org-left">

<col  class="org-left">

<col  class="org-left">
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Sun</th>
<th scope="col" class="org-left">Mon</th>
<th scope="col" class="org-left">Tue</th>
<th scope="col" class="org-left">Wed</th>
<th scope="col" class="org-left">Thu</th>
<th scope="col" class="org-left">Fri</th>
<th scope="col" class="org-left">Sat</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">1</td>
</tr>


<tr>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">--</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="org-left">2</td>
<td class="org-left">3</td>
<td class="org-left">4</td>
<td class="org-left">5</td>
<td class="org-left">6</td>
<td class="org-left">7</td>
<td class="org-left">8</td>
</tr>


<tr>
<td class="org-left">--</td>
<td class="org-left">--</td>
<td class="org-left">--</td>
<td class="org-left">--</td>
<td class="org-left">--</td>
<td class="org-left">--</td>
<td class="org-left">✔</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="org-left">9</td>
<td class="org-left">10</td>
<td class="org-left">11</td>
<td class="org-left">12</td>
<td class="org-left">13</td>
<td class="org-left">14</td>
<td class="org-left">15</td>
</tr>


<tr>
<td class="org-left">✘</td>
<td class="org-left">✔</td>
<td class="org-left">✔</td>
<td class="org-left">✘</td>
<td class="org-left">✔</td>
<td class="org-left">✔</td>
<td class="org-left">✘</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="org-left">16</td>
<td class="org-left">17</td>
<td class="org-left">18</td>
<td class="org-left">19</td>
<td class="org-left">20</td>
<td class="org-left">21</td>
<td class="org-left">22</td>
</tr>


<tr>
<td class="org-left">✔</td>
<td class="org-left">✔</td>
<td class="org-left">✔</td>
<td class="org-left">✔</td>
<td class="org-left">○</td>
<td class="org-left">○</td>
<td class="org-left">○</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="org-left">23</td>
<td class="org-left">24</td>
<td class="org-left">25</td>
<td class="org-left">26</td>
<td class="org-left">27</td>
<td class="org-left">28</td>
<td class="org-left">29</td>
</tr>


<tr>
<td class="org-left">○</td>
<td class="org-left">○</td>
<td class="org-left">○</td>
<td class="org-left">○</td>
<td class="org-left">○</td>
<td class="org-left">○</td>
<td class="org-left">○</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="org-left">30</td>
<td class="org-left">31</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">○</td>
<td class="org-left">○</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
</tr>
</tbody>
</table>

# 思路分析

解决这个问题的关键是把握几个操作org表格的函数 `org-create-table`, `org-table-next-field`, `org-table-insert-hline`, `org-table-kill-row`&#x2026; 然后就是在表格创建的过程中依次插入数据。用于创建表格的每一行的数据用列表表示，分隔线用 `hl` 对象表示。

-   首先创建一个一行n列的表格，因为 org table 的函数只有在表格内才能使用。其中n为每行元素的个数。
-   在数据列表中循环，如果元素是一个list，表示是数据。继续在该list中循环，插入数据后跳到下一个单元格(注意数字要转为字符串)。
-   如果元素是 `hl` 对象，表示是分隔线，直接插入一行分割线 `(org-table-insert-hline 1)` 。
-   每一行插入最后一个数据后会执行“跳到下一个单元格”的操作，当右边没有单元格时会自动插入新的一行。
-   因此，最后会多出一行，用 `org-table-kill-row` 函数删掉。

# 代码实现

    (defun gk-org-table-create (LIST)
      "Create org table from a LIST form at point."
      (let ((column (catch 'break
                      (dolist (row-data LIST)
                        (when (listp row-data)
                          (throw 'break (length row-data))))))
            (beg (point)))
        (org-table-create (concat (number-to-string column) "x1"))
        (goto-char beg)
        (when (org-at-table-p)
          (org-table-next-field)
          (dotimes (i (length LIST))
            (let ((row-data (nth i LIST)))
              (if (listp row-data)
                  (dolist (data row-data)
                    (cond
                     ((numberp data)
                      (insert (number-to-string data)))
                     ((null data)
                      (insert ""))
                     (t (insert data)))
                    (org-table-next-field))
                (when (equal 'hl row-data)
                  (org-table-insert-hline 1)))
              (when (= i (1- (length LIST)))
                (org-table-kill-row))))))
      (forward-line))

# 使用案例

    (gk-org-table-create
     '(("n1" "n2" "n3" "n4" "n5")
       hl
       (1 2 3 4 5)
       (6 7 8 9 10)
       hl
       ("c1" "c2" "c3" "c4" "c5")
       hl
       ("a" "b" "c" "d" "e")
       ("f" "g" "h" "i" "j")))

    | n1 | n2 | n3 | n4 | n5 |
    |----+----+----+----+----|
    | 1  | 2  | 3  | 4  | 5  |
    | 6  | 7  | 8  | 9  | 10 |
    |----+----+----+----+----|
    | c1 | c2 | c3 | c4 | c5 |
    |----+----+----+----+----|
    | a  | b  | c  | d  | e  |
    | f  | g  | h  | i  | j  |

实现开篇提出的习惯打卡的视图是个更复杂的问题，这里涉及到了不同月份的天数不同，起始星期不同，以及每天对应的打卡状态不同等问题。解决了这些问题后，将得到的数据整合成 `gk-org-table-create` 合法的数据列表形式即可生成相应的表格。相关代码在[这里](https://github.com/Kinneyzhang/gk-habit/blob/master/gk-habit.el)。

如果你有更简单、漂亮的实现，欢迎留言探讨～
