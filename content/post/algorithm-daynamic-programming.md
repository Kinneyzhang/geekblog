---
title: "“动态规划”总结"
date: 2023-11-09
draft: true
categories: ["数据结构与算法"]
tags: ["算法","动态规划"]
comment: false
---

动态规划是计算机中一种经典的解决复杂问题的算法。

动态规划的核心思想是：将复杂问题拆解为一个个子问题，求解子问题后，将子问题的结果保存起来，供后续计算使用，减少重复计算。

分治：最优子结构
动规：最优子结构、重叠子问题
贪心：最优子结构、重叠子问题、贪心选择性质

动态规划大多用于解决最优化问题（题目表述形如“使某个量最优”）。
它通过用空间换时间来实现算法时间复杂度的优化。
适用范围
分治：最优子结构
动规：最优子结构、重叠子问题
贪心：最优子结构、重叠子问题、贪心选择性质
分治
为了解决一个问题，把它分解成若干个与此问题相似的子问题。
这样的“能分解”的性质就叫做最优子结构（又称无后效性）。很多问题都可以满足这个性质。
动态规划
动态规划是分治的特例。采用分治思想得到的子问题“不一定需要再次求解”，因为之前可能已经计算过相同的子问题了。这样的性质叫做重叠子问题。
贪心
贪心比动态规划更特殊，它还需要问题满足另一个性质——贪心选择性质。每次都可以把原问题分解为一个子问题。
动态规划是一种特殊的分治，而贪心是一种特殊的动态规划。
实现思路
递归形式：改分治。先进行判断。如果这个子问题已经处理过，那就直接把数组里储存了的值输出；否则就“计算结果”，最后储存答案。
递推形式：找出一种可行的拓扑序列。
两者在时间复杂度上没什么区别，而递归形式代码一般比较容易实现。具体区别请见：为什么线性动态规划类问题通常使用递推求解子问题，而不使用记忆化递归。
事实上动态规划最关键的是上文提到的“计算结果”，即列一个数学方程，这个方程被称为状态转移方程。