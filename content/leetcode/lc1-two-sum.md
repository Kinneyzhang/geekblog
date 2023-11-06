---
title: "LC1-两数之和"
type: post
layout: daily
draft: false
categories: ["数据结构与算法"]
tags: ["leetcode"]
comment: false
toc: false
---

# 题目
给定一个整数数组 nums 和一个整数目标值 target，请你在该数组中找出 和为目标值 target 的那两个整数，并返回它们的数组下标。你可以假设每种输入只会对应一个答案。但是，数组中同一个元素在答案里不能重复出现。你可以按任意顺序返回答案。

# 思路
- 数组根据下标获取值的时间复杂度是 O(1)
- 问题需要求解的就是"两数"的下标，下标作为未知数
- 根据未知数获取数组中的值，至少要有一次对数组的遍历
- 降低时间复杂的关键就在于，如何最大程度利用好这次遍历：即从一次遍历中获得和利用尽可能多的信息。
- 遍历的可以获得的信息有：当前 下标(i) 及 数字(a)
  - 如何利用？确定了其中一个数的下标 i，以及另一个数 b = S - a
  - 目标是求取 b 的下标 j
  - 不能获取更多的信息
  - 存储已经确定的 i,a 供后续的遍历使用
    - 使用哈希表存储的原因是 时间复杂度为 O(1)
  - 后续遍历查询已经存储在hash表中的数组，能够找到目标数字，则成功
  - 遍历的每一次的操作应该相同，因此第一次也查找哈希表中是否存在目标数据，不管hash表是否为空。

**实现**

    public int[] twoSumIndexs(int[] nums, int target) {
        Map<Integer, Integer> map = new HashMap<>();
        for (int i=0; i<nums.length; ++i) {
            int num = nums[i];
            int otherNum = target - num;
            if (map.containsKey(otherNum)) {
                int j = map.get(otherNum);
                return new int[]{j, i};
            }
            map.put(num, i);
        }
        return null;
    }
