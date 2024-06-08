---
title: "LC53-最大子数组和"
type: post
layout: daily
draft: false
categories: ["数据结构与算法"]
tags: ["leetcode","动态规划"]
comment: false
toc: false
---

# 题目
给你一个整数数组 nums，请你找出一个具有最大和的连续子数组（子数组最少包含一个元素），返回其最大和。"子数组"是数组中的一个连续部分。

示例 1：\
输入：nums = [-2,1,-3,4,-1,2,1,-5,4]\
输出：6\
解释：连续子数组 [4,-1,2,1] 的和最大，为 6 。

示例 2：\
输入：nums = [1]\
输出：1

示例 3：\
输入：nums = [5,4,-1,7,8]\
输出：23

# 思路

## 朴素解法
双重循环，求解出所有情况的连续子数组的和，取最大值。这种方法简单直接，时间复杂度为 O(n^2/2)

    public int maxSubarray (int[] nums) {
        int maxSum = nums[0];
        for (int i=0; i<nums.length; i++) {
            int num1 = nums[i];
            int currSum = num1;
            for (int j=i; j<nums.length; j++) {
                System.out.println("i=" + i + ";j=" + j);
                if (j!=i) {
                    int num2 = nums[j];
                    currSum = currSum + num2;
                }
                if (currSum > maxSum) {
                    maxSum = currSum;
                    System.out.println("from:" + i + ", to:" + j);
                    System.out.println("maxSum:" + maxSum);
                }
            }
        }
        return maxSum;
    }

## 最优解
最优的方法是使用“动态规划”。

**动态规划的思路分析**
- 目标是求取“连续子数组”的最大和。
- 连续子数组的含义是“数组中多个相邻位置的数字”，特殊情况为单一位置的数字。
- 考察连续子数组的构造过程：前一个连续子数组 + 当前数字。
  - 设当前数字的位置为 `i`, 那么前一个连续子数组的最后数字的位置就是 `i-1`。
- 考察连续子数组的和的大小，如何获取一个最大和的连续子数组。
  - ★ 当在构造新的连续子数组时，需要关注“前一个连续子数组的和 + 当前数字” 与 “当前数字” 两者之间的大小。如果前者更大，则连上当前数字形成新的子数组；反正舍弃之前的连续子数组，将*当前数字*作为新的连续子数组。
  - 以上表达为: 前者`f(i-1) + nums[i]` 与 后者`nums[i]`的比较。
  - ★ 这里的逻辑重点在于：如果 前者 小于 后者，那么后续连上新的数字，前者永远小于后者，因为“连续性”要求它们必须共享后续的数字。(`a < b => a+c < b+c`)。

**代码实现**

    public int maxSubArray (int[] nums) {
        int maxSum = nums[0];
        int preSum = 0;
        for (int i=0; i<nums.length; i++) {
            // preSum:以前一个数字结尾的最大子数组的和
            // 前一个数字结尾的子数组 + 当前数字 构造新的子数组
            // 取这个新的子数组 与 当前的数字 的较大值
            preSum = Math.max(preSum + nums[i], nums[i]);
            // preSum:以当前数字结尾的最大子数组和
            maxSum = Math.max(maxSum, preSum);
        }
        return maxSum;
    }
