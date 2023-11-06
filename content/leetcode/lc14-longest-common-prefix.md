---
title: "LC14-最长公共前缀"
type: post
layout: daily
draft: false
categories: ["数据结构与算法"]
tags: ["leetcode"]
comment: false
toc: false
---
# 题目
编写一个函数来查找字符串数组中的最长公共前缀。如果不存在公共前缀，返回空字符串 ""。

示例 1：\
输入：strs = ["flower","flow","flight"]\
输出："fl"

示例 2：\
输入：strs = ["dog","racecar","car"]\
输出：""\
解释：输入不存在公共前缀。

# 第一思路
- 设第一个字符串为最长公共子串
- 从第二字符串开始遍历字符串数组，依次将当前字符串 与 最长公共子串 比较，获取新的最长公共子串

**代码实现**

    public String longestCommonPrefix (String[] strs) {
        String commonPrefix = strs[0];
        for (int i=1; i<strs.length; i++) {
            String str = strs[i];
            int minLen = Math.min(commonPrefix.length(), str.length());
            String tempStr = "";
            for (int j=0; j<minLen; j++) {
                char a = commonPrefix.charAt(j);
                char b = str.charAt(j);
                if (a != b) { break; }
                tempStr = tempStr + a;
            }
            commonPrefix = tempStr;
        }
        return commonPrefix;
    }


# 横向扫描
我的第一思路其实就是“横向扫描”的方式，即依次遍历数组中的字符串，更新最长前缀。但是代码实现不够完善：
1. 没有考虑为空的特殊情况。
2. 内层循环用 while 写法更合适。
3. 没有考虑提前结束的情况：公共子串已经为空字符串，则无需继续遍历。

优化后的代码:

    public String longestCommonPrefix (String[] strs) {
        if (strs == null || strs.length == 0) {
            return "";
        }
        String commonPrefix = strs[0];
        for (int i=1; i<strs.length; i++) {
            String str = strs[i];
            int minLen = Math.min(commonPrefix.length(), str.length());
            int j = 0;
            while (j<minLen && commonPrefix.charAt(j) == str.charAt(j)) {
                j++;
            }
            commonPrefix = str.substring(0, j);
            // commonPrefix = "" 时，无需继续遍历
            if (commonPrefix.length() == 0) {
                break;
            }
        }
        return commonPrefix;
    }

# 纵向扫描
另一种思路是按列比较字符串中的每个字符。

    public String longestCommonPrefix2 (String[] strs) {
        if (strs == null || strs.length == 0) {
            return "";
        }
        String str = strs[0];
        for (int i=0; i<str.length(); i++) {
            char c = str.charAt(i);
            for (int j=1; j<strs.length; j++) {
                // 结束遍历的情形
                // 1. i 等于任一字符串的长度
                // 2. 当前字符不相等
                String otherStr = strs[j];
                if (i == otherStr.length() || c != otherStr.charAt(i)) {
                    return str.substring(0, i);
                }
            }
        }
        return str;
    }
