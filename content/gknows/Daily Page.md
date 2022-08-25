---
title: Daily Page
type: post
layout: daily
toc: false
comment: false
---
---
<span><center>[INDEX](/gknows/index) > [生活消费](/gknows/生活消费) > [个人成长](/gknows/个人成长) > Daily Page</center></span>

---
# pelican
- https://getpelican.com
- https://docs.getpelican.com/en/latest/
- pneumatic: https://github.com/iKevinY/pneumatic
- pelican --print-settings

# Aug 25, 2022
- 今日待办
  - [X] 错单管理
    - [X] 批量错单已入历史情况考虑
      - 在什么时候做判断：分发之后，分析之前
    - [X] 错单主流程贯通测试
    - 问题：前后置错单不同实体，在传参数的时候如何复用同一个方法？
      - 每次都要用 instanceOf，太麻烦了

# Aug 24, 2022
- 今日待办
  - [X] 错单管理
    - [X] 批量分析前置流程开发
    - [X] 自动回收死循环问题考虑
    - [X] 参数表Map初始化问题解决
      - map赋值需要放在方法中，不能直接在类里面定义
    - [X] 多种表配置缺失或异常情况完善
      - 回收时，查不到特定错误码的配置
      - 回收时，查不到特定错误码的特定分析方法的配置
  - [X] gkroam daily page 输出到 md-wiki
