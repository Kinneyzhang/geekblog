---
title: CSS Grid 网格布局教程
date: 2023-10-22
draft: false
categories: ["技术文档"]
tags: ["CSS","grid"]
comment: false
---

网格布局（Grid）是最强大的 CSS 布局方案。
它将网页划分成一个个网格，可以任意组合不同的网格，做出各种各样的布局。以前，只能通过复杂的 CSS 框架达到的效果，现在浏览器内置了。
<!--more-->

> 本文转载自 [CSS Grid 网格布局教程 - 阮一峰的网络日志](https://www.ruanyifeng.com/blog/2019/03/grid-layout-tutorial.html)\
> 遵循 "自由转载-非商用-非衍生-保持署名（创意共享3.0许可证）"

# 概述

<p><img src="https://www.wangbase.com/blogimg/asset/201903/1_bg2019032501.png" alt="" title=""></p>

上图这样的布局，就是 Grid 布局的拿手好戏。
Grid 布局与 Flex 布局有一定的相似性，都可以指定容器内部多个项目的位置。但是，它们也存在重大区别。
Flex 布局是轴线布局，只能指定"项目"针对轴线的位置，可以看作是一维布局。Grid 布局则是将容器划分成"行"和"列"，产生单元格，然后指定"项目所在"的单元格，可以看作是二维布局。Grid 布局远比 Flex 布局强大。
