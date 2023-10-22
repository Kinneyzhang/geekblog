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

# 一、概述

<p><img src="https://www.wangbase.com/blogimg/asset/201903/1_bg2019032501.png" alt="" title=""></p>

上图这样的布局，就是 Grid 布局的拿手好戏。

Grid 布局与 Flex 布局有一定的相似性，都可以指定容器内部多个项目的位置。但是，它们也存在重大区别。

Flex 布局是轴线布局，只能指定"项目"针对轴线的位置，可以看作是**一维布局**。Grid 布局则是将容器划分成"行"和"列"，产生单元格，然后指定"项目所在"的单元格，可以看作是**二维布局**。Grid 布局远比 Flex 布局强大。

# 二、基本概念
学习 Grid 布局之前，需要了解一些基本概念。

## 2.1 容器和项目
采用网格布局的区域，称为"容器"（container）。容器内部采用网格定位的子元素，称为"项目"（item）。

    <div>
      <div><p>1</p></div>
      <div><p>2</p></div>
      <div><p>3</p></div>
    </div>

上面代码中，最外层的`<div>`元素就是容器，内层的三个`<div>`元素就是项目。

注意：项目只能是容器的顶层子元素，不包含项目的子元素，比如上面代码的`<p>`元素就不是项目。Grid 布局只对项目生效。

## 2.2 行和列
容器里面的水平区域称为"行"（row），垂直区域称为"列"（column）。

<p><img src="https://www.wangbase.com/blogimg/asset/201903/1_bg2019032502.png" alt="" title=""></p>

上图中，水平的深色区域就是"行"，垂直的深色区域就是"列"。

## 2.3 单元格
行和列的交叉区域，称为"单元格"（cell）。

正常情况下，`n` 行和 `m` 列会产生 `n x m` 个单元格。比如，3行3列会产生9个单元格。

## 2.4 网格线
划分网格的线，称为"网格线"（grid line）。水平网格线划分出行，垂直网格线划分出列。

正常情况下，`n`行有 `n + 1` 根水平网格线，`m` 列有 `m + 1` 根垂直网格线，比如三行就有四根水平网格线。

<p><img src="https://www.wangbase.com/blogimg/asset/201903/1_bg2019032503.png" alt="" title=""></p>

上图是一个 4 x 4 的网格，共有5根水平网格线和5根垂直网格线。

# 三、容器属性
Grid 布局的属性分成两类。一类定义在容器上面，称为容器属性；另一类定义在项目上面，称为项目属性。这部分先介绍容器属性。

## 3.1 display 属性
`display: grid` 指定一个容器采用网格布局。

    div {
      display: grid;
    }

<p><img src="https://www.wangbase.com/blogimg/asset/201903/bg2019032504.png" alt="" title=""></p>

上图是 `display: grid` 的效果。

默认情况下，容器元素都是块级元素，但也可以设成行内元素。

    div {
      display: inline-grid;
    }

上面代码指定`div`是一个行内元素，该元素内部采用网格布局。

<p><img src="https://www.wangbase.com/blogimg/asset/201903/bg2019032505.png" alt="" title=""></p>

上图是 `display: inline-grid` 的效果。

<small>注意，设为网格布局以后，容器子元素（项目）的 `float`、`display: inline-block`、`display: table-cell`、`vertical-align` 和 `column-*` 等设置都将失效。</small>

# 3.2 grid-template-columns 属性，grid-template-rows 属性

容器指定了网格布局以后，接着就要划分行和列。`grid-template-columns` 属性定义每一列的列宽，`grid-template-rows` 属性定义每一行的行高。

    .container {
      display: grid;
      grid-template-columns: 100px 100px 100px;
      grid-template-rows: 100px 100px 100px;
    }

上面代码指定了一个三行三列的网格，列宽和行高都是100px。

<p><img src="https://www.wangbase.com/blogimg/asset/201903/bg2019032506.png" alt="" title=""></p>

除了使用绝对单位，也可以使用百分比。

    .container {
      display: grid;
      grid-template-columns: 33.33% 33.33% 33.33%;
      grid-template-rows: 33.33% 33.33% 33.33%;
    }

### (1) repeat()

有时候，重复写同样的值非常麻烦，尤其网格很多时。这时，可以使用 `repeat()` 函数，简化重复的值。上面的代码用 `repeat()` 改写如下。

    .container {
      display: grid;
      grid-template-columns: repeat(3, 33.33%);
      grid-template-rows: repeat(3, 33.33%);
    }

`repeat()` 接受两个参数，第一个参数是重复的次数（上例是3），第二个参数是所要重复的值。

`repeat()` 重复某种模式也是可以的。

    grid-template-columns: repeat(2, 100px 20px 80px);

上面代码定义了6列，第一列和第四列的宽度为 100px，第二列和第五列为 20px，第三列和第六列为 80px。

<p><img src="https://www.wangbase.com/blogimg/asset/201903/bg2019032507.png" alt="" title=""></p>

### (2) auto-fill 关键字
