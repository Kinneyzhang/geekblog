---
title: Emacs Workflow - 开门见山
date: 2020-06-14
draft: false
categories: ["Happy Hacking Emacs"]
comment: false
---

Emacs Workflow 顾名思义 “emacs工作流”。这一系列的文章主要介绍emacs中的常用的工作流，比如：文件管理、日程管理、笔记管理、听音乐、网页浏览、收发邮件、版本控制、字典查阅、快捷搜索、代数计算、telegram&#x2026; 每一种工作流都可以替代一种额外的app或系统应用。最终，我们逐渐向 "live in emacs" 的目标靠近。

在介绍具体的emacs workflow之前，我觉得有必要先搞清楚下面几个问题。


<a id="org936d00f"></a>

# 什么是Emacs？

官网的介绍是 "An extensible, customizable, free/libre text editor — and more"。这里有5个关键词：可拓展、可定制、自由、编辑器、更多。

emacs本质上是一个 **编辑器** ，从这个意义上来说，它和办公中经常使用的 MS word，程序员使用的vim, notepad别无二致。 **可拓展** 意味着我们可以通过程序来实现新的功能、特性。 **可定制** 意味着我们可以通过配置使emacs符合个人使用习惯。 **自由** 说的是emacs是一个自由软件(非商业)。 **更多** 包含了不计其数的可能性。

上面的解释可以总结为一句话: "If related data, nothing is impossible in Emacs."。你可能觉得这有点夸张，但我可以解释给你听：

这是个物质的世界，也是个数据的世界。物质的联系产生了数据，数据的改变塑造了物质。数据的种类有很多：简单的阿拉伯数字、数学公式、化学方程式、计算机程序、历史资料、软件密码、基因序列、社会信息&#x2026;这些都是数据。因此，我们可以简单的得出一个结论，改变数据就可以改变一切。

编辑器的作用是编辑数据，处理文本。普通的编辑器只能对数据进行简单的处理，比如插入、删除、改变样式、公式计算等等。而Emacs的高度可拓展性为数据处理提供了无限的可能。使用elisp，调用外部程序，emacs可以为数据之间创造复杂的逻辑，实现各种功能，满足各种需求，从而无所不能。

我们可以随便举一个看似不可能在emacs中完成的例子，看看emacs如何把不可能变成可能。比如：用emacs来煮咖啡。值得注意的是，使用emacs的前提是 "data related"，即能够与emacs交换数据信息。所以，假设我们有一个智能咖啡机，可以通过外部指令控制。现在我们可以编写一个 `make-coffee` 函数执行煮咖啡的指令，然后 `M-x make-coffee` ，大功告成！这是一个看似很简单的例子，但却揭示了emacs的本质: `Emacs是一个使用程序来处理数据的万能前端` 。这一点和操作系统很像。

以上就是我对Emacs的理解。


<a id="org01fbda3"></a>

# 什么样的人适合使用Emacs?

很多人认为Emacs是程序员、hacker的专利，我不认同。我觉得Emacs可以是大众的编辑器，可以服务于我们每一个普通人。无论你的职业为何，只要生活和工作离不开计算机，Emacs都可以成为你提升效率的大杀器。从这个以上意义上来说, **Emacs适合每一个经常和计算机打交道的人** 。


<a id="orga82e577"></a>

# 为什么我推荐你使用Emacs?

我曾经向很多熟悉的，不熟悉的人安利过emacs，但成功的情况很少。原因在于，我总是会像上面"什么是Emacs?"中一样，把它介绍的很牛逼，因此让人望而生畏。事实上，emacs确实很牛逼，但牛逼的东西也是由简单的事物构成的，就像一个复杂的计算机系统是由简单的 0、1 组成。

不同职业、不同身份的人使用计算机的需求和程度不一样，学习的计算机知识的难度也不同。用计算机办公、处理邮件的人只需要学习office办公软件，学会收发邮件；用计算机制作和处理图片、音视频的人只需要学习诸如PS、PR、AE等专业软件；普通程序员需要了解计算机运行的原理，学习如何通过特定的程序语言与计算机交互；高级黑客需要学习计算机底层架构和原理，了解程序语言背后的逻辑 &#x2026;&#x2026;

**很少人因为计算机是个复杂的东西就放弃使用它，因为你不需要了解它的全部，只需学习你需要使用的部分；因为只要你用它，就会给工作和生活带来极大的便利。这句话用在Emacs上也再合适不过了！** 

这就是我推荐你使用Emacs的全部理由。


<a id="org40b1ed6"></a>

# 这是怎样的Emacs教程？

Emacs Workflow 系列的文章会更关注新手，因此概念解释会比较详细。一个完整的工作流包含与之相关的方方面面，我将在我知识所及的情况下尽可能的介绍全面。工作流的不同方面可能会使用不同的emacs package或我自己编写的elisp代码，对于package或代码的内容，新手无需理解，拷贝粘贴后掌握如何使用即可。工作流的介绍分为“基础”和“附加”两部分。附加部分介绍不常用的或与程序相关的功能，这部分新手可以直接跳过。实现某一特定功能的package可能不只一个，我的解决方案可能不是最优解，欢迎留言补充。

最后，希望我的工作对你有所帮助，希望emacs伴你走过每一个春夏秋冬～
