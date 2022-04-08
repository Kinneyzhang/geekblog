---
title: "C算法: 链表、队列和栈"
date: 2022-04-08
draft: false
categories: ["数据结构与算法"]
tags: ["数据结构", "算法", "C语言"]
---

# Table of Contents

1.  [数据结构和算法概述](#orgd038f83)
2.  [链表、队列和栈](#orga332937)

以前写代码总有一种担忧：担心逻辑不够规范、组织结构不够规范、命名不够规范&#x2026; 这种对规范的洁癖，浪费了我许多不必要的精力。比如链表的数据结构，我看过好几种不同的写法，便会纠结究竟哪一种更好，哪一种更合理？

当我决定用C系统的学习算法的时候，我终于想明白：对于数据结构和算法的实现，很多时候，并不存在所谓的最合理的规范，更多的是不同的思路和代码风格。在保证逻辑没有错误和遗漏的情况下，任何一种实现方法都是可以的。个人偏好形成风格，把握算法本质和实现的功能才是最重要的。在没有了这种心理负担后，我发现自己愈发能够看清数据结构和算法的本质，敲起代码来也顺畅了许多。


<a id="orgd038f83"></a>

# 数据结构和算法概述

我一直觉得，在学习具体内容前，对一块知识有个整体的认识是很有必要的。首先要了解的概率是 **数据结构** 。数据结构主要分为 逻辑结构 和 物理结构 。逻辑结构指 数据对象中数据元素的相互关系；物理结构指 数据逻辑结构在计算机中的存储方式。

常见的逻辑结构有：集合、线性、树形、图形结构。常见的物理结构有：顺序、链式存储结构。数据结构在逻辑和存储结构上的差异决定了它们适合什么样的功能场景。一种结构不擅长的场景，可能另一种结构更擅长。开销大操作，换一种结构表示可能会有更低的时间和空间复杂度。因此，想搞清楚什么场景下使用什么数据结构，了解每种结构的本质和优缺点是第一步。

当然了解是不够，也得会用计算机语言来实现并使用这些数据结构来实现具体功能，这便是算法。算法就是解决问题的思路。

从直觉来说，我们用程序解决问题的过程和学习的过程是相反的。学习时，我们先掌握各种数据结构，然后再去了解复杂场景下的算法实现。而解决问题的过程是，脑子里先有一个步骤思路，这是算法；然后再考虑如何在程序中存储数据来实现算法，这是数据结构。

用哪种程序语言实现都可以，并不影响概念的理解。我选择C，因为C在高级语言中更接近计算机底层，即你可以在使用C的过程中感知到内存的分配和释放。


<a id="orga332937"></a>

# 链表、队列和栈

第一篇，总结一下基础的数据结构：链表、队列和栈。链表、队列和栈都是线性的链式存储结构。我们可以把队列和栈理解为特殊的链表，因此核心的算法实现只有链表。

无论是C的结构体，还是C++, Java的类，链表的实现都要依赖两个基本结构：节点和链表结构本身。
链表由节点组成，节点包含数据域和指针域，数据域存放节点数据，指针域存储指向另一个节点的指针。链表包含两个指针，分别指向头尾节点。用C结构体表示如下：

    typedef struct _node {
      void* data;
      struct _node* next;
    } Node;
    
    typedef struct _linkedlist {
      Node* head;
      Node* tail;
    } LinkedList;

定义了数据的存储方式，接下来就是定义操作数据的方法。基础的方法无非就是：构建链表、查询、插入、删除节点等操作。定义的方法如下：

    typedef void(*PRINT)(void*);
    typedef int(*COMPARE)(void*, void*);
    
    void initList(LinkedList* list); // main
    int length(LinkedList* list); // main
    bool isEmpty(LinkedList* list); // main
    void printLinkedList(LinkedList* list, PRINT print); // main
    
    void addHead(LinkedList* list, void* data);
    void addTail(LinkedList* list, void* data);
    void buildLinkedList(LinkedList* list, void* arr[], int n);
    
    Node* getNode(LinkedList* list, COMPARE compare, void* data); // main
    Node* getNthNode(LinkedList* list, int nth); // main
    
    void insertNode(LinkedList* list, Node* node, void* newData, int flag); // main
    void insertNodeBefore(LinkedList* list, Node* node, void* data);
    void insertNodeAfter(LinkedList* list, Node* node, void* data);
    void insertByData(LinkedList* list, COMPARE compare, void* data, void* newData, int flag);
    void insertByDataBefore(LinkedList* list, COMPARE compare, void* data, void* newData);
    void insertByDataAfter(LinkedList* list, COMPARE compare, void* data, void* newData);
    void insertByNth(LinkedList* list, int nth, void* data);
    
    void* deleteNode(LinkedList* list, Node* node); // main
    void* deleteByNth(LinkedList* list, int nth);
    void deleteByData(LinkedList* list, COMPARE compare, void* data);
    
    void updateNode(Node* node, void* data); // main
    void updateByNth(LinkedList* list, int nth, void* newData);
    void updateByData(LinkedList* list, COMPARE compare, void* data, void* newData);

开头定义了两个函数指针，用于数据比较和打印。注释了"main"的是核心方法，其余方法都可以通过复用这些核心方法来实现。
