---
title: "C-Algorithm: 链表、队列和栈"
date: 2022-04-08
draft: false
categories: ["数据结构与算法"]
tags: ["数据结构", "算法", "C语言"]
comment: false
---

以前写代码总有一种担忧：担心逻辑不够规范、组织结构不够规范、命名不够规范&#x2026; 这种对规范的洁癖，浪费了我许多不必要的精力。比如链表的数据结构，我看过好几种不同的写法，便会纠结究竟哪一种更好，哪一种更合理？

当我决定用C系统的学习算法的时候，我终于想明白：对于数据结构和算法的实现，很多时候，并不存在所谓的最合理的规范，更多的是不同的思路和代码风格。在保证逻辑没有错误和遗漏的情况下，任何一种实现方法都是可以的。个人偏好形成风格，把握算法本质和实现的功能才是最重要的。在没有了这种心理负担后，我发现自己愈发能够看清数据结构和算法的本质，敲起代码来也顺畅了许多。

# 代码
链表、队列和栈所有代码实现查看 [Github](https://github.com/Kinneyzhang/LangC/tree/main/C-Algorithm/LinkedList) 。

# 概述
我一直觉得，在学习具体内容前，对一块知识有个整体的认识是很有必要的。首先要了解的概念是数据结构。数据结构主要分为 逻辑结构 和 物理结构 。逻辑结构指 数据对象中数据元素的相互关系；物理结构指 数据逻辑结构在计算机中的存储方式。

常见的逻辑结构有：集合、线性、树形、图形结构。常见的物理结构有：顺序、链式存储结构。数据结构在逻辑和存储结构上的差异决定了它们适合什么样的功能场景。一种结构不擅长的场景，可能另一种结构更擅长。开销大操作，换一种结构表示可能会有更低的时间和空间复杂度。因此，想搞清楚什么场景下使用什么数据结构，了解每种结构的本质和优缺点是第一步。

当然了解是不够，也得会用计算机语言来实现并使用这些数据结构来实现具体功能，这便是算法。算法就是解决问题的思路。

从直觉来说，我们用程序解决问题的过程和学习的过程是相反的。学习时，我们先掌握各种数据结构，然后再去了解复杂场景下的算法实现。而解决问题的过程是，脑子里先有一个步骤思路，这是算法；然后再考虑如何在程序中存储数据来实现算法，这是数据结构。

用哪种程序语言实现都可以，并不影响概念的理解。我选择C，因为C在高级语言中更接近计算机底层，即你可以在使用C的过程中感知到内存的分配和释放。

# 链表
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

开头定义了两个函数指针，用于数据比较和链表打印。注释了"main"的是核心方法，其余方法都可以通过复用这些核心方法来实现。

其他特殊的链表，可以用于一些特殊的场景：

1.  双链表：每个节点都有前驱和后继两个指针。
2.  循环链表：尾节点指向头节点。

# 注意点

对初学者来说，裸敲一遍链表代码通常会遇到很多bug。有bug不代表你对这个数据结构的原理和操作不熟悉，但能够说明缺少些代码实现的经验，这些经验就是在不断的敲代码和调试bug中积累的。所以，不要怕遇到bug，每一个bug都是进步的机会。下面我总结了自己的经验：

1.每个节点(Node)的数据和指针都要被赋值：这一点尤其是在双向链表的实现中，由于每个节点都有前驱和后继指针，在节点操作的过程中容易遗漏。

2.边界条件考虑：链表算法实现最容易犯的错误是对头尾节点的考虑不全面。有的实现中会额外增加一个头部的哨兵节点来排除一些边界情况。我的实现中就用了最原始的链表结构，因此要考虑边界情况。其实总结起来也并不复杂：如果链表的头尾节点发生了变化，需要更新链表的头尾指针。

一个典型的例子是删除节点的操作，我借此来讲解如何分析。首先我们简单的思考一下，可以得出结论：

1.  删除头节点，需要更新头指针；删除尾节点，需要更新尾指针；删除中间节点，不需要更新链表的头尾指针。
2.  删除头节点，不需要更新相关节点的next指针；删除中间节点和尾节点都需要更新前一个节点的next指针，单链表需要遍历才能拿的这个前驱节点。
3.  只有一个节点，且删除的就是该节点时，头尾指针要置空。

综合上面的考虑，合并一下逻辑，代码可以分为两块：删除的就是头节点 和 删除的是非头节点。其中删除头节点又分为 只有一个节点 和 多个节点的情况；删除非头节点分为：删除的是 中间节点 和 尾节点 两种情况。代码如下：

    void* deleteNode(LinkedList* list, Node* node) {
      if (node != NULL) {
        Node* curr = list->head;
        if (curr == node) {
          if (curr->next == NULL) {
            list->head = list->tail = NULL;
          } else {
            list->head = curr->next;
          }
        } else {
          while (curr != NULL && curr->next != node) {
            curr = curr->next;
          }
          if (curr != NULL) {
            if (node == list->tail) {
              list->tail = curr;
            }
            curr->next = node->next;
          }
        }
        void* data = node->data;
        free(node);
        return data;
      }
      return NULL;
    }

同时，删除节点的最后要释放掉内存。

3.插入节点时，避免丢失剩余节点：实现插入节点时，新手容易犯的错误就是提前断开链表节点的next指针，导致丢失了后面的节点。解决方法是：先将要插入的节点的next指针指到链表上，再将链表上的节点的next指向插入的节点。

# 队列和栈

队列和栈的结构与链表一致，只是数据操作的方式有特殊的规定。因此可以复用链表的方法。

-   队列：先进先出。入队列相当于从链尾插入一个节点，出队列相当于从链头删除一个节点。
-   栈：后进先出。入栈相当于从链尾插入一个节点，出栈相当于从链尾删除一个节点。

队列和链表的结构、方法定义如下：

    // 队列
    #include "linkedlist.h"
    typedef LinkedList Queue;
    
    void initQueue (Queue* queue);
    bool isQueueEmpty (Queue* queue);
    int queueLength (Queue* queue);
    void enqueue (Queue* queue, void* data);
    void* dequeue (Queue* queue);
    void printQueue (Queue* queue, PRINT print);
    
    // 栈
    #include "linkedlist.h"
    typedef LinkedList Stack;
    
    void initStack (Stack* stack);
    bool isStackEmpty (Stack* stack);
    int stackLength (Stack* stack);
    void push (Stack* stack, void* data);
    void* pop (Stack* stack);
    void* peek (Stack* stack);
    void printStack (Stack* stack, PRINT print);
