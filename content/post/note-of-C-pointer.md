---
title: "《深入理解C指针》笔记"
date: 2022-04-20
draft: true
categories: ["技术笔记"]
tags: ["指针","C语言","笔记"]
comment: false
---

# 认识指针


<a id="org6a19bca"></a>

## 指针和内存

-   不同内存中变量的作用域和生命周期

<table>


<colgroup>
<col  class="org-left">

<col  class="org-left">

<col  class="org-left">
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">内存类型</th>
<th scope="col" class="org-left">作用域</th>
<th scope="col" class="org-left">生命周期</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">全局内存</td>
<td class="org-left">整个文件</td>
<td class="org-left">应用程序的生命周期</td>
</tr>


<tr>
<td class="org-left">静态内存</td>
<td class="org-left">声明它的函数内部</td>
<td class="org-left">应用程序的生命周期</td>
</tr>


<tr>
<td class="org-left">自动内存(局部内存)</td>
<td class="org-left">声明它的函数内部</td>
<td class="org-left">限制在函数执行时间内</td>
</tr>


<tr>
<td class="org-left">动态内存</td>
<td class="org-left">由引用该内存的指针决定</td>
<td class="org-left">直到内存释放</td>
</tr>
</tbody>
</table>

-   指针的几种用途
    -   写出快速高效的代码
    -   为解决很多类问题提供方便的途径
    -   支持动态内存分配
    -   是表达式变得紧凑和简洁
    -   提供用指针传递数据的能力而不会带来庞大的开销
    -   保护作为参数传递给函数的数据
-   声明指针: `int *pi` 星号两边空白符无关紧要。
    -   pi的内容最终应该被赋值为一个整数变量的地址
    -   这些变量只有被初始化后，指针才能正常的工作，否则包含的是垃圾数据(该地址之前所存放的任何数据)
-   地址操作符(&)会返回操作数的地址
-   初始化指针
    
        int num = 2;
        int *pi;
        pi = &num;
        printf("pi指针的地址是：%p\n", &pi); // pi指针的地址是：0x30942f740
        printf("pi指针的内容是：%p\n", pi); // pi指针的内容是：0x30942f74c
        printf("num的地址是：%p\n", &num); // num的地址是：0x30942f74c
        printf("num的内容是：%d\n", num); // num的内容是：2
        printf("pi指针所指向的内容是：%d\n", *pi); // pi指针所指向的内容是：2
        
        // 想要在不同平台以一致的方式显示指针，可以将指针转换为void指针
        printf("pi指针的内容是：%p\n", (void*)pi);
-   用间接操作符(\*)解引指针：解引操作符的结果叫左值，左值可以被修改赋值。
    
        int num = 2;
        int *pi = num;
        printf("num的内容是：%d\n", num); // num的内容是：2
        *pi = 3;
        printf("修改后num的内容是：%d\n", num); // 修改后num的内容是：3
-   指向函数的指针：void (\*foo)();
-   NULL(指针)的概念
    -   NULL被赋值给指针意味着指针不指向任何地址。
    -   NULL宏是强制类型转化为void指针的整数常量0。 `#define NULL ((void *)0)`
    -   可以给指针赋值0或NULL，不会报错: `pi = 0; pi = NULL`
    -   0的含义会随上下文变化，有时表示null指针，有时表示整数0
        
            int num;
            int *pi = 0; // 表示NULL指针
            pi = &num;
            *pi = 0; // 表示整数0
-   void指针
    -   void指针具有与char指针相同的形式和内存对齐方式
    -   void指针与别的指针永远不会相等，除了两个赋值为NULL的void指针
    -   任何指针都可以被赋值给void指针，在原指针和void指针间准换不会改变指针的值
    -   void指针只能做数据指针，不能做函数指针
-   全局和静态指针
    -   全局和静态变量存放在堆上
    -   全局和静态指针在程序启动时会被初始化为NULL


<a id="org4879b76"></a>

## 指针的长度和类型

-   类型和长度
    -   指针长度取决于机器和编译器
    -   在大部分现代平台上，数据指针长度通常是一样的，与指针类型无关
    -   char指针和结构体指针长度相同
    -   函数指针长度可能与数据指针长度不同
-   内存模型(???)
    
    ![img](c:/Users/26289/Pictures/blog/机器内存模型.png)
-   指针相关的预定义类型
    -   size<sub>t</sub>: 用于安全的表示长度，目的是提供一种可移植的方法来声明与系统中可寻址的内存区域一致的长度
        -   表示C中任何对象所能达到的最大长度，是无符号整数。因此该类型的变量应该存放正数。
        -   size<sub>t是sizeof操作符的返回值类型</sub>，也是malloc、strlen等很多函数的参数类型
        -   它常用于循环计数器，数组索引，字符数、指针算术运算等。
            
                #ifndef __SIZE_T
                #define __SIZE_T
                typedef unsigned int size_t;
                #endif
        -   `printf("Size of *char: %d\n", sizeof(char*))`
    -   intptr<sub>t和uintptr</sub><sub>t</sub>: 用于处理指针算术运算，目的是提供一种可移植且安全的方法声明指针
        -   uintptr<sub>t</sub> 是 intptr<sub>t</sub> 的无符号版本，大对数操作使用 intptr<sub>t</sub> 较好


<a id="org3b4b39e"></a>

## 指针操作符

-   指针操作符
    
    <table>
    
    
    <colgroup>
    <col  class="org-left">
    
    <col  class="org-left">
    
    <col  class="org-left">
    </colgroup>
    <thead>
    <tr>
    <th scope="col" class="org-left">操作符</th>
    <th scope="col" class="org-left">名称</th>
    <th scope="col" class="org-left">含义</th>
    </tr>
    </thead>
    
    <tbody>
    <tr>
    <td class="org-left"><code>*</code></td>
    <td class="org-left">&#xa0;</td>
    <td class="org-left">用来声明指针</td>
    </tr>
    
    
    <tr>
    <td class="org-left"><code>*</code></td>
    <td class="org-left">解引</td>
    <td class="org-left">用开解引指针</td>
    </tr>
    
    
    <tr>
    <td class="org-left"><code>-&gt;</code></td>
    <td class="org-left">指向</td>
    <td class="org-left">用来访问指针引用的结构的字段</td>
    </tr>
    
    
    <tr>
    <td class="org-left"><code>+</code></td>
    <td class="org-left">加</td>
    <td class="org-left">用于对指针做加法</td>
    </tr>
    
    
    <tr>
    <td class="org-left"><code>-</code></td>
    <td class="org-left">减</td>
    <td class="org-left">用于对指针做减法</td>
    </tr>
    
    
    <tr>
    <td class="org-left"><code>==~、</code>!=~</td>
    <td class="org-left">相等、不等</td>
    <td class="org-left">比较两个指针</td>
    </tr>
    
    
    <tr>
    <td class="org-left"><code>&gt;~、~&gt;=~、~&lt;~、~&lt;=</code></td>
    <td class="org-left">&#x2026;</td>
    <td class="org-left">比较两个指针</td>
    </tr>
    
    
    <tr>
    <td class="org-left">数据类型</td>
    <td class="org-left">转换</td>
    <td class="org-left">改变指针类型</td>
    </tr>
    </tbody>
    </table>
-   指针加(减)整数：实际上加的是地址是 这个整数和指针数据类型对应字节数的乘积。同理减去一个整数。
-   指针相减：得到两个地址的差值，通常没什么，但可以用来判断数组中的元素顺序。
    -   指针相减后的值得类型是 ptrdiff<sub>t</sub>，这个类型简化了处理差值的任务
-   指针比较：指针比较通常没什么用，但把指针和数组元素比较可以用来判断数组元素的相对位置


<a id="org26d9977"></a>

## 指针的常见用法

-   多层间接引用：指针的指针(的指针..)
-   指向常量的指针
    -   `const int limit = 500;` `const int* pci = &limit;`
    -   可以被修改为指向不同的(非)整数常量
    -   可以解引以读取数据
    -   不能解引来修改它指向的数据
    -   `const int *pci` 和 `int const *pci` 等价
-   指向非常量的常量指针
    -   `int num;` `int *const cpi = &num;`
    -   有了这个声明
        -   cpi必须被初始化为指向非常量变量
        -   cpi不能被修改
        -   cpi指向的数据可以被修改(通过解引)
-   指向常量的常量指针
    -   `const int limit = 500;` `const int *const cpci = &limit`
    -   不能修改指针
    -   不能通过解引修改指针指向的数据
-   指向“指向常量的常量指针”的指针
    -   `const int limit = 500;`
    -   `const int *const cpci = &limit;`
    -   `const int *const *pcpci = &cpci;`
-   总结
    
    -   修改指针：修改指针所指向的变量地址。
    -   修改指向指针的数据：通过解引，修改指针所指向的变量的值。
    
    <table>
    
    
    <colgroup>
    <col  class="org-left">
    
    <col  class="org-left">
    
    <col  class="org-left">
    </colgroup>
    <thead>
    <tr>
    <th scope="col" class="org-left">指针类型</th>
    <th scope="col" class="org-left">指针是否可修改</th>
    <th scope="col" class="org-left">指向指针的数据是否可修改</th>
    </tr>
    </thead>
    
    <tbody>
    <tr>
    <td class="org-left">指向非常量的指针</td>
    <td class="org-left">是</td>
    <td class="org-left">是</td>
    </tr>
    
    
    <tr>
    <td class="org-left">指向常量的指针</td>
    <td class="org-left">是</td>
    <td class="org-left">否</td>
    </tr>
    
    
    <tr>
    <td class="org-left">指向非常量的常量指针</td>
    <td class="org-left">否</td>
    <td class="org-left">是</td>
    </tr>
    
    
    <tr>
    <td class="org-left">指向常量的常量指针</td>
    <td class="org-left">否</td>
    <td class="org-left">否</td>
    </tr>
    </tbody>
    </table>


<a id="org4824ef2"></a>

# C的动态内存管理


<a id="orgc430b49"></a>

## 动态内存分配

-   C中动态内存分配的基本步骤
    -   用 malloc 类的函数分配内存
    -   用这些内存支持应用程序
    -   用 free 函数释放内存
-   分配内存时，堆管理器维护的数据结构中会保存额外的信息。
-   内存泄漏：不再使用的已分配内存没有被释放。
    -   丢失内存地址
    -   应该调用free函数却没有调用


<a id="org95b6c6f"></a>

## 动态内存分配函数

-   **给指针分配一块内存实际上是给指针指向的地址分配内存**
-   动态内存分配函数有
    -   `malloc`: 从堆上分配内存
    -   `realoc`: 在之前分配的内存块的基础上，将内存重新分配为更大或更小的部分
    -   `calloc`: 从堆上分配内存并清零
    -   `free`: 将内存块返回堆
-   malloc
    -   `void* malloc(size_t);`
        -   参数表示从堆上分配的内存的字节数
        -   返回值是void指针，
        -   如果内存不足返回NULL
        -   所分配的内存默认包含垃圾数据
    -   `int* pi = (int*) malloc(sizeof(int))`
        -   从堆上分配内存
        -   内存不会被修改或清空
        -   返回首字节的地址
    -   除了size<sub>t所定义的内存字节数</sub>，对管理器还会分配额外的内存来管理这个块。
    -   初始化静态或全局变量不能调用函数。因此下面的语句会报错: `static int *pi = malloc(sizeof(int));` 。静态变量可以通过单独的语句给变量分配内存来避免错误，全局变量的单独语句需要写在函数内。
-   calloc
    -   calloc在分配时会将内容置为二进制0;
    -   函数原型: `void *calloc(size_t numElements, size_t elementSize);`
        
            int *pi = calloc(5, sizeof(int));
            //等价于
            int *pi = malloc(5 * sizeof(int));
            memset(pi, 0, 5 * sizeof(int));
-   realoc
    -   增加或减少指针分配的内存，比如变长数组
    -   原型: `void *realoc(void *ptr, size_t size);`
        
        <table>
        
        
        <colgroup>
        <col  class="org-left">
        
        <col  class="org-left">
        
        <col  class="org-left">
        </colgroup>
        <thead>
        <tr>
        <th scope="col" class="org-left">1st param</th>
        <th scope="col" class="org-left">2nd param</th>
        <th scope="col" class="org-left">behavior</th>
        </tr>
        </thead>
        
        <tbody>
        <tr>
        <td class="org-left">空</td>
        <td class="org-left">空</td>
        <td class="org-left">同 malloc</td>
        </tr>
        
        
        <tr>
        <td class="org-left">非空</td>
        <td class="org-left">0</td>
        <td class="org-left">原内存块被释放</td>
        </tr>
        
        
        <tr>
        <td class="org-left">非空</td>
        <td class="org-left">比原内存块小</td>
        <td class="org-left">利用当前块分配更小的块</td>
        </tr>
        
        
        <tr>
        <td class="org-left">非空</td>
        <td class="org-left">比原内存块大</td>
        <td class="org-left">紧挨着当前的位置 或 在其他位置分配更大的块</td>
        </tr>
        </tbody>
        </table>
-   alloca函数和变长数组
    -   alloca(malloca for MS)在函数的栈帧上分配内存，函数返回后将自动释放内存。
    -   若底层的运行时系统不基于栈，alloca函数会很难实现，因此这个函数是不标准的。
    -   变长数组使用了这个函数？


<a id="orgaa9b36f"></a>

## 用free函数释放内存

-   原型: `void free(void *ptr);`
-   参数是malloc类函数分配的内存地址，这块内存会被返回给堆。
-   被释放的内存只的值没有变，指针仍然指向该地址，只是可以被重新分配。
-   被free的指针，需要由malloc类的函数分配，不是函数的行为将是未定义。
-   在函数内分配的内存，也应该在同一个函数内释放它。
-   将已释放的指针赋值为NULL。
-   是否要在程序终止前释放所有内存取决于具体的应用。


<a id="orgf7000d4"></a>

## 迷途指针

-   含义
    -   内存已经释放，指针还在引用原始内存，这样的指针被称为迷途指针
    -   向迷途指针指向的地址写入数据的结构是不可预期的
    -   另一种情况是一个以上的指针指向同一块内存地址，其中一个指针被释放，尝试解引其余的，就都变成了迷途指针
    -   块语句中的指针，在块语句之外(出栈)可能变成迷途指针。
-   处理迷途指针
    -   释放指针后置为NULL
    -   写一个特殊的函数代替free函数
    -   有些系统会在释放后覆盖数据
    -   用第三方工具检测


<a id="orgfab8e29"></a>

## 动态内存分配技术

-   概念
    -   不同编译器在堆管理器分配和释放内存的技术实现上有所不同
    -   大部分堆管理器把堆或数据段作为内存资料，这种方法会造成碎片，而且可能和程序栈碰撞。
    -   堆管理器要处理很多问题，如堆是否基于进程或线程分配，如何保护堆不受安全攻击。
    -   主流的堆管理器：OpenBSD的malloc、Hoard的malloc 和 Google开发的TCMalloc。GNU的C库的分配器基于通用分配器dlmalloc（<http://dmalloc.com> ），它提供调试机制，能追踪内存泄漏。
-   资源获取即初始化(RAII)技术
    -   用来解决C++中的资源分配和释放：一旦变量超出作用域就会触发释放过程
    -   也可以在C中使用这种技术，GNU编译器提供了非标准的拓展来支持这个特性
        -   &#x2026;
    -   不用GNU扩展也可以达到类似的效果（<http://en.wikipedia.org/wiki/Resource_Acquisition_Is_Initialization#Ad-hoc_mechanisms> ）。
-   使用异常处理函数也可以处理内存释放
    -   &#x2026;


<a id="org483bcc6"></a>

# 指针和函数


<a id="orgfa8ac66"></a>

## 程序的栈和堆

-   程序栈和堆共享一块内存区域，通常堆占据上部，栈占据下部
-   堆管理动态内存，栈存放函数参数和局部变量
-   程序栈存放栈帧，栈帧也叫 活跃记录 或 活跃帧，栈帧存放函数参数和局部变量
-   栈帧的组织
    -   返回地址
    -   局部数据存储
    -   参数存储
    -   栈指针和基指针
-   经过测试，不同的机器的上面几个元素在栈帧中的顺序不一样??


<a id="orge833cd0"></a>

## 通过指针传递和返回数据

-   用指针传递数据，可以在函数中修改数据(交换两数字)。
-   用值传递数据，形参是实参的拷贝，无法在函数中修改原来的数据。
-   传递指向常量的指针
    -   传递指向常量的指针只传了数据的地址，效率很高
    -   无法在函数内(通过解引)修改指针指向的常量
    -   `assingAddressOfConstants(&23, &23);` 错误：取指操作符的操作数需要一个左值，因为它需要可以被修改。
-   返回指针的两种技术
    -   使用malloc在函数内部分配内存并返回其地址。调用者负责释放返回的内存。
    -   传递一个对象给函数并让函数修改它。这让分配和释放对象的内存都是调用者的职责。
-   返回指针时潜在的问题
    -   返回未初始化的指针
    -   返回指向无效地址的指针
    -   返回局部变量的指针：函数返回时，局部变量所在的函数栈帧会弹出，地址无效。
    -   返回指针但是没有释放内存
-   传递空指针：将一个空指针作为参数，在调用它之前分配内存。
-   传递指针的指针
-   实现自己的free函数：检查传入的指针是否为空，释放指针后置为NULL


<a id="org2d4e979"></a>

## 函数指针

-   声明函数指针: `void (*foo)()`
    -   `int *f4()` 返回整数指针的函数
    -   `int (*f5)` 返回整数的函数指针
    -   `int* (*f6)` 返回整数指针的函数指针
-   使用函数指针
    
        typedef int (*funcptr)(int);
        
        int square(int num) {
          return num*num;
        }
        
        int main() {
          funcptr fptr1 = square;
          printf("%d squared is %d\n", n, fptr1(6));
        }
-   传递函数指针
    
        int add(int num1, int num2) {
          return num1 + num2;
        }
        int subtract(int num1, int num2) {
          return num1 - num2;
        }
        typedef int (*fptrOperation)(int,int);
        int compute(fptrOperation operation, int num1, int num2) {
          return operation(num1, num2);
        }
        
        int main() {
          printf("%d\n", compute(add, 5, 6));
          printf("%d\n", compute(subtract, 5, 6));
        }
-   返回函数指针
-   使用函数指针数组:???
-   比较函数指针
-   转换函数指针???


<a id="org089d882"></a>

# 指针和数组


<a id="org4d2f8fc"></a>

## 数组概述

-   数组是能用索引访问的同质元素连续集合。
-   数组的长度是固定的，声明数组时，需要指明数组有多大。
-   求数组长度方法: `int vector[5];` `sizeof(vector)/sizeof(int)`
-   二维数组
    -   `int matrix[2][3]={{1,2,3},{4,5,6}};`
    -   按行分配内存地址
-   多维数组


<a id="orgf3f499a"></a>

## 指针表示法和数组

-   数组和指针的差别
    -   对于 `int vector[5] = {1, 2, 3, 4, 5};` `int *pv = vector;`
    -   `vector[i]` 与 `*(vector+i)` 尽管结果相同，但是生成的机器码不同
    -   前者表示从位置vector开始移动i个位置，取出内容。
    -   后者表示从vector开始，在地址上加i，然后取出地址中的内容。
    -   `sizeof(vector)` 返回数组分配的字节; `sizeof(pv)` 返回指针的长度
    -   pv是左值，可以被修改；数组名称vector是右值，不能被修改。
        -   `pv=pv+1` 正确
        -   `vector=vector+1` 错误
        -   `pv=vector+1` 正确


<a id="org30f2b9f"></a>

## 用 malloc 创建一维数组

    int main() {
      int *pv = (int*)malloc(5 * sizeof(int));
      for(int i=0; i<5; i++) {
        pv[i] = i+1;
        printf("pv[%d]:%d\n", i, pv[i]);
      }
    }


<a id="org958fb72"></a>

## 用realloc调整数组长度

-   调大指针内存：读取键盘输入的字符串
-   调小指针内存：trim函数去掉空白


<a id="orgbf21496"></a>

## 传递一维数组

-   数组表示法
-   指针表示法


<a id="org42df19b"></a>

## 使用指针的一维数组

-   `int* arr[5];` 数组中存放的是地址


<a id="org305edda"></a>

## 指针和多维数组


<a id="org5cdbc4b"></a>

## 传递多维数组

-   `void display2DArray(int arr[][5], int rows)`
    -   arr[] 是数组指针的隐式声明
-   `void display2DArray(int (*arr)[5], int rows)`
    -   每个指针指向有5个元素的数组。


<a id="orgf0b908b"></a>

## 动态分配二维数组

-   分配不连续内存
    
    ![img](c:/Users/26289/Pictures/blog/二维数组分配不连续内存.png)
-   分配连续内存
    -   调用两次malloc分配。
        
        ![img](c:/Users/26289/Pictures/blog/二维数组两次malloc分配连续内存.png)
    -   一次性分配，不能使用二维数组下标索引，但说明了二维数组和内存的一维本质的关系。
        
        ![img](c:/Users/26289/Pictures/blog/二维数组一次malloc分配连续内存.png)


<a id="org8d2a82d"></a>

## 不规则数组和指针

-   不规则数组是每一行列数不一样的二维数组
-   使用复合字面量创建不规则的二维数组


<a id="org082ae77"></a>

# 指针和字符串

-   字符串基础
    -   字符串是以ASCII字符NUL结尾的字符序列。NUL表示为\\0。
    -   字符串通常存储在数组或从堆上分配的内存中。
    -   C语言有两种类型的字符串
        -   单字节字符串：由char数据类型组成的序列
        -   宽字符串：由wchar<sub>t数据类型组成的序列</sub>，主要用来支持非拉丁字符集
    -   字符串长度是除了NUL字符之外的字符数
    -   字符串声明
        -   字面量 `"emacs"`
        -   字符数组 `char header[32];`
        -   字符指针 `char *header;`
    -   字面量池
        -   保存组成字符串的字符序列，多次用到同一个字面量时，字面量池中只有一份拷贝。
        -   大部分编译器提供了关闭字面量池的选项。关闭后，字面量可以生成多个拷贝，每个拷贝拥有自己的地址。(GCC用-fwritable-strings 选项来关闭字符串池。)
        -   字符串字面量一般分配在只读内存中，不可变。
        -   在大部分编译器中，可以讲字符串字面量看作常量。但在有些编译器(GCC)中可以修改，这时需要把它声明为常量。 `const char *tabHeader = "Sound";`
    -   字符串初始化
        -   初始化char数组: `char header[] = "Media Player";`
        -   也可以用strcpy: `char header[13];` `strcpy(header, "Media Player");`


<a id="org418a75a"></a>

# 指针和结构体


<a id="orgdd7400e"></a>

## 介绍

-   `->` 表示指针指向的结构体变量，等同于指针解引后再用点运算符。
-   为结构体分配内存至少是各个字段的长度和。实际会大于这个和，因为某些数据类型需要边界对齐。
-   结构体数组元素之间可能存在额外内存，要谨慎使用指针算术运算。


<a id="orgab11868"></a>

## 结构体释放

-   详见代码


<a id="org57f3839"></a>

## 避免malloc/free开销

-   使用结构体池：用指针数组维护结构体池。
-   详见代码


<a id="org0b5b36d"></a>

## 用指针支持数据结构

-   链表、队列、栈、树：详见代码
