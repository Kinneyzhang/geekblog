---
title: "C-Algorithm: 基本排序算法"
date: 2022-06-27
draft: true
categories: ["数据结构与算法"]
tags: ["排序", "算法", "C语言"]
comment: false
---

本篇所说的基本排序算法指：选择排序、插入排序 和 冒泡排序

# 算法实现

## 选择排序

    #include <stdlib.h>
    #include <stdio.h>

    void selectSort (int a[], int size) {
      for (int i=0; i<size; i++) {
        int m = i; // position of the minimum element.
        for (int j=i+1; j<size; j++)
          if (a[j] < a[m]) m = j;
        // exchange
        int t = a[i];
        a[i] = a[m];
        a[m] = t;
      }
    }

    int main () {
      int a[8] = {9, 8, 7, 6, 5, 4, 3, 2};
      int size = 8;
      
      printf("Before sort: ");
      for (int i=0; i<size; i++) {
        printf("%d ", a[i]);
      }
      
      printf("\n");
      
      selectSort(a, size);
      printf("After sort:  ");
      for (int i=0; i<size; i++) {
        printf("%d ", a[i]);
      }
      printf("\n");
    }

## 插入排序

    #include <stdlib.h>
    #include <stdio.h>

    void insertSort (int a[], int size) {
      for (int i=1; i<size; i++) {
        int t = a[i];
        for (int j=i-1; j>=0; j--) {
          if (t > a[j])
            break;
          else {
            a[j+1] = a[j];
            a[j] = t;
          }
        }
      }
    }

    int main () {
      int a[8] = {9, 8, 7, 6, 5, 4, 3, 2};
      int size = 8;

      printf("Before sort: ");
      for (int i=0; i<size; i++) {
        printf("%d ", a[i]);
      }
      
      printf("\n");

      insertSort(a, size);
      printf("After sort:  ");
      for (int i=0; i<size; i++) {
        printf("%d ", a[i]);
      }

      printf("\n");
    }

## 冒泡排序

    #include <stdlib.h>
    #include <stdio.h>

    void bubbleSort (int a[], int size) {
      for (int i=0; i<size-1; i++) {
        for (int j=size; j>i; j--) {
          if (a[j] < a[j-1]) {
            int t = a[j];
            a[j] = a[j-1];
            a[j-1] = t;
          }
        }
      }
    }

    int main () {
      int a[8] = {9, 8, 7, 6, 5, 4, 3, 2};
      int size = 8;

      printf("Before sort: ");
      for (int i=0; i<size; i++) {
        printf("%d ", a[i]);
      }
      
      printf("\n");

      bubbleSort(a, size);
      printf("After sort:  ");
      for (int i=0; i<size; i++) {
        printf("%d ", a[i]);
      }

      printf("\n");
    }

