---
title: Meteor API 文档
date: 2021-10-17
draft: true
categories: ["技术文档"]
tags: ["Meteor", "翻译"]
comment: false
---

# 核心
Meteor 核心函数的文档。

---
`Meteor.isClient`

<small>import { Meteor } from 'meteor/meteor'  [(meteor/client_environment.js, line 31)](https://github.com/meteor/meteor/blob/master/packages/meteor/client_environment.js#L31)</small>

布尔值变量。如果在客户端环境中运行，则为真。

---
`Meteor.isServer`

<small>import { Meteor } from 'meteor/meteor' [(meteor/client_environment.js, line 39)](https://github.com/meteor/meteor/blob/master/packages/meteor/client_environment.js#L39)</small>

布尔值变量。如果在服务端环境中运行，则为真。

> `Meteor.isServer` 可以用来限制代码的运行位置，但它并不能阻止代码被发送到客户端。任何你不希望提供给客户端的敏感代码，例如包含密码或认证机制的代码，应该被保存在服务器目录中。

---
`Meteor.isCordova`

<small>import { Meteor } from 'meteor/meteor' [(meteor/cordova_environment.js, line 7)](https://github.com/meteor/meteor/blob/master/packages/meteor/cordova_environment.js#L7)</small>

布尔值变量。如果在 Cordova 移动环境中运行，则为真。

---
`Meteor.isDevelopment`

<small>import { Meteor } from 'meteor/meteor' [(meteor/client_environment.js, line 23)](https://github.com/meteor/meteor/blob/master/packages/meteor/client_environment.js#L23)</small>

布尔值变量。如果在开发环境中运行，则为真。

---
`Meteor.isProduction`

<small>import { Meteor } from 'meteor/meteor' [(meteor/client_environment.js, line 15)](https://github.com/meteor/meteor/blob/master/packages/meteor/client_environment.js#L15)</small>

布尔值变量。如果在生产环境中运行，则为真。

---
`Meteor.startup(func)`

<small>import { Meteor } from 'meteor/meteor' [(meteor/startup_client.js, line 70)](https://github.com/meteor/meteor/blob/master/packages/meteor/startup_client.js#L70)</small>

当客户端或服务器启动时运行代码。

<small>**ARGUMENTS**: `func` - Function 一个在启动时运行的函数。</small>

---
