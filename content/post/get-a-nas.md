---
title: "Get A NAS"
date: 2023-12-18
draft: true
categories: [""]
tags: ["NAS"]
comment: false
---

距离听到《内核恐慌》get a nas 这一期节目已经不知道过了多久，我终于有了一台自己的NAS！

# 公网IP访问的方法
将光猫设置为桥接模式。简单来说，将光猫设置为桥接模式后，它就不再承担路由的功能，直接将光信号转换后传递给路由器 并且 路由器可以获得公网IP。

参考: https://zhuanlan.zhihu.com/p/607518654?utm_id=0

# DDNS

# 端口转发

# 完美的实时云同步
群晖自带的云同步软件是 Synology Drive，使用下来体验还是很不错的。需要现在群晖NAS的应用市场中安装服务端，然后本地机器安装对应的[客户端](https://www.synology.cn/zh-cn/support/download)。

注意使用 quickConnenct 连接时，本人测试无法从服务端实时拉取最新的文件版本，还是需要设置为公网ip访问。
