---
title: Linux
type: post
layout: daily
toc: false
comment: false
---
---
<span><center>[INDEX](/gknows/index) > [计算机科学](/gknows/计算机科学) > Linux</center></span>

---
# TODO
- [ ] puppy, plink and rsync 用法

# 命令行工具
## Windows免输入密码同步
- rsync: 数据(远程)同步工具 
- plink: PuTTY的命令行连接工具
- cygnative: 使用 cygwin 程序可以通过标准输入输出与本地win32程序通信。[download](http://diario.beerensalat.info/2009/08/18/new_cygnative_version_1_2_for_rsync_plink.html)

```shell
rsync -av -e "cygnative plink -ssh -P <port> -pw <passwd>" ./public/ root@119.28.186.136:/var/www/hugoblog/
```
