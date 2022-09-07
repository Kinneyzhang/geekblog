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
# Windows免输入密码同步远程主机
- rsync: 数据(远程)同步工具(cwrsync)
- plink: [PuTTY](https://www.chiark.greenend.org.uk/~sgtatham/putty/) 的命令行连接工具
- cygnative: 使用 cygwin 程序可以通过标准输入输出与本地win32程序通信。[download](http://diario.beerensalat.info/2009/08/18/new_cygnative_version_1_2_for_rsync_plink.html)

传输Win本地数据到远程主机:

    rsync -av -e "cygnative plink -ssh -P <port> -pw <passwd>" ./public/ root@119.28.186.136:/var/www/hugoblog/

# PuTTY
- [Download Page](https://www.chiark.greenend.org.uk/~sgtatham/putty/latest.html)
  - PuTTY is a free implementation of SSH and Telnet for Windows and Unix platforms.
- [Docs Page](https://the.earth.li/~sgtatham/putty/0.77/htmldoc/)
  - This manual documents PuTTY, and its companion utilities PSCP, PSFTP, Plink, Pageant and PuTTYgen.

# grep, sed, awk
