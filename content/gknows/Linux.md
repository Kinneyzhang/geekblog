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
- SSH 免输入密钥登录
  - Linux: `sshpass -p <passwd>`
  - Windows: Then you need cygnative because there is an incompatibility of stdin/stdout redirecion between cygwin and native Win32 programs.
  - cygnative
    - Cygnative.exe is a wrapper for cygwin programs which call native Win32 programs with stdin and/or stdout handle redirected. Without cygnative.exe it is possible that the native program can not read from the handle and receives a "invalid handle" error.
    - [download](http://diario.beerensalat.info/2009/08/18/new_cygnative_version_1_2_for_rsync_plink.html)
