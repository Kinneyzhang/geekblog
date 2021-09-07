#!/bin/bash
hugo
rsync -av /Users/geekinney/geekblog/public/ root@119.28.186.136:/var/www/hugoblog/
