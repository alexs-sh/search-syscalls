# About

![Build status](https://github.com/alexs-sh/search-syscalls/actions/workflows/build.yml/badge.svg)

A simple helper for searching words which looks like a syscall in C/C++ apps.

# Build and run

```
cabal run search-syscalls  -- ~/projects/iputils                                                                                                                                                                            1m44s
Checking root directory...
  -> OK
Building list of files...
  -> 30 matche(s) found
Searching for syscalls...
  -> 47 match(es) found

accept,access,alarm,bind,close,connect,exit,fcntl,fork,geteuid,getpid,getrandom,getsockname,getsockopt,gettimeofday,getuid,ioctl,link,listen,nanosleep,open,pipe,poll,ppoll,prctl,read,recvfrom,recvmsg,select,sendfile,sendmsg,sendto,setgid,setgroups,setitimer,setsockopt,setuid,signalfd,socket,stat,syslog,time,times,truncate,uname,unlink,write
```
