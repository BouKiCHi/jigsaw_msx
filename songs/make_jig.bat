@echo off
..\bin\mmckc -i %1.mml
..\bin\pceas -raw jighdr.asm

if exist define.inc (
    del define.inc
    del jighdr.sym
)

if not exist jighdr.pce (
    exit /b
)

copy jighdr.pce ..\jigdata\%1.jig

del effect.h
del jighdr.pce
del %1.h
