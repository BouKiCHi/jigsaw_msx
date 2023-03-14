#
# mmckc makefile for MinGW and POSIX
#
# target for POSIX,
#  make POSIX=1
#
# target for DEBUG,
#  make DEBUG=1
#

ifndef POSIX
WIN32 = 1
endif
# DEBUG = 1

CC = gcc
CFLAGS = 
EXEDIR= 
OBJDIR = objs

ifdef WIN32

 MKDIR = mkdir
 CP = copy
 RM = del
 EXESFX = .exe
 MAKODIR = $(OBJDIR)/
 CMDODIR = $(OBJDIR)\\
 INSTDIR = ..\..\bin

else

 MKDIR = mkdir -p
 CP = cp
 RM = rm -f
 CFLAGS += -DUNIX
 EXESFX = 
 MAKODIR = $(OBJDIR)/
 CMDODIR = $(OBJDIR)/
 INSTDIR = ../../bin

endif

ifdef DEBUG 

 CFLAGS += -g
 CFLAGS += -Wall# -mno-cygwin

else

 CFLAGS += -O2
 EXT_TARGET = strip

endif

CDEFS = 
LDFLAGS= # -mno-cygwin

OBJS = datamake.o file.o mckc.o strings.o

.PHONY: all clean strip objdir install
PRGNAME = mmckc

TARGET_E = $(EXEDIR)$(PRGNAME)$(EXESFX)
TARGET = $(EXEDIR)$(PRGNAME)_j$(EXESFX)

all: $(TARGET_E) $(TARGET) $(EXT_TARGET)

objdir : 
	$(MKDIR) $(CMDODIR)

$(TARGET_E): $(addprefix $(MAKODIR),$(OBJS) version_e.o) 
	$(CC) $(LDFLAGS) -o $@ $^

$(TARGET): $(addprefix $(MAKODIR),$(OBJS) version.o)
	$(CC) $(LDFLAGS) -o $@ $^

strip : $(TARGET_E) $(TARGET)
	strip $^

$(MAKODIR)version_e.o: version.c
	$(CC) $(CFLAGS) $(CDEFS) -DENGLISH -o $@ -c $^

$(MAKODIR)%.o : %.c
	$(CC) $(CFLAGS) $(CDEFS) -c $< -o $@

$(MAKODIR)mckc.o: mckc.h
$(MAKODIR)datamake.o: mckc.h

install:
	$(CP) $(TARGET_E) $(INSTDIR)
	$(CP) $(TARGET) $(INSTDIR)

clean:
	$(RM) $(addprefix $(CMDODIR),$(OBJS) version.o version_e.o)
	$(RM) $(TARGET_E) $(TARGET)

