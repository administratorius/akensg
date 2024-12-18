# Microsoft Developer Studio Generated NMAKE File, Based on LYGTIS.DSP
!IF "$(CFG)" == ""
CFG=LYGTIS - Win32 Debug
!MESSAGE No configuration specified. Defaulting to LYGTIS - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "LYGTIS - Win32 Release" && "$(CFG)" != "LYGTIS - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "LYGTIS.MAK" CFG="LYGTIS - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "LYGTIS - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "LYGTIS - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "LYGTIS - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

ALL : "$(OUTDIR)\LYGTIS.exe"


CLEAN :
	-@erase "$(INTDIR)\LYGTIS.OBJ"
	-@erase "$(OUTDIR)\LYGTIS.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

F90_PROJ=/compile_only /include:"$(INTDIR)\\" /nologo /warn:nofileopt /module:"Release/" /object:"Release/" 
F90_OBJS=.\Release/
CPP_PROJ=/nologo /ML /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /Fp"$(INTDIR)\LYGTIS.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\LYGTIS.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib /nologo /subsystem:console /incremental:no /pdb:"$(OUTDIR)\LYGTIS.pdb" /machine:I386 /out:"$(OUTDIR)\LYGTIS.exe" 
LINK32_OBJS= \
	"$(INTDIR)\LYGTIS.OBJ"

"$(OUTDIR)\LYGTIS.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "LYGTIS - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

ALL : "$(OUTDIR)\LYGTIS.exe"


CLEAN :
	-@erase "$(INTDIR)\LYGTIS.OBJ"
	-@erase "$(OUTDIR)\LYGTIS.exe"
	-@erase "$(OUTDIR)\LYGTIS.ilk"
	-@erase "$(OUTDIR)\LYGTIS.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

F90_PROJ=/check:bounds /compile_only /debug:full /include:"$(INTDIR)\\" /nologo /warn:argument_checking /warn:nofileopt /module:"Debug/" /object:"Debug/" /pdbfile:"Debug/DF60.PDB" 
F90_OBJS=.\Debug/
CPP_PROJ=/nologo /MLd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /Fp"$(INTDIR)\LYGTIS.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\LYGTIS.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib /nologo /subsystem:console /incremental:yes /pdb:"$(OUTDIR)\LYGTIS.pdb" /debug /machine:I386 /out:"$(OUTDIR)\LYGTIS.exe" /pdbtype:sept 
LINK32_OBJS= \
	"$(INTDIR)\LYGTIS.OBJ"

"$(OUTDIR)\LYGTIS.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 

.c{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.SUFFIXES: .fpp

.for{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

.f{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

.f90{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

.fpp{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  


!IF "$(NO_EXTERNAL_DEPS)" != "1"
!IF EXISTS("LYGTIS.DEP")
!INCLUDE "LYGTIS.DEP"
!ELSE 
!MESSAGE Warning: cannot find "LYGTIS.DEP"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "LYGTIS - Win32 Release" || "$(CFG)" == "LYGTIS - Win32 Debug"
SOURCE=.\LYGTIS.FOR

"$(INTDIR)\LYGTIS.OBJ" : $(SOURCE) "$(INTDIR)"



!ENDIF 

