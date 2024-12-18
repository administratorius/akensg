# Microsoft Developer Studio Generated NMAKE File, Based on FELNESPR.DSP
!IF "$(CFG)" == ""
CFG=FELNESPR - Win32 Debug
!MESSAGE No configuration specified. Defaulting to FELNESPR - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "FELNESPR - Win32 Release" && "$(CFG)" != "FELNESPR - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "FELNESPR.MAK" CFG="FELNESPR - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "FELNESPR - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "FELNESPR - Win32 Debug" (based on "Win32 (x86) Console Application")
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

!IF  "$(CFG)" == "FELNESPR - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

ALL : "$(OUTDIR)\FELNESPR.exe"


CLEAN :
	-@erase "$(INTDIR)\FELNESPR.OBJ"
	-@erase "$(OUTDIR)\FELNESPR.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

F90_PROJ=/compile_only /include:"$(INTDIR)\\" /nologo /warn:nofileopt /module:"Release/" /object:"Release/" 
F90_OBJS=.\Release/
CPP_PROJ=/nologo /ML /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /Fp"$(INTDIR)\FELNESPR.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\FELNESPR.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib /nologo /subsystem:console /incremental:no /pdb:"$(OUTDIR)\FELNESPR.pdb" /machine:I386 /out:"$(OUTDIR)\FELNESPR.exe" 
LINK32_OBJS= \
	"$(INTDIR)\FELNESPR.OBJ"

"$(OUTDIR)\FELNESPR.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "FELNESPR - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

ALL : "$(OUTDIR)\FELNESPR.exe"


CLEAN :
	-@erase "$(INTDIR)\FELNESPR.OBJ"
	-@erase "$(OUTDIR)\FELNESPR.exe"
	-@erase "$(OUTDIR)\FELNESPR.ilk"
	-@erase "$(OUTDIR)\FELNESPR.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

F90_PROJ=/check:bounds /compile_only /debug:full /include:"$(INTDIR)\\" /nologo /warn:argument_checking /warn:nofileopt /module:"Debug/" /object:"Debug/" /pdbfile:"Debug/DF60.PDB" 
F90_OBJS=.\Debug/
CPP_PROJ=/nologo /MLd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /Fp"$(INTDIR)\FELNESPR.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\FELNESPR.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib /nologo /subsystem:console /incremental:yes /pdb:"$(OUTDIR)\FELNESPR.pdb" /debug /machine:I386 /out:"$(OUTDIR)\FELNESPR.exe" /pdbtype:sept 
LINK32_OBJS= \
	"$(INTDIR)\FELNESPR.OBJ"

"$(OUTDIR)\FELNESPR.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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
!IF EXISTS("FELNESPR.DEP")
!INCLUDE "FELNESPR.DEP"
!ELSE 
!MESSAGE Warning: cannot find "FELNESPR.DEP"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "FELNESPR - Win32 Release" || "$(CFG)" == "FELNESPR - Win32 Debug"
SOURCE=.\FELNESPR.FOR

"$(INTDIR)\FELNESPR.OBJ" : $(SOURCE) "$(INTDIR)"



!ENDIF 

