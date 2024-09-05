C ***** NO TABS AND NO FORMFEEDS IN ANY UNIX FORTRAN FILES *****
C
        SUBROUTINE XZCALL(VBLOCK)
C       
C       This subroutine is called by XZINIT after XZINIT receives
C       the initial parameter block from TAE.  The purpose
C       of XZCALL is to call initialization entry points of
C       application subroutine packages.
C
C       The argument of each application initialization
C       subroutine is the initial parameter
C       block received from TAE; this allows the package to
C       obtain parameters, globals, and local TCL variables
C       without the main program having to make explicit calls.
C
C       An installation will typically edit this module to call
C       installation-dependent subroutines.
C
C       After editing this module, execute the following DCL commands
C       to update the VAX/VMS library:
C
C               $SET DEFAULT   TAE$OLB
C               $FORTRAN/LIST  XZCALL
C               $LIBRARY  LIB  XZCALL
C       
C       Under UNIX systems, the shell commands to update the library are:
C
C               $cd  $TOLB
C               $f77 -c xzcall.f
C               $ar r lib.a xzcall.o
C
C       It is possible to have several versions of XZCALL.  In such a
C       case, the XZCALLs would not be in the subroutine library but
C       would be explicitly referenced at LINK time.  An example for
C       a VAX/VMS LINK command is:
C
C               $LINK/DEBUG  my-program, my-xzcall, -
C               TAE$LIB:LIB/LIBRARY, TAE$LIB:COTS/LIBRARY
C
C
        INTEGER VBLOCK(1)               
                                        
C       
C       Currently, there are no calls.  A typical initialization call
C       looks like:
C
C       CALL PLOT_INIT (VBLOCK)         
C
        RETURN
        END
