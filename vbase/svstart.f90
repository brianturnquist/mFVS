!=====================!
!   vbase : svstart   !
!=====================!
! VBASE $Id$
subroutine SVSTART
  implicit none

  !     STAND VISUALIZATION GENERATION
  !     N.L.CROOKSTON -- RMRS MOSCOW -- NOVEMBER 1998

  !     BUILD THE INITIAL DISPLAY OF THE INITIAL TREES.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'
  include 'CONTRL.f90'
  include 'ARRAYS.f90'
  include 'FMCOM.f90'
  include 'SVDATA.f90'
  include 'SVRCOM.f90'

  ! Variables
  logical :: DEBUG
  integer :: I,ID,IFC,IH,ISVOBJ,J,K,L

  call DBCHK (DEBUG,'SVSTART',7,ICYC)

  if (JSVOUT.eq.0) return

  !     START THE SVRANN GENERATOR AT THE SAME PLACE AS THE
  !     BASE MODEL GENERATOR.  THIS MAKES THE SVRANN GENERATOR
  !     RESPOND TO THE RANNSEED KEYWORD.

  call RANNGET(SVS0)

  !     SET UP THE PLOT GEOMETRY

  call SVGTPL

  NSVOBJ = 0
  do ISVOBJ = 1,MXSVOB
     IOBJTP(ISVOBJ) = 0
  end do

  !     PLACE THE INITIAL TREES.  NOTE THAT THIS CODE ASSUMES
  !     THAT THERE ARE NO OBJECTS IN THE OBJECT LIST.

  call SVESTB(0)

  !     If the FFE is active:
  !     The FFE CWD pools have not been loaded yet (FMMAIN & FMCBA have
  !     yet to be called); therefore, temporarily load some fire model
  !     tree attributes, then call FMCBA to load the initial CWD.
  !     This will enable CWD objects to be displayed in the SVS
  !     initial inventory display.

  if ( LFMON ) then
     do I = 1,ITRN
        FMPROB(I) = PROB(I)
        FMICR(I) = ICR(I)
        FIRKIL(I) = 0.0
     end do
     call FMCBA (IY(1),1)
  end if
  if ( DEBUG ) then
     write(JOSTND,1020) ICYC
     1020 format (' ','IN SVSTART, ICYC=',I2,':', / ,&
          &' ',T5,'CWD ARRAY (UNPILED ONLY):')
     do IH = 1,2
        do ID = 1,4
           write(JOSTND,1040) (CWD(1,IFC,IH,ID),IFC = 1,11), IH, ID
           1040 format(T5,11(F7.3,1X),'IH=',I1,', ID=',I1)
        end do
     end do
  end if

  !     OUTPUT THE INITIAL PICTURE

  call SVOUT(IY(1),0,'Inventory conditions')

  !     If FFE variables were temporarily loaded for the initial SVS
  !     display, reverse the process now, to avoid double-counting of
  !     initial CWD loadings.

  if ( LFMON ) then
     do I = 1,ITRN
        FMPROB(I) = 0.0
        FMICR(I) = 0
     end do
     do I = 1,3
        do J = 1,MXFLCL
           do K = 1,2
              do L = 1,5
                 CWD(I,J,K,L) = 0.0
              end do
           end do
        end do
     end do
  end if

  !  Special processing for SO variant, when FFE is not active:

  !  The SO variant is atypical in how the snagfall/decay coefficients
  !  are loaded. For the SO variants, the snagfall/decay coefficients
  !  are not loaded in FMVINIT. Their initialization is delayed until
  !  FMCBA, because certain parameters are dependent on the forest
  !  location code (whether you're in region 5 or 6), and the location
  !  code either isn't set yet in FMVINIT, or, could change.
  !  The following logic calls SOSNAG (an entry point in the SO/
  !  FMCBA routine), to assign the missing snagfall/decay coefficients
  !  when FFE is not active

  if ( .not. LFMON ) then
     if (VARACD .eq. 'SO' ) then
        call SNGCOE
     end if
  end if

  !     QUICK CHECKS/DEBUG

  if (DEBUG) call SVCDBH(WK3,0)

  return
end
