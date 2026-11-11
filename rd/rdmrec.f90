!=================!
!   rd : rdmrec   !
!=================!
! RD $Id$
subroutine RDMREC (ITYP,I,KSP,OAMOVE)
  implicit none

  !  Purpose :
  !     This subroutine re-arranges other agent mortality so that when
  !     uninfected trees become infected or just inside a center, the
  !     appropriate other agent mortality will still be applied.

  !  Called By :
  !     RDINF   [ROOT DISEASE]
  !     RDINSD  [ROOT DISEASE]

  !  Calls :
  !     RDSSIZ   (SUBROUTINE)   [ROOT DISEASE]
  !     RDSTP    (SUBROUTINE)   [ROOT DISEASE]

  !  Revision History :
  !     06/12/96 - Matthew K. Thompson
  !                Moved the declaration of DSO, DSII, and DSIU to the
  !                parameter include file RDPARM.
  !     10/07/97 - Matthew K. Thompson
  !                Commented out the line of code that sets
  !                PROBI = PROBI - OAMOVE(DSII)
  !                The other agent mortality has already been deleted
  !                from PROBI.
  !   08/29/14 Lance R. David (FMSC)
  !     Added implicit none and declared variables.

  !.... Parameter include files.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'RDPARM.f90'

  !.... Coomon include fules.

  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'RDCOM.f90'
  include 'RDARRY.f90'
  include 'ARRAYS.f90'
  include 'RDADD.f90'

  !.... Local variable declaraions.

  ! Variables
  integer :: I,ISL,ITYP,KSP
  real    :: OAMOVE(3)
  integer :: IDANUW

  !.... Make sure that OAKL continues to contain the right number of
  !.... killed trees. Assume that some proportion of those trees that
  !.... were outside killed are now inside killed (either infected or
  !.... uninfected) (eg. If 10% of the outside trees were killed from
  !.... bark beetles or windthrow then 10% of those trees just becoming
  !.... inside trees were killed from bark beetles or windthrow).

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  IDANUW = ITYP

  OAKL(DSO,I) = OAKL(DSO,I) - OAMOVE(DSO)
  OAKL(DSII,I) = OAKL(DSII,I) + OAMOVE(DSII)
  OAKL(DSIU,I) = OAKL(DSIU,I) + OAMOVE(DSIU)

  if (OAMOVE(DSIU) .lt.0.0.and. OAKL(DSIU,I) .gt.0.0) then

     !....    New OAKL goes from DSIU to DSII.

     BBKILL(DSII,I) = BBKILL(DSII,I) + OAMOVE(DSII) *&
          &(BBKILL(DSIU,I) / OAKL(DSIU,I))
     BBKILL(DSIU,I) = BBKILL(DSIU,I) + OAMOVE(DSIU) *&
          &(BBKILL(DSIU,I) / OAKL(DSIU,I))

  else if (OAMOVE(DSIU) .ge.0.0.and. OAKL(DSO,I) .gt.0.0) then

     !....    New OAKL goes from DSO to DSII and DSIU.

     BBKILL(DSII,I) = BBKILL(DSII,I) + OAMOVE(DSII) *&
          &(BBKILL(DSO,I) / OAKL(DSO,I))
     BBKILL(DSIU,I) = BBKILL(DSIU,I) + OAMOVE(DSIU) *&
          &(BBKILL(DSO,I) / OAKL(DSO,I))
     BBKILL(DSO,I) = BBKILL(DSO,I) - (BBKILL(DSO,I) / OAKL(DSO,I)) *&
          &OAMOVE(DSO)
  end if

  !.... Bark beetle and windthrown trees are already removed from PROBI.
  !.... This line of code is double counting the removal.
  !     PROBI(I,ISTEP,ITYP) = PROBI(I,ISTEP,ITYP) - OAMOVE(DSII)

  RRKILL(I) = RRKILL(I) + OAMOVE(DSII)

  call RDSSIZ(KSP,DBH(I),STCUT,ISL,ISPS,IRTSPC)
  call RDSTP (ISL,KSP,OAMOVE(DSII),DBH(I),ROOTL(I))

  return
end
