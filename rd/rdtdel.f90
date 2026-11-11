!=================!
!   rd : rdtdel   !
!=================!
! RD $Id$
subroutine RDTDEL(IVAC,IREC)
  implicit none

  !  THIS SUBROUTINE DELETES AND PACKS THE ROOT DISEASE TREE LISTS.

  !  CALLED BY :
  !     TREDEL  [PROGNOSIS]

  !  CALLS     :
  !     RDATV   (SUBROUTINE)   [ROOT DISEASE]

  !  PARAMETERS :
  !     IVAC   -
  !     IREC   -

  !  Revision History:
  !    03/07/95 - Last revision date.
  !    22-JUL-02 Lance R. David (FHTET)
  !      Removed unused array PROBO. It was also unused in the old
  !      annosus model.
  !   09/04/14 Lance R. David (FMSC)
  !     Added implicit none and declared variables.
  !   03/01/2016 Lance R. David (FMSC)
  !     Moved check to exit to top.
  !   03/21/2018 Lance R. David (FMSC)
  !     Added debug control.
  !     Added LGO (true if RD is active) to conditional exit.
  !     Removed unused 3rd subroutine parameter.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'RDPARM.f90'
  include 'CONTRL.f90'
  include 'RDCOM.f90'
  include 'RDADD.f90'
  include 'RDARRY.f90'

  ! Variables
  logical :: DEBUG,LGO,LTEE
  integer :: IDI,IP,IREC,ITSTP,IVAC,J,JINF
  real    :: TPAREA

  !.... See if we need to do some debug.

  call DBCHK (DEBUG,'RDTDEL',6,ICYC)

  if (DEBUG) write (JOSTND,*) 'ENTER RDTDEL'

  call RDATV (LGO,LTEE)
  if (DEBUG) write(JOSTND,*) 'IN RDTDEL: LGO, LTEE, LSTART=',&
       &LGO,LTEE,LSTART

  if ((.not. LTEE .and. LSTART) .or..not. LGO) return

  !     IF ROOT DISEASE NOT ACTIVE OR NO PATCH AREA THEN RETURN.  ALSO
  !     RETURN IF USING MANUAL INITIALIZATION AND THIS IS CYCLE 0 BECAUSE
  !     ROOT DISEASE ARRAYS ARE EMPTY.  NOTE CODE IN RDMN1.

  TPAREA = 0.0
  do IDI = MINRR,MAXRR
     TPAREA = TPAREA + PAREA(IDI)
     700 continue
  end do
  if (TPAREA .eq.0.0) return

  !     PACK THE OUTSIDE TREE DENSITY
  !     WHEN COMPRESSION OCCURS BEFORE ROOT DISEASE CYCLING
  !     IE, WHEN COMPRS CALLED FROM RDMN1 THEN SET TEMP VARIABLE ITSTP
  !     FOR SUMMARIZATION OF PROBI AND PROPI.

  ITSTP = ISTEP
  if (ISTEP .eq.0) ITSTP = 1

  !     IF (DEBUG) WRITE(JOSTND,777) IVAC,IREC
  !777  FORMAT(' IN RDTDEL :  IVAC IREC', 2I5)

  WK22(IVAC) = WK22(IREC)
  WK22(IREC) = -1.0
  RROOTT(IVAC) = RROOTT(IREC)
  RROOTT(IREC) = -1.0

  do J = 1,4
     ROOTH(J,IVAC) = ROOTH(J,IREC)
     ROOTH(J,IREC) = -1.0
     XMTH(J,IVAC) = XMTH(J,IREC)
     XMTH(J,IREC) = -1.0
     800 continue
  end do

  ROOTL(IVAC) = ROOTL(IREC)
  RRKILL(IVAC) = RRKILL(IREC)
  FPROB(IVAC) = FPROB(IREC)
  PROBIU(IVAC) = PROBIU(IREC)
  PROBIT(IVAC) = PROBIT(IREC)
  PROBL(IVAC) = PROBL(IREC)
  FFPROB(IVAC,1) = FFPROB(IREC,1)
  FFPROB(IVAC,2) = FFPROB(IREC,2)

  do JINF = 1,ITSTP
     do IP = 1,2
        PROBI(IVAC,JINF,IP) = PROBI(IREC,JINF,IP)
        PROPI(IVAC,JINF,IP) = PROPI(IREC,JINF,IP)
        !         IF (DEBUG) WRITE(JOSTND,881) PROBI(IVAC,JINF,IP)
        ! 881     FORMAT(' IN RDTDEL :  PROBI',E20.8)
        990 continue
     end do
     999 continue
  end do

  !     IF (DEBUG) WRITE (JOSTND,888)
  !    &  PROBI(IVAC,1,1),PROBIU(IVAC),FPROB(IVAC)
  ! 888 FORMAT (' IN RDTDEL :  CALC PROBI,PROBIU,FPROB',3E20.8)

  return
end
