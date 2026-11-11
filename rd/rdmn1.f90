!================!
!   rd : rdmn1   !
!================!
! RD $Id$
subroutine RDMN1(INUM)
  implicit none

  !  THIS SUBROUTINE IS CALLED FROM PROGNOSIS MAIN AND INITIALIZES THE
  !  GENERAL ROOT DISEASE MODEL.

  !  CALLED BY :
  !     MAIN    [PROGNOSIS]

  !  CALLS     :
  !     RDATV   (SUBROUTINE)   [ROOT DISEASE]
  !     RDRANI  (SUBROUTINE)   [ROOT DISEASE]
  !     COMPRS  (SUBROUTINE)   [PROGNOSIS]
  !     OPADD   (SUBROUTINE)   [PROGNOSIS]
  !     OPINCR  (SUBROUTINE)   [PROGNOSIS]
  !     OPFIND  (SUBROUTINE)   [PROGNOSIS]
  !     OPDONE  (SUBROUTINE)   [PROGNOSIS]
  !     SPESRT  (SUBROUTINE)   [PROGNOSIS]
  !     RDPSRT  (SUBROUTINE)   [PROGNOSIS]
  !     PCTILE  (SUBROUTINE)   [PROGNOSIS]
  !     DIST    (SUBROUTINE)   [PROGNOSIS]
  !     COMP    (SUBROUTINE)   [PROGNOSIS]
  !     RDSETP  (SUBROUTINE)   [ROOT DISEASE]

  !  PARAMETERS :
  !     INUM   -

  !  Revision History:
  !     21-MAR-00 Lance David (FHTET)
  !       Removed equivalence of FVS array WK6 to RD array SPCNT and
  !       corrected the initialization process of SPCNT based on
  !       parameter MAXSP instead of literal 11.
  !       Reduced RETURN statements to 1 at the end of routine.
  !       Added debug code.
  !     25-MAR-2003 Lance R. David (FHTET)
  !       WK2 from ARRAYS common area is reset to zero before compression of
  !       treelist is requested. SVS processing had used WK2 and left residual
  !       values in it and comprs expects values in WK2 to be mortality which
  !       has not yet been predicted.
  !       Gary Dixon and Lance David discussed this problem and agreed
  !       on this solution to reset WK2 in this routine.
  !   08/29/14 Lance R. David (FMSC)
  !     Added implicit none and declared variables.

  !.... PARAMETER INCLUDE FILES

  ! Common blocks
  include 'PRGPRM.f90'
  include 'RDPARM.f90'

  !.... COMMON INCLUDE FILES

  include 'CONTRL.f90'
  include 'RDCOM.f90'
  include 'ARRAYS.f90'
  include 'OUTCOM.f90'

  ! Variables
  logical :: LGO,LTEE,DEBUG
  integer :: I,ICCY,IDI,II,IM,INUM,IS,KDT,MYACT(1)
  real    :: PRMS(2),SPCNT(MAXSP,3),TPAREA

  data MYACT /250/

  !.... SEE IF WE NEED TO DO SOME DEBUG.

  call DBCHK (DEBUG,'RDMN1',5,ICYC)
  if (DEBUG) write (JOSTND,*) 'ENTER RDMN1:  INUM=',INUM

  !     CHECK TO SEE IF GENERAL ROOT DISEASE MODEL IS ACTIVE

  call RDATV (LGO,LTEE)

  !     IF THIS IS FIRST CALL TO RDMN1 THEN PRINT SUMMARY ROOT DISEASE INFO.
  !     READ IN FROM TREE LIST

  if (LGO .and. INUM .eq.1) call RDTOUT

  !     RETURN IF GENERAL ROOT DISEASE MODEL NOT ACTIVE OR THERE IS NO
  !     ROOT DISEASE.

  TPAREA = 0.0
  do IDI = 1,ITOTRR
     TPAREA = TPAREA + PAREA(IDI)
     33 continue
  end do
  if (.not. LGO .or. TPAREA .eq.0.0) goto 225 !RETURN

  !     IF THIS IS FIRST CALL TO RDMN1 THEN SEED RANDOM NUMBER GENERATOR.

  if (INUM .eq.1) call RDRANI(DSEED)

  !     IF TREELIST INITIALIZATION IN EFFECT AND THIS IS FIRST PASS
  !     THRU RDMN1 THEN CALL RDSETP TO CALCULATE INITIALIZATION VALUES.
  !     IF TREELIST INITIALIZATION IN EFFECT AND THIS IS SECOND CALL TO
  !     RDMN1 THEN COMPRESS IF NUMBER OF TREES GREATER THAN VALUE
  !     SPECIFIED WITH RRCOMP KEYWORD. IF NO COMPRESS TO BE DONE THEN
  !     RETURN

  if (LTEE) then
     if (INUM .eq.1) then
        goto 222
     else if (INUM .eq.2.and. ITRN .gt. IRCOMP) then
        goto 90
     else
        goto 225
     end if
  end if

  !     IF MANUAL INITIALIZATION AND THIS IS FIRST PASS THRU RDMN1 THEN
  !     RETURN.  IF MANUAL INIT AND THIS IS SECOND PASS THRU RDMN1 AND
  !     COMPRESSION DESIRED THEN COMPRESS.  NOTE THAT COMPRESSION IS
  !     CALLED IF THERE ARE MORE THAN IRRTRE TREE RECORDS.  IN THIS
  !     SITUATION ONLY PROGNOSIS TREE VARIABLES ARE COMPRESSED AND NOT
  !     ROOT DISEASE VARIABLES.  OTHERWISE ROOT DISEASE COMPRESS
  !     WOULD REACH OUTSIDE ROOT DISEASE ARRAY BOUNDARIES.  DEACTIVATION
  !     OF ROOT DISEASE COMPRESS IS ACCOMPLISHED BY INSERTING SPECIAL CODE
  !     IN RDCMPR AND RDTDEL.

  if (.not. LTEE) then
     if (INUM .eq.1) then
        goto 225 !RETURN
     else if (INUM .eq.2.and. ITRN .le. IRRTRE) then
        goto 222
     else
        goto 90
     end if
  end if

  !     COMPRESS THE TREE LIST FOR ROOT DISEASE IF NUMBER OF TREES
  !     EXCEEDS DEFAULT VALUE FOR ROOT DISEASE MODEL.

  !     25-MAR-2003
  !     The FVS compression routine expects values in the WK2 array to
  !     be mortality. Mortality has not been predicted at this time, but
  !     other processes (SVS) may have used WK2 and left residual values.
  !     WK2 needs to be reset to zero for compression to function properly.
  !     As of this date, the next user of the WK2 array is CUTS which
  !     stores (not retrieves) removal priority values in WK2.
  !     This change was discussed and agreed upon by Gary Dixon and Lance David.

  90 continue
  ICCY = 1
  KDT = IY(ICCY)

  do I = 1,ITRN
     WK2(I) = 0.0
     91 continue
  end do

  call COMPRS (IRCOMP,0.5)

  !     ADD OPTION PROCESSING LOGIC FOR COMPRS.

  PRMS(1) = FLOAT(IRCOMP)
  PRMS(2) = 0.5

  call OPADD (KDT,MYACT(1),KDT,2,PRMS,II)

  !     REESTABLISH THE SPECIES-ORDER SORT.

  call SPESRT

  !     REESTABLISH THE DIAMETER SORT.

  call RDPSRT (ITRN,DBH,IND,.true.)

  !     INITIALIZE SPCNT (EQUIVALENCED TO WK6).

  !LRD     DO 50 I=1,33
  !LRD         WK6(I) = 0.0

  do I = 1,MAXSP
     do II = 1,3
        SPCNT(I,II) = 0.0
        49 continue
     end do
     50 continue
  end do

  !     RE-COMPUTE THE SPECIES COMPOSITION

  do I = 1,ITRN
     IS = ISP(I)
     IM = IMC(I)
     SPCNT(IS,IM) = SPCNT(IS,IM) + PROB(I)
     60 continue
  end do

  !     RE-COMPUTE THE DISTRIBUTION OF TREES PER ACRE AND SPECIES-
  !     TREE CLASS COMPOSITON BY TREES PER ACRE.

  !     (MAKE SURE IFST=1, TO GET A NEW SET OF POINTERS TO THE
  !      DISTRIBUTIONS).

  IFST = 1
  call PCTILE (ITRN,IND,PROB,WK3,ONTCUR(7))
  call DIST (ITRN,ONTCUR,WK3)
  call COMP (OSPCT,IOSPCT,SPCNT)
  if (LTEE .and. INUM .eq.2) goto 225

  !     CALL ROOT DISEASE SETUP ROUTINE.

  222 continue
  call RDSETP

  225 continue
  if (DEBUG) write (JOSTND,*) 'EXIT RDMN1'
  return
end
