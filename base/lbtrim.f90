!=====================================!
!   Match stand and activity labels   !
!=====================================!
! BASE $Id$
subroutine LBTRIM (MAE,MAELNK)
  implicit none

  !     CALLED FROM PPEVMI, DELETE ACTIVITY GROUPS THAT MIGHT CREATE
  !     BRANCHES IF THE INTERSECTION OF THE STAND LABEL SET AND THE
  !     ACTIVITY GROUP LABEL SET IS NULL.

  !     N.L. CROOKSTON--FORESTRY SCIENCES LAB, MOSCOW, ID--JAN 1987.

  !     MAE   = LENGTH OF MAELNK.
  !     MAELNK= LIST OF ACTIVITY GROUPS THAT ARE ASSOCIATED WITH
  !             AN EVENT THAT HAS MORE THAN ONE ACTIVITY GROUP
  !             ASSOCIATED WITH IT.
  !             (1,.)= POINTS TO THE ENTRY IN IEVACT, AGLSET, & LENAGL
  !             (2,.)= POINTS TO THE ENTRY IN IEVNTS

  ! Common blocks
  include 'PRGPRM.F77'
  include 'OPCOM.F77'

  ! Variables
  integer :: MAELNK(2,50),MAE,IDEL,I,J,KODE,LEN

  !     IF THE LENGTH OF MAELNK IS ZERO, BRANCH TO RETURN

  if (MAE.le.0) goto 100

  !     DO THROUGH THE ACTIVITY GROUPS, AND FLAG MEMBERS OF MEMLNK
  !     FOR ACTIVITY GROUPS THAT SHOULD BE DROPPED.

  IDEL = 0
  do I = 1,MAE

     J = MAELNK(1,I)

     !     IF THE LENGTH OF THE AGLSET IS -1, THEN KEEP THE ACTIVIY GROUP

     if (LENAGL(J).ge.0) then
        call LBINTR (LENSLS,SLSET,LENAGL(J), &
             & AGLSET(J),LEN,WKSTR1,KODE)
        if (LEN.eq.0) then
           MAELNK(1,I) = 0
           IDEL = IDEL+1
        end if
     end if
     10 continue
  end do

  !     IF SOME ACTIVITY GROUPS ARE DELETED, THEN

  if (IDEL.gt.0) then

     !        IF ALL OF THE ACTIVIY GROUPS HAVE BEEN DELETED, THEN:

     if (IDEL.eq.MAE) then
        MAE = 0
     else

        !           ELSE SOME HAVE BEEN DELETED, COMPRESS THE LIST MAELNK.

        LEN = 0
        do I = 1,MAE
           if (MAELNK(1,I).le.0) goto 20
           LEN = LEN+1
           MAELNK(1,LEN) = MAELNK(1,I)
           MAELNK(2,LEN) = MAELNK(2,I)
           20 continue
        end do
        MAE = LEN
     end if
  end if

  100 continue
  return
end subroutine LBTRIM
