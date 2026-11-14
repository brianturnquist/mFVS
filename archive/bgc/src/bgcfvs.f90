!==============================!
!   archive/bgc/src : bgcfvs   !
!==============================!
subroutine BGCFVS(X)

  !     BGCFVS BGC--DATE OF LAST REVISION: 05/29/01
  !                 Revised 11/12/02.  Removing index ISTND, and removing PPE
  !                                    common "includes" (PPEPRM, PPCNTL, &
  !                                    PRGPRM).  AJM
  !                 These changes--also made in BGCGO, BGCGROW, BGCIN, BGCINT,
  !                 BINITIAL, and BGCCOM.f77--remove all PPE funtionality.
  !                 The FVS-BGC code is now, once again, a single stand model.
  !                 Revised 6/3/03 AJM.  Added line re-setting WK2(I) to 0.0 if
  !                                      there is no BGC-projected mortality

  !     REPLACES DIAMETER, HEIGHT, CR AND PROB, IF USER INVOKES INTERACTIVE MODE
  !     (i.e. IF KEYWORD BGCGROW IS USED).
  !     CHANGES ARE MADE FROM HERE DIRECTLY TO FVS's COMMON BLOCK VARIABLES
  !     THIS ROUTINE IS CALLED TWICE,  FIRST CALL DEALS WITH HT/DIAM/PROB; SECOND CALL
  !     DEALS WITH CROWN RATIO.

  !     IBGC(ISTND)[note: IBGC is no longer dimensioned!--11/02 AJM ] IS SET
  !     TO 1 IN BGCIN IF BGC INCREMENTS ARE TO BE USED IN FVS.
  !     X IS FLAG PASSED FROM FVS INDICATING WHICH INCREMENTS ARE READY TO BE UPDATED.
  !     WHEN X=1, HT, DIAM, AND PROB ARE UPDATED; WHEN X=2, CROWN RATIOS ARE UPDATED.

  !     CALLED FROM: GRADD OF FVS BASE CODE

  ! Common blocks
  include 'ENTITY.f90'
  include 'BGCCOM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  !      INCLUDE 'PPCNTL.F77'                            ! removed 11/02 ajm
  include 'PRGPRM.f90'

  ! Variables
  integer :: X ! FLAG FROM FVS DICTATING WHICH INCREMENT IT IS READY TO HAVE BGC UPDATE.
  !     REAL BGC_DG(*), BGC_HG(*)

  !     LOAD BGC INCREMENTS INTO FVS ARRAYS. VALUES ARE IN ENGLISH UNITS

  !      IF (IBGC(ISTND) .EQ. 1 .AND. X .EQ. 1) THEN       !removed ajm 11/02
  if (IBGC .eq.1.and. X .eq.1) then
     do I = 1,ITRN
        DG(I) = -99.
        do J = 1, NT
           if (IDTREE(I).eq.TREENO(J)) then
              DG(I) = PASSDG(J)
              HTG(I) = PASSHG(J)
              WK2(I) = 0.0
              go to 100
           end if
           200 continue
        end do
        if (DG(I) .lt.0.0) then
           DG(I) = 0.0
           HTG(I) = 0.0
           WK2(I) = PROB(I)
        end if
        100 continue
     end do

     !      ELSE IF (IBGC(ISTND) .EQ. 1 .AND. X .EQ. 2) THEN  !removed ajm 11/02
  else if (IBGC .eq.1.and. X .eq.2) then
     do I = 1,ITRN
        do J = 1, NT
           if (IDTREE(I).eq.TREENO(J)) then
              ICR(I) = IFIX(CR(J)*100)
              go to 300
           end if
           400 continue
        end do
        300 continue
     end do

  end if

  return
end
