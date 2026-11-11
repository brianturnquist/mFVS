!===================!
!   wsbwe : bwep2   !
!===================!
! WSBWE $Id$

subroutine BWEP2(IYRCUR)
  implicit none

  !  PRINT ANNUAL OUTPUT RE: BUDWORM MORTALITY & FEEDING

  !  K.A. SHEEHAN  USDA-FS, R6-NATURAL RESOURCES, PORTLAND, OR

  !   CALLED FROM: BWELIT

  !   SUBROUTINES AND FUNCTIONS CALLED:    NONE

  !   PARAMETERS:

  !   ITITLB - TITLE FOR BW MODEL
  !   IYRCUR - CURRENT YEAR
  !   MGMIDB - MANAGEMENT ID TAG FOR BW MODEL [BWEBOX]
  !   OUT2(IH,ISIZE,I) -- VARIABLES FOR I: 1= INITIAL NO. OF BW EGGS;
  !     2= TOTAL % MORTALITY, 3= NO. OF EGGS PRODUCED, 4= % NEW DEFOL.,
  !     5= % TOTAL DEFOL.

  !  Revision History:
  !   25-FEB-97
  !      Noted last revision date.
  !   02-JUN-2009 Lance R. David (FMSC)
  !      Added Stand ID and comma delimiter to output tables, some header
  !      and column labels modified.

  !      ***** NOTE ***** Modified this subroutine before realizing it
  !      **************** is not actually called by any other routine.
  !      This subroutine is currently just hanging around and not in use.

  ! Common blocks
  include 'BWECM2.f90'
  include 'BWEBOX.f90'

  ! Variables
  logical :: HEADER
  character(len=3) :: TRESP(6)
  character(len=5) :: TRESIZ(3)
  integer :: I,IH,IS,IYRCUR,NPLT
  real    :: DEFNEW,DEFTOT,PMORT,SAVOUT(7),SAVTRE(7),TOTFOL

  data HEADER/.true./
  data TRESP/' WF',' DF',' GF','SAF',' ES',' WL'/,&
       &TRESIZ/'SMALL','MED. ','LARGE'/

  do I = 1,7
     10 SAVOUT(I) = 0.0
  end do

  ! IF THIS IS THE FIRST CALL, PRINT THE HEADER

  if (HEADER) then
     write (JOBWP2,15) MGMIDB,ITITLB
     15 format (A4,' -- ',A72/)
     write (JOBWP2,20)
     20 format (49X,'INITIAL     FINAL       %         % DEFOL.',/,&
          &'STAND ID,',18X,'YEAR, SPECIES, SIZE,  # EGGS,   # EGGS,',&
          &'    MORT.,    NEW,     TOTAL,',/,&
          &26(','),', ----, -------, ----,  ------,   ------,',&
          &'   ------,   -----,    -----,')
     HEADER = .false.
  else
     write (JOBWP2,80)
     80 format (1X,6('----------'))
  end if

  !  PRINT OUT THE WHOLE SHEBANG!

  do IH = 1,6
     TOTFOL = OUT2(IH,1,7)+OUT2(IH,2,7)+OUT2(IH,3,7)
     if (TOTFOL.lt.1.0) goto 180

     do I = 1,7
        85 SAVTRE(I) = 0.0
     end do
     write (JOBWP2,90)
     90 format (1X)
     do IS = 1,3
        if (OUT2(IH,IS,7).lt.1.0) goto 120

        !  CALC. % BW MORTALITY AND % DEFOLIATION

        PMORT = -1.0
        if (OUT2(IH,IS,1).gt.0.0) PMORT = 100.0*(1.0-(OUT2(IH,IS,8)/&
             &OUT2(IH,IS,1)))
        DEFNEW = -1.0
        if (OUT2(IH,IS,6).gt.0.0) DEFNEW = 100.0*(1.0-(OUT2(IH,IS,4)/&
             &OUT2(IH,IS,6)))
        DEFTOT = -1.0
        if (OUT2(IH,IS,7).gt.0.0) DEFTOT = 100.0*(1.0-(OUT2(IH,IS,5)/&
             &OUT2(IH,IS,7)))
        write (JOBWP2,100) NPLT,IYRCUR,TRESP(IH),TRESIZ(IS),&
             &OUT2(IH,IS,1),OUT2(IH,IS,2),PMORT,DEFNEW,DEFTOT
        100 format (A26,', 'I4,',  ',A3,',  ',A5,', ',F8.0,', ',F8.0,&
             &', ',F8.1,',  ',F7.0,', ',F7.0)

        SAVOUT(1) = SAVOUT(1)+OUT2(IH,IS,1)
        SAVOUT(2) = SAVOUT(2)+OUT2(IH,IS,2)
        SAVOUT(3) = SAVOUT(3)+OUT2(IH,IS,8)
        SAVOUT(4) = SAVOUT(4)+OUT2(IH,IS,4)
        SAVOUT(5) = SAVOUT(5)+OUT2(IH,IS,5)
        SAVOUT(6) = SAVOUT(6)+OUT2(IH,IS,6)
        SAVOUT(7) = SAVOUT(7)+OUT2(IH,IS,7)
        SAVTRE(1) = SAVTRE(1)+OUT2(IH,IS,1)
        SAVTRE(2) = SAVTRE(2)+OUT2(IH,IS,2)
        SAVTRE(3) = SAVTRE(3)+OUT2(IH,IS,8)
        SAVTRE(4) = SAVTRE(4)+OUT2(IH,IS,4)
        SAVTRE(5) = SAVTRE(5)+OUT2(IH,IS,5)
        SAVTRE(6) = SAVTRE(6)+OUT2(IH,IS,6)
        SAVTRE(7) = SAVTRE(7)+OUT2(IH,IS,7)

        120 continue
     end do

     !  PRINT SUMMARY FOR THIS TREE SPECIES

     PMORT = -1.0
     if (SAVTRE(1).gt.0.0) PMORT = 100.0*(1.0-(SAVTRE(3)/SAVTRE(1)))
     DEFNEW = -1.0
     if (SAVTRE(6).gt.0.0) DEFNEW = 100.0*(1.0-(SAVTRE(4)/SAVTRE(6)))
     DEFTOT = -1.0
     if (SAVTRE(7).gt.0.0) DEFTOT = 100.0*(1.0-(SAVTRE(5)/SAVTRE(7)))
     write (JOBWP2,150) NPLT,IYRCUR,TRESP(IH),SAVTRE(1),SAVTRE(2),&
          &PMORT,DEFNEW,DEFTOT
     150 format (A26,', ',I4,',  ',A3,',  ','ALL  , ',2(F8.0,', '),&
          &F8.1,',  ',F7.0,', ',F7.0)

     !  RESET THE ARRAY TO ZERO

     180 do I = 1,8
        OUT2(IH,1,I) = 0.0
        OUT2(IH,2,I) = 0.0
        OUT2(IH,3,I) = 0.0
        190 continue
     end do
     200 continue
  end do

  !  PRINT OUT THE SUMMARY FOR THE WHOLE STAND

  PMORT = -1.0
  if (SAVOUT(1).gt.0.0) PMORT = 100.0*(1.0-(SAVOUT(3)/SAVOUT(1)))
  DEFNEW = -1.0
  if (SAVOUT(6).gt.0.0) DEFNEW = 100.0*(1.0-(SAVOUT(4)/&
       &SAVOUT(6)))
  DEFTOT = -1.0
  if (SAVOUT(7).gt.0.0) DEFTOT = 100.0*(1.0-(SAVOUT(5)/SAVOUT(7)))
  write (JOBWP2,300) NPLT,IYRCUR,&
       &SAVOUT(1),SAVOUT(2),PMORT,DEFNEW,DEFTOT
  300 format(/,A26,', ',I4,',  ','ALL  ,  ','ALL  ,  ',F8.0,', ',&
       &F8.0,', ',F8.1,',  ',F7.0,', ',F7.0,', ')

  return
end
