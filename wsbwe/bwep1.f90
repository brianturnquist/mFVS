!===================!
!   wsbwe : bwep1   !
!===================!
! WSBWE $Id$
subroutine BWEP1(ISTAGE,IYRCUR)
  implicit none

  !  PRINT DETAILED OUTPUT RE: BUDWORM MORTALITY & FEEDING

  !  K.A. SHEEHAN  USDA-FS, R6-NATURAL RESOURCES, PORTLAND, OR

  !   CALLED FROM: BWELIT

  !   SUBROUTINES AND FUNCTIONS CALLED:    NONE

  !   PARAMETERS:

  !   ITITLB - TITLE FOR BW MODEL
  !   IYRCUR - CURRENT YEAR
  !   MGMIDB - MANAGEMENT ID TAG FOR BW MODEL [BWEBOX]
  !   OUT1(IC,IH,I) -- VARIABLES FOR I: 1= INITIAL NO. OF BW;
  !      2= NET BW DISPERSING IN; 3-9= % MORTALITY CAUSED BY DISPERSAL
  !      (3), STARVATION (4), ANTS (5), BIRDS (6), PARASITES (7),
  !      OTHERS (8), OR TOTAL (9); 10=KG NEW FOLIAGE, 11=KG OLD FOLIAGE,
  !      12-14= % DEFOLIATION FOR NEW (12), OLD (13), OR TOTAL (14)
  !      FOLIAGE; FOR OLDER LARVAE ONLY, 15-17 = % OF OLDER LARVAE THAT
  !      FEED ON NEW FOLIAGE (15), OLDER FOLIAGE (16), OR PUPATE EARLY (17).

  !  Revision History:
  !    14-JUL-2010 Lance R. David (FMSC)
  !       Previous noted revision 2/25/97.
  !       Added IMPLICIT NONE and declared variables as needed.

  ! Common blocks
  include 'BWECM2.f90'
  include 'BWEBOX.f90'

  ! Variables
  logical :: HEADER
  character(len=3) :: TRESP(6)
  character(len=3) :: CROWN(3)
  character(len=5) :: TRESIZ(3)
  character(len=5) :: STAGE(3)
  integer :: DEFTOT,I,IC,ICROWN,IH,ISIZE,ISTAGE,IYRCUR,J,K,LSIZE,LSPEC,LSTAGE,LYRCUR
  real    :: TEMP(7)

  data HEADER/.true./
  data TRESP/' WF',' DF',' GF','SAF',' ES',' WL'/,&
       &TRESIZ/'SMALL','MED. ','LARGE'/,&
       &CROWN/'TOP','MID','BOT'/,&
       &STAGE/'L2-L4','L4-L6','PUPAE'/

  ! IF THIS IS THE FIRST CALL, PRINT THE HEADER

  if (HEADER) then
     write (JOBWP1,15) MGMIDB,ITITLB
     15 format (A4,' -- ',A72/)
     write (JOBWP1,20)
     20 format (45X,'PERCENT MORTALITY CAUSED BY:',17X,'FOLIAGE',&
          &' (G)',11X,'  OLDER LARV FEED.'/&
          &45X,5('-----'),'---  TOTAL',2X,6('-----'),2X,&
          &3('-----'),/,5X,'TREE  TREE CROWN  BW   ',&
          &'INITIAL    NET ',34X,'%      AMT.   AMT.      % DEFOL.',6X,&
          &'%    %    %',/'YEAR SPEC. SIZE THIRD STAGE  NUMBER ',&
          &'  DISP.  DISP STAR ANTS BIRD PARA OTH. MORT.    NEW',&
          &'    OLD    NEW  OLD TOTAL',&
          &'  NEW  OLD EARLY'/'---- ----- ---- ----- -----  ------',&
          &' -------  ---- ---- ---- ---- ---- ---- -----  ------',&
          &' ------  ---- ---- ----- ---- ---- ----')
     LYRCUR = IYRCUR
     LSIZE = 0
     LSPEC = 0
  end if

  do IC = 1,9

     !  TRANSLATE CROWN LEVEL INTO TREE SIZE AND CROWN THIRD

     if (IC.le.3) then
        ISIZE = 1
     else if (IC.le.6) then
        ISIZE = 2
     else
        ISIZE = 3
     end if
     ICROWN = MOD(IC,3)
     if (ICROWN.eq.0) ICROWN = 3

     do IH = 1,6
        if (OUT1(IC,IH,1).lt.1.0) goto 200

        ! CALC. % MORT. FROM ACTUAL #'S KILLED

        OUT1(IC,IH,9) = 0.0
        do I = 3,8
           OUT1(IC,IH,9) = OUT1(IC,IH,9)+OUT1(IC,IH,I)
           40 continue
        end do
        do J = 1,7
           TEMP(J) = 100.0*OUT1(IC,IH,J+2)/(OUT1(IC,IH,1)+OUT1(IC,IH,2))
           60 continue
        end do

        ! CALC % DEFOL FOR TOTAL FOLIAGE

        if (ISTAGE.ne.3) then
           DEFTOT = ((OUT1(IC,IH,10)*OUT1(IC,IH,12))+(OUT1(IC,IH,11)*&
                &OUT1(IC,IH,13)))/(OUT1(IC,IH,10)+OUT1(IC,IH,11))
        end if

        !  PRINT OUT THE WHOLE SHEBANG!

        if (HEADER) then
           HEADER = .false.
           LYRCUR = IYRCUR
           LSTAGE = ISTAGE
           LSPEC = IH
           LSIZE = ISIZE
           write (JOBWP1,95)
        else if (LYRCUR.ne.IYRCUR) then
           write (JOBWP1,80)
           80 format (10X,11('----------'))
           write (JOBWP1,90)
           90 format (15X,11('*********'))
           write (JOBWP1,80)
           LYRCUR = IYRCUR
           LSTAGE = ISTAGE
           LSPEC = IH
           LSIZE = ISIZE
        else if (LSTAGE.ne.ISTAGE) then
           write (JOBWP1,80)
           LSTAGE = ISTAGE
           LSPEC = IH
           LSIZE = ISIZE
        else if (LSPEC.ne.IH) then
           write (JOBWP1,95)
           95 format (1X)
           LSPEC = IH
           LSIZE = ISIZE
        else if (LSIZE.ne.ISIZE) then
           write (JOBWP1,95)
           LSIZE = ISIZE
        end if
        if (ISTAGE.eq.1) then
           write (JOBWP1,100) IYRCUR,TRESP(IH),TRESIZ(ISIZE),&
                &CROWN(ICROWN),STAGE(ISTAGE),OUT1(IC,IH,1),OUT1(IC,IH,2),&
                &(TEMP(I),I = 1,7),(OUT1(IC,IH,J),J = 10,13),DEFTOT
           100 format (I4,2X,A3,2X,A5,1X,A3,2X,A5,1X,F8.0,F8.0,7F5.0,&
                &2F8.0,1X,F6.0,2F5.0,2X,' ---  ---  ---')
        else if (ISTAGE.eq.2) then
           write (JOBWP1,120) IYRCUR,TRESP(IH),TRESIZ(ISIZE),&
                &CROWN(ICROWN),STAGE(ISTAGE),OUT1(IC,IH,1),OUT1(IC,IH,2),&
                &(TEMP(I),I = 1,7),(OUT1(IC,IH,J),J = 10,13),DEFTOT,&
                &(OUT1(IC,IH,K),K = 15,17)
           120 format (I4,2X,A3,2X,A5,1X,A3,2X,A5,1X,F8.0,F8.0,7F5.0,&
                &2F8.0,1X,F6.0,2F5.0,1X,3F5.0)
        else
           write (JOBWP1,140) IYRCUR,TRESP(IH),TRESIZ(ISIZE),&
                &CROWN(ICROWN),STAGE(ISTAGE),OUT1(IC,IH,1),OUT1(IC,IH,2),&
                &(TEMP(I),I = 1,7)
           140 format (I4,2X,A3,2X,A5,1X,A3,2X,A5,1X,F8.0,F8.0,7F5.0,&
                &2X,'  ---     ---',3X,3('  ---'),3X,'---  ---  ---')
        end if

        !  RESET THE ARRAY TO ZERO

        do I = 1,17
           OUT1(IC,IH,I) = 0.0
           170 continue
        end do

        200 continue
     end do
  end do

  return
end
