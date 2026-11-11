!=================================!
!   Stand visualization: SVCDBH   !
!=================================!
! BASE $Id$
subroutine SVCDBH (REMOV,IST)
  implicit none

  !     STAND VISUALIZATION GENERATION
  !     J. J. MARCHINEK -- RMRS MOSCOW -- APRIL 1999
  !     N.L.CROOKSTON -- RMRS MOSCOW -- OCT 1999

  !     CALCULATES SOME PLOT AND TOTAL STATISTICS USEFUL TO
  !     COMPARE THE DISTRIBUTION OF TREES ON THE OBJECT LIST
  !     WITH THOSE ON THE FVS TREE LIST.

  !     CALLED FROM SVSTART, SVOUT, AND SVRMOV.
  !     INPUT:
  !     REMOV = VECTOR OF MORTALITY OR REMOVAL TREES/ACRE
  !     IST= 1 IF MORTALITY NEEDS TO BE TAKEN INTO ACCOUNT
  !        = 0 IF IT SHOULD NOT BE TAKEN INTO ACCOUNT

  ! Common blocks
  include 'PRGPRM.F77'
  include 'ARRAYS.F77'
  include 'CONTRL.F77'
  include 'SVDATA.F77'
  include 'PLOT.F77'

  !     WPP     - SUM OF REMOV PER PLOT
  !     PPP     - SUM OF PROB PER PLOT FROM FVS TREE LIST
  !     IPLCNT  - NUMBER OF OBJECT TREES PER PLOT
  !     CDBH    - CUM DBH*DBH PER PLOT FROM FVS TREE LIST
  !     CDBHOB  - CUM DBH*DBH PER PLOT FROM SV OBJECT LIST
  !     CPROB   - CUM PROB PER PLOT FROM SV OBJECT LIST
  !     REMOV   - MORTALITY BY TREE RECORD (PASSED IN)

  ! Variables
  real    :: WPP(MAXPLT)
  real    :: PPP(MAXPLT)
  integer :: IPLCNT(MAXPLT)
  real    :: CDBH(MAXPLT)
  real    :: CDBHOB(MAXPLT)
  real    :: CPROB(MAXPLT)
  real    :: REMOV(MAXTRE)
  integer :: IST,II,I,ICNT,ISVOBJ
  real    :: TPPP,RMV,TCDBHO,TCDBH,TDIFF,DIFF

  TPPP = 0
  do II = 1, MAXPLT
     WPP(II) = 0.
     PPP(II) = 0.
     IPLCNT(II) = 0
     CDBH(II) = 0.
     CDBHOB(II) = 0.
     CPROB(II) = 0.
  end do

  do II = 1,ITRN
     I = ITRE(II)
     if (IST.eq.1) then
        RMV = REMOV(II)
     else
        RMV = 0.
     end if
     WPP(I) = RMV + WPP(I)
     PPP(I) = PROB(II)-RMV + PPP(I)
     CDBH(I) = CDBH(I) +DBH(II)*DBH(II)*(PROB(II)-RMV)
  end do

  ICNT = 0
  do ISVOBJ = 1,NSVOBJ
     II = IS2F(ISVOBJ)
     if (II.gt.0 .and. IOBJTP(ISVOBJ).eq.1) then
        I = ITRE(II)
        if (IST.eq.1) then
           RMV = REMOV(II)
        else
           RMV = 0
        end if
        CDBHOB(I) = CDBHOB(I) + DBH(II)*DBH(II)
        CPROB(I) = PROB(II)-RMV+CPROB(I)
        IPLCNT(I) = IPLCNT(I)+1
        ICNT = ICNT+1
     end if
  end do
  TCDBHO = 0
  TCDBH = 0
  TDIFF = 0
  do II = 1, ISVINV
     if (IPLCNT(II) .eq. 0) then
        CDBHOB(II) = 0.
     else
        CDBHOB(II) = CDBHOB(II)/FLOAT(IPLCNT(II))
     end if
     TCDBHO = CDBHOB(II) +TCDBHO
     if (PPP(II) .eq. 0) then
        CDBH(II) = 0
     else
        CDBH(II) = CDBH(II)/PPP(II)
     end if
     TCDBH = CDBH(II)+TCDBH
     DIFF = CDBH(II)-CDBHOB(II)
     TDIFF = TDIFF+DIFF
     if (IST.eq.1) then
        RMV = REMOV(II)
     else
        RMV = 0
     end if
     write (JOSTND,*) 'PLOT=',II,'WPP=',WPP(II), &
          & 'PPP=',PPP(II),'IPLCNT=',IPLCNT(II),'D=', &
          & PPP(II)-IPLCNT(II),'CDBH=',CDBH(II), &
          & 'CDBHOB=',CDBHOB(II),'D=',DIFF
     TPPP = PPP(II)+TPPP
  end do
  write (JOSTND,*) 'TPPP=',TPPP,'ICNT=',ICNT,' D=',TPPP-ICNT, &
       & 'TCDBH=', TCDBH,'TCDBHO=',TCDBHO,'TDIF=',TDIFF

  return
end subroutine SVCDBH
