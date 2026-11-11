!===============================!
!   Compute tree crown widths   !
!===============================!
! BASE $Id$
subroutine CWIDTH
  implicit none

  !  THIS SUBROUTINE COMPUTES A CROWN WIDTH FOR EACH TREE RECORD USING
  !  EITHER USER-DEFINED EQUATIONS OR DEFAULT EQUATIONS. IT ALSO
  !  PROCESSES CROWN WIDTH MODIFIERS ENTERED VIA THE FIXCW KEYWORD.
  !  THIS ROUTINE IS CALLED FROM **MAIN** TO LOAD THE INITIAL CROWN
  !  WIDTHS, AND FROM **UPDATE** TO LOAD THE END-OF-CYCLE VALUES.

  ! Common blocks
  include 'PRGPRM.F77'
  include 'ARRAYS.F77'
  include 'CONTRL.F77'
  include 'CWDCOM.F77'

  !  DEFINITIONS:

  !        I -- TREE SUBSCRIPT.
  !        D -- TREE DIAMETER.
  !        P -- NUMBER OF TREES PER ACRE REPRESENTED BY A TREE.

  ! Variables
  logical :: DEBUG,LINCL
  real    :: PRM(5)
  integer :: MYACTS(1)
  integer :: IG,IULIM,IGRP,ISPCC,IDATE,IACTK,NP,ITODO,NTODO,IWHO,IICR
  integer :: ISPC,I
  real    :: P,D,H,CR,CW

  ! Data statements
  !  ACTIVITY 90 = FIXCW

  data MYACTS/90/

  !  CHECK FOR DEBUG.

  call DBCHK (DEBUG,'CWIDTH',6,ICYC)
  if (DEBUG)write(JOSTND,*)' ENTERING CWIDTH ICYC,ITRN= ',ICYC,ITRN

  !  IF THERE ARE NO TREE RECORDS, SIMPLY PROCESS THE FIXCW REQUESTS.

  if (ITRN.le.0) goto 100

  !  START LOOP TO ESTIMATE CROWN WIDTH.  TREES ARE PROCESSED
  !  ONE AT A TIME WITHIN A SPECIES.

  do I = 1,ITRN
     ISPC = ISP(I)
     P = PROB(I)
     D = DBH(I)
     H = HT(I)
     CR = FLOAT(ICR(I))
     IICR = ICR(I)
     CRWDTH(I) = 0.0
     if (LSPCWE(ISPC)) then
        if (D .lt. CWTDBH(ISPC)) then
           CW = CWDS0(ISPC)+CWDS1(ISPC)*DBH(I)+ &
                & CWDS2(ISPC)*(DBH(I)**CWDS3(ISPC))
        else
           CW = CWDL0(ISPC)+CWDL1(ISPC)*DBH(I)+ &
                & CWDL2(ISPC)*(DBH(I)**CWDL3(ISPC))
        end if
     else
        IWHO = 0
        call CWCALC(ISPC,P,D,H,CR,IICR,CW,IWHO,JOSTND)
     end if
     CRWDTH(I) = CW
     if (DEBUG)write(JOSTND,*)' LIVE: I,ISPC,D,CR,CW= ', &
          & I,ISPC,D,CR,CW
     50 continue
  end do

  100 continue

  !  ESTIMATE CROWN WIDTH ON CYCLE 0 DEAD TREES
  !  PROB IS NOT USED IN ESTIMATION OF CROWN WIDTH SO DON'T
  !  WORRY ABOUT INFLATION OF DEAD-TREE PROB HERE.

  if (IREC2.gt.MAXTRE) go to 200
  do I = IREC2,MAXTRE
     ISPC = ISP(I)
     P = PROB(I)
     D = DBH(I)
     H = HT(I)
     CR = FLOAT(ICR(I))
     CRWDTH(I) = 0.0
     if (LSPCWE(ISPC)) then
        if (D .lt. CWTDBH(ISPC)) then
           CW = CWDS0(ISPC)+CWDS1(ISPC)*DBH(I)+ &
                & CWDS2(ISPC)*(DBH(I)**CWDS3(ISPC))
        else
           CW = CWDL0(ISPC)+CWDL1(ISPC)*DBH(I)+ &
                & CWDL2(ISPC)*(DBH(I)**CWDL3(ISPC))
        end if
     else
        IWHO = 0
        call CWCALC(ISPC,P,D,H,CR,IICR,CW,IWHO,JOSTND)
     end if
     CRWDTH(I) = CW
     if (DEBUG)write(JOSTND,*)' DEAD: I,ISPC,D,H,CW= ', &
          & I,ISPC,D,H,CW
     150 continue
  end do
  200 continue

  ! PROCESS THE FIXCW OPTIONS.

  call OPFIND (1,MYACTS(1),NTODO)
  if (DEBUG)write(JOSTND,*)' PROCESSING FIXCW, NTODO= ',NTODO
  if (NTODO.gt.0) then
     do ITODO = 1,NTODO
        call OPGET (ITODO,4,IDATE,IACTK,NP,PRM)
        if (DEBUG)write(JOSTND,*)' ITODO,IDATE,IACTK,NP,PRM= ', &
             & ITODO,IDATE,IACTK,NP,PRM
        if (IACTK.lt.0) goto 300
        if (ICYC.gt.0)call OPDONE(ITODO,IY(ICYC))
        ISPCC = IFIX(PRM(1))
        if (PRM(2) .lt. 0.) PRM(2) = 0.
        if (PRM(3) .lt. 0.) PRM(3) = 0.
        if (PRM(4) .le. 0.) PRM(4) = 999.
        if (DEBUG)write(JOSTND,*)' FIXCW: ISPCC,MULT,DBHLO,DBHHI= ', &
             & ISPCC,PRM(2),PRM(3),PRM(4)
        if (ITRN.gt.0) then
           do I = 1,ITRN
              LINCL = .false.
              if (ISPCC.eq.0 .or. ISPCC.eq.ISP(I)) then
                 LINCL = .true.
              else if (ISPCC.lt.0) then
                 IGRP = -ISPCC
                 IULIM = ISPGRP(IGRP,1)+1
                 do IG = 2,IULIM
                    if (ISP(I) .eq. ISPGRP(IGRP,IG)) then
                       LINCL = .true.
                       go to 215
                    end if
                    210 continue
                 end do
              end if
              215 continue
              if (LINCL .and. &
                   & (PRM(3).le.DBH(I) .and. DBH(I).lt.PRM(4))) then
                 CRWDTH(I) = CRWDTH(I)*PRM(2)
                 if (CRWDTH(I).gt. 99.9)CRWDTH(I) = 99.9
                 if (DEBUG)write(JOSTND,*)' LIVE: I,ISPCC,ISP,DBH,CRWDTH= ', &
                      & I,ISPCC,ISP(I),DBH(I),CRWDTH(I)
              end if
              290 continue
           end do
        end if
        if (IREC2.gt.MAXTRE)go to 295
        do I = IREC2,MAXTRE
           LINCL = .false.
           if (ISPCC.eq.0 .or. ISPCC.eq.ISP(I)) then
              LINCL = .true.
           else if (ISPCC.lt.0) then
              IGRP = -ISPCC
              IULIM = ISPGRP(IGRP,1)+1
              do IG = 2,IULIM
                 if (ISP(I) .eq. ISPGRP(IGRP,IG)) then
                    LINCL = .true.
                    go to 225
                 end if
                 220 continue
              end do
           end if
           225 continue
           if (LINCL .and. &
                & (PRM(3).le.DBH(I) .and. DBH(I).lt.PRM(4))) then
              CRWDTH(I) = CRWDTH(I)*PRM(2)
              if (CRWDTH(I).gt. 99.9)CRWDTH(I) = 99.9
              if (DEBUG)write(JOSTND,*)' DEAD: I,ISPCC,ISP,DBH,CRWDTH= ', &
                   & I,ISPCC,ISP(I),DBH(I),CRWDTH(I)
           end if
           293 continue
        end do
        295 continue
        300 continue
     end do
  end if

  if (DEBUG)write(JOSTND,*)' LEAVING CWIDTH ICYC= ',ICYC
  return
end subroutine CWIDTH
