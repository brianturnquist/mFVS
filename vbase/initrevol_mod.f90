!=======================================================!
!   Volume-equation keyword handlers shared by INITRE   !
!=======================================================!
module INITREVOL_MOD
  implicit none

  ! VOLEQNUM handler shared by the US, ON and BC INITRE. The US copy's FIA
  ! VBC guard and AK forest branch, and ON/BC's region-list entries 'BC' and
  ! 'ON', are unreachable in the variants that lacked them. Locals are passed
  ! by reference; RETURN replaces INITRE's GOTO 10 and IKEXIT reports any
  ! other exit INITRE must take.

  private
  public :: INVVOLEQNUM

contains

  subroutine INVVOLEQNUM(LKECHO,KEYWRD,IRDUM,IREGN,IFORST,FORST,INTDIST,DIST,&
       &IFOREST,FORDUM,IS,ARRAY,KARD,IGRP,IULIM,LNOTBK,IG,IGSP,ISPEC,PROD,&
       &ERRFLAG,ILEN,I)
    implicit none

    include 'PRGPRM.f90'
    include 'CONTRL.f90'
    include 'PLOT.f90'
    include 'VOLSTD.f90'

    logical :: LKECHO
    character(len=8) :: KEYWRD
    integer :: IRDUM
    integer :: IREGN
    integer :: IFORST
    character(len=2) :: FORST
    integer :: INTDIST
    character(len=2) :: DIST
    integer :: IFOREST
    character(len=2) :: FORDUM
    integer :: IS
    real :: ARRAY(12)
    character(len=10) :: KARD(12)
    integer :: IGRP
    integer :: IULIM
    logical :: LNOTBK(12)
    integer :: IG
    integer :: IGSP
    integer :: ISPEC
    character(len=2) :: PROD
    integer :: ERRFLAG
    integer :: ILEN
    integer :: I

    if (LFIANVB) then
       if (LKECHO)write(JOSTND,13010) KEYWRD
       13010 format (/A8,'   KEYWORD REQUEST HAS BEEN RECOGNIZED BUT HAS ',&
            &'BEEN DEACTIVIATED BY A PREVIOUS CALL TO USE FIA VBC ',&
            &'FOR COMPUTATION OF CUBIC FOOT VOLUMES')
       return
    end if

    !  IF WE DONT HAVE A KODFOR VALUE YET SET REGION BASED ON VARIANT

    if (KODFOR.le.0) then
       select case (VARACD)
       case('EM','KT','NI','IE','BC')
          IRDUM = 1
       case( 'CR')
          IRDUM = 2
       case('CI','TT','UT')
          IRDUM = 4
       case('CA','NC','SO','WS')
          IRDUM = 5
       case('BM','PN','WC','EC')
          IRDUM = 6
       case('OC','OP')
          IRDUM = 7
       case('SN')
          IRDUM = 8
       case('CS','LS','NE','ON')
          IRDUM = 9
       case('AK')
          IRDUM = 10
       end select
    else

       !   FOREST CODE PROCESSING

       if ((VARACD.eq.'SN').and.&
            &(KODFOR.ge.1000)) then
          IREGN = KODFOR/10000
          IRDUM = IREGN
          IFORST = KODFOR/100-IREGN*100
          write(FORST,'(I2)')IFORST
          if (IFORST.lt.10)FORST(1:1) = '0'
          INTDIST = KODFOR - (KODFOR/100)*100
          write(DIST,'(I2)')INTDIST
          if (INTDIST.lt.10)DIST(1:1) = '0'
       else if (VARACD.eq.'AK') then
          IREGN = 10
          FORST = '02'
       else
          IREGN = KODFOR/100
          IRDUM = IREGN
          IFOREST = KODFOR-900
          write(FORST,'(I2)')IFOREST
          if (IFOREST.lt.10)FORST(1:1) = '0'
       end if

       !  USE SPECIAL FORESTS FOR BIA AND PRIVATE VOLUME EQS.
       !  PN-QUINAULT IR (FC=800 USES OLYMIC 609)
       !  NC SIMPSOM TIMBER (FC=800 USES R5 SAY 510)
       !  SO INDUSTRY LANDS (FC=701 USES R5 SAY 505)
       !  SO WARM SPRINGS (FC=799 USES R6 FC=601)

       if (VARACD.eq.'PN' .or. VARACD.eq.'OP') then
          if (IREGN.eq.8) then
             IRDUM = 6
             FORDUM = '09'
          else
             IRDUM = IREGN
             FORDUM = FORST
          end if
       else if (VARACD.eq.'NC') then
          if (IREGN.eq.8) then
             IRDUM = 5
             FORDUM = '10'
          else
             IRDUM = IREGN
             FORDUM = FORST
          end if
       else if (VARACD.eq.'SO') then
          if (IREGN.eq.7) then
             select case (KODFOR)
             case (701)
                IRDUM = 5
                FORDUM = '05'
             case (799)
                IRDUM = 6
                FORDUM = '01'
             end select
          else
             IRDUM = IREGN
             FORDUM = FORST
          end if
       else
          IRDUM = IREGN
          FORDUM = FORST
       end if
    end if

    !   SPECIES CODE PROCESSING.

    call SPDECD (1,IS,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
         &ARRAY,KARD)
    if (IS .eq. -999) return

    !  SPECIES GROUP PROCESSING

    if (IS .lt.0) then
       IGRP = -IS
       IULIM = ISPGRP(IGRP,1)+1
       if (LNOTBK(2)) then
          do IG = 2,IULIM
             IGSP = ISPGRP(IGRP,IG)
             VEQNNC(IGSP) = KARD(2)
          end do

          !  CHECK FOR VALID CUBIC FT EQUATION NUMBER

          ISPEC = 9999
          PROD = '02'
          call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
               &VEQNNC(IGSP),ERRFLAG)

          !  CHECK FOR CASES WHERE VARIANTS CROSS REGIONAL BOUNDARIES

          if ((KODFOR.le.0).and.(ISPEC.ne.8888)) then
             select case(IRDUM)
             case(1)
                if (VARACD.eq.'IE') then
                   IRDUM = 6
                   call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
                        &VEQNNC(IGSP),ERRFLAG)
                end if
             case(2)
                if (VARACD.eq.'CR') then
                   IRDUM = 3
                   call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
                        &VEQNNC(IGSP),ERRFLAG)
                end if
             case(5)
                if ((VARACD.eq.'CA').or.(VARACD.eq.'NC').or.&
                     &(VARACD.eq.'SO').or.(VARACD.eq.'OC')) then
                   IRDUM = 6
                   call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
                        &VEQNNC(IGSP),ERRFLAG)
                end if
             case(7)
                if (VARACD.eq.'OC') then
                   IRDUM = 6
                   call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
                        &VEQNNC(IGSP),ERRFLAG)
                   if (ISPEC.ne.8888) then
                      IRDUM = 5
                      call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
                           &VEQNNC(IGSP),ERRFLAG)
                   end if
                end if
             end select
          end if

          if (ISPEC.ne.8888) then
             do IG = 2,IULIM
                IGSP = ISPGRP(IGRP,IG)
                VEQNNC(IGSP) = '           '
             end do
             ILEN = ISPGRP(-IS,92)
             write(JOSTND,13003)KEYWRD,KARD(1)(1:ILEN),IS
          else
             ILEN = ISPGRP(-IS,92)
             if (LKECHO)write(JOSTND,13008)KEYWRD,KARD(1)(1:ILEN),IS
          end if
       end if
       if (LNOTBK(3)) then
          do IG = 2,IULIM
             IGSP = ISPGRP(IGRP,IG)
             VEQNNB(IGSP) = KARD(3)
          end do

          !  CHECK FOR VALID BOARD FT EQUATION NUMBER

          ISPEC = 9999
          PROD = '01'
          call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
               &VEQNNB(IGSP),ERRFLAG)

          !  CHECK FOR CASES WHERE VARIANTS CROSS REGIONAL BOUNDARIES

          if ((KODFOR.le.0).and.(ISPEC.ne.8888)) then
             select case(IRDUM)
             case(1)
                if (VARACD.eq.'IE') then
                   IRDUM = 6
                   call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
                        &VEQNNB(IGSP),ERRFLAG)
                end if
             case(2)
                if (VARACD.eq.'CR') then
                   IRDUM = 3
                   call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
                        &VEQNNB(IGSP),ERRFLAG)
                end if
             case(5)
                if ((VARACD.eq.'CA').or.(VARACD.eq.'NC').or.&
                     &(VARACD.eq.'SO').or.(VARACD.eq.'OC')) then
                   IRDUM = 6
                   call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
                        &VEQNNB(IGSP),ERRFLAG)
                end if
             case(7)
                if (VARACD.eq.'OC') then
                   IRDUM = 6
                   call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
                        &VEQNNB(IGSP),ERRFLAG)
                   if (ISPEC.ne.8888) then
                      IRDUM = 5
                      call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
                           &VEQNNB(IGSP),ERRFLAG)
                   end if
                end if
             end select
          end if

          if (ISPEC.ne.8888) then
             do IG = 2,IULIM
                IGSP = ISPGRP(IGRP,IG)
                VEQNNB(IGSP) = '           '
             end do
             ILEN = ISPGRP(-IS,92)
             if (LNOTBK(2))KEYWRD = '        '
             write(JOSTND,13004)KEYWRD,KARD(1)(1:ILEN),IS
          else
             ILEN = ISPGRP(-IS,92)
             if (LNOTBK(2))KEYWRD = '        '
             if (LKECHO)write(JOSTND,13009)KEYWRD,KARD(1)(1:ILEN),IS
          end if
       end if
       return
       13003 format(/A8,'   INVALID VOLUME EQUATION NUMBER: THE DEFAULT ',&
            &'CUBIC FT EQUATION NUMBER WILL BE USED FOR SPECIES GROUP=',A,&
            &' (CODE=',I3,');')
       13004 format(/A8,'   INVALID VOLUME EQUATION NUMBER: THE DEFAULT ',&
            &'BOARD FT EQUATION NUMBER WILL BE USED FOR SPECIES GROUP=',A,&
            &' (CODE=',I3,');')
       13008 format(/A8,'   NATIONAL CRUISE SYSTEM CUBIC FT VOLUME EQUA',&
            &'TION NUMBERS CHANGED FOR SPECIES GROUP=',A,' (CODE=',I3,');')
       13009 format(/A8,'   NATIONAL CRUISE SYSTEM BOARD FT VOLUME EQUA',&
            &'TION NUMBERS CHANGED FOR SPECIES GROUP=',A,' (CODE=',I3,');')

       !  ALL SPECIES PROCESSING

    else if (IS.eq.0) then
       if (LNOTBK(2)) then
          do I = 1,MAXSP
             VEQNNC(I) = KARD(2)
          end do

          !  CHECK FOR VALID CUBIC FT EQUATION NUMBER

          ISPEC = 9999
          PROD = '02'
          call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
               &VEQNNC(1),ERRFLAG)

          !  CHECK FOR CASES WHERE VARIANTS CROSS REGIONAL BOUNDARIES

          if ((KODFOR.le.0).and.(ISPEC.ne.8888)) then
             select case(IRDUM)
             case(1)
                if (VARACD.eq.'IE') then
                   IRDUM = 6
                   call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
                        &VEQNNC(1),ERRFLAG)
                end if
             case(2)
                if (VARACD.eq.'CR') then
                   IRDUM = 3
                   call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
                        &VEQNNC(1),ERRFLAG)
                end if
             case(5)
                if ((VARACD.eq.'CA').or.(VARACD.eq.'NC').or.&
                     &(VARACD.eq.'SO').or.(VARACD.eq.'OC')) then
                   IRDUM = 6
                   call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
                        &VEQNNC(1),ERRFLAG)
                end if
             case(7)
                if (VARACD.eq.'OC') then
                   IRDUM = 6
                   call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
                        &VEQNNC(1),ERRFLAG)
                   if (ISPEC.ne.8888) then
                      IRDUM = 5
                      call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
                           &VEQNNC(1),ERRFLAG)
                   end if
                end if
             end select
          end if

          if (ISPEC.ne.8888) then
             do I = 1,MAXSP
                VEQNNC(I) = '           '
             end do
             write(JOSTND,13006)KEYWRD
          else
             if (LKECHO)write(JOSTND,13020)KEYWRD
          end if
       end if
       if (LNOTBK(3)) then
          do I = 1,MAXSP
             VEQNNB(I) = KARD(3)
          end do

          !  CHECK FOR VALID BOARD FT EQUATION NUMBER

          ISPEC = 9999
          PROD = '01'
          call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
               &VEQNNB(1),ERRFLAG)

          !  CHECK FOR CASES WHERE VARIANTS CROSS REGIONAL BOUNDARIES

          if ((KODFOR.le.0).and.(ISPEC.ne.8888)) then
             select case(IRDUM)
             case(1)
                if (VARACD.eq.'IE') then
                   IRDUM = 6
                   call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
                        &VEQNNB(1),ERRFLAG)
                end if
             case(2)
                if (VARACD.eq.'CR') then
                   IRDUM = 3
                   call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
                        &VEQNNB(1),ERRFLAG)
                end if
             case(5)
                if ((VARACD.eq.'CA').or.(VARACD.eq.'NC').or.&
                     &(VARACD.eq.'SO').or.(VARACD.eq.'OC')) then
                   IRDUM = 6
                   call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
                        &VEQNNB(1),ERRFLAG)
                end if
             case(7)
                if (VARACD.eq.'OC') then
                   IRDUM = 6
                   call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
                        &VEQNNB(1),ERRFLAG)
                   if (ISPEC.ne.8888) then
                      IRDUM = 5
                      call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
                           &VEQNNB(1),ERRFLAG)
                   end if
                end if
             end select
          end if

          if (ISPEC.ne.8888) then
             do I = 1,MAXSP
                VEQNNB(I) = '           '
             end do
             if (LNOTBK(2))KEYWRD = '        '
             write(JOSTND,13016)KEYWRD
          else
             if (LNOTBK(2))KEYWRD = '        '
             if (LKECHO)write(JOSTND,13021)KEYWRD
          end if
       end if
       return
       13006 format(/A8,'   INVALID VOLUME EQUATION NUMBER: THE',&
            &' DEFAULT CUBIC FT EQUATION NUMBERS WILL BE USED FOR ALL ',&
            &'SPECIES:')
       !       IF(LKECHO)WRITE(JOSTND,13021)KEYWRD
       13020 format(/A8,'   NATIONAL CRUISE SYSTEM CUBIC FT ',&
            &'VOLUME EQUATION NUMBERS CHANGED FOR ALL SPECIES:')
       13016 format(/A8,'   INVALID VOLUME EQUATION NUMBER: THE',&
            &' DEFAULT BOARD FT EQUATION NUMBERS WILL BE USED FOR ALL ',&
            &'SPECIES:')
       !       IF(LKECHO)WRITE(JOSTND,13021)KEYWRD
       13021 format(/A8,'   NATIONAL CRUISE SYSTEM BOARD FT VOLUME ',&
            &'EQUATION NUMBERS CHANGED FOR ALL SPECIES:')

       !  SINGLE SPECIES PROCESSING

    else

       !  CHECK FOR VALID EQUATION NUMBER CUBIC FT

       if (LNOTBK(2)) then
          VEQNNC(IS) = KARD(2)
          ISPEC = 9999
          PROD = '02'
          call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
               &VEQNNC(IS),ERRFLAG)

          !  CHECK FOR CASES WHERE VARIANTS CROSS REGIONAL BOUNDARIES

          if ((KODFOR.le.0).and.(ISPEC.ne.8888)) then
             select case(IRDUM)
             case(1)
                if (VARACD.eq.'IE') then
                   IRDUM = 6
                   call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
                        &VEQNNC(IS),ERRFLAG)
                end if
             case(2)
                if (VARACD.eq.'CR') then
                   IRDUM = 3
                   call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
                        &VEQNNC(IS),ERRFLAG)
                end if
             case(5)
                if ((VARACD.eq.'CA').or.(VARACD.eq.'NC').or.&
                     &(VARACD.eq.'SO').or.(VARACD.eq.'OC')) then
                   IRDUM = 6
                   call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
                        &VEQNNC(IS),ERRFLAG)
                end if
             case(7)
                if (VARACD.eq.'OC') then
                   IRDUM = 6
                   call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
                        &VEQNNC(IS),ERRFLAG)
                   if (ISPEC.ne.8888) then
                      IRDUM = 5
                      call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
                           &VEQNNC(IS),ERRFLAG)
                   end if
                end if
             end select
          end if

          if (ISPEC.ne.8888) then
             VEQNNC(IS) = '           '
             KARD(2) = '***INVALID'
             write(JOSTND,13030)KEYWRD,KARD(1)(1:3),IS
          else
             if (LKECHO)write(JOSTND,13030)KEYWRD,KARD(1)(1:3),IS
          end if
       end if

       !  CHECK FOR VALID EQUATION NUMBER BOARD FT

       if (LNOTBK(3)) then
          VEQNNB(IS) = KARD(3)
          ISPEC = 9999
          PROD = '01'
          call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
               &VEQNNB(IS),ERRFLAG)

          !  CHECK FOR CASES WHERE VARIANTS CROSS REGIONAL BOUNDARIES

          if ((KODFOR.le.0).and.(ISPEC.ne.8888)) then
             select case(IRDUM)
             case(1)
                if (VARACD.eq.'IE') then
                   IRDUM = 6
                   call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
                        &VEQNNB(IS),ERRFLAG)
                end if
             case(2)
                if (VARACD.eq.'CR') then
                   IRDUM = 3
                   call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
                        &VEQNNB(IS),ERRFLAG)
                end if
             case(5)
                if ((VARACD.eq.'CA').or.(VARACD.eq.'NC').or.&
                     &(VARACD.eq.'SO').or.(VARACD.eq.'OC')) then
                   IRDUM = 6
                   call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
                        &VEQNNB(IS),ERRFLAG)
                end if
             case(7)
                if (VARACD.eq.'OC') then
                   IRDUM = 6
                   call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
                        &VEQNNB(IS),ERRFLAG)
                   if (ISPEC.ne.8888) then
                      IRDUM = 5
                      call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
                           &VEQNNB(IS),ERRFLAG)
                   end if
                end if
             end select
          end if

          if (ISPEC.ne.8888) then
             VEQNNB(IS) = '           '
             KARD(3) = '***INVALID'
             if (.not.LNOTBK(2))write(JOSTND,13030)KEYWRD,KARD(1)(1:3),IS
          else
             if (.not.LNOTBK(2).and.LKECHO)&
                  &write(JOSTND,13030)KEYWRD,KARD(1)(1:3),IS
          end if
       end if
       13030 format(/A8,'   NATIONAL CRUISE SYSTEM VOLUME EQUATION ',&
            &'NUMBERS CHANGED FOR SPECIES=',A,' (CODE=',I3,');')
    end if
    if (LNOTBK(2).and.LNOTBK(3)) then
       if (LKECHO)write(JOSTND,13040)KARD(2),KARD(3)
       13040 format(T9,'   CUBIC FOOT = ',A10,'   BOARD FOOT = ',A10)
    else if (LNOTBK(2)) then
       if (LKECHO)write(JOSTND,13041)KARD(2)
       13041 format(T9,'   CUBIC FOOT = ',A10)
    else if (LNOTBK(3)) then
       if (LKECHO)write(JOSTND,13042)KARD(3)
       13042 format(T9,'   BOARD FOOT = ',A10)
    else
       write(JOSTND,13043)
       13043 format(T9,' EQUATION NUMBERS NOT SPECIFIED.',&
            &'  REQUEST CANCELLED.')
    end if
  end subroutine INVVOLEQNUM

end module INITREVOL_MOD
