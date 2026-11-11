!===========================!
!   vbase : setcubicdflts   !
!===========================!
subroutine SETCUBICDFLTS
  implicit none

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PLOT.f90'
  include 'CONTRL.f90'
  include 'VARCOM.f90'
  include 'VOLSTD.f90'
  include 'GGCOM.f90'
  include 'SNCOM.f90'

  ! Variables
  integer :: AKMERCHCDS(2,17),AKMERCHCAT,I,ISPC,IREGN
  integer :: CRDEFMT(23)

  data CRDEFMT/ 5, 3, 4, 5, 3, 4, 5, 5, 4, 4, 5, 4,&
       &2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2/

  !  AKMERCHCDS CONTAINS MERCH SPEC CATEOGORIES (MERCHCAT) FOR EACH
  !  AK LOCATION CODE
  data AKMERCHCDS/&
       &713,  1,&
       &720,  2,&
       &7400, 2,&
       &7401, 2,&
       &7402, 2,&
       &7403, 2,&
       &7404, 2,&
       &7405, 2,&
       &7406, 2,&
       &7407, 2,&
       &7408, 2,&
       &703,  3,&
       &1005, 3,&
       &8134, 3,&
       &8135, 3,&
       &8112, 3,&
       &1004, 4/

  select case (VARACD)
  case('AK')

     !  LOOP THROUGH AKMERCHCDS AND SELECT MERCHCAT BASED ON
     !  LOCATION CODE (KODFOR - 17 OPTIONS)

     do I = 1, 17
        if (KODFOR .eq. AKMERCHCDS(1,I)) then
           AKMERCHCAT = AKMERCHCDS(2,I)
           exit
           !  DEFAULT TO TONGASS IF LOCATION CODE IS NOT FOUND IN
           !  AKMERCHCDS
        else
           AKMERCHCAT = 3
        end if
     end do

     !  DETERMINE DBHMIN, TOPD, SCFMIND AND SCFTOPD BASED ON AKMERCHCAT

     !  SET DBHMIN DEFAULTS
     do ISPC = 1,MAXSP

        if (STMP(ISPC) .le.0)    STMP(ISPC) = 1
        if (SCFSTMP(ISPC) .le.0) SCFSTMP(ISPC) = 1

        select case(AKMERCHCAT)
        case(1)
           if (DBHMIN(ISPC) .le.0)   DBHMIN(ISPC) = 6.
           if (TOPD(ISPC) .le.0)    TOPD(ISPC) = 4.
           if (SCFMIND(ISPC) .le.0) SCFMIND(ISPC) = 9.
           if (SCFTOPD(ISPC) .le.0) SCFTOPD(ISPC) = 6.
        case(2)
           if (DBHMIN(ISPC) .le.0)   DBHMIN(ISPC) = 5.
           if (TOPD(ISPC) .le.0)    TOPD(ISPC) = 4.
           if (SCFMIND(ISPC) .le.0) SCFMIND(ISPC) = 9.
           if (SCFTOPD(ISPC) .le.0) SCFTOPD(ISPC) = 6.
        case(3)
           if (DBHMIN(ISPC) .le.0)   DBHMIN(ISPC) = 9.
           if (TOPD(ISPC) .le.0)    TOPD(ISPC) = 7.
           if (SCFMIND(ISPC) .le.0) SCFMIND(ISPC) = 9.
           if (SCFTOPD(ISPC) .le.0)  SCFTOPD(ISPC) = 7.
        case (4)
           if (DBHMIN(ISPC) .le.0)   DBHMIN(ISPC) = 9.
           if (TOPD(ISPC) .le.0)    TOPD(ISPC) = 6.
           if (SCFMIND(ISPC) .le.0) SCFMIND(ISPC) = 9.
           if (SCFTOPD(ISPC) .le.0) SCFTOPD(ISPC) = 6.
        case default
           if (DBHMIN(ISPC) .le.0)   DBHMIN(ISPC) = 9.
           if (TOPD(ISPC) .le.0)    TOPD(ISPC) = 7.
           if (SCFMIND(ISPC) .le.0) SCFMIND(ISPC) = 9.
           if (SCFTOPD(ISPC) .le.0) SCFTOPD(ISPC) = 7.
        end select
     end do

  case ("BM")
     if (DBHMIN(7) .le.0)        DBHMIN(7) = 6.0
     if (SCFMIND(7) .le.0)      SCFMIND(7) = 6.0

     do ISPC = 1,MAXSP
        if (STMP(ISPC) .le.0)    STMP(ISPC) = 1.
        if (DBHMIN(ISPC) .le.0)   DBHMIN(ISPC) = 7.0
        if (TOPD(ISPC) .le.0)    TOPD(ISPC) = 4.5
        if (SCFSTMP(ISPC) .le.0) SCFSTMP(ISPC) = 1.
        if (SCFMIND(ISPC) .le.0) SCFMIND(ISPC) = 7.0
        if (SCFTOPD(ISPC) .le.0) SCFTOPD(ISPC) = 4.5
     end do

  case ("CA")
     if (DBHMIN(11) .le.0)   DBHMIN(11) = 6.0
     if (SCFMIND(11) .le.0) SCFMIND(11) = 6.0

     do ISPC = 1,MAXSP
        select case (IFOR)
        case(6:10)
           if (TOPD(ISPC) .le.0)    TOPD(ISPC) = 4.5
           if (SCFTOPD(ISPC) .le.0) SCFTOPD(ISPC) = 4.5
        case default
           if (TOPD(ISPC) .le.0)    TOPD(ISPC) = 6
           if (SCFTOPD(ISPC) .le.0) SCFTOPD(ISPC) = 6
        end select

        if (STMP(ISPC) .le.0)    STMP(ISPC) = 1.
        if (DBHMIN(ISPC) .le.0)   DBHMIN(ISPC) = 7.0
        if (SCFSTMP(ISPC) .le.0) SCFSTMP(ISPC) = 1.
        if (SCFMIND(ISPC) .le.0) SCFMIND(ISPC) = 7.0

     end do

  case ("CI")
     if (DBHMIN(7) .le.0)   DBHMIN(7) = 7.0
     if (SCFMIND(7) .le.0) SCFMIND(7) = 7.0

     do ISPC = 1,MAXSP
        if (STMP(ISPC) .le.0)    STMP(ISPC) = 1.
        if (DBHMIN(ISPC) .le.0)   DBHMIN(ISPC) = 8.0
        if (TOPD(ISPC) .le.0)    TOPD(ISPC) = 6.0
        if (SCFSTMP(ISPC) .le.0) SCFSTMP(ISPC) = 1.
        if (SCFMIND(ISPC) .le.0) SCFMIND(ISPC) = 8.0
        if (SCFTOPD(ISPC) .le.0) SCFTOPD(ISPC) = 6.0
     end do

  case ("CR")
     if (IMODTY .le.0.or. IMODTY .gt.5) IMODTY = CRDEFMT(IFOR)
     do ISPC = 1,MAXSP
        if (STMP(ISPC) .le.0)    STMP(ISPC) = 1.
        if (SCFSTMP(ISPC) .le.0) SCFSTMP(ISPC) = 1.
        select case (IMODTY)
        case (1,2,4,5)
           if (IFOR .ge. IGFOR .and. SCFMIND(ISPC) .le.0)&
                &SCFMIND(ISPC) = 9.0
           if (DBHMIN(ISPC) .le.0)   DBHMIN(ISPC) = 5.0
           if (TOPD(ISPC) .le.0)    TOPD(ISPC) = 4.0
           if (SCFTOPD(ISPC) .le.0) SCFTOPD(ISPC) = 6.0
           if (SCFMIND(ISPC) .le.0) SCFMIND(ISPC) = 7.0

        case (3)
           if (DBHMIN(ISPC) .le.0)   DBHMIN(ISPC) = 9.0
           if (TOPD(ISPC) .le.0)    TOPD(ISPC) = 6.0
           if (SCFMIND(ISPC) .le.0) SCFMIND(ISPC) = 9.0
           if (SCFTOPD(ISPC) .le.0) SCFTOPD(ISPC) = 6.0
        end select
     end do

  case ("CS")
     do ISPC = 1,MAXSP
        !            CHECK FOR UNIQUE VALUES FIRST
        if (ISPC .le.7.or. IFOR .eq.1) then
           if (DBHMIN(ISPC) .le.0)  DBHMIN(ISPC) = 5.
        end if
        if (ISPC .gt.7.and. IFOR .eq.2) then
           if (TOPD(ISPC) .le.0)   TOPD(ISPC) = 5.
        end if
        if (ISPC .eq.1.and. IFOR .eq.1.and.SCFMIND(ISPC) .le.0)&
             &SCFMIND(ISPC) = 6.
        if (ISPC .gt.7.and. IFOR .ne.1.and. SCFMIND(ISPC).le.0)&
             &SCFMIND(ISPC) = 11.
        if (ISPC .eq.1.and. IFOR .eq.1) then
           if (SCFTOPD(ISPC) .le.0) SCFTOPD(ISPC) = 5.
        end if
        if (ISPC .gt.7.and. IFOR .ne.1) then
           if (SCFTOPD(ISPC) .le.0) SCFTOPD(ISPC) = 9.6
        end if

        if (STMP(ISPC) .le.0)    STMP(ISPC) = 0.5
        if (SCFSTMP(ISPC) .le.0) SCFSTMP(ISPC) = 1.
        if (DBHMIN(ISPC) .le.0)   DBHMIN(ISPC) = 6.
        if (TOPD(ISPC) .le.0)    TOPD(ISPC) = 4.
        if (SCFMIND(ISPC) .le.0) SCFMIND(ISPC) = 9.
        if (SCFTOPD(ISPC) .le.0) SCFTOPD(ISPC) = 7.6

     end do

  case ("EC")
     if (DBHMIN(7) .le.0)   DBHMIN(7) = 6.
     if (SCFMIND(7) .le.0) SCFMIND(7) = 6.

     do ISPC = 1,MAXSP
        if (STMP(ISPC) .le.0)    STMP(ISPC) = 1.
        if (SCFSTMP(ISPC) .le.0) SCFSTMP(ISPC) = 1.
        if (DBHMIN(ISPC) .le.0)   DBHMIN(ISPC) = 7.
        if (TOPD(ISPC) .le.0)    TOPD(ISPC) = 4.5
        if (SCFMIND(ISPC) .le.0) SCFMIND(ISPC) = 7.0
        if (SCFTOPD(ISPC) .le.0) SCFTOPD(ISPC) = 4.5
     end do

  case ("EM")
     if (DBHMIN(7) .le.0)   DBHMIN(7) = 6.
     if (SCFMIND(7) .le.0) SCFMIND(7) = 6.

     do ISPC = 1,MAXSP
        if (STMP(ISPC) .le.0)    STMP(ISPC) = 1.
        if (SCFSTMP(ISPC) .le.0) SCFSTMP(ISPC) = 1.
        if (DBHMIN(ISPC) .le.0)   DBHMIN(ISPC) = 7.0
        if (TOPD(ISPC) .le.0)    TOPD(ISPC) = 4.5
        if (SCFMIND(ISPC) .le.0) SCFMIND(ISPC) = 7.0
        if (SCFTOPD(ISPC) .le.0) SCFTOPD(ISPC) = 4.5
     end do

  case ("IE")
     if (DBHMIN(7) .le.0)   DBHMIN(7) = 6.
     if (SCFMIND(7) .le.0) SCFMIND(7) = 6.

     do ISPC = 1,MAXSP
        if (STMP(ISPC) .le.0)    STMP(ISPC) = 1.
        if (SCFSTMP(ISPC) .le.0) SCFSTMP(ISPC) = 1.
        if (DBHMIN(ISPC) .le.0)   DBHMIN(ISPC) = 7.0
        if (TOPD(ISPC) .le.0)    TOPD(ISPC) = 4.5
        if (SCFMIND(ISPC) .le.0) SCFMIND(ISPC) = 7.0
        if (SCFTOPD(ISPC) .le.0) SCFTOPD(ISPC) = 4.5
     end do

  case ("KT")
     if (DBHMIN(7) .le.0)   DBHMIN(7) = 6.
     if (SCFMIND(7) .le.0) SCFMIND(7) = 6.

     do ISPC = 1,MAXSP
        if (STMP(ISPC) .le.0)    STMP(ISPC) = 1.
        if (SCFSTMP(ISPC) .le.0) SCFSTMP(ISPC) = 1.
        if (DBHMIN(ISPC) .le.0)   DBHMIN(ISPC) = 7.0
        if (TOPD(ISPC) .le.0)    TOPD(ISPC) = 4.5
        if (SCFMIND(ISPC) .le.0) SCFMIND(ISPC) = 7.0
        if (SCFTOPD(ISPC) .le.0) SCFTOPD(ISPC) = 4.5
     end do

  case ("LS")
     do ISPC = 1,MAXSP
        if ((ISPC.ge.40.and.ISPC.le.42.and.IFOR.eq.2)&
             &.or.(IFOR.eq.6.and.ISPC.gt.14)) then
           if (DBHMIN(ISPC) .le.0) DBHMIN(ISPC) = 6.
        end if
        if (ISPC .le.14) then
           if (SCFMIND(ISPC) .le.0) SCFMIND(ISPC) = 9.
           if (SCFTOPD(ISPC) .le.0) SCFTOPD(ISPC) = 7.6
        else
           select case(IFOR)
           case (2)

              if (ISPC.ge.40.and.ISPC.le.42) then
                 if (SCFMIND(ISPC) .le.0) SCFMIND(ISPC) = 11.
                 if (SCFTOPD(ISPC) .le.0) SCFTOPD(ISPC) = 9.6
              end if
              if (SCFMIND(ISPC) .le.0) SCFMIND(ISPC) = 9.0
              if (SCFTOPD(ISPC) .le.0) SCFTOPD(ISPC) = 7.6
           case (5)
              if (ISPC.ge.40.and.ISPC.le.42) then
                 if (SCFMIND(ISPC) .le.0) SCFMIND(ISPC) = 9.
              end if
              if (SCFMIND(ISPC) .le.0) SCFMIND(ISPC) = 11.
              if (SCFTOPD(ISPC) .le.0) SCFTOPD(ISPC) = 7.6
           case default
              if (SCFMIND(ISPC) .le.0) SCFMIND(ISPC) = 11.
              if (SCFTOPD(ISPC) .le.0) SCFTOPD(ISPC) = 9.6
           end select
        end if

        if (STMP(ISPC) .le.0)    STMP(ISPC) = 0.5
        if (SCFSTMP(ISPC) .le.0) SCFSTMP(ISPC) = 1.
        if (DBHMIN(ISPC) .le.0)   DBHMIN(ISPC) = 5.
        if (TOPD(ISPC) .le.0)    TOPD(ISPC) = 4.
     end do

  case ("NC")
     do ISPC = 1,MAXSP
        select case (IFOR)
        case (4)
           if (TOPD(ISPC) .le.0)    TOPD(ISPC) = 4.5
           if (SCFTOPD(ISPC) .le.0) SCFTOPD(ISPC) = 4.5
        case (5,7)
           if (TOPD(ISPC) .le.0)    TOPD(ISPC) = 5.
           if (SCFTOPD(ISPC) .le.0) SCFTOPD(ISPC) = 5.
        case default
           if (TOPD(ISPC) .le.0)    TOPD(ISPC) = 6.
           if (SCFTOPD(ISPC) .le.0) SCFTOPD(ISPC) = 6.
        end select
        if (STMP(ISPC) .le.0)    STMP(ISPC) = 1.
        if (SCFSTMP(ISPC) .le.0) SCFSTMP(ISPC) = 1.
        if (DBHMIN(ISPC) .le.0)   DBHMIN(ISPC) = 9.0
        if (SCFMIND(ISPC) .le.0) SCFMIND(ISPC) = 9.0
     end do

  case ("NE")
     do ISPC = 1,MAXSP
        if ((IFOR .eq.1.or. IFOR .eq.3) .and. ISPC .gt.25) then
           if (DBHMIN(ISPC) .le.0) DBHMIN(ISPC) = 6.
        end if
        if (IFOR .eq.4.and. ISPC .gt.25) then
           if (DBHMIN(ISPC) .le.0) DBHMIN(ISPC) = 8.
        end if
        if (IFOR .eq.3.and. ISPC .gt.25) then
           if (TOPD(ISPC) .le.0) TOPD(ISPC) = 5.
        end if
        if (TOPD(ISPC) .le.0) TOPD(ISPC) = 4.
        if (ISPC .gt.25.and. SCFMIND(ISPC) .le.0)&
             &SCFMIND(ISPC) = 11.
        if (SCFMIND(ISPC) .le.0) SCFMIND(ISPC) = 9.
        if (ISPC .gt.25) then
           if (SCFTOPD(ISPC) .le.0) SCFTOPD(ISPC) = 9.6
        end if
        if (SCFTOPD(ISPC) .le.0) SCFTOPD(ISPC) = 7.6
        if (STMP(ISPC) .le.0)    STMP(ISPC) = 0.5
        if (SCFSTMP(ISPC) .le.0) SCFSTMP(ISPC) = 1.
        if (DBHMIN(ISPC) .le.0)   DBHMIN(ISPC) = 5.
     end do

  case ("OC")
     if (DBHMIN(11) .le.0)   DBHMIN(11) = 6.
     if (SCFMIND(11) .le.0) SCFMIND(11) = 6.
     do ISPC = 1,MAXSP
        select case (IFOR)
        case (6:10)
           if (TOPD(ISPC) .le.0)    TOPD(ISPC) = 4.5
           if (SCFTOPD(ISPC) .le.0) SCFTOPD(ISPC) = 4.5
        case default
           if (TOPD(ISPC) .le.0)    TOPD(ISPC) = 6.
           if (SCFTOPD(ISPC) .le.0) SCFTOPD(ISPC) = 6.
        end select
        if (STMP(ISPC) .le.0)    STMP(ISPC) = 1.
        if (SCFSTMP(ISPC) .le.0) SCFSTMP(ISPC) = 1.
        if (DBHMIN(ISPC) .le.0)   DBHMIN(ISPC) = 7.
        if (SCFMIND(ISPC) .le.0) SCFMIND(ISPC) = 7.
     end do

  case ("OP")
     do ISPC = 1,MAXSP
        select case (IFOR)
        case (4,5,6)
           if (TOPD(ISPC) .le.0)    TOPD(ISPC) = 5.
           if (SCFTOPD(ISPC) .le.0) SCFTOPD(ISPC) = 5.
        case default
           if (ISPC.eq.11) then
              DBHMIN(ISPC) = 6.
              if (SCFMIND(ISPC) .le.0) SCFMIND(ISPC) = 6.
           end if
           if (TOPD(ISPC) .le.0)   TOPD(ISPC) = 4.5
           if (SCFTOPD(ISPC) .le.0) SCFTOPD(ISPC) = 4.5
        end select
        if (STMP(ISPC) .le.0)    STMP(ISPC) = 1.
        if (SCFSTMP(ISPC) .le.0) SCFSTMP(ISPC) = 1.
        if (DBHMIN(ISPC) .le.0)   DBHMIN(ISPC) = 7.
        if (SCFMIND(ISPC) .le.0) SCFMIND(ISPC) = 7.
     end do

  case ("PN")
     do ISPC = 1,MAXSP
        select case (IFOR)
        case (4,5,6)
           if (TOPD(ISPC) .le.0)   TOPD(ISPC) = 5.
           if (SCFTOPD(ISPC) .le.0) SCFTOPD(ISPC) = 5.
        case default
           if (ISPC.eq.11) then
              if (DBHMIN(ISPC) .le.0)   DBHMIN(ISPC) = 6.
              if (SCFMIND(ISPC) .le.0) SCFMIND(ISPC) = 6.
           end if
           if (TOPD(ISPC) .le.0)    TOPD(ISPC) = 4.5
           if (SCFTOPD(ISPC) .le.0) SCFTOPD(ISPC) = 4.5
        end select
        if (STMP(ISPC) .le.0)    STMP(ISPC) = 1.
        if (SCFSTMP(ISPC) .le.0) SCFSTMP(ISPC) = 1.
        if (DBHMIN(ISPC) .le.0)   DBHMIN(ISPC) = 7.
        if (SCFMIND(ISPC) .le.0) SCFMIND(ISPC) = 7.
     end do

  case ("SN")
     if (ISEFOR.ne.0) then
        IREGN = KODFOR/10000
     else
        IREGN = 9
     end if

     if (IREGN .eq.8) then
        do ISPC = 1,MAXSP
           if (STMP(ISPC) .le.0)    STMP(ISPC) = 0.5
           if (SCFSTMP(ISPC) .le.0) SCFSTMP(ISPC) = 1.0

           ! SET NORTH CAROLINA DBHMIN AND TOPD DEFAULTS
           if (IFOR .eq.11) then
              if (TOPD(ISPC) .le.0) TOPD(ISPC) = 3.5
              select case (KODIST)
              case (3,10)
                 if (ISPC .le.17.or. ISPC .eq.88) then
                    if (DBHMIN(ISPC) .le.0) DBHMIN(ISPC) = 5.6
                 else
                    if (DBHMIN(ISPC) .le.0) DBHMIN(ISPC) = 6.0
                 end if
              case default
                 if (DBHMIN(ISPC) .le.0) DBHMIN(ISPC) = 8.0
              end select
           end if

           ! SET DBHMIN AND TOPD DEFAULTS FOR REMAINDER OF FORESTS
           if (TOPD(ISPC) .le.0) TOPD(ISPC) = 4.0
           select case (ISPC)
           case (7,13,39,43,44,52,53,55,63)
              if (DBHMIN(ISPC) .le.0) DBHMIN(ISPC) = 6.0
           case default
              if (DBHMIN(ISPC) .le.0) DBHMIN(ISPC) = 4.0
           end select

           ! SET SCFMIND AND SCFTOD DEFAULTS
           if (ISPC .le.17.or. ISPC .eq.88) then ! SOFTWOODS

              ! SET NORTH CAROLINA SOFTWOOD SPECS
              if (IFOR .eq.11) then
                 if (KODIST.eq.3.or. KODIST.eq.10) then
                    if (SCFMIND(ISPC) .le.0) SCFMIND(ISPC) = 11.0
                    if (SCFTOPD(ISPC) .le.0) SCFTOPD(ISPC) = 6.3
                 else
                    select case (ISPC)
                    case (2,12,15,16,17)
                       if (SCFMIND(ISPC) .le.0) SCFMIND(ISPC) = 12.0
                       if (SCFTOPD(ISPC) .le.0) SCFTOPD(ISPC) = 9.0
                    case default
                       if (SCFMIND(ISPC) .le.0) SCFMIND(ISPC) = 10.0
                       if (SCFTOPD(ISPC) .le.0) SCFTOPD(ISPC) = 6.3
                    end select
                 end if
              end if

              ! SET SPECS FOR REMAINDER OF FORESTS
              if (IFOR .eq.10.and. ISPC .eq.2) then
                 if (SCFMIND(ISPC) .le.0) SCFMIND(ISPC) = 9.0
                 if (SCFTOPD(ISPC) .le.0) SCFTOPD(ISPC) = 7.0
              end if
              if (SCFMIND(ISPC) .le.0) SCFMIND(ISPC) = 10.0
              if (SCFTOPD(ISPC) .le.0) SCFTOPD(ISPC) = 7.0

           else ! HARDWOODS

              ! SET NORTH CAROLINA HARDWOOD SPECS
              if (IFOR .eq.11) then
                 if (KODIST .eq.3.or. KODIST .eq.10) then
                    if (SCFMIND(ISPC) .le.0) SCFMIND(ISPC) = 13.0
                    if (SCFTOPD(ISPC) .le.0) SCFTOPD(ISPC) = 8.0
                 end if
                 if (SCFMIND(ISPC) .le.0) SCFMIND(ISPC) = 15.0
                 if (SCFTOPD(ISPC) .le.0) SCFTOPD(ISPC) = 11.0
              end if

              ! SET SPECS FOR REMAINDER OF FORESTS
              if (SCFMIND(ISPC) .le.0) SCFMIND(ISPC) = 12.0
              if (SCFTOPD(ISPC) .le.0) SCFTOPD(ISPC) = 9.0
           end if
        end do
        ! END OF REGION 8 LOGIC

     else
        do ISPC = 1,MAXSP
           if (STMP(ISPC) .le.0)    STMP(ISPC) = 0.5
           if (SCFSTMP(ISPC) .le.0) SCFSTMP(ISPC) = 1.0
           ! SET DBHMIN AND TOPD SPECS
           if (IFOR.eq.15.and. (ISPC.gt.17.and. ISPC.ne.88)) then
              if (DBHMIN(ISPC) .le.0) DBHMIN(ISPC) = 6.0
              if (TOPD(ISPC) .le.0) TOPD(ISPC) = 5.0
           else
              if (DBHMIN(ISPC) .le.0) DBHMIN(ISPC) = 5.0
              if (TOPD(ISPC) .le.0) TOPD(ISPC) = 4.0
           end if
           ! SCFMIND AND SCFTOPD SPECS
           if (IFOR.eq.15.and. (ISPC.gt.17.and. ISPC.ne.88)) then
              if (SCFMIND(ISPC) .le.0) SCFMIND(ISPC) = 11.0
              if (SCFTOPD(ISPC) .le.0) SCFTOPD(ISPC) = 9.6
           end if
           if (SCFMIND(ISPC) .le.0) SCFMIND(ISPC) = 9.0
           if (SCFTOPD(ISPC) .le.0) SCFTOPD(ISPC) = 7.6
        end do
     end if

  case ("SO")
     do ISPC = 1,MAXSP
        if (STMP(ISPC) .le.0)    STMP(ISPC) = 1.0
        if (SCFSTMP(ISPC) .le.0) SCFSTMP(ISPC) = 1.0
        if (DBHMIN(ISPC) .le.0)   DBHMIN(ISPC) = 9.0
        if (SCFMIND(ISPC) .le.0) SCFMIND(ISPC) = 9.0
        select case (IFOR)
        case (1:3,10)
           if (TOPD(ISPC) .le.0)   TOPD(ISPC) = 4.5
           if (SCFTOPD(ISPC) .le.0) SCFTOPD(ISPC) = 4.5
        case default
           if (TOPD(ISPC) .le.0)    TOPD(ISPC) = 6.0
           if (SCFTOPD(ISPC) .le.0) SCFTOPD(ISPC) = 6.0
        end select
     end do

  case ("TT")
     if (DBHMIN(7) .le.0)        DBHMIN(7) = 7.0
     if (SCFMIND(7) .le.0)      SCFMIND(7) = 7.0

     do ISPC = 1,MAXSP
        if (STMP(ISPC) .le.0)    STMP(ISPC) = 1.0
        if (SCFSTMP(ISPC) .le.0) SCFSTMP(ISPC) = 1.0
        if (DBHMIN(ISPC) .le.0)   DBHMIN(ISPC) = 8.0
        if (TOPD(ISPC) .le.0)    TOPD(ISPC) = 6.0
        if (SCFMIND(ISPC) .le.0) SCFMIND(ISPC) = 8.0
        if (SCFTOPD(ISPC) .le.0) SCFTOPD(ISPC) = 6.0
     end do

  case ("UT")
     if (DBHMIN(7) .le.0)        DBHMIN(7) = 7.0
     if (SCFMIND(7) .le.0)      SCFMIND(7) = 7.0

     do ISPC = 1,MAXSP
        if (STMP(ISPC) .le.0)    STMP(ISPC) = 1.0
        if (SCFSTMP(ISPC) .le.0) SCFSTMP(ISPC) = 1.0
        if (DBHMIN(ISPC) .le.0)   DBHMIN(ISPC) = 8.0
        if (TOPD(ISPC) .le.0)    TOPD(ISPC) = 6.0
        if (SCFMIND(ISPC) .le.0) SCFMIND(ISPC) = 8.0
        if (SCFTOPD(ISPC) .le.0) SCFTOPD(ISPC) = 6.0
     end do

  case ("WC")
     do ISPC = 1,MAXSP
        if (STMP(ISPC) .le.0)    STMP(ISPC) = 1.0
        if (SCFSTMP(ISPC) .le.0) SCFSTMP(ISPC) = 1.0
        select case (IFOR)
        case (7:10)
           if (DBHMIN(ISPC) .le.0)   DBHMIN(ISPC) = 7.0
           if (TOPD(ISPC) .le.0)    TOPD(ISPC) = 5.0
           if (SCFMIND(ISPC) .le.0) SCFMIND(ISPC) = 7.0
           if (SCFTOPD(ISPC) .le.0) SCFTOPD(ISPC) = 5.0
        case default
           if (ISPC .eq.11 ) then
              if (DBHMIN(ISPC) .le.0)  DBHMIN(ISPC) = 6.0
              if (SCFMIND(ISPC) .le.0) SCFMIND(ISPC) = 6.0
           end if
           if (DBHMIN(ISPC) .le.0)   DBHMIN(ISPC) = 7.0
           if (TOPD(ISPC) .le.0)    TOPD(ISPC) = 4.5
           if (SCFMIND(ISPC) .le.0) SCFMIND(ISPC) = 7.0
           if (SCFTOPD(ISPC) .le.0) SCFTOPD(ISPC) = 4.5
        end select
     end do

  case ("WS")
     do ISPC = 1,MAXSP
        if (STMP(ISPC) .le.0)    STMP(ISPC) = 1.0
        if (SCFSTMP(ISPC) .le.0) SCFSTMP(ISPC) = 1.0
        if (DBHMIN(ISPC) .le.0)   DBHMIN(ISPC) = 7.0
        if (TOPD(ISPC) .le.0)    TOPD(ISPC) = 4.5
        if (SCFMIND(ISPC) .le.0) SCFMIND(ISPC) = 10.0
        if (SCFTOPD(ISPC) .le.0) SCFTOPD(ISPC) = 6.0
     end do

  end select

  return
end
