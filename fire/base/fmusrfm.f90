!=========================!
!   fire/base : fmusrfm   !
!=========================!
! FIRE-BASE $Id$
subroutine FMUSRFM (IYR, FMD)
  implicit none

  !     PROCESSES THE FUELMODL KEYWORD AND THE FUELTRET KEYWORD.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'
  include 'FMCOM.f90'
  include 'FMFCOM.f90'

  ! Variables
  integer :: IYR,FMD,MYACT(3),NTODO,ITODO,NPRM,IACTK,JYR,J,I,JDO
  integer :: INDX(4)
  real    :: PRMS(8),SUM
  real    :: DPMULT(3,3)
  logical :: LOK

  !     LOCAL VARIABLE DEFINITIONS:
  !     DPMULT:  DEPTH MULTIPLIER FOR ACTIVITY FUELS, BASED ON HARVEST
  !              TYPE(x,) AND FUEL TREATMENT TYPE(,x).
  !              (1,)ground-based;(2,)high-lead;(3,)precomm/heli
  !              (,1)none;(,2)lopping;(,3)trampling

  !     Depth modifiers based on fuel treatment and harvest type

  data DPMULT /&
       &1.0,  1.3,  1.6,&
       &0.83, 0.83, 0.83,&
       &0.75, 0.75, 0.75 /

  !     Initialize Weights of Dynamic Fire Models

  do I = 1, MXFMOD
     FMOD(I) = 0
     FWT(I) = 0.
  end do

  !     FUELMODL (2538) - FUEL MODELS TO USE IN PLACE OF AUTOMATIC ONES
  !     FUELTRET(2525) - HARVEST TYPE, FUEL TREATMENT
  !     FIRECALC(2549) - USE NEW FM LOGIC OR MODELLED LOADS

  data MYACT/2538,2525,2549/

  !     CHECK WHETHER THE FIRECALC KEYWORD IS SCHEDULED FOR THIS YEAR

  call OPFIND(1,MYACT(3),NTODO)
  if (NTODO.gt.0) then
     do ITODO = 1,NTODO
        call OPGET(ITODO,8,JYR,IACTK,NPRM,PRMS)
        !          IF (JYR .NE. IYR) GO TO 400
        call OPDONE (ITODO,IYR)

        !         SET THE FIRECALC PARAMETER VALUES.

        IFLOGIC = INT(PRMS(1))
        IFMSET = INT(PRMS(2))
        USAV(1) = PRMS(3)
        USAV(2) = PRMS(4)
        USAV(3) = PRMS(5)
        UBD(1) = PRMS(6)
        UBD(2) = PRMS(7)
        ULHV = PRMS(8)
        400 continue
     end do
  end if

  !     Check to see if the user has entered fuel models.
  !     If so, we will use those in preference to anything else.

  call OPFIND(1,MYACT(1),NTODO)
  if (NTODO.gt.0) then
     do ITODO = 1,NTODO
        do I = 1,8
           PRMS(I) = 0
        end do
        call OPGET(ITODO,8,JYR,IACTK,NPRM,PRMS)
        !            IF (JYR .EQ. IYR) THEN
        if (NPRM.le.0) then
           LUSRFM = .false.
           call OPDONE(ITODO,IYR)
        else if (PRMS(1).eq.0.) then
           LUSRFM = .false.
           call OPDONE(ITODO,IYR)
        else if (INT(PRMS(1)).gt.MXDFMD .or.&
             &INT(PRMS(3)).gt.MXDFMD .or. INT(PRMS(5)).gt.MXDFMD&
             &.or. INT(PRMS(7)).gt.MXDFMD) then
           LUSRFM = .false.
        else
           select case (INT(PRMS(1)))
           case (31:100,110:120,125:140,150:160,166:180,190:200,&
                &205:256)
              LUSRFM = .false.
           case default
              select case (INT(PRMS(3)))
              case (31:100,110:120,125:140,150:160,166:180,190:200,&
                   &205:256)
                 LUSRFM = .false.
              case default
                 select case (INT(PRMS(5)))
                 case (31:100,110:120,125:140,150:160,166:180,&
                      &190:200,205:256)
                    LUSRFM = .false.
                 case default
                    select case (INT(PRMS(7)))
                    case (31:100,110:120,125:140,150:160,166:180,&
                         &190:200,205:256)
                       LUSRFM = .false.
                    case default
                       call OPDONE(ITODO,IYR)
                       LUSRFM = .true.
                       if (MOD(NPRM,2).eq.1) then
                          NPRM = NPRM+1
                          PRMS(NPRM) = 1. ! DEFAULT WEIGHT IS 1.0
                       end if
                       SUM = 0.
                       do I = 2,NPRM,2
                          if (PRMS(I).le.0.) PRMS(I) = 1.
                          SUM = SUM+PRMS(I)
                       end do
                       SUM = 1./SUM
                       do I = 2,NPRM,2
                          PRMS(I) = PRMS(I)*SUM
                       end do
                       do J = 1,4
                          FMDUSR(J) = 0
                          FWTUSR(J) = 0.0
                       end do
                       J = 0
                       do I = 1,NPRM-1,2
                          J = J+1
                          FMDUSR(J) = INT(PRMS(I))
                          FWTUSR(J) = PRMS(I+1)
                       end do
                    end select
                 end select
              end select
           end select
        end if
        !            ENDIF
     end do
  end if

  if (LUSRFM) then

     !       SORT WEIGHTS SUCH THAT HIGHEST WEIGHT IS ALWAYS FIRST
     !       WHAT ABOUT TIES?

     FMOD(MXFMOD) = 0
     FWT(MXFMOD) = 0.0
     call RDPSRT (4,FWTUSR,INDX,.true.)
     do I = 1,4
        J = INDX(I)
        FMOD(I) = FMDUSR(J)
        FWT(I) = FWTUSR(J)
     end do

     !     SELECT NON-DYNAMIC MODEL BASED ON MOST HEAVILY WEIGHTED MODEL
     !     FAILSAFE IS TO NAME MODEL 8 AND ISSUE A WARNING

     FMD = -1
     if (FWT(1) .gt.1.0E-6) FMD = FMOD(1)
     if (FMD .lt.0) then
        FMOD(1) = 8
        FWT(1) = 1.0
        NFMODS = 1
        do I = 2,MXFMOD
           FMOD(I) = 0
           FWT(I) = 0.0
        end do
        call RCDSET(2, .true.)
     end if

     !     COMPUTE THE NUMBER OF ACTIVE MODELS

     do I = 1,MXFMOD
        if (FWT(I) .le.1.0E-6) then
           NFMODS = I-1
           exit
        end if
     end do
     NFMODS = MIN(NFMODS,4)

     !     IF STATIC FUEL MODEL IS DESIRED, UNSET ALL OTHER WEIGHTS

     if (.not. LDYNFM) then
        FMOD(1) = FMD
        FWT(1) = 1.0
        NFMODS = 1
        do I = 2,MXFMOD
           FMOD(I) = 0
           FWT(I) = 0.0
        end do
     end if

  else ! NOT LUSRFM

     !     IF A USER WANTS TO CALCULATE FIRE BEHAVIOR USING MODELLED FUEL LOADS
     !     (FIRECALC KEYWORD) THEN FORCE THE SELECTION OF FM89.  THIS IS A CUSTOM
     !     FUEL MODEL THAT IS FILLED WITH THE RIGHT PARAMETERS IN FMBURN.
     !     ONLY DO THIS IF THE USER HASN'T SET THE FUEL MODEL WITH THE FUELMODEL
     !     KEYWORD.

     if (IFLOGIC .eq.2) then
        FMD = 89
        FMOD(1) = 89
        FWT(1) = 1.0
        NFMODS = 1
        do I = 2,MXFMOD
           FMOD(I) = 0
           FWT(I) = 0.0
        end do
     end if
  end if

  !     CHECK TO SEE IF THERE WAS ANY HARVEST TYPE OR FUEL TREATMENT INFO
  !     ASSIGNED.

  call OPFIND(1,MYACT(2),NTODO)
  if (NTODO.gt.0) then
     do JDO = 1,NTODO
        call OPGET(JDO,3,JYR,IACTK,NPRM,PRMS)
        !          IF (JYR .EQ. IYR) THEN

        LOK = .true.
        if (PRMS(1) .lt.0.0) LOK = .false.
        if (PRMS(1) .gt.2.0) LOK = .false.
        if (PRMS(2) .lt.1.0) LOK = .false.
        if (PRMS(2) .gt.3.0) LOK = .false.

        if (LOK) then
           FTREAT = INT(PRMS(1))
           HARTYP = INT(PRMS(2))
           DPMOD = PRMS(3)
           if (DPMOD .lt.0.0) DPMOD = DPMULT(HARTYP,FTREAT+1)
           IFTYR = IYR
           call OPDONE(JDO,IYR)
        else
           call OPDEL1(JDO)
        end if

        !          ENDIF
     end do
  end if
  if (IFTYR .gt.0) then
     if ((IYR-IFTYR) .gt.5) then
        IFTYR = 0
        FTREAT = 0
        HARTYP = 0
        DPMOD = 1.0
     end if
  else
     FTREAT = 0
     HARTYP = 0
     DPMOD = 1.0
  end if

  return
end
