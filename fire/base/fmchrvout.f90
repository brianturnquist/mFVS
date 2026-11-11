!===========================!
!   fire/base : fmchrvout   !
!===========================!
! FIRE-BASE $Id$
subroutine FMCHRVOUT (IYR)
  implicit none

  !     SINGLE-STAND VERSION
  !     CALLED FROM: FMMAIN
  !  PURPOSE:
  !     PRINT THE HARVESTED PRODUCTS CARBON REPORT;

  !  CALL LIST DEFINITIONS:
  !     IYR:     CURRENT YEAR

  !     PARAMETER INCLUDE FILES.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'

  !     COMMON INCLUDE FILES

  include 'PLOT.f90'
  include 'CONTRL.f90'
  include 'ARRAYS.f90'
  include 'FMCOM.f90'
  include 'FMFCOM.f90'
  include 'FMPROP.f90'
  include 'METRIC.f90'

  !     VARIABLE DECLARATIONS

  ! Variables
  integer :: IYR

  logical :: DEBUG
  integer :: I,JROUT,DBSKODE
  real    :: V(6),XTMP
  integer :: JCYC,KYR,IFATE,IHW,IPL

  !     CHECK FOR DEBUG.

  call DBCHK (DEBUG,'FMCHRVOUT',9,ICYC)
  if (DEBUG) write(JOSTND,1) ICYC
  1 format(' ENTERING FMCHRVOUT CYCLE = ',I2)

  !     HARVESTED CARBON REPORT.
  !     RETRIEVE THE UNIT NUMBER TO BE USED FOR THE REPORT.

  call GETLUN (JROUT)

  if (DEBUG) write(JOSTND,40) ICHRVB,ICHRVE,IDCHRV,JROUT
  40 format(' FMDOUT: ICHRVB=',I5,' ICHRVE=',I5,&
       &' IDCHRV=',I5,' JROUT=',I3)

  !     THERE ARE 6 INDICATORS:

  !     1 = PRODUCTS
  !     2 = LANDFILL
  !     3 = ENERGY
  !     4 = EMISSIONS
  !     5 = MERCH CARBON STORED
  !     6 = MERCH CARBON REMOVED FROM STAND

  !     1: CALCULATE THE INDICATORS, USING EITHER FFE OR JENKINS LOGIC
  !        THERE MAY BE SOME EAST VS WEST VARIANT DIFFERENCES (THIS VERSION
  !        IS IN /FIRE/NI/SRC, SO WILL DO FOR WEST; OTHER WOULD BE /FIRE/LS/SRC
  !        WITH SAME FILENAME

  !     2: LOAD ANY CONVERSION FACTORS (FOR IMPERIAL -> METRIC)
  !        INTO THE CNV() ARRAY BEFORE THE CALL TO THE DATABASE WRITE.

  !     FATE(1, HWSW(ISP(I)), ICYC)
  !     DETERMINE THE AMOUNT IN EACH OF THE POOLS, USING THE PROVIDED VALUES

  do I = 1,6
     V(I) = 0.0
  end do

  !     THERE ARE 3 FATES IN FAPROP(); A 4TH IS THE SUM OF 1:3

  do JCYC = 1, ICYC
     KYR = IYR - IY(JCYC) + 1
     if (KYR .ge.101) KYR = 101
     do IPL = 1,2 ! PULP/SAW
        do IHW = 1,2 !SW/HW
           XTMP = 0.
           do IFATE = 1,3 ! 3 FATES (INUSE,LANDFILL,ENERGY); 4=sum 1:3
              XTMP = XTMP + FAPROP(ICHABT,KYR,IFATE,IPL,IHW)
              V(IFATE) = V(IFATE) +&
                   &FATE(IPL,IHW,JCYC) * FAPROP(ICHABT,KYR,IFATE,IPL,IHW)
           end do
           V(4) = V(4) + (FATE(IPL,IHW,JCYC) * (1.0 - XTMP))
        end do
     end do
  end do

  V(5) = V(1) + V(2)
  V(6) = V(3) + V(4) + V(5)

  do I = 1,6
     V(I) = V(I) * 0.5
  end do

  if (ICMETRC.eq.1) then
     do I = 1,6
        V(I) = V(I) * TItoTM / ACRtoHA
     end do
  else if (ICMETRC.eq.2) then
     do I = 1,6
        V(I) = V(I) * TItoTM
     end do
  end if

  !     SET ARRAY FOR CARBSTAT EVENT MONITOR FUNCTION

  do I = 1,6
     CARBVAL(11 + I) = V(I)
  end do

  !     RETURN IF REPORT BEGIN YEAR REMAINS 0.
  !     THIS USED TO BE BASED ON A REPORTING PERIOD, BEGIN YEAR TO END YEAR.

  if (ICHRVB .eq.0) return

  !     CALL THE DBS MODULE TO OUTPUT FUEL DATA TO A DATABASE
  DBSKODE = 1
  call DBSFMHRPT(IYR,NPLT,V,6,DBSKODE)
  if (DBSKODE.eq.0) return

  !     IF HEADER REQUESTED AND THIS IS THE FIRST OPPORTUNITY TO PRINT
  !     IT, THEN DO SO.

  ICHPAS = ICHPAS + 1
  if (ICHPAS .eq.1) then
     write(JROUT,699) IDCHRV
     write(JROUT,699) IDCHRV
     write(JROUT,700) IDCHRV
     write(JROUT,701) IDCHRV
     write(JROUT,702) IDCHRV
     if (ICMETRC.eq.1) then
        write(JROUT,707) IDCHRV
     else if (ICMETRC.eq.2) then
        write(JROUT,709) IDCHRV
     else
        write(JROUT,708) IDCHRV
     end if
     write(JROUT,699) IDCHRV
     write(JROUT, 44) IDCHRV,NPLT,MGMID
     write(JROUT,700) IDCHRV
     write(JROUT,704) IDCHRV
     write(JROUT,705) IDCHRV
     write(JROUT,706) IDCHRV
     write(JROUT,700) IDCHRV

     699 format(1(/1X,I5))
     700 format(1X,I5,1X,122('-'))
     701 format(1X,I5,1X,30X,'******  CARBON REPORT VERSION 1.0 ******')
     702 format(1X,I5,1X,38X,'HARVESTED PRODUCTS REPORT ',&
          &'(BASED ON STOCKABLE AREA)')
     44 format(1X,I5,' STAND ID: ',A26,4X,'MGMT ID: ',A4)
     704 format(1X,I5,1X,44(' '),'Merch Carbon')
     705 format(1X,I5,1X,43(' '),15('-'))
     706 format(1X,I5,1X,'YEAR  Prducts  Lndfill   Energy  Emissns  ',&
          &' Stored  Removed')
     707 format(1X,I5,1X,25(' '),&
          &('ALL VARIABLES ARE REPORTED IN METRIC TONS/HECTARE'))
     708 format(1X,I5,1X,30(' '),&
          &('ALL VARIABLES ARE REPORTED IN TONS/ACRE'))
     709 format(1X,I5,1X,27(' '),&
          &('ALL VARIABLES ARE REPORTED IN METRIC TONS/ACRE'))
  end if

  write(JROUT,800) IDCHRV,IYR,(V(I),I = 1,6)
  800 format(1X,I5,1X,I4,6(2X,F7.1))

  return
end
