!=============================!
!   fire/vbase : fmsoilheat   !
!=============================!
! FIRE-VBASE $Id$
subroutine FMSOILHEAT (IYR, LNMOUT)
  implicit none

  !     CALLED FROM: FMBURN
  !     CALLS fm_fofem

  !  PURPOSE:
  !     THIS SUBROUTINE IS THE INTERFACE BETWEEN FFE AND THE FOFEM SOIL HEADING
  !     MODEL.

  !  CALL LIST DEFINITIONS:
  !     IYR:  CURRENT YEAR
  !     FMD:  FUEL MODEL THAT IS USED IN THE STATIC CASE
  !     LNMOUT: TRUE IF NORMAL OUTPUT PROCESS, FALSE TO SUPPRESS ALL OUTPUTS

  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'
  include 'FMCOM.f90'
  include 'FMFCOM.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'

  !DEC$ ATTRIBUTES DLLIMPORT :: FM_FOFEM
#if !(_WIN64)
  !DEC$ ATTRIBUTES ALIAS:'_FM_FOFEM' :: FM_FOFEM
#endif

  ! Variables
  integer :: iyr
  logical :: lnmout
  character(len=1) :: fofem_msg(101)
  real    :: fofem_input(32),fofem_output(30)
  integer :: fofem_rc
  logical :: debug
  integer :: i,jrout
  integer :: j,k,l,regnum,covnum,seasnum,moiscode
  real    :: smcat(4),duffcat(4),algslp
  real    :: MOIS1(2,5),MOIS2(2,5),MOIS3(2,5),MOIS4(2,5),soilmois
  logical :: LDANUW

  integer :: fm_fofem

  data SMCAT /5,10,15,25/

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  LDANUW = LNMOUT

  !     CHECK FOR DEBUG

  call DBCHK (DEBUG,'FMSOILHEAT',10,ICYC)
  if (DEBUG) write(JOSTND,7) ICYC, IYR, ISHEATB, ISHEATE, IDSHEAT
  7 format(' FMSOILHEAT CYCLE=',I2,' IYR=',I5,' ISHEATB,ISHEATE=',&
       &2I5,' IDSHEAT=',I5)

  !     RETURN IF REPORT IS NOT REQUESTED BY KEYWORD (BEGIN YEAR REMAINS 9999)
  !     THIS USED TO BE BASED ON A REPORTING PERIOD, BEGIN AND END YEAR.

  if (ISHEATB .eq.9999) return

  if (idsheat.eq.0) call getid(idsheat)

  ! C subscript +1 = Fortran subscript
  fofem_input (  0 +1) = 100 ! maximum length of msg string

  !     Zero the summation columns (index 1=3 and 4=5)

  do I = 1, MXFLCL
     do J = 1, 2
        CWD(3,I,J,5) = 0.0
     end do
  end do

  !     Calculate summation in Pile categories

  do I = 1, 2
     do J = 1, MXFLCL
        do K = 1, 2
           do L = 1, 4
              CWD(3,J,K,5) = CWD(3,J,K,5) + CWD(I,J,K,L)
           end do
        end do
     end do
  end do

  !     This assumes that "hard" surface fuel is sound and "soft" surface
  !     fuel is rotten.  It also assumes that half of the 6-12" stuff is
  !     6-9" and the other half is 9-12".

  fofem_input (  1 +1) = CWD(3,1,1,5) + CWD(3,1,2,5) ! 1 hour wood
  fofem_input (  2 +1) = CWD(3,2,1,5) + CWD(3,2,2,5) ! 10 hour wood
  fofem_input (  3 +1) = CWD(3,3,1,5) + CWD(3,3,2,5) ! 100 hour wood

  fofem_input (  4 +1) = CWD(3,4,2,5) ! Sound Large Woods
  fofem_input (  5 +1) = 0.5*CWD(3,5,2,5)
  fofem_input (  6 +1) = 0.5*CWD(3,5,2,5) + CWD(3,6,2,5)
  fofem_input (  7 +1) = CWD(3,7,2,5)+ CWD(3,8,2,5)+ CWD(3,9,2,5)

  fofem_input (  8 +1) = CWD(3,4,1,5) ! Rotten Large Wood
  fofem_input (  9 +1) = 0.5*CWD(3,5,1,5)
  fofem_input ( 10 +1) = 0.5*CWD(3,5,1,5) + CWD(3,6,1,5)
  fofem_input ( 11 +1) = CWD(3,7,1,5)+ CWD(3,8,1,5)+ CWD(3,9,1,5)

  fofem_input ( 12 +1) = CWD(3,11,1,5) + CWD(3,11,2,5) ! Duff load
  fofem_input ( 12 +1) = MAX (0.45, fofem_input ( 12 +1))

  !     this duff depth assumes that duff is 12 tons/acre/inch

  ! Duff depth, inches
  fofem_input ( 13 +1) = (CWD(3,11,1,5) + CWD(3,11,2,5))/12
  ! Litter Load
  fofem_input ( 14 +1) = CWD(3,10,1,5) + CWD(3,10,2,5)
  fofem_input ( 15 +1) = FLIVE(1) ! Herb load
  fofem_input ( 16 +1) = FLIVE(2) ! Shrub

  ! Crown Foliage load
  fofem_input ( 17 +1) = 0.5*TCLOAD*P2T*43560
  ! Crown Branch load
  fofem_input ( 18 +1) = 0.5*TCLOAD*P2T*43560
  ! Percent of crown that will burn
  fofem_input ( 19 +1) = CRBURN*100

  fofem_input ( 20 +1) = MOIS(1,2)*100 ! 10 Hr Moisture
  fofem_input ( 21 +1) = MOIS(1,4)*100 ! 1000 Hr Moisture
  fofem_input ( 22 +1) = MOIS(1,5)*100 ! Duff Moisture

  !     get default fuel moistures for this variant

  call FMMOIS(1, MOIS1)
  call FMMOIS(2, MOIS2)
  call FMMOIS(3, MOIS3)
  call FMMOIS(4, MOIS4)

  !     use duff moisture to determine the soil moisture and moisture code

  duffcat(1) = mois1(1,5)
  duffcat(2) = mois2(1,5)
  duffcat(3) = mois3(1,5)
  duffcat(4) = mois4(1,5)

  soilmois = ALGSLP(MOIS(1,5),DUFFCAT,SMCAT,4)

  if (MOIS(1,5) .le. ((MOIS1(1,5) + MOIS2(1,5))/2)) then
     moiscode = 1
  else if (MOIS(1,5) .le. ((MOIS2(1,5) + MOIS3(1,5))/2)) then
     moiscode = 2
  else if (MOIS(1,5) .le. ((MOIS3(1,5) + MOIS4(1,5))/2)) then
     moiscode = 3
  else
     moiscode = 4
  end if

  ! Soil Moisture
  fofem_input ( 23 +1) = soilmois
  ! Moisture Condition, 1-VeryDry,2-Dry,3-Moderate,4-Wet
  fofem_input ( 24 +1) = moiscode
  ! Soil Type,  1-LoaSke,2-FinSil,3-Fin,4-CoaSil,5-CoaLoa
  fofem_input ( 25 +1) = SOILTP
  ! Duff Moisture Method, 1-Entire, 2-Low, 3-NFDR, 4-AdjNFDR
  fofem_input ( 26 +1) = 1

  if (BURNSEAS .eq.4) then
     seasnum = 3
  else if (BURNSEAS .eq.3) then
     seasnum = 2
  else
     seasnum = 1
  end if

  ! Season, 1-Spring, 2-Summer, 3-Fall, 4-Winter
  fofem_input ( 27 +1) = seasnum

  select case (VARACD)

  case ('CS','LS','NE','ON')
     regnum = 4

  case ('SN')
     regnum = 3

  case ('NC','OP','PN','WC')
     regnum = 2

  case default
     regnum = 1

  end select

  ! Region, 1-InteriorWest, 2-Pacific, 3-SE, 4-NE
  fofem_input ( 28 +1) = regnum
  ! Fuel Category, 1-Natural, 2-Pile, 3-Slash
  fofem_input ( 29 +1) = 1

  select case (VARACD)

  case ('NE')
     covnum = 8

  case ('LS')
     covnum = 7

  case default
     covnum = 5

  end select

  ! Cover Group:
  ! 0-None, 1-Grass, 2-Sage, 3-Shrub,  4-Pocosin,
  ! 5-Ponderosa, 6-White Pine Hemlock , 7-Red Jack Pine,
  ! 8-Balsam, Blk Red Whit Spruce
  fofem_input ( 30 +1) = covnum
  ! Output flag, 0=no output, 1=a file of output.
  fofem_input ( 31 +1) = 0

  if (debug) write (jostnd, 10) fofem_input
  10 format (/' FMSOILHEAT, FOFEM_INPUT:',32F7.2)

  fofem_rc = fm_fofem(fofem_input,fofem_output,fofem_msg)

  if (fofem_rc.gt.0) then
     write (jostnd, 20) fofem_msg(1:FOFEM_RC)
     20 format (/' ******** ERROR: FMSOILHEAT MESSAGE TEXT:', A)
     call rcdset (2,.true.)
     return
  end if

  call getlun (jrout)

  if (isheatb.lt.0) then
     !         isheatb=-isheatb
     write (jrout,30) idsheat,idsheat,idsheat,idsheat,nplt,&
          &mgmid,idsheat,idsheat,(i,i = 0,13),idsheat
     30 format (1x,i5/1x,i5,1x, 75('-')/1x,i5,t21,&
          &'FOFEM SOIL HEATING ESTIMATES FOR SIMULATED FIRES'/&
          &1x,i5,' STAND ID: ',A,' MGMT ID: ',A,T71,'DEPTH WHERE'/&
          &1x,i5,'       --TEMP IN CELCIUS AT THESE DEPTHS ',&
          &'BELOW SURFACE (CM)---  TEMP EXCEEDS'/&
          &1x,i5,' YEAR ',14i4,'   60  275 C'/&
          &1x,i5,1x, 75('-'))
  end if

  write (jrout,40) idsheat,iyr,&
       &(int(fofem_output(i)),i = 1,16)
  40 format (1x,i5,1x,i4,1x,14i4,2i5)

  return
end
