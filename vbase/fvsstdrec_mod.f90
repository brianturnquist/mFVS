!=======================================================!
!   Write the normal-precision FVSSTD live-tree record   !
!=======================================================!
module FVSSTDREC_MOD
  implicit none
  private
  public :: FVSSTDREC

contains

  subroutine FVSSTDREC(I,TID,P,DP,DGI,CW,IDMR,IPTBAL,ICDF,IBDF,&
       &PDBHI,PHTI,RTCUFV,RMCUFV,RPTOCFV,RMRCFV)
    implicit none

    include 'PRGPRM.f90'
    include 'ARRAYS.f90'
    include 'PLOT.f90'
    include 'STDSTK.f90'
    include 'FVSSTDCM.f90'
    include 'METRIC.f90'

    integer, intent(in) :: I,IDMR,IPTBAL,ICDF,IBDF
    character(len=8), intent(in) :: TID
    real, intent(in) :: P,DP,DGI,CW,PDBHI,PHTI
    real, intent(in) :: RTCUFV,RMCUFV,RPTOCFV,RMRCFV

    ! FVSSTD chooses the four cubic-volume fields before this call:
    ! imperial eastern variants retain their reported volume selection,
    ! while metric callers retain CFV/WK1 and PTOCFV/PMRCFV. Scheduling,
    ! density-format selection and PDBH/PHT resets remain in the caller.
    if (LMETRICVAR) then
      ! Ontario's alternate volume methods divide BFV by FT3toM3 in VOLS
      ! to compensate for this output factor. Keep that producer contract
      ! and the original conversion-before-rounding of integer heights.
      write(KOLIST,21)&
           &TID,&
           &I,&
           &NSP(ISP(I),1)(1:2),&
           &ISP(I),&
           &IMC(I),&
           &ISPECL(I),&
           &ITRE(I),&
           &P/ACRtoHA,&
           &DP/ACRtoHA,&
           &DBH(I)*INtoCM,&
           &DGI*INtoCM,&
           &HT(I)*FTtoM,&
           &HTG(I)*FTtoM,&
           &ICR(I),&
           &CW*FTtoM,&
           &IDMR,&
           &PCT(I),&
           &IPTBAL,&
           &RTCUFV*FT3toM3,&
           &RMCUFV*FT3toM3,&
           &BFV(I)*FT3toM3,&
           &ICDF,&
           &IBDF,&
           &NINT(FLOAT(ITRUNC(I)+5)*.01*FTtoM),&
           &PDBHI*INtoCM,&
           &PHTI*FTtoM,&
           &RPTOCFV*FT3toM3,&
           &RMRCFV*FT3toM3,&
           &PMRBFV(I)*FT3toM3,&
           &NCFDEF(I),&
           &NBFDEF(I),&
           &YRDLOS(I),&
           &NINT(FLOAT(NORMHT(I)+5)*.01*FTtoM),&
           &ABIRTH(I),&
           &IPVEC(ITRE(I))
    else
      ! Imperial values pass through without conversion arithmetic.
      write(KOLIST,21) TID,I,NSP(ISP(I),1)(1:2),ISP(I),IMC(I),&
           &ISPECL(I),ITRE(I),P,DP,DBH(I),DGI,HT(I),HTG(I),ICR(I),CW,&
           &IDMR,PCT(I),IPTBAL,RTCUFV,RMCUFV,BFV(I),ICDF,IBDF,&
           &((ITRUNC(I)+5)/100),PDBHI,PHTI,&
           &RPTOCFV,RMRCFV,PMRBFV(I),NCFDEF(I),NBFDEF(I),&
           &YRDLOS(I),((NORMHT(I)+5)/100),ABIRTH(I),IPVEC(ITRE(I))
    end if
    21 format(1X,A8,1X,I4,1X,A2,I3,2(1X,I2),1X,I3,1X,F8.3,1X,F8.3,&
         &1X,F5.1,1X,F5.2,1X,F5.1,1X,F4.1,1X,I2,1X,F4.1,1X,I2,&
         &1X,F6.2,1X,I5,1X,F6.1,1X,F6.1,1X,F7.1,1X,I2,1X,I2,&
         &1X,I3,&
         &1X,F5.1,1X,F5.1,1X,F6.1,1X,F6.1,1X,F7.1,1X,I2,1X,I2,&
         &1X,F7.5,1X,I5,1X,F6.0,1X,I8)
  end subroutine FVSSTDREC

end module FVSSTDREC_MOD
