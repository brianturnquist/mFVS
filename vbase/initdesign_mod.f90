!============================================!
!   Handle the DESIGN keyword in INITRE      !
!============================================!
module INITDESIGN_MOD
  implicit none
  private
  public :: INITDESIGN

contains

  subroutine INITDESIGN(ARRAY,LNOTBK,KEYWRD,LKECHO)
    implicit none

    include 'PRGPRM.f90'
    include 'CONTRL.f90'
    include 'PLOT.f90'
    include 'METRIC.f90'

    real, intent(inout) :: ARRAY(12)
    logical, intent(in) :: LNOTBK(12),LKECHO
    character(len=8), intent(in) :: KEYWRD
    real :: XXG,XBAF

    ! Convert the incoming fields before testing presence, as in the metric
    ! readers. Only fields 1-7 belong to DESIGN; leave the other five intact.
    if (LMETRICVAR) then
      if (ARRAY(1) .lt.0.0) then
        ARRAY(1) = ARRAY(1) / HAtoACR
      else
        ARRAY(1) = ARRAY(1) * M2pHAtoFT2pACR
      end if
      ARRAY(2) = ARRAY(2) / HAtoACR
      ARRAY(3) = ARRAY(3) * CMtoIN
    end if

    if ( LNOTBK(1) ) BAF = ARRAY(1)
    if ( LNOTBK(2) ) FPA = ARRAY(2)
    if ( LNOTBK(3) ) BRK = ARRAY(3)
    if ( LNOTBK(4) ) IPTINV = IFIX( ARRAY(4) )
    if ( LNOTBK(5) ) NONSTK = IFIX( ARRAY(5) )
    if ( LNOTBK(6) ) SAMWT = ARRAY(6)
    if (SAMWT.le.0..and. IPTINV.gt.0) SAMWT = FLOAT(IPTINV)

    ! Stocking historically tests the value without LNOTBK(7). XXG previews
    ! automatic stocking; PROCESS still owns finalization and inversion.
    if (ARRAY(7).gt.1.0.and. ARRAY(7).le.100.) ARRAY(7) = ARRAY(7)*.01
    if (ARRAY(7).gt.0.0.and. ARRAY(7).le.1.0) GROSPC = ARRAY(7)
    if (GROSPC.lt.0.) then
      XXG = 1.0
      if (IPTINV.gt.0.and. NONSTK.gt.0.and.&
           &IPTINV-NONSTK.gt.0) XXG = (FLOAT(IPTINV)-FLOAT(NONSTK))/&
           &FLOAT(IPTINV)
    else
      XXG = GROSPC
    end if

    if (LMETRICVAR) then
      if (BAF .lt.0.0) then
        XBAF = BAF/ACRtoHA
      else
        XBAF = BAF*FT2pACRtoM2pHA
      end if
      if (LKECHO)write(JOSTND,2010) KEYWRD,XBAF,FPA/ACRtoHA,BRK*INtoCM,&
           &MAX(0,IPTINV),MAX(0,NONSTK),MAX(0.,SAMWT),XXG
    else
      if (LKECHO)write(JOSTND,2010) KEYWRD,BAF,FPA,BRK,&
           &MAX(0,IPTINV),MAX(0,NONSTK),MAX(0.,SAMWT),XXG
    end if
    2010 format (/A,T12,'BASAL AREA FACTOR= ',F6.1,&
         &'; INVERSE OF FIXED PLOT AREA= ',F6.1,&
         &'; BREAK DBH= ',F6.1/T12,'NUMBER OF PLOTS= ',I5,&
         &'; NON-STOCKABLE PLOTS= ',I5,&
         &'; STAND SAMPLING WEIGHT=',F12.5/T12,&
         &'PROPORTION OF STAND CONSIDERED STOCKABLE= ',F6.3)
    if (LKECHO)write(JOSTND,2030)
    2030 format (T12,'SEE "OPTIONS SELECTED BY DEFAULT" FOR FINAL ',&
         &'DESIGN VALUES.')
  end subroutine INITDESIGN

end module INITDESIGN_MOD
