!===================================!
!   fire/vbase : fmburnreport_mod   !
!===================================!
module FMBURNREPORT_MOD
  implicit none
  private
  public :: FMBURNREPORT

contains

  subroutine FMBURNREPORT(IYR,FLAME,FMD,IFIRE,CFTMP,JROUT)
    implicit none

    include 'PRGPRM.f90'
    include 'FMPARM.f90'
    include 'CONTRL.f90'
    include 'PLOT.f90'
    include 'FMCOM.f90'
    include 'FMFCOM.f90'
    include 'METRIC.f90'

    integer, intent(in) :: IYR,FMD,IFIRE,JROUT
    real, intent(in) :: FLAME
    character(len=*), intent(in) :: CFTMP
    integer :: DBSKODEB,I
    real :: WIND,FLAMOUT,SCORCH
    logical :: LPRINT

    ! Metric BURNREPT still uses the old negative begin flag (bug 25).
    ! Preserve this independently of the imperial begin/end-year contract.
    if (LMETRICVAR) then
       LPRINT = IFMBRB .lt.0.and. IFIRE .ge.0
    else
       LPRINT = IYR .ge. IFMBRB .and. IYR .le. IFMBRE .and. IFIRE .ge.0
    end if
    if (LPRINT) then
       WIND = FWIND
       FLAMOUT = FLAME
       SCORCH = SCH
       if (LMETRICVAR) then
          WIND = FWIND*MItoKM
          FLAMOUT = FLAME*FTtoM
          SCORCH = SCH*FTtoM
       end if

       !     CALL THE DBS MODULE TO OUTPUT BURN CONDITIONS DATA TO A DATABASE

       if (IFIRE .ne.0) then
          DBSKODEB = 1
          call DBSFMBURN(IYR,NPLT,MOIS(1,1)*100,MOIS(1,2)*100,&
               &MOIS(1,3)*100,MOIS(1,4)*100,MOIS(1,5)*100,MOIS(2,1)*100,&
               &MOIS(2,2)*100,WIND,INT(FMSLOP*100),FLAMOUT,SCORCH,CFTMP,FMOD,&
               &FWT,DBSKODEB)
          if (DBSKODEB.eq.0) return
       end if

       IBRPAS = IBRPAS + 1
       if (IBRPAS .eq.1) then
          write (JROUT,39) IDBRN,IDBRN
          write (JROUT,40) IDBRN
          write (JROUT,41) IDBRN
          write (JROUT,43) IDBRN
          write (JROUT,44) IDBRN,NPLT,MGMID
          write (JROUT,40) IDBRN
          write (JROUT,45) IDBRN
          write (JROUT,46) IDBRN
          if (LMETRICVAR) then
             write (JROUT,48) IDBRN
          else
             write (JROUT,47) IDBRN
          end if
          write (JROUT,40) IDBRN
          39 format(2(/1X,I5))
          40 format (1X,I5,1X,113('-'))
          41 format (1X,I5,37X,'******  FIRE MODEL VERSION 1.0 ******')
          43 format (1X,I5,26X,'BURN CONDITIONS REPORT --',&
               &' CONDITIONS AT THE TIME OF THE FIRE')
          44 format (1X,I5,' STAND ID: ',A26,4X,'MGMT ID: ',A4)
          45 format (1X,I5,43X,'MIDFLAME       FLAME  SCORCH',21X,&
               &'FUEL MODELS')
          46 format (1X,I5,7X,'------------ % MOISTURE ------------- WIND',&
               &'  SLOPE  LENGTH HEIGHT',12X,31('-'))
          47 format (1X,I5,1X,'YEAR  1HR 10HR 100HR  3+ DUFF LIVE W LIVE',&
               &' H (MPH)  (%)    (FT)  (FT)  FIRE TYPE   ',4('MOD %WT '))
          48 format (1X,I5,1X,'YEAR  1HR 10HR 100HR  3+ DUFF LIVE W LIVE',&
               &' H (KMH)  (%)    (M)    (M)  FIRE TYPE   ',4('MOD %WT '))
       end if

       if (IFIRE .ne.0) then
          if (FMD.eq.0) then
             write (JROUT,100) IDBRN,IYR,&
                  &(MOIS(1,1)*100),(MOIS(1,2)*100),&
                  &(MOIS(1,3)*100),(MOIS(1,4)*100),&
                  &(MOIS(1,5)*100),(MOIS(2,1)*100),(MOIS(2,2)*100),&
                  &WIND, INT(FMSLOP*100), FLAMOUT, SCORCH, CFTMP,&
                  &0,100
          else
             write (JROUT,100) IDBRN,IYR,&
                  &(MOIS(1,1)*100),(MOIS(1,2)*100),&
                  &(MOIS(1,3)*100),(MOIS(1,4)*100),&
                  &(MOIS(1,5)*100),(MOIS(2,1)*100),(MOIS(2,2)*100),&
                  &WIND, INT(FMSLOP*100.), FLAMOUT, SCORCH, CFTMP,&
                  &(FMOD(I),INT((FWT(I)*100.)+0.5),&
                  &I = 1,NFMODS)
             100 format(1X,2I5,5F5.0,F6.0,1X,F6.0,1X,F5.1,I6,2F7.1,2X,A,T89,&
                  &8(1X,I3))
          end if
       end if
    end if
  end subroutine FMBURNREPORT

end module FMBURNREPORT_MOD
