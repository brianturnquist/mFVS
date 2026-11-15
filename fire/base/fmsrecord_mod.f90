!===============================!
!   fire/base : fmsrecord_mod   !
!===============================!
module FMSRECORD_MOD
  implicit none
  private
  public :: FMSRECORD

contains

  subroutine FMSRECORD(IYR,YRLAST,TOTDBH,TOTHTH,TOTHTS,&
       &TOTVLH,TOTVLS,TOTDH,TOTDS)
    implicit none

    include 'PRGPRM.f90'
    include 'FMPARM.f90'
    include 'CONTRL.f90'
    include 'PLOT.f90'
    include 'FMCOM.f90'
    include 'FMFCOM.f90'
    include 'METRIC.f90'

    integer, intent(in) :: IYR,YRLAST
    real, intent(inout) :: TOTDBH(MAXSP,100,6),TOTHTH(MAXSP,100,6)
    real, intent(inout) :: TOTHTS(MAXSP,100,6),TOTVLH(MAXSP,100,6)
    real, intent(inout) :: TOTVLS(MAXSP,100,6),TOTDH(MAXSP,100,6),TOTDS(MAXSP,100,6)
    integer :: JYR,IDC,JCL,DBSKODE
    real :: TOTN
    logical :: LOK

    ! Average in native units in FMSOUT before entering this block. Metric
    ! volumes retain the original FT3toM3 factor and acre basis; changing it
    ! to a volume-per-hectare factor is a separate scientific decision.
    if (LMETRICVAR) then
      do JYR = 1,YRLAST
         do IDC = 1,MAXSP
            do JCL = 1,6
               TOTDBH(IDC,JYR,JCL) = TOTDBH(IDC,JYR,JCL) * INtoCM
               TOTHTH(IDC,JYR,JCL) = TOTHTH(IDC,JYR,JCL) * FTtoM
               TOTHTS(IDC,JYR,JCL) = TOTHTS(IDC,JYR,JCL) * FTtoM
               TOTVLH(IDC,JYR,JCL) = TOTVLH(IDC,JYR,JCL) * FT3toM3
               TOTVLS(IDC,JYR,JCL) = TOTVLS(IDC,JYR,JCL) * FT3toM3
               TOTDH(IDC,JYR,JCL) = TOTDH(IDC,JYR,JCL)  / ACRtoHA
               TOTDS(IDC,JYR,JCL) = TOTDS(IDC,JYR,JCL)  / ACRtoHA
               142 continue
            end do
            141 continue
         end do
         140 continue
      end do
    end if

    !     CALL THE DBS MODULE TO OUTPUT DETAILED SNAG REPORT TO A DATABASE

    DBSKODE = 1
    call DBSFMDSNAG(IYR,TOTDBH,TOTHTH,TOTHTS,&
         &TOTVLH,TOTVLS,TOTDH,TOTDS,YRLAST,DBSKODE)
    if (DBSKODE.eq.0) return

    !     Make sure JSNOUT is opened.

    call openIfClosed (JSNOUT,"sng",LOK)
    if (.not.LOK) return

    !     Print the snag output headings.

    if (LSHEAD) then
       if (LMETRICVAR) then
          write(JSNOUT,201) NPLT
          write(JSNOUT,222)
          write(JSNOUT,212)
          write(JSNOUT,213)
          write(JSNOUT,221)
       else
          write(JSNOUT,200) NPLT
          write(JSNOUT,222)
          write(JSNOUT,210)
          write(JSNOUT,211)
          write(JSNOUT,220)
       end if
       write(JSNOUT,222)
       ! Retain the existing metric heading apostrophe (bug 22) for parity
       ! with the immediate baseline; its original F77 text is recorded there.
       201 format(' ESTIMATED SNAG CHARACTERISTICS '&
            &'(BASED ON STOCKABLE AREA), STAND ID=',A)
       212 format(13X,'DEATH CURR',&
            &' HEIGHT CURR VOLUME (M3)       ',&
            &'    DENSITY (SNAGS/HA)   ')
       213 format(9X,'DBH  DBH ',1X,&
            &4('-'),' (M)',3('-'),1X,17('-'),1X,'YEAR',1X,23('-'))
       221 format(' YEAR SP  CL  (CM)',1X,&
            &' HARD  SOFT  HARD  SOFT TOTAL',&
            &1X,'DIED   HARD    SOFT    TOTAL')
       200 format(' ESTIMATED SNAG CHARACTERISTICS ',&
            &'(BASED ON STOCKABLE AREA), STAND ID=',A)
       210 format(13X,'DEATH CURR',&
            &' HEIGHT CURR VOLUME (FT3)      ',&
            &'   DENSITY (SNAGS/ACRE)  ')
       211 format(9X,'DBH  DBH ',1X,&
            &4('-'),'(FT)',3('-'),1X,17('-'),1X,'YEAR',1X,23('-'))
       220 format(' YEAR SP  CL  (IN)',1X,&
            &' HARD  SOFT  HARD  SOFT TOTAL',&
            &1X,'DIED   HARD    SOFT    TOTAL')
       222 format(1X,76('-'))
       LSHEAD = .false.
    end if

    !     Print information on each snag printing-class, first dividing
    !     the total heights and dbhs to get the class-averages.

    do JYR = 1,YRLAST
       do IDC = 1,MAXSP
          do JCL = 1,6
             TOTN = TOTDH(IDC,JYR,JCL) + TOTDS(IDC,JYR,JCL)
             if (TOTN .eq.0.0) goto 410
             write(JSNOUT,300) IYR,JSP(IDC),JCL,&
                  &TOTDBH(IDC,JYR,JCL),&
                  &TOTHTH(IDC,JYR,JCL),&
                  &TOTHTS(IDC,JYR,JCL),&
                  &INT(TOTVLH(IDC,JYR,JCL)),&
                  &INT(TOTVLS(IDC,JYR,JCL)),&
                  &INT(TOTVLH(IDC,JYR,JCL)+TOTVLS(IDC,JYR,JCL)),&
                  &(IYR-JYR+1),&
                  &TOTDH(IDC,JYR,JCL),&
                  &TOTDS(IDC,JYR,JCL), TOTN
             300 format(1X,I4,1X,A2,1X,I3,1X,3(F5.1,1X),&
                  &3(I5,1X),I4,1X,3(F7.2,1X))
             410 continue
          end do
          420 continue
       end do
       430 continue
    end do

    inquire(unit = JSNOUT,opened = LOK)
    if (LOK) close(unit = JSNOUT)

  end subroutine FMSRECORD

end module FMSRECORD_MOD
