!===========================!
!   metric/ppbase : hvsel   !
!===========================!
! METRIC-PPBASE $Id$
subroutine HVSEL
  implicit none

  !     CALLED BY HVALOC TO SET UP THE STATUS WORD FOR ALL OF
  !     THE STANDS AND POLICIES.

  !     MULTISTAND POLICY ROUTINE - N.L. CROOKSTON  - JULY 1987
  !     FORESTRY SCIENCES LABORATORY - MOSCOW, ID 83843

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PPEPRM.f90'
  include 'PPHVCM.f90'
  include 'PPCNTL.f90'
  include 'METRIC.f90'

  ! Variables
  integer :: IRC,IHV,ICPRT,IST,II,IST1,N,JJ,I,IHVST
  real    :: XC,CONTIG,PNEED,THNYLD,HRVYLD
  character(len=5) :: CSEL(5)
  character(len=4) :: COUTID(3)
  character(len=7) :: VVER
  character(len=8) :: TIM
  character(len=10) :: DAT
  integer :: IPTOUT(3)
  real    :: XCUT(3)
  logical :: LODSRT,LMORE
  data CSEL/'ERROR','  NO ','  YES','**YES','ENYP '/

  LODSRT = .true.

  !     IF MAX CONTIGUOUS CLEAR CUT ACRE CONSTRAINTS ARE BEING USED,
  !     THEN, SET UP NEIGHBORS AND AREAS DATA.
  !     IF ERRORS ARE FOUND, TURN OFF THE OPTION.

  IF (LHVDEB) WRITE (JOPPRT,'(/'' IN HVSEL: LHIER,LHVMXC,LHVUNT='', &
       &     3L2)') LHIER,LHVMXC,LHVUNT
  if (LHVMXC) then
     call C26SRT (NOSTND,STDIDS,ISDWK1,.true.)

     !        EXTRACT THE AREA DATA.

     call SPLAEX (STDIDS,NOSTND,ISDWK1,IRC)
     if (IRC.ne.0) then
        write (JOPPRT,10) 'AREAS'
        10 format (/' ********  ERROR: MAX CONTIGUOUS CLEAR',&
             &'CUT HECTARES CONSTRAINT NO LONGER BEING USED.  ',&
             &A,' DATA ERROR.')
        call RCDSET (2,.true.)
        LHVMXC = .false.
     else

        !           EXTRACT THE NEIGHBORS DATA.

        call SPNBEX (STDIDS,NOSTND,ISDWK1,IRC)
        if (IRC.ne.0) then
           write (JOPPRT,10) 'NEIGHBORS'
           call RCDSET (2,.true.)
           LHVMXC = .false.
        end if
     end if
  end if

  !     IF MAX CONTIG. CLEAR CUT CONSTRANT IS BEING USED, OR IF
  !     HIERARCHIES ARE BEING USED, TURN OFF PARTIAL CUTTING.

  if (LHVMXC.or.LHIER) LPRTCT = .false.

  !     LOOP OVER POLICIES

  do IHV = 1,IXHRVP

     !     IF THE POLICY IS NOT ACTIVE, SKIP IT

     if (IHVTAB(IHV,1).eq.0) goto 200

     !     INITIALIZE SOME VARIABLES.

     HRVYLD = 0.0
     LMORE = .true.
     ICPRT = 0
     HVPART(IHV) = 0.0

     !     SORT THE STAND PRIORITIES INTO DESCENDING ORDER.
     !     LODSRT IS A SWITCH THAT CONTROLS INITIALIZING ISNSRT.

     if (NOSTND .gt.0) call RDPSRT (NOSTND,HVPRI(1,IHV),ISNSRT,LODSRT)
     LODSRT = .false.

     !     SUM UP THE YIELD DUE TO THINNING.

     THNYLD = 0.0
     do IST = 1,NOSTND
        if (IHVSTA(IST,IHV).eq.1) THNYLD = THNYLD+HVTHIN(IST,IHV)
     end do
     IF (LHVDEB) WRITE (JOPPRT,'(/'' IN HVSEL: IHV,THNYLD,TRGETS'',    &
          &    I4,2E15.5)') IHV,THNYLD,TRGETS(IHV)

     !     IF THE TARGET HAS BEEN REACHED, SET ALL OF THE STATUS
     !     CODES TO 2, AND GO ON TO THE NEXT POLICY.

     if (THNYLD.ge.TRGETS(IHV).or.TRGETS(IHV).le.0.0) then
        do IST = 1,NOSTND
           if (IABS(IHVSTA(IST,IHV)).eq.1) IHVSTA(IST,IHV) = 2
        end do
     else

        !        GO THROUGH THE STANDS IN DESCENDING ORDER.

        do II = 1,NOSTND
           IST1 = ISNSRT(II)
           IF (LHVDEB) WRITE (JOPPRT,'(/'' IN HVSEL 1: IHV,IST1='',       &
                &        2I4,'' IHVSTA,HVYLDS,HRVYLD='',I4,2E15.5)') IHV,IST1,&
                &IHVSTA(IST1,IHV),HVYLDS(IST1,IHV),HRVYLD

           !        SKIP STANDS THAT HAVE A STATUS CODE NE 1 IN ABS.

           if (IABS(IHVSTA(IST1,IHV)).ne.1) goto 60

           !        IF LHIER IS TRUE (USING HIERARCHIES), THEN:
           !        IF WE ARE PROCESSING A SECOND OR SUBSEQUENT POLICY, AND
           !        IF THE STAND WAS SELECTED UNDER THE FIRST POLICY, THEN:
           !        SET THIS STAND AS "NOT SELECTED", AND BRANCH TO THE NEXT.

           if (LHIER) then
              if (IHV.gt.1.and. IABS(IHVSTA(IST1,1)).eq.3) then
                 IHVSTA(IST1,IHV) = 2
                 goto 60
              end if
           end if

           !        IF SELECTING COORDINATED MGMT UNITS (THOSE THAT HAVE THE SAME
           !        MGMTID SO LONG AS THE MGMTID IS NOT EQUAL TO NONE), THEN:
           !        SET THE UPPER INDEX OF AN INNER DO LOOP EQUAL TO THE
           !        NUMBER OF STANDS.  OTHERWISE, SET IT EQUAL TO THE CURRENT ONE.

           N = II
           if (LHVUNT) then
              if (MGMIDS(IST1).ne.'NONE') N = NOSTND
           end if

           !        OPEN AN INNER DO LOOP FROM THE CURRENT STAND TO THE CURRENT
           !        STAND OR THE LAST STAND (AS DEFINED ABOVE).

           do JJ = II,N

              !        IF WE ARE ON A SUBSEQUENT STAND, THEN: FIND OUT IF THE
              !        'FIRST' STAND IN A UNIT WAS SELECTED.  IF NOT, THEN
              !        BRANCH TO THE OUTER LOOP.  IF YES, SEARCH FOR THE NEXT STAND
              !        IN THE COORDINATED MGMT UNIT IN ORDER OF PRIORITY.

              IST = ISNSRT(JJ)
              if (JJ.gt.II) then
                 if (IABS(IHVSTA(IST1,IHV)).eq.2) goto 60
                 if (IABS(IHVSTA( IST,IHV)).ne.1) goto 50
                 if (MGMIDS(IST1).ne.MGMIDS(IST)) goto 50
              end if

              !        IF EXTERNAL SELECTION LOGIC IS BEING USED, WE WILL NEVER SELECT
              !        A STAND THAT HAS A PRIORITY OF ZERO OR LESS THAN ZERO.

              if (IHVEXT.eq.1) then
                 if (HVPRI(IST,IHV).le.0) then
                    IHVSTA(IST,IHV) = 2
                    goto 50
                 end if
              end if

              !        IF MORE STANDS ARE NEEDED, THEN...

              if (LMORE) then

                 !           IF THE YIELD IS ALMOST ZERO, THEN DON'T SELECT
                 !           THE STAND--SIGNAL THAT IT IS NOT NEEDED AND BRANCH
                 !           TO THE NEXT STAND.

                 if (HVYLDS(IST,IHV).lt.0.000001) then
                    PNEED = 0.0
                    IHVSTA(IST,IHV) = 2
                    goto 50
                 else if (LHVMXC) then

                    !              IF THE MAX CONTIGUOUS ACRE CONSTRAINT IS IN USE, AND
                    !              IF CONTIGUOUS CLEAR CUT ACRES CREATED IF THIS STAND IS
                    !              SELECTED EXCEEDS THE MAX, DO NOT SELECT THE STAND.

                    call HVCNTG (II,IHV,CONTIG,IRC)
                    if (IRC.gt.0) then
                       write (JOPPRT,10) 'AREAS OR NEIGHBORS'
                       call RCDSET (2,.true.)
                       LHVMXC = .false.
                    else if (CONTIG.gt.HVMXCC) then
                       PNEED = 0.0
                       IHVSTA(IST,IHV) = 2
                       goto 50
                    end if
                 end if

                 !           COMPUTE THE PORTION OF THE NEXT NEEDED STAND.

                 PNEED = (TRGETS(IHV)-(THNYLD+HRVYLD))/HVYLDS(IST,IHV)

                 IF (LHVDEB) WRITE (JOPPRT,'('' IN HVSEL: IHV,IST='',        &
                      &        2I4,'' PNEED='',E15.5)') IHV,IST,PNEED

                 !           IF THE PROPORTION NEEDED IS GT .999, ASSUME THAT IT IS
                 !           AT LEAST 1.0.

                 if (PNEED.ge.0.999) then

                    !              SET THE HARVEST STATUS CODE TO +/-3 (SELECTED), ADD UP
                    !              THE YIELD AND SUBTRACT THE 'THIN' (NOT SELECTED) YIELD.

                    IHVSTA(IST,IHV) = 3*IHVSTA(IST,IHV)
                    HRVYLD = HRVYLD+HVYLDS(IST,IHV)
                    THNYLD = THNYLD-HVTHIN(IST,IHV)
                 else

                    !              OTHERWISE, ONLY A PORTION OF THE YIELD FOR THIS STAND
                    !              IS NEEDED.  IF LPRTCT IS TRUE, THEN PREPARE TO SELECT
                    !              PART OF THE STAND, IF LPRTCT IS FALSE SELECT THE STAND
                    !              IF PNEED IS OVER 0.5 AND DON'T IF IT IS UNDER 0.5.

                    !              WHEN LPRTCT IS TRUE, AND IF THE PROTION IS VERY SMALL,
                    !              DO NOT CUT THE STAND, "THIN" IT.

                    !              SAVE THE PORTION IN HVPART...
                    !              A STATUS OF 4 IS CUT AND THIN.

                    if (LPRTCT) then
                       if (PNEED.gt.0.01) then
                          IHVSTA(IST,IHV) = 4
                          HRVYLD = HRVYLD+(HVYLDS(IST,IHV)*PNEED)
                          HVPART(IHV) = PNEED
                          ICPRT = IST
                       else
                          IHVSTA(IST,IHV) = 2
                       end if
                    else
                       if (PNEED.gt.0.5) then
                          IHVSTA(IST,IHV) = 3*IHVSTA(IST,IHV)
                          HRVYLD = HRVYLD+HVYLDS(IST,IHV)
                          THNYLD = THNYLD-HVTHIN(IST,IHV)
                       else
                          IHVSTA(IST,IHV) = 2
                       end if
                    end if
                    LMORE = .false.
                 end if
              else

                 !           ELSE: THE STAND IS NOT NEEDED TO REACH THE TARGET.

                 IHVSTA(IST,IHV) = 2
              end if
              IF (LHVDEB) WRITE (JOPPRT,'('' IN HVSEL 2: IHV,IST1='',        &
                   &        2I4,'' IHVSTA,HVYLDS,HRVYLD='',I4,2E15.5)') IHV,IST1,&
                   &IHVSTA(IST1,IHV),HVYLDS(IST,IHV),HRVYLD

              50 continue
           end do
           60 continue
        end do
     end if

     !     MAKE SURE THAT IHVSTA IS POSITIVE (USE OF SIGN BIT HAS ENDED).

     do IST = 1,NOSTND
        IHVSTA(IST,IHV) = IABS(IHVSTA(IST,IHV))
        90 continue
     end do

     IF (LHVDEB) WRITE (JOPPRT,'(/'' IN HVSEL: IHV='',I3,'' NOSTND='', &
          &   I4,'' LHVOUT(IHV)='',L1)') IHV,NOSTND,LHVOUT(IHV)

     !     WRITE OUTPUT STATISTICS:

     if (LHVDEB.or.LHVOUT(IHV)) then
        write (JOPPRT,100) MIY(MICYC-1),MICYC-1,IHV,&
             &TRGETS(IHV)*FT3pACRtoM3pHA,&
             &HVPLAB(IHV)(1:LNHPLB(IHV,1))
        100 format (//' YEAR=',I5,'  MASTER CYCLE=',I2,'  POLICY=',&
             &I2,'  TARGETED RESOURCE=',E15.7,'  MSPLABEL= ',A)
        if (LHIER) then
           if (IHV.eq.1) then
              write (JOPPRT,105) IHV,&
                   &'CAN NOT BE SELECTED UNDER ANY OTHER POLICY.'
           else
              write (JOPPRT,105) IHV,&
                   &'ARE LIMITED TO STANDS NOT SELECTED UNDER POLICY 1.'
              105 format (' NOTE:  POLICY HIERARCHY IN USE.  ',&
                   &'STANDS SELECTED UNDER POLICY',I3,' ',A)
           end if
        end if

        !        IF THE TARGET IS GT ZERO, WRITE THE BODY OF THE TABLE, ELSE
        !        SKIP THIS OUTPUT.

        if (THNYLD.lt.0.) THNYLD = 0.0
        if (HRVYLD.lt.0.) HRVYLD = 0.0
        if (TRGETS(IHV).gt.0.0) then
           write (JOPPRT,110)
           110 format (2(1X,60('-'),1X)/&
                &2(' STDIDENT',19X,'MGMT  PRIORITY    CREDIT   SELECT ')/&
                &2(' ',26('-'),   ' ---- ---------- ---------- ------ '))
           N = 0
           do II = 1,NOSTND
              IST = ISNSRT(II)

              !           SKIP STANDS THAT DO NOT APPLY TO THIS POLICY.

              if (IHVSTA(IST,IHV).eq.0) goto 130
              N = N+1
              IPTOUT(N) = IST
              if (IHVSTA(IST,IHV).eq.2) then
                 XCUT(N) = HVTHIN(IST,IHV)
              else
                 XCUT(N) = HVYLDS(IST,IHV)
              end if
              if (MGMIDS(IST).eq.'NONE') then
                 COUTID(N) = ' '
              else
                 COUTID(N) = MGMIDS(IST)
              end if
              if (N.ge.2) then
                 write (JOPPRT,120) (STDIDS(IPTOUT(I)),COUTID(I),&
                      &HVPRI(IPTOUT(I),IHV),XCUT(I)*FT3pACRtoM3pHA,&
                      &CSEL(IHVSTA(IPTOUT(I),IHV)), I = 1,N)
                 120 format (2(1X,A26,1X,A4,2E11.4,1X,A5,2X))
                 N = 0
              end if
              130 continue
           end do
           if (N.ne.0) write (JOPPRT,120) (STDIDS(IPTOUT(I)),&
                &COUTID(I),HVPRI(IPTOUT(I),IHV),&
                &XCUT(I)*FT3pACRtoM3pHA,&
                &CSEL(IHVSTA(IPTOUT(I),IHV)), I = 1,N)

           !           IF ICPRT IS GT 0, WRITE A MSG ABOUT PARTIALLY SELECTED
           !           STANDS.

           if (ICPRT.gt.0) write (JOPPRT,140) HVPART(IHV)*100.0,&
                &STDIDS(ICPRT)
           140 format (/' **YES= ',F5.1,' PERCENT OF STAND ',A,&
                &' WILL BE SELECTED.  THE REMAINING PORTION IS',&
                &' NOT SELECTED.')

           !           COMPUTE PERCENT OF TARGET ACTUALLY SELECTED.

           PNEED = (((HRVYLD+THNYLD)/TRGETS(IHV))*100.)
           write (JOPPRT,'()')
           write (JOPPRT,150) HRVYLD*FT3pACRtoM3pHA,&
                &THNYLD*FT3pACRtoM3pHA,&
                &(HRVYLD+THNYLD)*FT3pACRtoM3pHA,PNEED
           150 format  (' SELECTED RESOURCE=',E14.7,&
                &'  NON-SELECTED RESOURCE SUPPLY=',E14.7,&
                &'  TOTAL=',E14.7,:,'  PERCENT OF TARGET=',F6.1)
        else
           write (JOPPRT,150) HRVYLD*FT3pACRtoM3pHA,&
                &THNYLD*FT3pACRtoM3pHA,&
                &(HRVYLD+THNYLD)*FT3pACRtoM3pHA
        end if
     end if

     !     WRITE THE MACHINE READABLE VERSION OF THE OUTPUT IF IT IS
     !     REQUESTED.

     if (JOHVDS(IHV).gt.0) then

        !        COUNT UP THE NUMBER OF OUTPUT RECORDS.

        N = 0
        do II = 1,NOSTND
           if (IHVSTA(ISNSRT(II),IHV).ne.0) N = N+1
           160 continue
        end do

        !        WRITE THE HEADER RECORDS

        call VARVER (VVER)
        call GRDTIM (DAT,TIM)
        write (JOHVDS(IHV),170) N,MIY(MICYC-1),MICYC-1,IHV,LHIER,&
             &VVER,DAT,TIM,HVPLAB(IHV)(1:LNHPLB(IHV,1)),&
             &TRGETS(IHV)*FT3pACRtoM3pHA,HRVYLD*FT3pACRtoM3pHA,&
             &THNYLD*FT3pACRtoM3pHA,HVPART(IHV)
        170 format ('-9MS1',2I5,2I3,L2,4(1X,A)/'-9MS2',4E14.7)

        !        IF THE NUMBER OF OUTPUT RECORDS IS ZERO, SKIP THE POLICY.

        if (N.le.0) goto 200

        !        WRITE THE BASIC OUTPUT RECORDS.

        do II = 1,NOSTND
           IST = ISNSRT(II)
           IHVST = IHVSTA(IST,IHV)
           if (IHVST.eq.0) goto 190
           IHVST = IABS(IHVST)
           if (IHVST.eq.2) then
              XC = HVTHIN(IST,IHV)
           else
              XC = HVYLDS(IST,IHV)
           end if
           write (JOHVDS(IHV),180) STDIDS(IST),MGMIDS(IST),&
                &HVPRI(IST,IHV),XC,CSEL(5)(IHVST:IHVST)
           180 format (A26,1X,A4,2E11.4,1X,A1)
           190 continue
        end do
     end if

     !     IF EXTERNAL SELECTION METHOD 1 IS USED, WRITE OUT THE ACTUAL SELECTIONS
     !     IN STAND-SORTED ORDER.

     if (IHVEXT.eq.1) then
        call C11SRT (NOSTND,CISNUM,ISNSRT,.false.)
        JEXOPT = 83
        open (UNIT = JEXOPT,FILE = 'PPE_ActualSelections.txt',&
             &STATUS = 'REPLACE')
        write(JEXOPT,'(''"StandID","Year","Selected"'')')
        do II = 1,NOSTND
           IST = ISNSRT(II)
           if (IHVSTA(IST,1).ne.0) then
              N = MAX(1,IHVSTA(IST,1)-1)
              if (N.gt.2) N = 1
              write (JEXOPT,'(''"'',A,''",'',I5,'','',I2)')&
                   &TRIM(STDIDS(IST)),MIY(MICYC-1),N
           end if
        end do
        close (UNIT = JEXOPT)
        write (6,191) MIY(MICYC-1)
        191 format (/' PPE Note: Year=',I4,&
             &' SYSTEM CALL MakeNewActivities')
        call SYSTEM ("MakeNewActivities")
        open (UNIT = JEXOPT,FILE = 'PPE_MoreActivities.txt',&
             &STATUS = 'OLD',ERR = 195)
        goto 196
        195 continue
        JEXOPT = -1
        196 continue
     else
        JEXOPT = -1
     end if
     200 continue
  end do
  return
end
