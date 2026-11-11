!=================================!
!   Stand visualization: SVSNAD   !
!=================================!
! BASE $Id$
subroutine SVSNAD(IYEAR,ISNADD,NSNGS,ISWTCH)
  implicit none

  !     STAND VISUALIZATION GENERATION
  !     J.J.MARCHINEK -- RMRS MOSCOW -- MAY 1999
  !     A.H.DALLMANN  -- RMRS MOSCOW -- JANUARY 2000
  !     L.R. DAVID    -- FHTET FORT COLLINS -- JULY 2005
  !     S.N.SCHAROSCH -- ABACUS -- APRIL 2008

  !     USED FOR PROCESSING SNAG ADDING INFORMATION

  !     ISNADD = VECTOR OF SNAGS THAT NEED TO BE ADDED
  !     NSNGS  = NUMBER OF SNAGS THAT NEED TO BE ADDED
  !     IYEAR  = CURRENT YEAR
  !     IYOFTD = YEAR OF TREE DEATH
  !     ISWTCH = 0 IF SVESTB CALLED SVSNAD DIRECTLY
  !            = 1 IF SVMORT CALLED SVRMOV, FIRE-CAUSED MORTALITY
  !            = 2 IF SVMORT CALLED SVRMOV, NORMAL MORTALITY
  !            = 3 IF SVMORT CALLED SVRMOV, WESTWIDE PINE BEETLE MORT
  !            = 4 IF SVCUTS CALLED SVRMOV

  ! Common blocks
  include 'PRGPRM.F77'
  include 'FMPARM.F77'
  include 'FMCOM.F77'
  include 'SVDEAD.F77'
  include 'ARRAYS.F77'
  include 'PLOT.F77'
  include 'SVDATA.F77'
  include 'CONTRL.F77'

  !     ISNADD - VECTOR OF SNAGS THAT NEED TO BE ADDED
  !     SNASCO - SNAG SCORE
  !     ADDSCO - SCORE FOR ISNADD
  !     INDEX  - INDEX OF SNAG LIST
  !     IBACK  - INDEX SNAGS REMOVED, POINTING BACK TO THE OBJECT LIST

  ! Variables
  integer :: ISNADD(MXSVOB)
  real    :: ADDSCO(MXSVOB)

  real    :: SNASCO(MXDEAD)
  integer :: INDEX(MXDEAD)
  integer :: IBACK(MXDEAD)

  integer :: ISWTCH,NSNGS,IYEAR,ITCYC,IYOFTD,IX,ISVOBJ,ISNAG,I,IXX,IDEAD,HRATE2,ITIDIF,IIX,IPUT,J,K
  real    :: X,CHPOS,XHMOD,Y,FACTOR,TEMP,SNDI,SNHT,SNCRTO,SNCRDI,XPROB

  logical :: DEBUG
  call DBCHK (DEBUG,'SVSNAD',6,ICYC)

  !     SINCE

  if (ICYC.eq.0) then
     ITCYC = 1
  else
     ITCYC = ICYC
  end if

  !     TO DETERMINE IF SVCUTS(1) CALLED IT, OR SVMORTS(0)

  !     INITIALIZE THE ILYEAR

  if (ICYC.eq.0 .and. ISWTCH.eq.0) ILYEAR = IYEAR

  !     Initialize the year-of-tree-death variable.
  !     This variable was originally designed to be incremented with each
  !     new snag record, so that the tree deaths were spread evenly over
  !     the current growth cycle.
  !     However, that logic conflicts with the FFE snag logic that adds
  !     snags in the final year of the growth period. The end result
  !     was significantly different snagfall rates being predicted between
  !     the FFE and SVS logic. Therefore, the SVS logic is now set to
  !     match the FFE logic, with all mortality trees added at the end of
  !     the current growth cycle.
  !        IY -- IY(1)=INVENTORY DATE, IY(2)=ENDPOINT OF FIRST CYCLE,
  !              IY(3)=ENDPOINT OF SECOND CYCLE,...,IY(MAXCY1)=ENDPOINT
  !              OF FORTIETH CYCLE.

  !>>>  IYOFTD = IYEAR
  if ( ICYC.eq.0 .or. ISWTCH.eq.1 .or. ISWTCH.eq.4 ) then
     IYOFTD = IYEAR
  else
     IYOFTD = IY(ITCYC+1) - 1
  end if

  !     DEBUG

  if (DEBUG) then
     IX = 0
     do ISVOBJ = 1,NSVOBJ
        if (IOBJTP(ISVOBJ).eq.2) IX = IX+1
     end do
     write (JOSTND,1010) ICYC, IYEAR, ISWTCH, &
          & NDEAD, NSVOBJ, IX
     1010 format (/ ' ','ENTERING SVSNAD, ICYC=',I2,', IYEAR=',I4, &
          & ', ISWTCH=',I1,':', / , &
          & ' ',T5,'NDEAD=',I4,', SVS TOTAL OBJECTS=',I6, &
          & ', SVS SNAG OBJECTS=',I5,':',/)
  end if

  !     RETURN IF NO OBJECTS TO ADD

  if (NSNGS .eq. 0) return

  if (DEBUG) then
     do ISVOBJ = 1,NSVOBJ
        if (IOBJTP(ISVOBJ).eq.2) &
             & write (JOSTND,*) '   IS2F(',ISVOBJ,')=',IS2F(ISVOBJ)
     end do
     write(JOSTND,1020) NSNGS
     1020 format(/, '    SNAG RECORDS TO ADD (NSNGS)=',I4)
  end if

  !     CALCULATE THE SCORES FOR THE SNAGS BEING ADDED.

  do ISNAG = 1,NSNGS
     I = ISNADD(ISNAG)
     ADDSCO(I) = DBH(IS2F(I))*DBH(IS2F(I))*HT(IS2F(I))
  end do

  !     IF THERE IS ENOUGH ROOM, ADD THE SNAGS

  if (MXDEAD - NDEAD .ge. NSNGS) then
     IXX = 0
     ISNAG = 0
     do IDEAD = 1,MXDEAD

        !           LOOPS THROUGH THE LIST OF SNAGS TO FIND AN OPEN SPOT

        if (ISTATUS(IDEAD).eq.0 .and. NSNGS.gt.ISNAG) then

           !              WE FOUND AN OPEN SPOT, AND STILL HAVE SNAGS TO ADD

           ISNSP(IDEAD) = ISP(IS2F(ISNADD(NSNGS-ISNAG)))
           if (IOBJTP(ISNADD(NSNGS-ISNAG)) .eq. -3) then

              ISTATUS(IDEAD) = 1
              call SVRANN(X)
              FALLDIR(IDEAD) = IFIX(360. *X +.5)
              CRNRTO(IDEAD) = 99.
              OLEN(IDEAD) = HT(IS2F(ISNADD(NSNGS-ISNAG)))* &
                   & ICR(IS2F(ISNADD(NSNGS-ISNAG)))*.01
              SNGLEN(IDEAD) = OLEN(IDEAD)
              CHPOS = HT(IS2F(ISNADD(NSNGS-ISNAG)))- &
                   & OLEN(IDEAD)

              !                 0.0174532778 IS APPROXIMATELY PI/180.

              XSLOC(ISNADD(NSNGS-ISNAG)) = XSLOC(ISNADD(NSNGS-ISNAG)) &
                   & +CHPOS*SIN(FALLDIR(IDEAD)*0.0174532778)
              YSLOC(ISNADD(NSNGS-ISNAG)) = YSLOC(ISNADD(NSNGS-ISNAG)) &
                   & +CHPOS*COS(FALLDIR(IDEAD)*0.0174532778)
              ODIA(IDEAD) = DBH(IS2F(ISNADD(NSNGS-ISNAG)))* &
                   & OLEN(IDEAD)/HT(IS2F(ISNADD(NSNGS-ISNAG)))
              SNGDIA(IDEAD) = ODIA(IDEAD)
              CRNDIA(IDEAD) = CRWDTH(IS2F(ISNADD(NSNGS-ISNAG)))
              SNGCNWT(IDEAD,0:3) =  &
                   & CROWNW(IS2F(ISNADD(NSNGS-ISNAG)),0:3)
              OIDTRE(IDEAD) = IDTREE(IS2F(ISNADD(NSNGS-ISNAG)))

           else
              OLEN(IDEAD) = HT(IS2F(ISNADD(NSNGS-ISNAG)))
              SNGLEN(IDEAD) = OLEN(IDEAD)
              ODIA(IDEAD) = DBH(IS2F(ISNADD(NSNGS-ISNAG)))
              SNGDIA(IDEAD) = ODIA(IDEAD)
              CRNRTO(IDEAD) = ICR(IS2F(ISNADD(NSNGS-ISNAG)))
              CRNDIA(IDEAD) = CRWDTH(IS2F(ISNADD(NSNGS-ISNAG)))
              SNGCNWT(IDEAD,0:3) =  &
                   & CROWNW(IS2F(ISNADD(NSNGS-ISNAG)),0:3)
              OIDTRE(IDEAD) = IDTREE(IS2F(ISNADD(NSNGS-ISNAG)))

              if (IOBJTP(ISNADD(NSNGS-ISNAG)) .eq. -2) then
                 call SVRANN(X)
                 FALLDIR(IDEAD) = IFIX(360. *X +.5)
              else
                 FALLDIR(IDEAD) = -1
              end if
              if (ISWTCH .eq. 1) then
                 !                    THIS IS A FIRE MORTALITY, SET ISTATUS TO 5.
                 ISTATUS(IDEAD) = 5
              else if (ISWTCH .eq. 3) then
                 !                    THIS IS A WWPB MORTALITY, SET ISTATUS TO 90.
                 ISTATUS(IDEAD) = 90
              else
                 !                    CALCULATE ISTATUS NORMALLY.
                 if (IOBJTP(ISNADD(NSNGS-ISNAG)) .eq. -1) then
                    ISTATUS(IDEAD) = 2
                 else
                    ISTATUS(IDEAD) = 1
                 end if
              end if
           end if
           IYRCOD(IDEAD) = IYOFTD

           !              IF SNAG IS ADDED AT THE BEGINNING, THE YEAR OF DEATH
           !              OF THE SNAG MUST BE DETERMINED, AS WELL AS THE
           !              ORIGINAL INFORMATION

           if (ICYC .eq. 0) then
              if (IMC(IS2F(ISNADD(NSNGS-ISNAG))) .eq. 7) then
                 IYRCOD(IDEAD) = IYRCOD(IDEAD)-MAX(IFIX(FINTM*.7),1)
                 ISTATUS(IDEAD) = 3
              else if (IMC(IS2F(ISNADD(NSNGS-ISNAG))) .eq. 9) then
                 IYRCOD(IDEAD) = IYRCOD(IDEAD)-IFIX(FINTM+2)
                 ISTATUS(IDEAD) = 4
              end if
              call SVHABT(XHMOD)
              if (ISTATUS(IDEAD) .eq. 4) then
                 HRATE2 = 2
                 Y = YHFHTS(ISNSP(IDEAD))
              else
                 HRATE2 = 1
                 Y = YHFHTH(ISNSP(IDEAD))
              end if
              ITIDIF = IYEAR-IYRCOD(IDEAD)

              if ( DEBUG ) then
                 write(JOSTND,1025) IDEAD, IYRCOD(IDEAD), ITIDIF, &
                      & ODIA(IDEAD), OLEN(IDEAD)
                 1025 format (/ ' ',T5,'ADDING REC FOR ', &
                      & 'INITIAL TREELIST SNAG:',/, &
                      & ' ',T8,'IDEAD=',I4,', IYRCOD=',I4, &
                      & ', ITIDIF=',I2,/, &
                      & ' ',T8,'BEFORE BACKDATING, ODIA=',F4.1, &
                      & ', OLEN=',F5.1)
              end if

              SNGLEN(IDEAD) = OLEN(IDEAD)
              if (ITIDIF .gt. 0) then
                 FACTOR = (1-0.0228*XHMOD*HRATE(ISNSP(IDEAD))*HRATE2)
                 if (FACTOR**(ITIDIF) .gt. 0.5) then
                    OLEN(IDEAD) = OLEN(IDEAD)/(FACTOR**(ITIDIF))
                 else
                    OLEN(IDEAD) = OLEN(IDEAD) / &
                         & (0.5*FACTOR**(MAX(0.0,(ITIDIF-Y))))
                 end if
              end if

              if ( DEBUG ) then
                 write (JOSTND,1026) OLEN(IDEAD), SNGLEN(IDEAD)
                 1026 format (' ',T8,'AFTER BACKDATING, OLEN=',F5.1, &
                      & ',SNGLEN=',F5.1)
              end if
              CRNDIA(IDEAD) = CRNDIA(IDEAD)/(.90**ITIDIF)
              !>>>           ELSE

              !                 The following logic increments IYOFTD (yr of tree death),
              !                 by one for each consecutively-added snag record, so that
              !                 snag additions are spread equally across the years
              !                 comprising the current growth period.

              !                 Override IYOFTD assignment if SVSNAD is being called for
              !                 fire mortality. In this case, assign all snags a year-of-death
              !                 equal to the burn year.

              !>>>              NOTE: logic to spread mortality across years in current growth
              !>>>                    period is disabled, in order to match FFE logic.
              !>>>
              !>>>              IF (IYOFTD+1 .LT. IY(ITCYC+1) .AND. ISWTCH.NE.4) THEN
              !>>>                 IYOFTD=IYOFTD+1
              !>>>              ELSE
              !>>>                 IYOFTD=IYEAR
              !>>>              ENDIF
              !>>>              IF ( ISWTCH .EQ. 1 ) THEN
              !>>>                 IYOFTD=IYEAR
              !>>>              ENDIF
           end if
           IS2F(ISNADD(NSNGS-ISNAG)) = IDEAD
           IOBJTP(ISNADD(NSNGS-ISNAG)) = 2
           NDEAD = NDEAD+1
           ISNAG = ISNAG+1
        end if
     end do

  else
     !        THERE IS NOT ENOUGH ROOM.  ONLY ADD THE LARGEST ONES
     if (DEBUG) then
        IX = 0
        do ISVOBJ = 1,NSVOBJ
           if (IOBJTP(ISVOBJ).eq.2) IX = IX+1
        end do
        write (JOSTND,*) 'SVSNAD1NDEAD=', NDEAD, &
             & 'DEADOBJECTS=',IX
     end if

     !        SORT ADDSCO: ORIGINAL CODE USED IAPSRT, WHICH RETURNS INDEX TO AN
     !        INTEGER VECTOR SORTED IN ASCENDING VECTOR; SINCE RDPSRT GIVES AN
     !        INDEX TO A REAL VECTOR SORTED IN DESCENDING ORDER, THE RETURNED
     !        INDICES (ISNADD) TO THE REAL VECTOR ARE REVERSED IN PLACE TO GIVE AN
     !        ASCENDING REAL VECTOR. THIS IS MAKES THE SMALLEST POSSIBLE CHANGE
     !        TO THE CODE. - DR/ESSA

     call RDPSRT(NSNGS,ADDSCO,ISNADD,.false.)
     if (NSNGS .gt. 1) then
        K = NSNGS
        J = ISNADD(K)
        do I = 1, (NSNGS/2)
           ISNADD(K) = ISNADD(I)
           ISNADD(I) = J
           K = K-1
           J = ISNADD(K)
        end do
     end if

     ISNAG = 0
     if (MXDEAD .gt. NDEAD) then
        do IDEAD = 1,MXDEAD
           if (ISTATUS(IDEAD) .eq. 0 .and. NSNGS.gt.ISNAG) then
              ISNSP(IDEAD) = ISP(IS2F(ISNADD(NSNGS-ISNAG)))
              if (IOBJTP(ISNADD(NSNGS-ISNAG)) .eq. -3) then
                 print *, 'IOBJTP IS -3'
                 ISTATUS(IDEAD) = 1
                 call SVRANN(X)
                 FALLDIR(IDEAD) = IFIX(360. *X +.5)
                 CRNRTO(IDEAD) = 99.
                 OLEN(IDEAD) = HT(IS2F(ISNADD(NSNGS-ISNAG)))* &
                      & ICR(IS2F(ISNADD(NSNGS-ISNAG)))*.01
                 SNGLEN(IDEAD) = OLEN(IDEAD)
                 CHPOS = HT(IS2F(ISNADD(NSNGS-ISNAG)))- &
                      & OLEN(IDEAD)
                 !                    0.0174532778 IS APPROXIMATELY PI/180.
                 XSLOC(ISNADD(NSNGS-ISNAG)) =  &
                      & XSLOC(ISNADD(NSNGS-ISNAG)) &
                      & +CHPOS*SIN(FALLDIR(IDEAD)*0.0174532778)
                 YSLOC(ISNADD(NSNGS-ISNAG)) =  &
                      & YSLOC(ISNADD(NSNGS-ISNAG)) &
                      & +CHPOS*COS(FALLDIR(IDEAD)*0.0174532778)
                 ODIA(IDEAD) = DBH(IS2F(ISNADD(NSNGS-ISNAG)))* &
                      & OLEN(IDEAD)/HT(IS2F(ISNADD(NSNGS-ISNAG)))
                 SNGCNWT(IDEAD,0:3) =  &
                      & CROWNW(IS2F(ISNADD(NSNGS-ISNAG)),0:3)
                 SNGDIA(IDEAD) = ODIA(IDEAD)
                 CRNDIA(IDEAD) = CRWDTH(IS2F(ISNADD(NSNGS-ISNAG)))

                 OIDTRE(IDEAD) = IDTREE(IS2F(ISNADD(NSNGS-ISNAG)))

              else
                 OLEN(IDEAD) = HT(IS2F(ISNADD(NSNGS-ISNAG)))
                 SNGLEN(IDEAD) = OLEN(IDEAD)
                 ODIA(IDEAD) = DBH(IS2F(ISNADD(NSNGS-ISNAG)))
                 SNGDIA(IDEAD) = ODIA(IDEAD)
                 CRNRTO(IDEAD) = ICR(IS2F(ISNADD(NSNGS-ISNAG)))
                 CRNDIA(IDEAD) = CRWDTH(IS2F(ISNADD(NSNGS-ISNAG)))
                 SNGCNWT(IDEAD,0:3) =  &
                      & CROWNW(IS2F(ISNADD(NSNGS-ISNAG)),0:3)
                 OIDTRE(IDEAD) = IDTREE(IS2F(ISNADD(NSNGS-ISNAG)))

                 if (IOBJTP(ISNADD(NSNGS-ISNAG)) .eq. -2) then
                    call SVRANN(X)
                    FALLDIR(IDEAD) = IFIX(360. *X +.5)
                 else
                    FALLDIR(IDEAD) = -1
                 end if
                 if (ISWTCH .eq. 1) then
                    !                       THIS IS A FIRE MORTALITY, SET ISTATUS TO 5.
                    ISTATUS(IDEAD) = 5
                    TEMP = IOBJTP(ISNADD(NSNGS-ISNAG))
                 else if (ISWTCH .eq. 3) then
                    !                       THIS IS A WWPB MORTALITY, SET ISTATUS TO 90.
                    ISTATUS(IDEAD) = 90
                 else
                    !                       THIS IS NOT A FIRE MORTALITY, CALCULATE ISTATUS NORMALLY.
                    if (IOBJTP(ISNADD(NSNGS-ISNAG)) .eq. -1) then
                       ISTATUS(IDEAD) = 2
                    else
                       ISTATUS(IDEAD) = 1
                    end if
                 end if
              end if
              IYRCOD(IDEAD) = IYOFTD

              !              IF SNAG IS ADDED AT THE BEGINNING, THE YEAR OF DEATH
              !              OF THE SNAG MUST BE DETERMINED, AS WELL AS THE
              !              ORIGINAL INFORMATION

              if (ICYC .eq. 0) then
                 if (IMC(IS2F(ISNADD(NSNGS-ISNAG))) .eq. 7) then
                    IYRCOD(IDEAD) =  &
                         & IYRCOD(IDEAD)-MAX(IFIX(FINTM*.7),1)
                    ISTATUS(IDEAD) = 3
                 else if (IMC(IS2F(ISNADD(NSNGS-ISNAG))) .eq. 9) then
                    IYRCOD(IDEAD) =  &
                         & IYRCOD(IDEAD)-IFIX(FINTM+2)
                    ISTATUS(IDEAD) = 4
                 end if
                 call SVHABT(XHMOD)
                 if (ISTATUS(IDEAD) .eq. 4) then
                    HRATE2 = 2
                    Y = YHFHTS(ISNSP(IDEAD))
                 else
                    HRATE2 = 1
                    Y = YHFHTH(ISNSP(IDEAD))
                 end if
                 ITIDIF = IYEAR-IYRCOD(IDEAD)

                 if ( DEBUG ) then
                    write(JOSTND,1025) IDEAD, IYRCOD(IDEAD), ITIDIF, &
                         & ODIA(IDEAD), OLEN(IDEAD)
                 end if

                 SNGLEN(IDEAD) = OLEN(IDEAD)
                 if (ITIDIF .gt. 0) then
                    FACTOR =  &
                         & (1-0.0228*XHMOD*HRATE(ISNSP(IDEAD))*HRATE2)
                    if (FACTOR**(ITIDIF) .gt. 0.5) then
                       OLEN(IDEAD) = OLEN(IDEAD)/(FACTOR**(ITIDIF))
                    else
                       OLEN(IDEAD) = OLEN(IDEAD) / &
                            & (0.5*FACTOR**(MAX(0.0,(ITIDIF-Y))))
                    end if
                 end if

                 if ( DEBUG ) then
                    write (JOSTND,1026) OLEN(IDEAD), SNGLEN(IDEAD)
                 end if
                 CRNDIA(IDEAD) = CRNDIA(IDEAD)/(.90**ITIDIF)
                 !>>>              ELSE

                 !                    The following logic increments IYOFTD (yr of tree death),
                 !                    by one for each consecutively-added snag record, so that
                 !                    snag additions are spread equally across the years
                 !                    comprising the current growth period.

                 !                    Override IYOFTD assignment if SVSNAD is being called for
                 !                    fire mortality. In this case, assign all snags a year-of-death
                 !                    equal to the burn year.

                 !>>>              NOTE: logic to spread mortality across years in current growth
                 !>>>                    period is disabled, in order to match FFE logic.
                 !>>>
                 !>>>                 IF (IYOFTD+1.LT.IY(ITCYC+1).AND. ISWTCH.NE.4) THEN
                 !>>>                    IYOFTD=IYOFTD+1
                 !>>>                 ELSE
                 !>>>                    IYOFTD=IYEAR
                 !>>>                 ENDIF
                 !>>>                 IF ( ISWTCH .EQ. 1 ) THEN
                 !>>>                    IYOFTD=IYEAR
                 !>>>                 ENDIF
              end if
              IS2F(ISNADD(NSNGS-ISNAG)) = IDEAD
              IOBJTP(ISNADD(NSNGS-ISNAG)) = 2
              NDEAD = NDEAD+1
              ISNAG = ISNAG+1
           end if
        end do
     end if

     !        CREATE INDEX AND THE SCORE VECTOR FOR THE SNAG LIST

     IX = 0
     do IDEAD = 1,MXDEAD
        if (ISTATUS(IDEAD).gt.0) then

           !              AGES THE SNAG LIST

           call SVSNAGE(IYEAR,IDEAD,SNCRDI,SNCRTO,SNHT,SNDI)
           IX = IX+1
           INDEX(IX) = IDEAD
           SNASCO(IDEAD) = SNDI*SNDI*SNHT
        end if
     end do

     !        MODIFY ILYEAR TO SHOW THAT THESE SNAGS HAVE BEEN AGED.

     ILYEAR = IYEAR

     !        SORT SNASCO: ORIGINAL CODE USE IAPSRT, WHICH RETURNS INDEX TO AN
     !        INTEGER VECTOR SORTED IN ASCENDING VECTOR; SINCE RDPSRT GIVES AN
     !        INDEX TO A REAL VECTOR SORTED IN DESCENDING ORDER, THE RETURNED
     !        INDICES (INDEX) TO THE REAL VECTOR ARE REVERSED IN PLACE TO GIVE AN
     !        ASCENDING REAL VECTOR. THIS IS MAKES THE SMALLEST POSSIBLE CHANGE
     !        TO THE CODE. - DR/ESSA

     call RDPSRT(NDEAD,SNASCO,INDEX,.false.)
     if (NDEAD .gt. 1) then
        K = NDEAD
        J = INDEX(K)
        do I = 1, (NDEAD/2)
           INDEX(K) = INDEX(I)
           INDEX(I) = J
           K = K-1
           J = INDEX(K)
        end do
     end if

     if (DEBUG) then
        IX = 0
        do ISVOBJ = 1,NSVOBJ
           if (IOBJTP(ISVOBJ).eq.2) IX = IX+1
        end do
        write (JOSTND,*) 'BETWEENNDEAD=', NDEAD, &
             & 'DEADOBJECTS=',IX
     end if

     do ISVOBJ = 1,NSVOBJ
        if (IOBJTP(ISVOBJ) .eq. 2) then
           IBACK(IS2F(ISVOBJ)) = ISVOBJ
        end if
     end do

     do IDEAD = 1,MXDEAD
        if (NSNGS.gt.ISNAG .and. INDEX(IDEAD).ne.0) then
           if (ADDSCO(ISNADD(NSNGS-ISNAG)) .gt. &
                & SNASCO(INDEX(IDEAD))) then

              if (DEBUG) write (JOSTND,*) IX,NDEAD, &
                   & 'ISNADD(',NSNGS-ISNAG,')=', &
                   & ISNADD(NSNGS-ISNAG),'INDEX(',IDEAD, &
                   & ')=', INDEX(IDEAD), &
                   & 'IS2F(',ISNADD(NSNGS-ISNAG),')=', &
                   & IS2F(ISNADD(NSNGS-ISNAG)), &
                   & 'ADDSCO=',ADDSCO(ISNADD(NSNGS-ISNAG)), &
                   & 'SNASCO=',SNASCO(INDEX(IDEAD))

              ISNSP(INDEX(IDEAD)) = ISP(IS2F(ISNADD(NSNGS-ISNAG)))
              if (IOBJTP(ISNADD(NSNGS-ISNAG)) .eq. -3) then

                 !**                     PRINT *, 'IOBJTP IS -3'

                 if (ISTATUS(INDEX(IDEAD)).eq.0) then
                    NDEAD = NDEAD+1
                    if (DEBUG) write (JOSTND,*) 'ADDEDTO', NDEAD
                 end if
                 ISTATUS(INDEX(IDEAD)) = 1
                 call SVRANN(X)
                 FALLDIR(INDEX(IDEAD)) = IFIX(360. *X +.5)
                 CRNRTO(INDEX(IDEAD)) = 99.
                 OLEN(INDEX(IDEAD)) = HT(IS2F(ISNADD(NSNGS-ISNAG)))* &
                      & ICR(IS2F(ISNADD(NSNGS-ISNAG)))*.01
                 SNGLEN(INDEX(IDEAD)) = OLEN(INDEX(IDEAD))
                 CHPOS = HT(IS2F(ISNADD(NSNGS-ISNAG)))- &
                      & OLEN(INDEX(IDEAD))

                 !                    0.0174532778 IS APPROXIMATELY PI/180.

                 XSLOC(ISNADD(NSNGS-ISNAG)) =  &
                      & XSLOC(ISNADD(NSNGS-ISNAG)) &
                      & +CHPOS*SIN(FALLDIR(INDEX(IDEAD))*0.0174532778)
                 YSLOC(ISNADD(NSNGS-ISNAG)) =  &
                      & YSLOC(ISNADD(NSNGS-ISNAG)) &
                      & +CHPOS*COS(FALLDIR(INDEX(IDEAD))*0.0174532778)
                 ODIA(INDEX(IDEAD)) =  &
                      & DBH(IS2F(ISNADD(NSNGS-ISNAG)))* &
                      & OLEN(INDEX(IDEAD)) &
                      & /HT(IS2F(ISNADD(NSNGS-ISNAG)))
                 SNGCNWT(INDEX(IDEAD),0:3) =  &
                      & CROWNW(IS2F(ISNADD(NSNGS-ISNAG)),0:3)
                 SNGDIA(INDEX(IDEAD)) = ODIA(INDEX(IDEAD))
                 CRNDIA(INDEX(IDEAD)) =  &
                      & CRWDTH(IS2F(ISNADD(NSNGS-ISNAG)))

                 OIDTRE(INDEX(IDEAD)) =  &
                      & IDTREE(IS2F(ISNADD(NSNGS-ISNAG)))

              else
                 OLEN(INDEX(IDEAD)) = HT(IS2F(ISNADD(NSNGS-ISNAG)))
                 SNGLEN(INDEX(IDEAD)) = OLEN(INDEX(IDEAD))
                 ODIA(INDEX(IDEAD)) = DBH(IS2F(ISNADD(NSNGS-ISNAG)))
                 SNGCNWT(INDEX(IDEAD),0:3) =  &
                      & CROWNW(IS2F(ISNADD(NSNGS-ISNAG)),0:3)
                 SNGDIA(INDEX(IDEAD)) = ODIA(INDEX(IDEAD))
                 CRNRTO(INDEX(IDEAD)) = ICR(IS2F(ISNADD(NSNGS-ISNAG)))

                 CRNDIA(INDEX(IDEAD)) =  &
                      & CRWDTH(IS2F(ISNADD(NSNGS-ISNAG)))

                 OIDTRE(INDEX(IDEAD)) =  &
                      & IDTREE(IS2F(ISNADD(NSNGS-ISNAG)))

                 if (IOBJTP(ISNADD(NSNGS-ISNAG)) .eq. -2) then
                    call SVRANN(X)
                    FALLDIR(INDEX(IDEAD)) = IFIX(360. *X +.5)
                 else
                    FALLDIR(INDEX(IDEAD)) = -1
                 end if
                 if (ISTATUS(INDEX(IDEAD)).eq.0) then
                    NDEAD = NDEAD+1
                    if (DEBUG) write (JOSTND,*) 'ADDEDTO', NDEAD
                 end if
                 if (IOBJTP(ISNADD(NSNGS-ISNAG)) .eq. -1) then
                    ISTATUS(INDEX(IDEAD)) = 2
                 else
                    ISTATUS(INDEX(IDEAD)) = 1
                 end if
                 IIX = 0
                 if (DEBUG) then
                    do ISVOBJ = 1,NSVOBJ
                       if (IOBJTP(ISVOBJ).eq.2) IIX = IIX+1
                    end do
                    if (DEBUG) write (JOSTND,*) 'DEADOBJECTS=',IIX
                 end if
              end if
              IS2F(ISNADD(NSNGS-ISNAG)) = INDEX(IDEAD)
              IOBJTP(ISNADD(NSNGS-ISNAG)) = 2
              if (IBACK(INDEX(IDEAD)).ge.1 .and. &
                   & IBACK(INDEX(IDEAD)).le.MXSVOB) &
                   & IOBJTP(IBACK(INDEX(IDEAD))) = 0
              ISNAG = ISNAG+1
              IYRCOD(INDEX(IDEAD)) = IYOFTD

              !              IF SNAG IS ADDED AT THE BEGINNING, THE YEAR OF DEATH
              !              OF THE SNAG MUST BE DETERMINED, AS WELL AS THE
              !              ORIGINAL INFORMATION

              if (ICYC .eq. 0) then
                 if (IMC(IS2F(ISNADD(NSNGS-ISNAG))) .eq. 7) then
                    IYRCOD(INDEX(IDEAD)) =  &
                         & IYRCOD(INDEX(IDEAD))-MAX(IFIX(FINTM*.7),1)
                    ISTATUS(INDEX(IDEAD)) = 3
                 else if (IMC(IS2F(ISNADD(NSNGS-ISNAG))) .eq. 9) then
                    IYRCOD(INDEX(IDEAD)) =  &
                         & IYRCOD(INDEX(IDEAD))-IFIX(FINTM+2)
                    ISTATUS(INDEX(IDEAD)) = 4
                 end if
                 call SVHABT(XHMOD)
                 if (ISTATUS(INDEX(IDEAD)) .eq. 4) then
                    HRATE2 = 2
                    Y = YHFHTS(ISNSP(INDEX(IDEAD)))
                 else
                    HRATE2 = 1
                    Y = YHFHTH(ISNSP(INDEX(IDEAD)))
                 end if
                 ITIDIF = IYEAR-IYRCOD(INDEX(IDEAD))

                 if ( DEBUG ) then
                    write(JOSTND,1025) IDEAD, IYRCOD(INDEX(IDEAD)), &
                         & ITIDIF, ODIA(INDEX(IDEAD)), &
                         & OLEN(INDEX(IDEAD))
                 end if

                 SNGLEN(INDEX(IDEAD)) = OLEN(INDEX(IDEAD))
                 if (ITIDIF .gt. 0) then
                    FACTOR = (1-0.0228*XHMOD* &
                         & HRATE(ISNSP(INDEX(IDEAD)))*HRATE2)
                    if (FACTOR**ITIDIF .gt. 0.5) then
                       OLEN(INDEX(IDEAD)) =  &
                            & OLEN(INDEX(IDEAD))/(FACTOR**(ITIDIF))
                    else
                       OLEN(INDEX(IDEAD)) = OLEN(INDEX(IDEAD))/ &
                            & (0.5*FACTOR**(MAX(0.0,(ITIDIF-Y))))
                    end if
                 end if
                 if ( DEBUG ) then
                    write (JOSTND,1026) OLEN(INDEX(IDEAD)), &
                         & SNGLEN(INDEX(IDEAD))
                 end if
                 CRNDIA(INDEX(IDEAD)) =  &
                      & CRNDIA(INDEX(IDEAD))/(.90**ITIDIF)
                 !>>>              ELSE

                 !                    The following logic increments IYOFTD (yr of tree death),
                 !                    by one for each consecutively-added snag record, so that
                 !                    snag additions are spread equally across the years
                 !                    comprising the current growth period.

                 !                    Override IYOFTD assignment if SVSNAD is being called for
                 !                    fire mortality. In this case, assign all snags a year-of-death
                 !                    equal to the burn year.

                 !>>>              NOTE: logic to spread mortality across years in current growth
                 !>>>                    period is disabled, in order to match FFE logic.
                 !>>>
                 !>>>                 IF (IYOFTD+1.LT.IY(ITCYC+1) .AND. ISWTCH.NE.4) THEN
                 !>>>                    IYOFTD=IYOFTD+1
                 !>>>                 ELSE
                 !>>>                    IYOFTD=IYEAR
                 !>>>                 ENDIF
                 !>>>                 IF ( ISWTCH .EQ. 1 ) THEN
                 !>>>                    IYOFTD=IYEAR
                 !>>>                 ENDIF
              end if
           end if
        end if
     end do
  end if

  if (DEBUG) then
     IX = 0
     do ISVOBJ = 1,NSVOBJ
        if (IOBJTP(ISVOBJ).eq.2) IX = IX+1
     end do
     write (JOSTND,1030) NDEAD, NSVOBJ, IX
     1030 format (/ ' ',T5,'AFTER SNAG ADDITIONS:', / , &
          & ' ',T5,'   NDEAD=',I4,', SVS TOTAL OBJECTS=',I6, &
          & ', SVS SNAG OBJECTS=',I5,':',/)
     do ISVOBJ = 1,NSVOBJ
        if (IOBJTP(ISVOBJ).eq.2) &
             & write (JOSTND,*) '   IS2F(',ISVOBJ,')=',IS2F(ISVOBJ)
     end do
  end if

  !     CLEAN UP OBJECTS IF NOT CALLED BY FIRE MODEL.

  if ( ISWTCH.ne.1 ) then
     IPUT = 0
     do ISVOBJ = 1,NSVOBJ
        if (IOBJTP(ISVOBJ).le.0 .or. IS2F(ISVOBJ).eq.0) then
           if (IPUT.eq.0) IPUT = ISVOBJ
        else
           if (IPUT.gt.0 .and. IPUT.lt.ISVOBJ) then
              IS2F(IPUT) = IS2F(ISVOBJ)
              XSLOC(IPUT) = XSLOC(ISVOBJ)
              YSLOC(IPUT) = YSLOC(ISVOBJ)
              IOBJTP(IPUT) = IOBJTP(ISVOBJ)
              IPUT = IPUT+1
           end if
        end if
     end do
     if (IPUT.gt.0) NSVOBJ = IPUT-1

     if (DEBUG) then
        IX = 0
        do ISVOBJ = 1,NSVOBJ
           if (IOBJTP(ISVOBJ).eq.2) IX = IX+1
        end do
        write (JOSTND,*) 'SVSNADOUTNDEAD=', NDEAD, &
             & 'DEADOBJECTS=',IX

        do ISVOBJ = 1,NSVOBJ
           if (IOBJTP(ISVOBJ).eq.2) &
                & write (JOSTND,*) 'IS2F(',ISVOBJ,')=',IS2F(ISVOBJ)
        end do
     end if
  end if

  !     Compute, by source tree record and year of death:
  !        1) Initial snag records created.
  !        2) Snag records still standing.

  if (ICYC .eq. 0) then
     do IDEAD = 1,NDEAD
        if ( IYRCOD(IDEAD) .le. IYEAR ) then
           XPROB = 0.0
           do J = 1,NDEAD
              if ( OIDTRE(J) .eq. OIDTRE(IDEAD) .and. &
                   & IYRCOD(J) .eq. IYRCOD(IDEAD) ) XPROB = XPROB+1
           end do
           SPROBS(IDEAD,1) = XPROB
           SPROBS(IDEAD,3) = 1.0
        end if
     end do
  else
     do IDEAD = 1,NDEAD
        if ( IYRCOD(IDEAD) .ge. IYEAR ) then
           XPROB = 0.0
           do J = 1,NDEAD
              if ( OIDTRE(J) .eq. OIDTRE(IDEAD) .and. &
                   & IYRCOD(J) .eq. IYRCOD(IDEAD) ) XPROB = XPROB+1
           end do
           SPROBS(IDEAD,1) = XPROB
           SPROBS(IDEAD,3) = 1.0
        end if
     end do
  end if

  do IDEAD = 1,NDEAD
     XPROB = 0.0
     do J = 1,NDEAD
        if ( OIDTRE(J)  .eq. OIDTRE(IDEAD) .and. &
             & IYRCOD(J)  .eq. IYRCOD(IDEAD) .and. &
             & FALLDIR(J) .eq. -1 ) XPROB = XPROB+1
     end do
     SPROBS(IDEAD,2) = XPROB
  end do

  if (DEBUG) then
     write (JOSTND,1040) ICYC, IYEAR, &
          & NSNGS, NDEAD, NSVOBJ
     1040 format (' ','LEAVING SVSNAD, ICYC=',I2, &
          & ', IYEAR=',I4,':', / , &
          & ' ',T5,'NSNGS(add)=',I4,', NDEAD=',I4,', NSVOBJ=',I5,//, &
          & ' ',T5,'SNAG LIST AFTER ADDING CUT/MORTALITY:',//, &
          & ' ',T5,'  I   IDTREE  SPP ODIA OLEN IYRCOD STATUS ', &
          & 'FALLDIR OPROB STDNG',/, &
          & ' ',T5,'---- -------- --- ---- ---- ------ ------ ', &
          & '------- ----- -----' )
     !                       XXXX XXXXXXXX XXX XX.X XXX. XXXXXX XXXXXX
     !                       XXXXXX. XXXX. XXXX.
     do I = 1,NDEAD
        write(JOSTND,1050) I, OIDTRE(I), ISNSP(I), ODIA(I), OLEN(I), &
             & IYRCOD(I), ISTATUS(I), &
             & FALLDIR(I), SPROBS(I,1), SPROBS(I,2)
        1050 format(' ',T5,I4,1X,I8,1X,I3,1X,F4.1,1X,F4.0,1X, &
             & I6,1X,I6,1X,F7.0,1X,F5.0,1X,F5.0)
        300 continue
     end do
  end if

  return
end subroutine SVSNAD
