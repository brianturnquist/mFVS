!=================================!
!   Stand visualization: SVRMOV   !
!=================================!
! BASE $Id$
subroutine SVRMOV (REMOVE,ISWTCH,SSNG,DSNG,CTCRWN,ICURYEAR)
  implicit none

  !     STAND VISUALIZATION GENERATION
  !     N.L.CROOKSTON -- RMRS MOSCOW -- NOVEMBER 1998
  !     J.J.MARCHINEK -- RMRS MOSCOW -- JANUARY 1999
  !     A.H.DALLMANN  -- RMRS MOSCOW -- JANUARY 2000
  !     D. ROBINSON   -- ESSA        -- MAY 2005

  !     REMOVES TREES FOR EITHER SVMORT OR SVCUTS

  !     UPDATES THE VISUALIZATION FOR HARVEST, WRITES THE POST
  !     CUTTING REPORT, AND CLEANS UP THE LIST OF REFERENCES
  !     TO "REMOVED" TREE RECORDS.

  !     CALLED FROM SVCUTS AND SVMORT.
  !     INPUT:
  !     REMOVE  = VECTOR OF MORTALITY OR REMOVAL TREES/ACRE
  !     ISWTCH = 1 IF SVMORT CALLED THIS SUBROUTINE, FIRE-CAUSED MORTALITY
  !            = 2 IF SVMORT CALLED THIS SUBROUTINE, NORMAL MORTALITY
  !            = 3 IF SVMORT CALLED THIS SUBROUTINE, WESTWIDE PINE BEETLE MORT
  !            = 4 IF SVCUTS CALLED THIS SUBROUTINE

  ! Common blocks
  include 'PRGPRM.F77'
  include 'ARRAYS.F77'
  include 'CONTRL.F77'
  include 'SVDATA.F77'
  include 'PLOT.F77'
  include 'SVDEAD.F77'
  include 'METRIC.F77'

  !     WPP     - SUM OF REMOVE PER PLOT
  !     PPP     - SUM OF PROB PER PLOT
  !     IPLCNT  - SUM OF OBJECT TREES PER PLOT
  !     NOBTS   - SUM OF OBJECT TREES PER TREE RECORD
  !     NTORMV  - SUM OF NUMBER TO REMOVE PER TREE RECORD
  !     RMVSUM  - INCREASING SUM OF REMOVE
  !     REMOVE  - TREES PER ACRE OF MORTALITY/CUT
  !     NTRPLT  - A LIST OF INDICES OF THE TREES ON THE CURRENT PLOT
  !     NTOADD  - SUM OF NUMBER TO ADD PER TREE RECORD
  !     FRACAD  - FRACTIONAL TREES PER ACRE OF NEW TREES (ie <1)
  !     SCORE   - A VECTOR BASED ON ITRE, AND DBH
  !     ISNADD  - VECTOR OF SNAGS THAT NEED TO BE ADDED

  ! Variables
  real    :: WPP(MAXPLT)
  real    :: PPP(MAXPLT)
  integer :: IPLCNT(MAXPLT)

  integer :: NOBTS(MAXTRE)
  integer :: NTORMV(MAXTRE)
  real    :: RMVSUM(MAXTRE)
  real    :: REMOVE(MAXTRE)
  integer :: NTRPLT(MAXTRE)
  integer :: NTOADD(MAXTRE)
  real    :: FRACAD(MAXTRE)
  real    :: SCORE(MAXTRE)

  integer :: ISNADD(MXSVOB)

  integer :: ICURYEAR,ISWTCH,II,I,ITOTAL,ISVOBJ,IPLT,N,I1,NTA,ITOADD,IADD,K,IDEL,IBOT,ITOP,INUM,IPT, &
       & NCKS,IT,KODE,IX,ISI,J,ITEMP
  real    :: RMV,X,WRMV,TOADD,XINT,START,DIAI,Y,DIAOBJ,XS,YS,DIFF,XM

  !     SSNG  = SNAGS/ACRE CREATED BY CUTTING, BUT LEFT STANDING
  !     DSNG  = SNAGS/ACRE CREATED BY CUTTING, DOWN LEFT IN STAND
  !     CTCRWN= TREES/ACRE REMOVED BUT WHOSE CROWNS ARE STILL IN THE
  !             STAND.

  real    :: SSNG(*),DSNG(*),CTCRWN(*)
  character(len=3) :: SBLANK
  logical :: DEBUG

  call DBCHK (DEBUG,'SVRMOV',6,ICYC)

  if (DEBUG) write (JOSTND,*) 'IN SVRMOV: JSVOUT=',JSVOUT, &
       & ' ISWTCH=',ISWTCH

  if (JSVOUT.eq.0) return

  !     INITIALIZE THE PLOT-BASED VECTORS

  do II = 1, MAXPLT
     WPP(II) = 0
     PPP(II) = 0
     IPLCNT(II) = 0
  end do

  !     INITIALIZE THE TREE-RECORD BASED VECTORS

  do II = 1,ITRN
     SCORE(II) = FLOAT(ISP(II))*1000+DBH(II)
     I = ITRE(II)
     if (IMETRIC.eq.0) then
        WPP(I) = REMOVE(II) + WPP(I)
        PPP(I) = PROB(II) + PPP(I)
     else
        WPP(I) = (REMOVE(II)/ACRtoHA) + WPP(I)
        PPP(I) = (PROB(II)/ACRtoHA) + PPP(I)
     end if
  end do

  !     CLEAR OUT THE ISNADD VECTOR

  do I = 1, MXSVOB
     ISNADD(I) = 0
  end do

  !     CLEAR OUT OTHER VECTORS

  do I = 1,MAXTRE
     NTOADD(I) = 0
     FRACAD(I) = 0
     NOBTS(I) = 0
     NTORMV(I) = 0
     RMVSUM(I) = 0
  end do

  !     LOAD NUMBER OF OBJECTS PER TREE RECORD IN NOBTS
  !     LOOPS THROUGH THE TOTAL NUMBER OF DRAWN OBJECTS (NSVOBJ) AND ADDS
  !     ONE TO THE NOBTS ARRAY

  ITOTAL = 0
  do ISVOBJ = 1,NSVOBJ
     if (IOBJTP(ISVOBJ).eq.1 .and. IS2F(ISVOBJ).gt.0) then

        !           THIS IS A FVS TREE OBJECT WITH A CORRESPONDING TREE RECORD

        NOBTS(IS2F(ISVOBJ)) = NOBTS(IS2F(ISVOBJ)) + 1
        ITOTAL = ITOTAL + 1
     end if
  end do

  !     ADD INTEGER VALUE TREE RECORDS

  do I = 1,ITRN
     if (ISWTCH.eq.4) then
        RMV = 0.
     else
        RMV = REMOVE(I)
     end if

     if (IMETRIC.eq.0) then
        XM = PROB(I)-RMV
     else
        XM = (PROB(I) - RMV)/ACRtoHA
     end if

     if (XM .ge. 0) then

        !         THERE ARE A SUFFICENT NUMBER OF TREES IN PROB TO HANDLE THE
        !         DESIRED REMOVAL.

        if (XM+1. .gt. 0.) then

           !           THERE IS AT LEAST ONE FULL TREE TO ADD TO NTOADD(I)

           NTOADD(I) = IFIX(XM)
        end if

        !         ADD THE FRACTIONAL PORTION LEFT OVER TO FRACAD(I)

        FRACAD(I) = XM - FLOAT(NTOADD(I))
        if (FRACAD(I) .lt. 0.00001) FRACAD(I) = 0.0
     else
        FRACAD(I) = 0.
     end if
  end do

  !     DEBUG ROUTINE TO SEE HOW MANY TREES WE NOW HAVE.

  if (DEBUG) then
     X = 0
     do I = 1,ITRN
        X = NTOADD(I)+X
     end do
     write (JOSTND,*) 'NONFRACTIONAL TOTAL TO HAVE=', X
  end if

  !     USING THE FRACTIONAL NUMBER OF TREES CALCULATE AND LOAD
  !     THE NUMBER OF TREES TO ADD PER TREE RECORD PER PLOT

  do IPLT = 1,ISVINV
     if (DEBUG) write (JOSTND,*) '-------ADD IPLT=',IPLT

     !       INITALIZE RMVSUM

     do I = 1,ITRN
        RMVSUM(I) = 0
     end do

     !       FIND THE TREES ON THIS PLOT "IPLT" AND STORES A LINK NTRPLT(N) TO
     !       THE CORRESPONDING TREE IN THE OVERALL TREE-RECORDS LIST "ITRE".
     !       N= THE NUMBER OF TREES ON THIS PLOT

     N = 0
     do I = 1,ITRN
        if (ITRE(I).eq.IPLT) then
           N = N+1
           NTRPLT(N) = I
        end if
     end do

     !       IF THERE ARE NO TREES ON THIS PLOT GO TO NEXT PLOT

     if (N.eq.0) goto 25

     !       SORT NTRPLT ON DESCENDING SCORE (WHERE SCORE IS BASED ON AVERAGE
     !       TREE DIAMETER)

     call RDPSRT(N,SCORE,NTRPLT,.false.)

     !       LOAD RMVSUM WITH FRACTIONAL NEW TREES IN ORDER BY NTRPLT

     I1 = NTRPLT(1)
     RMVSUM(I1) = FRACAD(I1)
     if (DEBUG) write (JOSTND,*) 'RMVSUM(',I1,')=',RMVSUM(I1)
     if (N.gt.1) then
        do II = 2,N
           I = NTRPLT(II)
           RMVSUM(I) = FRACAD(I)+RMVSUM(I1)
           I1 = I
           if (DEBUG) write (JOSTND,*) 'RMVSUM(',I,')=',RMVSUM(I)
        end do
     end if

     !       IF THERE IS NO ACCUMULATED FRACTIONAL REMOVAL, GO TO NEXT PLOT.

     if (RMVSUM(NTRPLT(N)).eq.0) goto 25

     !       CALCULATE THE NUMBER OF TREES ADDED FROM THIS PLOT

     NTA = 0
     do I = 1,N
        NTA = NTOADD(NTRPLT(I)) +NTA
     end do

     !       SETUP INFORMATION TO START DECIDING WHAT TREES SHOULD BE ADDED

     if (ISWTCH.eq.4) then
        WRMV = 0
     else
        WRMV = WPP(IPLT)
     end if
     TOADD = PPP(IPLT) - WRMV  - FLOAT(NTA)
     if (DEBUG) write (JOSTND,*) 'N=',N,'TOADD=',TOADD, &
          & 'PPP(IPLT)=',PPP(IPLT),'NTA=', NTA
     if (TOADD.lt. .001) goto 25 ! TOO FEW TO BOTHER WITH.
     ITOADD = IFIX(TOADD+.5)
     XINT = RMVSUM(NTRPLT(N))/TOADD
     call SVRANN(X)
     START = XINT * X

     if (DEBUG) write (JOSTND,*) 'ITOADD=', &
          & ITOADD,' XINT=',XINT,' START=',START
     X = 0
     IADD = 0

     !       LOOP TO FIND WHICH TREES SHOULD BE ADDED

     do II = 1,N
        I = NTRPLT(II)
        if (DEBUG) write(JOSTND,*) 'START=',START, &
             & ' RMVSUM(',I,')=', RMVSUM(I),' X=',X
        if (START.gt.X .and. START.lt.RMVSUM(I)) then
           START = START+XINT
           NTOADD(I) = NTOADD(I)+1
           IADD = IADD + 1
           X = RMVSUM(I)
           if (DEBUG) write (JOSTND,*) 'IADD=',IADD, &
                & ' NTOADD(', I,')=', &
                & NTOADD(I), ' FRACAD(I)=',FRACAD(I)
           if (IADD.ge.ITOADD) goto 25
        else
           X = RMVSUM(I)
        end if
     end do
     25 continue
  end do

  !     FINDING OUT HOW MUCH NTORMV NEEDS TO BE

  do I = 1,ITRN
     NTORMV(I) = NOBTS(I) - NTOADD(I)
  end do

  !     USED FOR DEBUG PURPOSES

  if (DEBUG) then
     X = 0
     do I = 1,ITRN
        X = NTOADD(I)+X
        if (NTOADD(I) .gt. 0) then
           write (JOSTND,*) 'NTOADD(',I,')=', NTOADD(I)
        end if
     end do
     write (JOSTND,*) 'TOTAL NEEDED=', X
     do I = 1, ITRN
        if (NTORMV(I).lt.-1) then
           if (NTORMV(I).lt.-2) then
              SBLANK = ' ++'
           else
              SBLANK = ' **'
           end if
        else
           SBLANK = ' '
        end if
        write (JOSTND,*) 'I=',I,' NOBTS(I)=', NOBTS(I), &
             & ' NTOADD(I)=',NTOADD(I), ' NTORMV(I)=', NTORMV(I), &
             & ' DBH(I)=', DBH(I), SBLANK
     end do
  end if

  !     END OF DEBUG

  !     IF NTORMV IS NEGATIVE, THEN THERE IS A DIFFICIENT FOR THAT
  !     TREE RECORD (IE, TOO MANY ALREADY TAKEN OUT).  THIS FINDS
  !     ANOTHER SUITABLE TREE FOR THE DIFFICIENT TREE RECORD.

  !     RERUN THE SORT...TRY TO KEEP TREES REPRESENTING LIKE TREES.

  if (ITRN .gt. 0) call RDPSRT(ITRN,SCORE,NTRPLT,.true.)

  do K = 1,ITRN
     I = NTRPLT(K)
     if (NTORMV(I).lt.0) then
        IDEL = -1
        IBOT = 0
        ITOP = 0
        40 continue

        !            IF (DEBUG) WRITE (JOSTND,*) 'I=',I,' ITOP=',ITOP,
        !     >                 ' IBOT=',IBOT,' IDEL=',IDEL

        if (ITOP+IBOT.eq.2) goto 50
        if (IDEL.lt.0) then

           !              LOOK AT THE TREES "ABOVE" THIS TREE, IF ANY

           IDEL = -IDEL
           if (IDEL+K.gt.ITRN) ITOP = 1
           if (ITOP.eq.1) goto 40
        else

           !              LOOK AT THE TREE "BELOW" THIS TREE, IF ANY.

           IDEL = -(IDEL+1)
           if (IDEL+K.lt.1) IBOT = 1
           if (IBOT.eq.1) goto 40
        end if

        !           SEARCH FOR A TREE THAT REPRESENT THIS TREE FOLLOWING THE
        !           SORTED ORDER.  II NOW REFERENCES A CANIDATE TREE.

        II = NTRPLT(K+IDEL)

        !            II = I+IDEL
        !            IF (DEBUG) WRITE (JOSTND,*) 'I=',I,' IDEL=',IDEL,
        !     >                 ' II=',II

        !           RMV IS THE NUMBER OF TREES TO SUBTRACT FROM PROB...
        !           NEEDED HERE TO INSURE THAT THE CANIDATE TREE HAS A
        !           SURPLUS OF OBJECTS...

        if (ISWTCH.eq.4) then
           RMV = 0.
        else
           RMV = REMOVE(II)
        end if

        !         CAN'T USE THE CANIDATE UNLESS IT HAS A SURPLUS OF OBJECTS

        if (NTORMV(II).lt.1) goto 40

        !         IT MUST BE THE THE SAME SPECIES

        if (ISP(II) .ne. ISP(I)) goto 40

        !         HAVE A DIAMETER WITHIN 25%

        if (MIN(DBH(II),DBH(I)).lt. &
             & MAX(DBH(II),DBH(I))*.75) goto 40

        if (DEBUG) write (JOSTND,45) &
             & II,DBH(II),ISP(II),I,DBH(I),ISP(I)
        45 format (' ASSIGN TREE',I4,' DBH=',F7.2,' ISP=', &
             & I3,' TO REPRESENT TREE',I4,' DBH=',F7.2,' ISP=',I3)

        !         OK, WE CAN USE THE TREE.

        NTORMV(II) = NTORMV(II)-1
        NTORMV(I ) = NTORMV(I) +1

        !         IF WE STILL HAVE A DEFICIT, KEEP WORKING!

        if (NTORMV(I).lt.0) goto 40
        goto 100

        50 continue
        if (DEBUG) write (JOSTND,*) 'ADD TREE I=',I,' DBH=',DBH(I), &
             & ' ISP(I)=',ISP(I),' PROB=',PROB(I)

        !         IF NTORMV(I) IS STILL LESS THAN 0, AND WE ARE OUT
        !         OF TREE (COULD NOT FIND A CANIDATE), THEN ADD THE TREE.

        if (NTORMV(I).lt.0 .and. ISWTCH.eq.4) then
           INUM = -NTORMV(I)
           NTORMV(I) = 0
           IPT = ITRE(I)

           if (IMETRIC.eq.0) then
              DIAI = DBH(I) * .04166667
           else
              DIAI = DBH(I) * INtoM * 0.5
           end if

           NCKS = 0
           do IT = 1,INUM
              NCKS = 0
              60 continue
              call SVGTPT(IPLGEM,X1R1S(IPT),X2R2S(IPT), &
                   & Y1A1S(IPT),Y2A2S(IPT),X,Y,IMETRIC)

              !             IF THE OBJECT OVERLAPS, GET A NEW POINT.
              !             (DON'T CHECK FOR OVERLAP WITH CWD OBJECTS)

              if (NSVOBJ.eq.0) goto 70
              do ISVOBJ = 1,NSVOBJ
                 if (IOBJTP(ISVOBJ).eq.4) cycle
                 if (IS2F(ISVOBJ).gt.0) then
                    if (ITRE(IS2F(ISVOBJ)).eq.ITRE(I)) then

                       if (IMETRIC.eq.0) then
                          DIAOBJ = DBH(IS2F(ISVOBJ)) * .04166667
                       else
                          DIAOBJ = DBH(IS2F(ISVOBJ)) * INtoM * 0.5
                       end if

                       call SVOBOL (1,X,Y,DIAI,0., &
                            & IOBJTP(ISVOBJ),XSLOC(ISVOBJ), &
                            & YSLOC(ISVOBJ),DIAOBJ,0., &
                            & XS,YS,KODE)

                       if (KODE.eq.1) then
                          NCKS = NCKS+1
                          if (NCKS.gt.40) goto 70
                          goto 60
                       end if
                    end if
                 end if
              end do
              70 continue

              !             PLACE THE TREE AT X,Y

              NSVOBJ = NSVOBJ+1
              if (NSVOBJ.gt.MXSVOB) then
                 write (JOSTND,72)
                 72 format (//'********   STAND VISUALIZATION ', &
                      & 'ERROR: TOO MANY OBJECTS')
                 NSVOBJ = NSVOBJ -1
                 call RCDSET (2,.true.)
              end if
              XSLOC(NSVOBJ) = X
              YSLOC(NSVOBJ) = Y
              IS2F(NSVOBJ) = I
              IOBJTP(NSVOBJ) = 1
           end do
        end if
     end if
     100 continue
  end do

  !     LOAD SCORE NOW WITH A RANDOM NUMBER IF THERE IS SOMETHING FOR
  !     THAT TREE TO BE REMOVED.

  do II = 1,ITRN
     if (NTORMV(II) .gt. 0) then
        call SVRANN(X)
        SCORE(II) = X
     else
        SCORE(II) = 0.
     end if
  end do

  !     NOW REMOVE THE TREES THAT NEED TO BE REMOVED.

  IX = 0
  do ISVOBJ = 1,NSVOBJ
     if ( IOBJTP(ISVOBJ) .eq. 4 ) cycle
     ISI = IS2F(ISVOBJ)
     if (ISI.gt.0 ) then
        if (NTORMV(ISI).gt.0) then
           if (DEBUG) write (JOSTND,*) 'REMOVE(',ISI,')=', &
                & REMOVE(ISI),' DBH=',DBH(ISI),' ISP=',ISP(ISI), &
                & 'NTORMV(',ISI,')=', NTORMV(ISI), &
                & 'TOTAL REMOVED=', IX
           if (IOBJTP(ISVOBJ).eq.1 .and. NTORMV(ISI).gt.0) then
              if (ISWTCH .eq. 4) then
                 if (DEBUG) write (JOSTND,*) 'SNAG ISI=',ISI, &
                      & 'SSNG=', SSNG(ISI),'DSNG=', DSNG(ISI), &
                      & 'CTCRWN=', CTCRWN(ISI)
                 if (SCORE(ISI) .le. CTCRWN(ISI)-DSNG(ISI)) then
                    IOBJTP(ISVOBJ) = 3
                    NTORMV(ISI) = NTORMV(ISI) - 1
                 else if (SCORE(ISI) .le. CTCRWN(ISI)) then
                    IOBJTP(ISVOBJ) = -2
                    NTORMV(ISI) = NTORMV(ISI) - 1
                    IX = IX+1
                    ISNADD(IX) = ISVOBJ
                 else if (SCORE(ISI) .le. SSNG(ISI)+CTCRWN(ISI)) then
                    IOBJTP(ISVOBJ) = -1
                    NTORMV(ISI) = NTORMV(ISI) - 1
                    IX = IX+1
                    ISNADD(IX) = ISVOBJ
                 else
                    NTORMV(ISI) = NTORMV(ISI) - 1
                    IOBJTP(ISVOBJ) = 3
                 end if
                 SCORE(ISI) = SCORE(ISI)+1
              else
                 IOBJTP(ISVOBJ) = 0
                 NTORMV(ISI) = NTORMV(ISI) - 1
                 IX = IX+1
                 ISNADD(IX) = ISVOBJ
              end if
           end if
        end if
     end if
  end do

  if (IX.gt.0) then
     call SVSNAD(ICURYEAR,ISNADD,IX,ISWTCH)
  end if

  !     DEBUG TO FIND HOW MANY TREES NEED REMOVED, AND HOW MANY
  !     WERE NOT REMOVED.

  if (DEBUG) then
     K = 0
     do I = 1,ITRN
        K = NTORMV(I)+K
     end do
     write (JOSTND,*) 'TOTAL REMOVED=',IX,' TOTAL LEFT=',K, &
          & ' NEG NUM=TOO FEW OBJECTS, POS=TOO MANY OBJS'
  end if

  !     IF DEBUG...

  if (DEBUG) then
     do J = 1,ITRN
        ITEMP = 0
        do ISVOBJ = 1,NSVOBJ
           II = IS2F(ISVOBJ)
           if (II.eq.J .and. IOBJTP(ISVOBJ).eq.1) ITEMP = ITEMP+1
        end do
        if (ISWTCH.eq.2) then
           RMV = REMOVE(J)
        else
           RMV = 0
        end if
        DIFF = (PROB(J)-RMV)-ITEMP
        SBLANK = ' '
        if (ABS(DIFF).gt.1) then
           if (ABS(DIFF).gt.2) then
              SBLANK = ' ++'
           else
              SBLANK = ' **'
           end if
        end if
        write (JOSTND,105) J,PROB(J),DBH(J),RMV,ITEMP,DIFF,SBLANK
        105 format (' J=',I5,' PROB=',F8.4,' DBH=',F7.2, &
             & ' REMOVE=',F8.4,' OBJECTL=',I3,' DIFF=',F8.3,A)
     end do
     K = 0
     if (ISWTCH.eq.2) K = 1
     call SVCDBH(REMOVE,K)
  end if

  return
end subroutine SVRMOV
