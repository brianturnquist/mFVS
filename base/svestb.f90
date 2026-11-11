!=================================!
!   Stand visualization: SVESTB   !
!=================================!
! BASE $Id$
subroutine SVESTB (I1)
  implicit none

  !     STAND VISUALIZATION GENERATION
  !     N.L.CROOKSTON -- RMRS MOSCOW -- NOVEMBER 1998
  !     J.J.MARCHINEK -- RMRS MOSCOW -- MARCH 1999
  !     D. ROBINSON   -- ESSA        -- MAY 2005

  !     I1= 0 IS A CALL FROM SVSTART
  !         1 IS A CALL FROM GRADD

  !     NOTE THAT THE DBH, PROB, ITRE (POINT ID) FOR ALL THE TREES
  !     MUST BE SET PRIOR TO CALLING THIS ROUTINE FOR THE NEW TREE.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'SVDATA.f90'
  include 'SVDEAD.f90'
  include 'PLOT.f90'
  include 'WORKCM.f90'
  include 'METRIC.f90'

  !     PPP     - SUM OF PROB PER PLOT
  !     IPLCNT  - SUM OF OBJECT TREES PER PLOT
  !     NOBTS   - SUM OF OBJECT TREES PER TREE RECORD
  !     NTOADD  - SUM OF NUMBER TO ADD PER TREE RECORD
  !     RMVSUM  - INCREASING SUM OF (ADD/NEW) PROB
  !     FRACAD  - FRACTIONAL TREES PER ACRE OF NEW TREES (ie <1)
  !     NTRPLT  - A LIST OF INDICIES OF THE TREES ON THE CURRENT PLOT
  !     ITARGN  - TIME AFTER (SINCE) REGENERATION
  !     IESCYC  - ESTAB CYCLE
  !     ISNADD - VECTOR OF SNAGS THAT NEED TO BE ADDED

  ! Variables
  integer :: I1,II,I,ISVOBJ,ITOTAL,IPLT,N,IX,NTA,ITOADD,IADD,NN,NP,IPT,IT,NCKS,KODE,ICNT,J,ITEMP, &
       & NSNAGS,IYEAR
  real    :: TPPP,X,START,DIAI,Y,DIAOBJ,YS,XS,DIFF,TDIFF,TOADD,XINT,XM

  real    :: PPP(MAXPLT)
  integer :: IPLCNT(MAXPLT)
  integer :: NTRPLT(MAXTRE)
  integer :: NOBTS(MAXTRE)
  real    :: FRACAD(MAXTRE)
  real    :: RMVSUM(MAXTRE)
  integer :: NTOADD(MAXTRE)
  integer :: ISNADD(MXSVOB)
  character(len=3) :: SBLANK
  integer*4 :: IDCMP1,IDCMP2,IESCYC,ITARGN(MAXTRE)
  logical :: DEBUG
  data IDCMP1,IDCMP2/10000000,20000000/
  call DBCHK (DEBUG,'SVESTB',6,ICYC)

  if (DEBUG) write (JOSTND,5) JSVOUT,I1,ICYC,IREC2,ISVINV
  5 format (/' IN SVESTB:  JSVOUT=',I3,' I1=',I6,' ICYC=',I3, &
       & ' IREC2=',I5,' ISVINV=',I5)

  if (JSVOUT.eq.0) return

  !     IF THERE ARE NO LIVE TREES, SKIP TO PROCESS DEAD TREES.

  if (ITRN.eq.0) goto 200

  !     INITIALIZATION OF VECTORS

  do II = 1,MAXPLT
     PPP(II) = 0
     IPLCNT(II) = 0
  end do
  do I = 1,MAXTRE
     NOBTS(I) = 0
     NTOADD(I) = 0
     FRACAD(I) = 0
     ITARGN(I) = -1
  end do

  TPPP = 0
  do II = 1,ITRN
     I = ITRE(II)
     if (IMETRIC.eq.0) then
        PPP(I) = PROB(II) + PPP(I)
     else
        PPP(I) = (PROB(II)/ACRtoHA) + PPP(I)
     end if
  end do

  do II = 1, ISVINV
     TPPP = PPP(II)+TPPP
  end do

  if (NSVOBJ.gt.0) then
     do ISVOBJ = 1,NSVOBJ
        II = IS2F(ISVOBJ)
        if (II.gt.0 .and. IOBJTP(ISVOBJ).eq.1) then
           I = ITRE(II)
           IPLCNT(I) = IPLCNT(I) + 1
        end if
     end do

     !       LOAD NUMBER OF OBJECTS PER TREE RECORD IN NOBTS

     do ISVOBJ = 1,NSVOBJ
        if (IOBJTP(ISVOBJ).eq.1 .and. IS2F(ISVOBJ).gt.0) then
           NOBTS(IS2F(ISVOBJ)) = NOBTS(IS2F(ISVOBJ)) + 1
        end if
     end do
  end if

  !     ADD INTEGER VALUE TREE RECORDS AND FIND FRACTIONAL VALUES

  ITOTAL = 0
  IESCYC = 0
  do I = 1,ITRN

     if (IDTREE(I) .lt. IDCMP2) then

        !       RECORD IS FROM NEW ESTABLISHMENT

        IESCYC = IDTREE(I) - IDCMP1
        IESCYC = (IESCYC - MOD(IESCYC, 10000))/10000
        ITARGN(I) = ICYC - IESCYC
     end if

     if (DEBUG) write (JOSTND,*) 'I=',I, &
          & ' IESCYC=',IESCYC, ' ITARGN',ITARGN(I), &
          & ' IDTREE = ', IDTREE(I)

     if (I1.eq.0 .or. ITARGN(I).eq.0) then

        if (IMETRIC.eq.0) then
           XM = PROB(I)
        else
           XM = PROB(I)/ACRtoHA
        end if

        if (NOBTS(I) .lt. XM) then
           if (NOBTS(I) .le. (XM-1.)) then
              NTOADD(I) = IFIX(XM - NOBTS(I))
           end if
           FRACAD(I) = XM - NOBTS(I) - NTOADD(I)
        else
           FRACAD(I) = 0
        end if

     end if
     ITOTAL = NTOADD(I) + ITOTAL + NOBTS(I)
  end do

  if (DEBUG) then
     X = 0
     do I = 1,ITRN
        X = NTOADD(I) + X
     end do
     write (JOSTND,*) 'NONFRACTIONAL TOTAL TO HAVE=', &
          & X, ITOTAL
  end if

  !     USING THE FRACTIONAL NUMBER OF TREES CALCULATE AND LOAD
  !     THE NUMBER OF TREES TO ADD PER TREE RECORD PER PLOT

  do IPLT = 1,ISVINV
     if (DEBUG) write (JOSTND,*) '--------------ADD IPLT=', &
          & IPLT,'----------------'

     !       INITALIZE RMVSUM

     do I = 1,ITRN
        RMVSUM(I) = 0
     end do

     !       FIND TOTAL NUMBER OF TREES RECORDS FOR EACH PLOT

     N = 0
     do I = 1,ITRN
        if (ITRE(I).eq.IPLT) then
           N = N + 1
           NTRPLT(N) = I
        end if
     end do

     !       IF THERE ARE NO TREES ON THIS PLOT GO TO NEXT PLOT

     if (N.eq.0) goto 20

     !       SORT NTRPLT ON DBH

     call RDPSRT(N,DBH,NTRPLT,.false.)

     !       LOAD RMVSUM WITH FRACTIONAL NEW TREES IN ORDER BY NTRPLT

     IX = NTRPLT(1)
     RMVSUM(IX) = FRACAD(IX)
     if (DEBUG) write (JOSTND,*) 'RMVSUM(',IX,')=',RMVSUM(IX)
     if (N.gt.1) then
        do II = 2,N
           I = NTRPLT(II)
           RMVSUM(I) = FRACAD(I) + RMVSUM(IX)
           IX = I
           if (DEBUG) write (JOSTND,*) 'RMVSUM(',I,')=',RMVSUM(I)
        end do
     end if

     !       CALCULATE THE NUMBER OF TREES ADDED TO THIS PLOT

     NTA = 0
     do I = 1,N
        NTA = NTOADD(NTRPLT(I)) + NTA
     end do

     !       SETUP INFORMATION TO START DECIDING WHAT TREES SHOULD BE ADDED

     TOADD = PPP(IPLT) - FLOAT(IPLCNT(IPLT)) - NTA
     if (TOADD.le.0.0) goto 20
     ITOADD = IFIX(TOADD) + 1
     XINT = RMVSUM(NTRPLT(N)) / (TOADD)
     call SVRANN(X)
     START = XINT * X
     X = 0
     if (DEBUG) write (JOSTND,*) 'N=',N,'ITOADD=', ITOADD, &
          & ' XINT=',XINT,' START=',START,'NTA=', NTA
     IADD = 0

     !       LOOP TO FIND WHICH TREES SHOULD BE ADDED

     NTA = NTA + IPLCNT(IPLT)
     do II = 1,N
        if (DEBUG) write (JOSTND,*)  'NTA=',NTA, 'PPP(IPLT)=', &
             & PPP(IPLT),'ITOTAL=',ITOTAL, 'TPPP=',TPPP
        if (ITOTAL.ge.TPPP) goto 30
        if (NTA .ge. PPP(IPLT)) goto 20
        I = NTRPLT(II)
        if (DEBUG) write(JOSTND,*)'START=',START, &
             & ' RMVSUM(',I,')=',RMVSUM(I),' X=',X
        if (START.gt.X .and. START.lt.RMVSUM(I)) then
           if (I1 .gt. 0) then
              if (ITARGN(I).ne.0) then
                 X = (I)
                 START = START + XINT
                 if (DEBUG) write (JOSTND,*) 'TOO BIG'
                 goto 15
              end if
           end if
           START = START + XINT
           NTOADD(I) = NTOADD(I) + 1
           ITOTAL = ITOTAL + 1
           IADD = IADD + 1
           NTA = NTA + 1
           X = RMVSUM(I)

           if (DEBUG) write (JOSTND,*) 'IADD=',IADD, &
                & ' NTOADD(', I,')=', &
                & NTOADD(I), ' FRACAD(I)=',FRACAD(I)
           if (IADD.ge.ITOADD) goto 20
        else
           X = RMVSUM(I)
        end if
        15 continue
     end do
     20 continue
  end do
  30 continue

  !     USED FOR DEBUG PURPOSES

  if (DEBUG) then
     X = 0
     do I = 1,ITRN
        X = NTOADD(I) + X
        write (JOSTND,*) 'I=',I,'PROB(I)=',PROB(I)/ACRtoHA, &
             & 'NTOADD(I)=', NTOADD(I), 'NOBTS(I)=', NOBTS(I), &
             & 'TOTAL=', NTOADD(I)+NOBTS(I)
     end do
     write (JOSTND,*) 'NEED ADDING=', X
  end if

  !     UPDATING AND CHECKING TO SEE WHERE CALL WAS MADE FROM

  if (NSVOBJ.eq.0) then
     NN = 2
  else
     NN = 1
  end if

  do II = 1,NN
     do I = 1,ITRN
        NP = NTOADD(I)
        if (NP.gt.0) then

           !           THE TREE WON THE TOSS...PLACE IT...NP TIMES
           !           1/12 * .5 = .04166667

           IPT = ITRE(I)

           if (IMETRIC.eq.0) then
              DIAI = DBH(I) * .04166667
           else
              DIAI = DBH(I) * INtoM * 0.5 ! radius in metres
           end if

           do IT = 1,NP
              NCKS = 0
              40 continue
              call SVGTPT(IPLGEM,X1R1S(IPT),X2R2S(IPT), &
                   & Y1A1S(IPT),Y2A2S(IPT),X,Y,IMETRIC)

              !             IF THE OBJECT OVERLAPS, GET A NEW POINT.
              !             (DON'T CHECK FOR OVERLAP WITH CWD OBJECTS)

              if (NSVOBJ.eq.0) goto 50
              if (II.eq.1) then
                 if (ITARGN(I).ne.0 .or. NN.eq.1) then
                    do ISVOBJ = 1,NSVOBJ
                       if (IOBJTP(ISVOBJ).eq.4) cycle
                       if (IS2F(ISVOBJ).gt.0) then
                          if (ITRE(IS2F(ISVOBJ)).eq.ITRE(I)) then

                             if (IMETRIC.eq.0) then
                                DIAOBJ = DBH(IS2F(ISVOBJ))*.04166667
                             else
                                DIAOBJ = DBH(IS2F(ISVOBJ))*INtoM*0.5
                             end if

                             call SVOBOL (1,X,Y,DIAI,0., &
                                  & IOBJTP(ISVOBJ),XSLOC(ISVOBJ), &
                                  & YSLOC(ISVOBJ),DIAOBJ,0., &
                                  & XS,YS,KODE)

                             if (KODE.eq.1) then
                                NCKS = NCKS + 1
                                if (NCKS.gt.40) goto 50
                                goto 40
                             end if
                          end if
                       end if
                    end do
                 end if
              end if
              50 continue

              !             PLACE THE TREE AT X,Y

              NSVOBJ = NSVOBJ + 1
              if (NSVOBJ.gt.MXSVOB) then
                 write (JOSTND,60)
                 60 format (//'********   STAND VISUALIZATION ', &
                      & 'ERROR: TOO MANY OBJECTS')
                 NSVOBJ = NSVOBJ -1
                 call RCDSET (2,.true.)
                 return
              end if
              XSLOC(NSVOBJ) = X
              YSLOC(NSVOBJ) = Y
              IS2F(NSVOBJ) = I
              IOBJTP(NSVOBJ) = 1
              NTOADD(I) = NTOADD(I) - 1
           end do
        end if
     end do
  end do

  !     USED FOR DEBUG PURPOSES

  if (DEBUG) write (JOSTND,*) 'NSVOBJ=',NSVOBJ
  if (DEBUG .and. NSVOBJ.gt.0) then
     X = 0
     do I = 1,ITRN
        if (NTOADD(I).gt.0) write (JOSTND,*) &
             & 'TREE =', I,',',NTOADD(I)
        X = NTOADD(I) + X
     end do
     write (JOSTND,*) 'LEFT NOT ADDED=', X
     TPPP = 0
     do II = 1, MAXPLT
        PPP(II) = 0
        IPLCNT(II) = 0
     end do
     do II = 1,ITRN
        I = ITRE(II)

        if (IMETRIC.eq.0) then
           PPP(I) = PROB(II) + PPP(I)
        else
           PPP(I) = (PROB(II)/ACRtoHA) + PPP(I)
        end if

     end do

     ICNT = 0
     do ISVOBJ = 1,NSVOBJ
        II = IS2F(ISVOBJ)
        if (II.gt.0) then
           if (IOBJTP(ISVOBJ).eq.1) then
              I = ITRE(II)
              IPLCNT(I) = IPLCNT(I) + 1
              ICNT = ICNT + 1
           end if
        end if
     end do

     do J = 1,ITRN
        ITEMP = 0
        do ISVOBJ = 1,NSVOBJ
           II = IS2F(ISVOBJ)
           if (II.eq.J .and. IOBJTP(ISVOBJ).eq.1) ITEMP = ITEMP+1
        end do

        if (IMETRIC.eq.0) then
           DIFF = PROB(J)-ITEMP
        else
           DIFF = (PROB(J)/ACRtoHA)-ITEMP
        end if

        DIFF = PROB(J)-ITEMP
        if (DIFF.gt.1.or.DIFF.lt.-1) then
           if (DIFF.gt.2.or.DIFF.lt.-2) then
              SBLANK = ' ++'
           else
              SBLANK = ' **'
           end if
        else
           SBLANK = ' '
        end if
        if (PROB(J).eq.0) SBLANK = ' '

        if (IMETRIC.eq.0) then
           XM = PROB(J)
        else
           XM = PROB(J)/ACRtoHA
        end if

        write (JOSTND,*) '  J=',J,'PROB(',J,')=',XM, &
             & ' OBJECTL=',ITEMP, &
             & ' DIFF=',DIFF,SBLANK
     end do

     TDIFF = 0
     do II = 1, ISVINV
        DIFF = PPP(II) - IPLCNT(II)
        TDIFF = TDIFF + DIFF
        write (JOSTND,*) 'PPP(',II,')=',PPP(II), &
             & 'IPLCNT(',II,')=',IPLCNT(II),'DIFF=',DIFF
     end do

     do II = 1, ISVINV
        TPPP = PPP(II)+TPPP
     end do
     write (JOSTND,*) 'TPPP=',TPPP,'ICNT=',ICNT,'TDIFF=',TDIFF
  end if

  200 continue

  !     IF CALLED FROM SVSTART (I1 EQ 0) THEN INITIALAIZE THE SNAG LIST
  !     AND THE CWD OBJECT LIST.

  if (I1.eq.0) then
     NDEAD = 0
     do IX = 1,MXDEAD
        ISTATUS(IX) = 0
     end do

     NCWD = 0
     do IX = 1,MXCWD
        CWDDIA(IX) = 0.0
     end do
  end if

  !     PROCESS THE SNAGS THAT MAY BE IN THE TREE LIST

  if (IREC2.lt.MAXTP1) then

     !     INITIALIZATION OF VECTORS

     do II = 1,MAXPLT
        PPP(II) = 0
        IPLCNT(II) = 0
     end do
     do I = 1,MAXTRE
        NOBTS(I) = 0
        NTOADD(I) = 0
        RMVSUM(I) = 0
        FRACAD(I) = 0
     end do

     TPPP = 0
     do II = IREC2,MAXTRE
        I = ITRE(II)

        if (IMETRIC.eq.0) then
           PPP(I) = (PROB(II)*(FINTM/FINT)) + PPP(I)
        else
           PPP(I) = (PROB(II)*(FINTM/FINT)/ACRtoHA) + PPP(I)
        end if

     end do

     do II = 1, ISVINV
        TPPP = PPP(II)+TPPP
     end do

     !       NOTE: NOBTS IS ALWAYS 0 HERE FOR THE GIVEN RECORD

     ITOTAL = 0
     do I = IREC2,MAXTRE
        if (I1.eq.0) then

           if (IMETRIC.eq.0) then
              XM = PROB(I)*(FINTM/FINT)
           else
              XM = PROB(I)*(FINTM/FINT)/ACRtoHA
           end if

           if (NOBTS(I) .lt. XM) then
              if (NOBTS(I) .le. XM - 1.) then
                 NTOADD(I) = IFIX(XM-NOBTS(I))
              end if
              FRACAD(I) = XM-NOBTS(I)-NTOADD(I)
           else
              FRACAD(I) = 0
           end if
        end if
        ITOTAL = NTOADD(I) + ITOTAL + NOBTS(I)
     end do

     !       USING THE FRACTIONAL NUMBER OF TREES CALCULATE AND LOAD
     !       THE NUMBER OF TREES TO ADD PER TREE RECORD PER PLOT

     do IPLT = 1,ISVINV
        if (DEBUG) write (JOSTND,*) '--------------ADD IPLT=', &
             & IPLT,'----------------'

        !         INITALIZE RMVSUM

        do I = IREC2,MAXTRE
           RMVSUM(I) = 0
        end do

        !         FIND TOTAL NUMBER OF TREES PER THIS PLOT

        N = 0
        do I = IREC2,MAXTRE
           if (ITRE(I).eq.IPLT) then
              N = N + 1
              NTRPLT(N) = I
           end if
        end do

        !         IF THERE ARE NO TREES ON THIS PLOT GO TO NEXT PLOT

        if (N.eq.0) goto 220

        !         SORT NTRPLT ON DBH

        call RDPSRT(N,DBH,NTRPLT,.false.)

        !         LOAD RMVSUM WITH FRACTIONAL NEW TREES IN ORDER BY NTRPLT

        IX = NTRPLT(1)
        RMVSUM(IX) = FRACAD(IX)
        if (DEBUG) write (JOSTND,*) 'RMVSUM(',IX,')=',RMVSUM(IX)
        if (N.gt.1) then
           do II = 2,N
              I = NTRPLT(II)
              RMVSUM(I) = FRACAD(I) + RMVSUM(IX)
              IX = I
           end do
        end if

        !         CALCULATE THE NUMBER OF TREES ADDED TO THIS PLOT

        NTA = 0
        do I = 1,N
           NTA = NTOADD(NTRPLT(I)) + NTA
        end do

        !         SETUP INFORMATION TO START DECIDING WHAT TREES SHOULD BE ADDED

        TOADD = PPP(IPLT) - FLOAT(IPLCNT(IPLT)) - NTA
        if (TOADD.le.0.0) goto 220
        ITOADD = IFIX(TOADD) + 1
        XINT = RMVSUM(NTRPLT(N)) / (TOADD)
        call SVRANN(X)
        START = XINT * X
        X = 0
        IADD = 0

        !         LOOP TO FIND WHICH TREES SHOULD BE ADDED

        NTA = NTA + IPLCNT(IPLT)
        do II = 1,N
           if (ITOTAL.ge.TPPP) goto 230
           if (NTA .ge. PPP(IPLT)) goto 220
           I = NTRPLT(II)
           if (START.gt.X .and. START.lt.RMVSUM(I)) then
              if (I1 .gt. 0) then
                 X = (I)
                 START = START + XINT
                 goto 215
              end if
              START = START + XINT
              NTOADD(I) = NTOADD(I) + 1
              ITOTAL = ITOTAL + 1
              IADD = IADD + 1
              NTA = NTA + 1
              X = RMVSUM(I)
              if (IADD.ge.ITOADD) goto 220
           else
              X = RMVSUM(I)
           end if
           215 continue
        end do
        220 continue
     end do
     230 continue

     !       UPDATING AND CHECKING TO SEE WHERE CALL WAS MADE FROM

     NN = 2
     NSNAGS = 0
     do II = 1,NN
        do I = IREC2,MAXTRE
           NP = NTOADD(I)
           if (NP.gt.0) then

              !             THE TREE WON THE TOSS...PLACE IT...NP TIMES
              !             1/12 * .5 = .04166667

              IPT = ITRE(I)

              if (IMETRIC.eq.0) then
                 DIAI = DBH(I) * .04166667
              else
                 DIAI = DBH(I) * INtoM * 0.5
              end if

              do IT = 1,NP
                 NCKS = 0
                 240 continue
                 call SVGTPT(IPLGEM,X1R1S(IPT),X2R2S(IPT), &
                      & Y1A1S(IPT),Y2A2S(IPT),X,Y,IMETRIC)

                 !               IF THE OBJECT OVERLAPS, GET A NEW POINT.
                 !               (DON'T CHECK FOR OVERLAP WITH CWD OBJECTS)

                 if (NSVOBJ.eq.0) goto 250
                 if (II.eq.1) then
                    if (NN.eq.1) then
                       do ISVOBJ = 1,NSVOBJ
                          if (IOBJTP(ISVOBJ).eq.4) cycle
                          if (IS2F(ISVOBJ).gt.0) then
                             if (ITRE(IS2F(ISVOBJ)).eq.ITRE(I)) then

                                if (IMETRIC.eq.0) then
                                   DIAOBJ = DBH(IS2F(ISVOBJ))* &
                                        & .04166667
                                else
                                   DIAOBJ = DBH(IS2F(ISVOBJ))* &
                                        & INtoM * 0.5
                                end if

                                call SVOBOL (1,X,Y,DIAI,0., &
                                     & IOBJTP(ISVOBJ),XSLOC(ISVOBJ), &
                                     & YSLOC(ISVOBJ),DIAOBJ,0., &
                                     & XS,YS,KODE)

                                if (KODE.eq.1) then
                                   NCKS = NCKS + 1
                                   if (NCKS.gt.40) goto 250
                                   goto 240
                                end if
                             end if
                          end if
                       end do
                    end if
                 end if
                 250 continue

                 !               PLACE THE TREE AT X,Y

                 NSVOBJ = NSVOBJ + 1
                 NSNAGS = NSNAGS + 1
                 if (NSVOBJ.gt.MXSVOB) then
                    write (JOSTND,260)
                    260 format (//'********   STAND VISUALIZATION ', &
                         & 'ERROR: TOO MANY OBJECTS')
                    NSVOBJ = NSVOBJ -1
                    call RCDSET (2,.true.)
                    return
                 end if
                 XSLOC(NSVOBJ) = X
                 YSLOC(NSVOBJ) = Y
                 IS2F(NSVOBJ) = I
                 ISNADD(NSNAGS) = NSVOBJ
                 IOBJTP(NSVOBJ) = 0
                 NTOADD(I) = NTOADD(I) - 1
              end do
           end if
        end do
     end do

     IYEAR = IY(1)
     call SVSNAD(IYEAR,ISNADD,ITOTAL,0)

  end if

  return
end subroutine SVESTB
