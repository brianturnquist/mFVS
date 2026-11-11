!===================!
!   strp : esplt2   !
!===================!
! STRP $Id$
subroutine ESPLT2 (IPTKNT)
  implicit none

  !     CALLED BY INITRE TO TRANSLATE PLOT SPECIFIC VARIABLES FOR ESTAB

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'ESHAP.f90'
  include 'ESHAP2.f90'

  ! Variables
  logical :: DEBUG

  !     WARNING: ESB1 AND SUMPRB ARE BOTH MEMBERS OF /ESHAP2/ ... THEY
  !              ARE BEING USED TO PASS DATA FROM ESPLT1 TO ESPLT2.

  integer :: IEND(33),MYGRUP(33),NEW(MAXPLT),NON(MAXPLT),NSID(MAXPLT)
  integer :: IPTKNT,NSTK,I,NM,N,J,ME,J1,ITEMP
  real    :: XXSLP,XXASP
  equivalence (NEW,PROB1),(NON,PNN),(NSID,ESB1),(NSTK,SUMPRB)
  data IEND/269,299,319,335,385,394,399,499,509,515,519,522,&
       &523,524,529,564,579,584,589,599,634,637,644,649,659,669,689,&
       &699,709,719,739,744,799/, MYGRUP/3,1,4,2,4,3,4,3,8,6,8,7,&
       &5,7,8,9,10,6,8,5,13,16,11,14,16,12,15,12,14,15,11,15,14/

  call DBCHK (DEBUG,'ESPLT2',6,ICYC)

  !     MAP THE STAND HABITAT CODE TO THE ESTABLISHMENT CODES.

  IHTYPE = ICL5
  do I = 1,33
     if (IHTYPE.gt.IEND(I)) go to 3
     IHTYPE = MYGRUP(I)
     go to 4
     3 continue
  end do
  IHTYPE = 16
  4 continue
  XXSLP = ISLOP*0.01
  XXASP = IASPEC*0.0174533
  if (DEBUG) write (JOSTND,5) IPINFO,IPTKNT,IPTINV,NPTIDS,&
       &NONSTK,IHTYPE,XXSLP,XXASP
  5 format (/'IN ESPLT2: IPINFO=',I3,' IPTKNT=',I4,' IPTINV=',I4,&
       &' NPTIDS=',I4,' NONSTK=',I4,' IHTYPE=',I3,' XXSLP=',F5.2,&
       &' XXASP=',F5.2)

  !     INITIALIZE THE NON MATCH COUNT TO 0.

  NM = 0

  !     IF THERE WERE TREEDATA RECORDS (THE POINT COUNT IS GT 0);
  !     THEN: BRANCH TO NORMAL PROCESSING.

  if (IPTKNT.gt.0) goto 20

  !     LOAD PLOT ID VECTOR WITH POINTERS.

  if (NPTIDS.le.0.or. IREC1.eq.0) then
     NPTIDS = IPTINV-NONSTK
     if (NPTIDS.le.0) NPTIDS = 1
  end if
  do I = 1,NPTIDS
     IPTIDS(I) = I
     10 continue
  end do

  !     IF THERE ARE PLOTINFO RECORDS OR TREE RECORDS, THEN:
  !     BRANCH TO DECODE.

  if (IPINFO.ne.0) goto 110

  !     ELSE:  BRANCH TO LOAD STAND VALUES.

  NM = 1
  goto 150
  20 continue

  !     IF PLOTINFO OPTION WAS USED (IPINFO=1), MATCH PLOT IDENTIFICATION
  !     CODES WITH THOSE IN THE TREE FILE.  KEEP STOCKABLE PLOTS ONLY.

  if (IPINFO.ne.1) goto 80

  !     INITIALIZE PLOTINFO COUNT.

  N = NPTIDS
  NPTIDS = 0

  !     DO FOR ALL TREEDATA PLOTS.

  do J = 1,IPTKNT
     ME = IPVEC(J)

     !     DO FOR ALL PLOTINFO PLOTS.

     do I = 1,N
        if (ME.eq.IPTIDS(I)) goto 40
        30 continue
     end do

     !     THERE IS NO MATCH (NO PLOTINFO RECORD MATCHES THE TREEDATA PLOT
     !     ID); THEN:IF THE PLOT IS STOCKABLE, INCREMENT THE NON-MATCH
     !     COUNTER AND SAVE THE POINT INDENTIFICATION.

     if (NSTK.eq.0) goto 38
     do J1 = 1,NSTK
        if (ME.eq.NSID(J1)) goto 50
        35 continue
     end do
     38 continue
     NM = NM+1
     NON(NM) = J
     goto 50
     40 continue

     !     THERE IS A MATCH. IF THE PLOT IS NONSTOCKABLE, SKIP IT.

     if (IPPREP(J).eq.-1) goto 50
     NPTIDS = NPTIDS+1
     NEW(NPTIDS) = J
     50 continue
  end do

  !     IF THERE WERE SOME MATCHES, THEN: BRANCH TO PROCESS MATCHED PLOTS.

  if (NPTIDS.gt.0) goto 60

  !     IF THERE WERE NO STOCKABLE PLOTS, BRANCH TO SET FLAG.

  if (NM.le.0) then
     IPINFO = 4
     goto 140
  end if

  !     ELSE: SAVE THE NON-MATCH CODES AND BRANCH TO SET STAND SITE
  !     VALUES FOR THE PLOTS.

  do J = 1,NM
     IPTIDS(J) = NON(J)
     55 continue
  end do
  IPINFO = 5
  NPTIDS = NM
  NM = 1
  goto 150
  60 continue

  !     COPY MATCHED PLOT IDS TO IPTIDS.

  do I = 1,NPTIDS
     IPTIDS(I) = NEW(I)
     70 continue
  end do

  !     BRANCH TO DECODE SITE VARIABLES.

  goto 110
  80 continue

  !     PLOTINFO WAS NOT USED. PROCESS IPTIDS TO ELIMINATE NONSTOCKABLE
  !     PLOTS.  SITE PREP CODE IS -1 FOR THESE PLOTS.

  N = NPTIDS
  NPTIDS = 0
  do I = 1,N
     if (IPPREP(I).ge.0) then
        NPTIDS = NPTIDS+1
        IPTIDS(NPTIDS) = I
     end if
  end do

  !     IF THERE WERE MORE PLOTS SAID TO EXIST (IPTINV-NONSTK IS GT
  !     THE NUMBER OF PLOTS FOUND IN THE TREEDATA), THEN MAKE MORE
  !     PLOTS AND SET THE ATTRIBUTES TO THE STAND-LEVEL VALUES.

  if (IPTINV-NONSTK-N.gt.0) then
     do I = 1,IPTINV-NONSTK-N
        NPTIDS = NPTIDS+1
        IPTIDS(NPTIDS) = NPTIDS
        PSLO(NPTIDS) = -1.
        PASP(NPTIDS) = -1.
        IPHAB(NPTIDS) = ICL5
        IPPREP(NPTIDS) = 1
        IPHYS(NPTIDS) = 3
     end do
  end if

  !     IF ALL ARE NONSTOCKABLE, SET IPINFO=4 AND SET UP ONE PLOT USING
  !     STAND SITE VALUES.

  if (NPTIDS.le.0) then
     IPINFO = 4
     goto 140
  end if

  !     IF PLOT SITE DATA READ FROM TREE RECORDS, BRANCH TO DECODE VALUES.

  if (IPINFO.eq.2) goto 110

  !     ELSE: BRANCH TO STAND VALUES.

  NM = 1
  goto 150
  110 continue

  !     DECODE THE PLOT SPECIFIC VARIABLES.

  do N = 1,NPTIDS
     I = IPTIDS(N)
     if (PSLO(I).lt.0.0) then
        PSLO(I) = XXSLP
     else
        PSLO(I) = PSLO(I)*0.01
     end if
     if (IPHYS(I).lt.1.or. IPHYS(I).gt.5) IPHYS(I) = 3
     if (PASP(I).lt.0.0) then
        PASP(I) = XXASP
     else
        PASP(I) = PASP(I)*0.0174533
     end if
     if (IPPREP(I).lt.1.or. IPPREP(I).gt.4) IPPREP(I) = 1
     if (IPHAB(I).eq.0)IPHAB(I) = ICL5
     ITEMP = IPHAB(I)
     do J = 1,33
        if (ITEMP.gt.IEND(J)) go to 122
        ITEMP = MYGRUP(J)
        go to 124
        122 continue
     end do
     ITEMP = 16
     124 continue
     IPHAB(I) = ITEMP
     130 continue
  end do

  !     IF THERE WERE SOME ADDITIONAL PLOTS FROM THE TREEDATA WHICH WERE
  !     STOCKABLE BUT DID NOT HAVE PLOTINFO, ADD THEM TO THE LIST AND
  !     BRANCH TO SET STAND SITE VALUES.

  if (NM.eq.0) goto 170
  do J = 1,NM
     IPTIDS(NPTIDS+J) = NON(J)
     135 continue
  end do
  IPINFO = 3
  NPTIDS = NPTIDS+NM
  NM = NPTIDS-NM+1
  goto 150
  140 continue
  IPTIDS(1) = 1
  NPTIDS = 1
  NM = 1
  150 continue

  !     SET STAND VALUES.

  do N = NM,NPTIDS
     I = IPTIDS(N)
     PSLO(I) = XXSLP
     PASP(I) = XXASP
     IPHAB(I) = IHTYPE
     IPPREP(I) = 1
     IPHYS(I) = 3
     160 continue
  end do
  170 continue

  !     SIGNAL THAT SITE PREPS ARE READ FROM INPUT RECORDS.

  if (IPINFO.gt.0.and. IPINFO.ne.5) LOAD = 1
  if (IPINFO.eq.5) IPINFO = 3
  if (DEBUG) then
     write (JOSTND,180) IPINFO,NPTIDS
     180 format (/'IN ESPLT2:  PLOT SITE DATA CODE=',I3,&
          &'; 0=NO DATA,1=PLOTINFO,2=TREEDATA,3=MISSMATCHED IDS,4=ALL',&
          &' PLOTS NONSTOCKABLE.'/'PLOT SPECIFIC ATTRIBUTES FOR',I4,&
          &' PLOTS',/,'    I IPTIDS IPPREP IPHYS IPHAB PASP   PSLO')
     do I = 1,NPTIDS
        N = IPTIDS(I)
        write (JOSTND,190) I,IPTIDS(I),IPPREP(N),IPHYS(N),IPHAB(N),&
             &PASP(N),PSLO(N)
        190 format (I5,4I6,2F7.2)
        200 continue
     end do
  end if
  return
end
