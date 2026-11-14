!===================!
!   lpmpb : ptplt   !
!===================!
! LPMPB $Id$
subroutine PTPLT(IPLTNO,IPTLUN,IXFORM,LCTX,NEXP)
  implicit none

  !     LIBRARY ROUTINE TO PLOT GRAPH NUMBER IPLTNO
  !             ASSUMES 122 CHAR WIDTH

  !             IPLTNO -- PLOT NUMBER
  !             IPTLUN -- OUTPUT UNIT NUMBER
  !             IXFORM -- FORMAT FOR X VALUES ('F6.?')
  !             LCTX -- LINES TO BE LABELED WITH X VALUES
  !                     (NEGATIVE LCTX SUPRESSES - - - ACROSS PAGE)
  !             NEXP -- EXPANSION FACTOR FOR X AXIS

  !             NVAR--NUMBER OF VARIABLES
  !             VAR--VALUES TO VARIABLES TO BE PLOTTED
  !             LCT--LINE COUNTER
  !             LSP(.,1) COLUMN WITH SUPERPOSITION
  !             LSP(.,2-10) SUPERIMPOSED VARIABLES
  !             NLS(.)  NUMBER SUPERIMPOSED LETTERS +1

  ! Revision History
  !   02/16/88 Last noted revision date.
  !   07/02/10 Lance R. David (FMSC)
  !     Added IMPLICIT NONE.

  ! Common blocks
  include 'PT.f90'

  ! Variables
  character(len=18) :: IFORM1
  character(len=10) :: IFORM2
  character(len=23) :: IFORM3
  character(len=15) :: IFORM4
  character(len=1) :: PL
  character(len=1) :: SPC
  character(len=1) :: COM
  character(len=1) :: MIN
  character(len=1) :: IPLUS
  character(len=1) :: MINX
  character(len=1) :: LINE(101)
  character(len=1) :: LNS(14)
  character(len=1) :: LSP(5,10)

  integer :: I,IFLAG,ILUN,IPLTNO,IPTLUN,IXFORM,J,JLINE,K,L,LCT,LCTX,LCTX1,LIN,M,N,NEXP,NEXP1,NLS(5),&
       &NSP,NSPL,NVAR

  real    :: ALIN,RANGE,TEMP,VAR(10),XAXIS

  data  IFORM1 /'(7X,101A1,1X,14A1)'/
  data  IFORM2 /'(7X,101A1)'/
  data  IFORM3 /'(1X,F6.0,101A1,1X,14A1)'/
  data  IFORM4 /'(1X,F6.0,101A1)'/
  data  PL/'.'/, SPC/' '/, COM/','/, MIN/'-'/, IPLUS/'+'/
  data  IFLAG/0/

  !         POSITION PAGE
  write (IPTLUN,100)
  100 format (//)
  go to 10

  entry PTNSC(IPLTNO,IPTLUN,IXFORM,LCTX,NEXP)
  !        ALLOWS USER TO SUPPRESS PRINTING OF SCALES.
  !        HE MUST SUPPLY HIS OWN CARRIAGE CONTROL.
  IFLAG = 1

  entry PTOPT(IPLTNO,IPTLUN,IXFORM,LCTX,NEXP)
  !        ALLOWS USER TO PRINT A TITLE AND/OR COMMENTS AT TOP
  !        OF GRAPH.  HE MUST FIRST SUPPLY HIS OWN CARRIAGE CONTROL.

  10 if (IPTSPL(IPLTNO) .eq.0) return

  !        SET SCALES
  call PTSSC(IPLTNO)
  if (IFLAG .eq.1) go to 11

  !        PRINT SCALES
  call PTSCL(IPLTNO,IPTLUN)

  !         SET LINE LENGTH PARAMETERS
  11 continue
  LIN = 101
  ALIN = 100.

  !        SET UP X AXIS
  MINX = MIN
  if (LCTX .lt.0) MINX = SPC
  !        ABOVE ALLOWS SUPRESSION OF - - - FOR NEGATIVE LCTX

  NEXP1 = NEXP - 1
  if (NEXP1.lt.0) NEXP1 = 0
  LCTX1 = IABS(LCTX)/(NEXP1 + 1)
  !     NOTE1 -- WHILE NEXP IS THE X AXIS EXPANSION FACTOR,
  !              NEXP1 HAS MEANING AS THE NUMBER OF 'BLANK'
  !              LINES TO BE INSERTED TO CAUSE AN EXPANSION
  !              OF NEXP TIMES.

  !     NOTE2 -- NEXP1 AND LCTX1 ARE USED INSTEAD OF REDEFINING
  !              NEXP AND LCTX IN ORDER TO AVOID CHANGING THE
  !              CALLING PARAMETERS.

  !        BUILD BODY OF PLOT
  LCT = 0
  ILUN = 19+IPLTNO
  rewind ILUN
  NVAR = IPTVAR(IPLTNO)
  NSPL = IPTSPL(IPLTNO)

  do JLINE = 1,NSPL
     !**** WRITE(IPTLUN,777)
     !****  777 FORMAT('READ ILUN')

     !         ******EACH PASS PLOTS ONE LINE*********

     read(ILUN) XAXIS,(VAR(I),I = 1,10)

     !             INITIALIZE SUPERPOSITION COUNTER
     NSP = 0
     LNS(1) = SPC

     !             BLANK OUT LINE IMAGE
     do I = 1,LIN
        1 LINE(I) = SPC
     end do

     !             FILL IN LINE
     do N = 1,NVAR
        TEMP = 0.
        RANGE = PTU(IPLTNO,N) - PTL(IPLTNO,N)
        if (RANGE .ne.0.) TEMP = (VAR(N) - PTL(IPLTNO,N))/RANGE
        TEMP = TEMP*ALIN+1.5
        if (TEMP .lt.1..or. TEMP .ge. ALIN+2.) go to 2
        I = TEMP
        if (LINE(I) .eq. SPC) go to 4

        !             HAVE SUPERPOSITION
        if (NSP .eq.0) go to 6
        do M = 1,NSP
           if (LSP(M,1) .eq. LINE(I)) go to 8
           7 continue
        end do
        6 NSP = NSP+1
        LSP(NSP,1) = LINE(I)
        LSP(NSP,2) = IPTLET(IPLTNO,N)
        NLS(NSP) = 2
        go to 2

        !             MANY SUPERPOSITIONS ON THIS COMUMN
        8 J = NLS(M)+1
        LSP(M,J) = IPTLET(IPLTNO,N)
        NLS(M) = J
        go to 2
        4 LINE(I) = IPTLET(IPLTNO,N)

        2 continue
     end do
     !             END OF FILL IN LINE LOOP

     !             NEXT ELEMENT OF LNS(.) TO BE FILLED
     K = 1
     if (NSP.eq.0) go to 12
     do L = 1,NSP
        if (K .eq.1) go to 14
        LNS(K) = COM
        K = K+1
        14 J = NLS(L)
        do I = 1,J
           LNS(K) = LSP(L,I)
           16 K = K+1
        end do
        15 continue
     end do
     12 K = K-1
     LCT = LCT-1
     if (LCT .le.0.or. JLINE .eq. NSPL) go to 19

     !           PRINT UNLABELED LINE
     do I = 1,LIN,25
        if (LINE(I) .eq. SPC) LINE(I) = PL
        9 continue
     end do
     !             PUTS ...DOWN THE PAGE

     !***  WRITE(IPTLUN,778) LIN,K
     !*** 778  FORMAT ('WRITE IFORM1 LIN K',2I5)
     if (K .eq.0) go to 55
     write(IPTLUN,IFORM1) (LINE(I),I = 1,LIN),(LNS(I),I = 1,K)
     go to 56
     55 write(IPTLUN,IFORM2) (LINE(I),I = 1,LIN)
     56 continue
     if (NEXP1.gt.0) go to 30
     go to 32

     !        PRINT LABELED LINE
     19 do I = 1,LIN,25
        if (LINE(I) .eq. SPC) LINE(I) = IPLUS
        20 continue
     end do
     do I = 3,LIN,2
        if (LINE(I) .eq. SPC) LINE(I) = MINX
        22 continue
     end do

     LCT = LCTX1
     !***  WRITE(IPTLUN,779) LIN,K
     !*** 779  FORMAT ('WRITE IFORM2 LIN K',2I5)
     if (K .eq.0) go to 65
     write(IPTLUN,IFORM3) XAXIS,(LINE(I),I = 1,LIN),(LNS(I),I = 1,K)
     go to 66
     65 write(IPTLUN,IFORM4) XAXIS,(LINE(I),I = 1,LIN)
     66 continue
     if (NEXP1.eq.0.or. JLINE .eq. NSPL) go to 32

     !              EXPAND X AXIS IF REQUESTED
     30 do I = 1,NEXP1
        !*****  WRITE(IPTLUN,780)
        !*****  780   FORMAT('EXPAND WRITE LOOP')
        write(IPTLUN,120)
        120 format(6X,'.',4(24X,'.'))
        31 continue
     end do
     !        NOTE-- ABOVE FORMAT MUST BE CHANGED FOR VARIABLE LINE LENGTH

     32 continue
  end do

  IFLAG = 0

  !     REWIND ADDED BY N. CROOKSTON

  rewind ILUN

  return
end
