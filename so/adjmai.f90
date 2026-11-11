!=================!
!   so : adjmai   !
!=================!
! SO $Id$
function ADJMAI(INSPEC,SINDEX,POINTS,IERROR)
  implicit none

  !         THIS FUNCTION COMPUTES THE ADJUSTED MAI FOR AN NFI PLOT USING
  !         THREE DATA ITEMS FROM THE NFI PLOT SUMMARY FILE...
  !         THE INPUT ITEMS ARE INSPEC, THE SITE SPECIES CODE,
  !                             SINDEX, THE SITE INDEX,
  !                             POINTS, THE NUMBER OF STOCKABLE POINTS.
  !         THE VARIABLE IERROR RETURNS A VALUE OF 1 IF A SPECIES
  !         CODE IS UNACCEPTABLE. OTHERWISE, IERROR IS SET TO ZERO.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'

  ! Variables
  integer :: IMAP(36),ISP(36),IERROR,INSPEC,NCALLS,I,INUM
  real    :: POINTS,SINDEX,ADJMAI

  !     FIA SPECIES CODES ORGANIZED IN SITE INDEX EQUATION GROUPS

  data ISP/&
       !     EQ GROUP 1
       !      AF  PC  YC  TA  IC  WS  BE  RC  WH  MH  SS  OS
       &019,041,042,071,081,094,095,242,263,264,098,298,&
       !     EQ GROUP 2
       !      WB  KP  LP  CP  BP  MP
       &101,103,108,109,120,124,&
       !     EQ GROUP 3
       !      WP
       &119,&
       !     EQ GROUP 4
       !      JP  SP  PP
       &116,117,122,&
       !     EQ GROUP 5
       !      BD  DF
       &201,202,&
       !     EQ GROUP 6
       !       WL
       &073,&
       !     EQ GROUP 7
       !      BR  ES
       &092,093,&
       !     EQ GROUP 8 IS THE HARDWOOD GROUP WITHOUT SPECIFIC SPECIES ASSGNED
       !                FIA CODES OF 300 AND GREATER ARE ASSIGNED GROUP 8.
       !     EQ GROUP 9
       !      SF  WF  GF  AF  RF  SH  NF
       &011,015,017,019,020,021,022,&
       !     EQ GROUP 10
       !      RW  GS  ??
       &211,212/

  !     EQUATION GROUP ASSIGNMENT MAP
  data IMAP/12*1,6*2,3,3*4,2*5,6,2*7,7*9,2*10/

  NCALLS = 1
  IERROR = 0
  ADJMAI = 0.0

  !   IF SPECIES CODE IS > 300 THEN SPECIES IS HARDWOOD. GO TO GROUP 8.

  if (INSPEC.ge.300) go to 80

  !      CHECK FOR A LEGAL SPECIES CODE...

  do I = 1,32
     if (INSPEC.eq.ISP(I))go to 9
     2 continue
  end do

  write (JOSTND,3) INSPEC,NCALLS
  3 format(/,' ******** WARNING:  ILLEGAL SITE INDEX SPECIES (',I3,&
       &') WAS DETECTED FOR RECORD NUMBER ',I5,'.'/T21,&
       &'NO ADJUSTED MAI WAS CALCULATED.')
  IERROR = 1
  return

  !        LEGAL SPECIES FOUND - FIND EQUATION GROUP USING COMPUTED GOTO
  !        STATEMENT

  9 continue
  INUM = IMAP(I)

  go to (10,20,30,40,50,60,70,80,90,100),INUM

  !                   GROUP 1...
  10 continue
  if (SINDEX.lt.33.0) return
  ADJMAI = -63.689706 + (1.9402941 * SINDEX)
  go to 200

  !                   GROUP 2...
  20 continue
  if (SINDEX.lt.11.0) return
  ADJMAI = -12.0388 + (1.18672 * SINDEX)
  go to 200

  !                   GROUP 3...
  30 continue
  ADJMAI = 5.972615 + (1.857675 * SINDEX)
  go to 200

  !                   GROUP 4...
  40 continue
  ADJMAI = 2.305357  + 0.033890056 * SINDEX + 0.0090108543 * SINDEX**2
  go to 200

  !                   GROUP 5...
  50 continue
  if (SINDEX.lt.29.0) return
  ADJMAI = -10.303313 + .032929911 * SINDEX + .012207163 * SINDEX**2&
       &+ (-.00003543129 * SINDEX**3)
  go to 200

  !                   GROUP 6...
  60 continue
  if (SINDEX.lt.11.0) return
  ADJMAI = -6.0892857 + .45178571 * SINDEX + .014464286 * SINDEX**2
  go to 200

  !                   GROUP 7...
  70 continue
  if (SINDEX.lt.10.0) return
  ADJMAI = -18.4 + 1.92 * SINDEX
  go to 200

  !                   GROUP 8 ...
  80 continue
  if (SINDEX.lt.32.0) return
  ADJMAI = -53.892857 + 1.7178571 * SINDEX
  go to 200

  !                   GROUP 9...
  90 continue
  ADJMAI = -4.89001 + 311.29546 * ((EXP( (SINDEX / 170.0 - 1.0)**3&
       &/ 0.343) - 0.055) / 0.95)
  go to 200

  !                    GROUP 10 ...
  100 continue
  if (SINDEX.lt.62.0) return
  ADJMAI = 157.94643 - 1.78125 * SINDEX + .014330357 * SINDEX**2
  200 continue
  ADJMAI = ADJMAI * POINTS/10.0
  if (ADJMAI.lt.0.0) ADJMAI = 0.0
  return
end
