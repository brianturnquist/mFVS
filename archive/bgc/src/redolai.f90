!===============================!
!   archive/bgc/src : redolai   !
!===============================!
subroutine REDOLAI
  !  This subroutine written 6/30/03 AJ McMahan, INTECS International, Inc for
  !  FHTET, Ft Collins, CO.
  !  This subroutine recalculates the initialized total leaf area (LA) of
  !  individual trees. Heretofore, the amount of LA assigned to each entity was
  !  a function of bole volume and DBH.  No crown information was provided.
  !  So, without this change, two trees--identical in Ht and DBH but with
  !  differing crowns--would be (unrealistically) assigned the SAME AMOUNT of
  !  total leaf area; resulting in a longer-crowned trees having a
  !  disproportionately low foliage density, and shorter crowned trees having
  !  disproportionately large foliage density.  Since LA diretly controls the
  !  size of an entity's water bucket, this mis-representation of LA can result
  !  (at least in a water limited system) in the shorter crowned trees having a
  !  competative advantage over long-crowned trees over simulated time.
  !  This subroutine attempts to ameliorate this behaviour by modifying how LA
  !  is initialized.
  !  In lieu of a better method to assign more leaf area to larger crowns, we
  !  we make this relatively simple, minor adjustment.  The total LA assigned
  !  to a stand (i.e. the sum for all of the [large tree*] entities) will not
  !  change from that assigned by the original, unchanged code.  What this
  !  routine does is re-distribute the leaf area in the stand in such a way
  !  that the amount of LA assigned to each entity is PROPORTIONAL TO EACH
  !  ENTITY'S CROWN VOLUME..

  !  * Large trees in this sense are the tree entities that can access both
  !  upper and lower water buckets, which are trees that are GE 1.3m tall.

  !  Since this subroutine is called (from within BSTRUCTN) after GEOMETRY, CW
  !  and CL info is available (for first DO-loop).  This subroutine is
  !  necessarily called immediately before the call to LAIJ, where each
  !  entity's crown layer's leaf areas are determined.

  ! Amended 7/7/03  Adding an alternative method for redistribution of LA.
  ! Instead of prorating LA by proportion of total crown VOLUME represented by
  ! entity's crown VOLUME; now use SURFACE AREA of entity's cone/crown. We'll
  ! use identical logic/loop structure as the crown-volume based logic.  User
  ! dictates which method to use via the B2(9) parameter in BETA.dat. [i.e. we
  ! maintain the "co-opt"ing of the BETA.dat file's B2(9) parameter.]
  ! If B2(9) = 1.0, use volume logic; if B2(9) = -1.0, we go to SA logic.
  ! Note: this "flagging" necessitates a change to BSTRUCTN.f subroutine as well.

  ! THis Version3 adjusts root biomass as a function of leaf biomass (as the
  ! original code (without this subroutine) does.

  ! Common blocks
  include 'ENTITY.f90'
  include 'SITE.f90'

  ! New Local Variable declarations:

  ! Variables
  real    :: VCRN(SIZE),TOTCRNVL,SACRN(SIZE),TOTCRNSA
  real    :: TEMPLA1(SIZE),TEMPLA2(SIZE),TEMPLA3(SIZE)

  ! Begin
  ! Get each entity's crown volume, surface area, and accumulate total stand
  ! crown volume and surface areas.
  !      IF (B2(9) .EQ. 1.0) THEN
  do I = 1,NB
     if ((ID(I) .eq.'T').and. (H(I) .ge. HTLIMIT)) then
        VCRN(I) = (1./3.)*3.1415*((CW(I)/2.)**2)*(CTOP(I)-CBOT(I))
        TOTCRNVL = TOTCRNVL+VCRN(I)
        SACRN(I) = 3.1415*(CW(I)/2)*&
             &SQRT(((CW(I)/2.)**2)+((CTOP(I)-CBOT(I))**2))
        TOTCRNSA = TOTCRNSA+SACRN(I)
     end if
     10 continue
  end do

  !  Calculate new LAs.based on three methods, Volume(VOL), Surface Area (SA),
  ! or hybrid (mean of VOL and SA)
  !  Note: SUMLA_LG is from BINITIAL; it represents the total leaf area on an
  !  entire hectare (i.e. the individual entity LAs have been expanded
  !  that is, they have been multiplied by EXPAND(I)

  do I = 1,NB
     if ((ID(I) .eq. 'T').and.(H(I) .ge. HTLIMIT)) then
        TEMPLA1(I) = (SUMLA_LG * (VCRN(I)/TOTCRNVL))/EXPAND(I)
        TEMPLA2(I) = (SUMLA_LG * (SACRN(I)/TOTCRNSA))/EXPAND(I)
        TEMPLA3(I) = (TEMPLA1(I)+TEMPLA2(I))/2.0
     end if
     20 continue
  end do
  !      ENDIF  !END IF B2(9) = 1.0

  ! NOW, BASED ON FLAG, ASSIGN APPROPRIATE NEW LA TO ENTITY

  ! METHOD USED DICTATED BY USER VIA B2(9) FLAG [1,-1, OR -3].

  if (B2(9) .eq.1.) then
     do I = 1,NB
        if ((ID(I) .eq. 'T').and.(H(I) .ge. HTLIMIT)) then
           LA(I) = TEMPLA1(I)
           LEAF(I) = LA(I)/B1(13,1)
           ROOT(I) = 2.0*LEAF(I)
        end if
        30 continue
     end do

  else if (B2(9) .eq. -1.) then
     do I = 1,NB
        if ((ID(I) .eq. 'T').and.(H(I) .ge. HTLIMIT)) then
           LA(I) = TEMPLA2(I)
           LEAF(I) = LA(I)/B1(13,1)
           ROOT(I) = 2.0*LEAF(I)
        end if
        40 continue
     end do

  else if (B2(9) .eq. -3.) then
     do I = 1,NB
        if ((ID(I) .eq. 'T').and.(H(I) .ge. HTLIMIT)) then
           LA(I) = TEMPLA3(I)
           LEAF(I) = LA(I)/B1(13,1)
           ROOT(I) = 2.0*LEAF(I)
        end if
        50 continue
     end do
  end if
  return
end
