!=========================!
!   fire/base : fmbrkt   !
!=========================!
! FIRE-BASE $Id$
function FMBRKT(DBH,ISP)
  implicit none

  !     COMPUTES THE BARK THICKNESS FOR USE IN THE FIRE-CAUSED MORTALITY
  !     ROUTINE (FMEFF). DATA ARE FROM FOFEM V5.0 (REINHARDT ET AL. 2000).

  !     BARK THICKNESS = DBH * B1(IEQ), WHERE B1 HOLDS THE 39 FOFEM BARK
  !     THICKNESS COEFFICIENTS AND IEQ IS THE FOFEM EQUATION ASSIGNED TO
  !     EACH SPECIES OF THE CURRENT VARIANT (TABLES EQxx BELOW).

  !     IN CS (SPECIES 3) AND SN (SPECIES 5), SHORTLEAF PINE USES THE BARK
  !     THICKNESS EQUATION FROM HARMON, SURVIVAL OF TREES AFTER LOW-
  !     INTENSITY SURFACE FIRES IN GREAT SMOKY MOUNTAINS NATIONAL PARK,
  !     ECOLOGY 65(3), 1984, BECAUSE USERS ON THE OZARK NF THOUGHT THE
  !     FIRE-RELATED MORTALITY FOR SHORTLEAF WAS TOO HIGH.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'

  ! Variables
  integer :: ISP,IEQ
  real    :: DBH,FMBRKT

  !     B1(0) IS USED ONLY FOR UNUSED SPECIES SLOTS (---) IN OP, PN AND WC.
  real, parameter :: B1(0:39) = (/-0.000,&
       &.019, .022, .024, .025, .026, .027, .028, .029, .03, .031, .032, .033, .034,&
       &.035, .036, .037, .038, .039, .04, .041, .042, .043, .044, .045, .046, .047,&
       &.048, .049, .05, .052, .055, .057, .059, .06, .062, .063, .068, .072, .081/)

  !     AK SPECIES:
  !         1 pacific silver fir; 2 subalpine fir; 3 alaska-cedar; 4 tamarack; 5 white spruce
  !         6 Lutz's spruce; 7 black spruce; 8 sitka spruce; 9 lodgepole pine; 10 western redcedar
  !         11 western hemlock; 12 mountain hemlock; 13 other softwoods; 14 alder species
  !         15 red alder; 16 paper birch; 17 Alaska birch; 18 balsam poplar; 19 quaking aspen
  !         20 black cottonwood; 21 willow species; 22 Scouler's willow; 23 other hardwoods
  integer, parameter :: EQAK(23) = (/&
       &26,20,2,10,4,4,11,6,7,14,19,19,4,5,5,6,6,19,23,23,&
       &20,20,23/)

  !     BC SPECIES:
  !         1 white pine; 2 western larch; 3 Douglas-fir; 4 grand fir; 5 western hemlock
  !         6 western redcedar; 7 lodgepole pine; 8 Engelmann spruce; 9 subalpine fir
  !         10 ponderosa pine; 11 birch (from ie variant); 12 aspen (from ie variant)
  !         13 cottonwood (from ie variant); 14 other conifer (FD); 15 other hardwood (birch)
  integer, parameter :: EQBC(15) = (/&
       &14,36,36,25,19,14,7,15,20,36,6,23,17,36,6/)

  !     BM SPECIES:
  !         1 white pine; 2 western larch; 3 Douglas fir; 4 grand fir; 5 mountain hemlock
  !         6 western juniper (use so wj); 7 lodgepole pine; 8 Engelmann spruce; 9 subalpine fir
  !         10 ponderosa pine; 11 whitebark pine (use so wb); 12 limber pine (use so wb)
  !         13 pacific yew (use so py); 14 Alaska cedar (use wc yc); 15 quaking aspen (use so as)
  !         16 black cottonwood (use so cw); 17 other softwoods -  use BM ponderosa pine
  !         18 other hardwoods - used SO OH (WC other (AS))
  integer, parameter :: EQBM(18) = (/&
       &14,36,36,25,19,4,7,15,20,36,9,9,4,2,23,23,36,23/)

  !     CA SPECIES:
  !         1 Port Orford cedar (pc); 2 incense cedar (ic); 3 western redcedar (rc)
  !         4 white fir (wf); 5 California red fir (rf); 6 Shasta red fir (sh); 7 Douglas-fir (df)
  !         8 western hemlock (wh); 9 mountain hemlock (mh); 10 whitebark pine (wb)
  !         11 knobcone pine (kp); 12 lodgepole pine (lp); 13 Coulter pine (cp) ! PP
  !         14 limber pine (lm); 15 Jeffrey pine (jp); 16 sugar pine (sp)
  !         17 western white pine (wp); 18 ponderosa pine (pp); 19 Monterey pine (mp) ! KP
  !         20 gray pine (gp); 21 western juniper (ju); 22 Brewer spruce (br) ! white spruce
  !         23 giant sequoia (gs); 24 Pacific yew (py); 25 other softwoods (os) ! PP
  !         26 coast live oak (lo); 27 canyon live oak (cy); 28 blue oak (bl)
  !         29 Engelmann oak (eo); 30 Oregon white oak (wo); 31 California black oak (bo)
  !         32 valley white oak (vo); 33 interior live oak (io); 34 bigleaf maple (bm)
  !         35 California buckeye (bu) ! Ohio buckeye; 36 red alder (ra); 37 pacific madrone (ma)
  !         38 golden chinkapin (gc); 39 pacific dogwood (dg); 40 oregon ash (fl) ! Ash spp
  !         41 walnut (wn); 42 tanoak (to); 43 California sycamore (sy) ! American sycamore
  !         44 quaking aspen (as); 45 black cottonwood (cw); 46 willow (wi) ! Salix spp.
  !         47 California nutmeg (cn) ! PY; 48 California laurel (cl)
  !         49 other hardwoods (oh) ! BO; 50 coast redwood (RW)
  integer, parameter :: EQCA(50) = (/&
       &39,34,14,27,18,18,36,14,19,9,9,7,36,9,37,38,14,36,9,12,&
       &4,4,39,4,36,29,3,12,33,8,9,22,13,3,15,5,34,24,35,21,&
       &20,30,12,23,23,20,4,5,9,39/)

  !     CI SPECIES:
  !         1 white pine; 2 western larch; 3 Douglas-fir; 4 grand fir; 5 western hemlock
  !         6 western redcedar; 7 lodgepole pine; 8 Engelmann spruce; 9 subalpine fir
  !         10 ponderosa pine; 11 whitebark pine uses UT WB/LM = pinus spp.
  !         12 pacific yew uses IE PY; 13 quaking aspen uses UT AS; 14 western juniper uses UT WJ
  !         15 curl-leaf mt. mahog.- uses WC-other (AS); 16 limber pine uses UT WB/LM = pinus spp.
  !         17 cottonwood, uses populus spp.; 18 other softwoods (mountain hemlock)
  !         19 other hardwoods, uses populus spp.
  integer, parameter :: EQCI(19) = (/&
       &14,36,36,25,19,14,7,15,20,36,9,4,23,4,23,9,17,19,17/)

  !     CR SPECIES:
  !         1 subalpine fir; 2 corkbark fir - use subalpine fir; 3 Douglas-fir; 4 grand fir
  !         5 white fir; 6 mountain hemlock; 7 western redcedar; 8 western larch
  !         9 bristlecone pine - use Pinus sp; 10 limber pine; 11 lodgepole pine
  !         12 pinyon pine - use Pinus sp; 13 ponderosa pine; 14 whitebark pine
  !         15 southwestern white pine - use white pine; 16 Utah juniper; 17 blue spruce
  !         18 Engelmann spruce; 19 white spruce; 20 quaking aspen
  !         21 narrowleaf cottonwood - use Populus sp; 22 plains cottonwood - use Populus sp
  !         23 gambel oak - use Quercus sp; 24 Arizona white oak - use Quercus sp
  !         25 emory oak - use Quercus sp; 26 bur oak - use Quercus sp
  !         27 silverleaf oak - use Quercus sp; 28 paper birch - use quaking aspen
  !         29 alligator juniper - use Utah juniper; 30 Rocky Mountain juinper - use Utah juniper
  !         31 oneseed juniper - use Utah juniper; 32 Eastern redcedar - use Utah juniper
  !         33 singleleaf pinyon - use Pinus sp; 34 border pinyon - use Pinus sp
  !         35 Arizona pinyon - use Pinus sp; 36 Chihuahua pine - use ponderosa pine
  !         37 other softwoods - use Pinus sp; 38 other hardwoods - use Populus sp
  integer, parameter :: EQCR(38) = (/&
       &20,20,36,25,27,19,14,36,9,9,7,9,36,9,14,4,10,15,4,23,&
       &17,17,24,24,24,24,24,23,4,4,4,4,9,9,9,36,9,17/)

  !     CS SPECIES: SEE THE CS BLKDAT SPECIES LIST
  integer, parameter :: EQCS(96) = (/&
       &17,12,16,12,30,24,24,20,20,4,16,9,18,23,19,22,22,16,23,23,&
       &16,15,19,4,14,16,9,19,7,13,10,9,10,15,15,10,18,17,11,12,&
       &20,17,12,21,21,18,19,21,23,24,19,16,21,24,21,25,23,23,28,20,&
       &23,20,18,15,9,20,16,17,14,15,16,20,17,19,18,23,28,12,12,4,&
       &8,15,20,19,20,9,14,20,17,10,16,15,19,12,16,15/)

  !     EC SPECIES:
  !         1 white pine; 2 larch; 3 Douglas-fir; 4 pacific silver fir; 5 western redcedar
  !         6 grand fir; 7 lodgepole pine; 8 Englemann spruce; 9 subalpine fir; 10 ponderosa pine
  !         11 western hemlock from WC; 12 mountain hemlock from WC; 13 Pacific yew from WC
  !         14 whitebark pine from WC; 15 noble fir from WC; 16 white fir - use grand fir
  !         17 subalpine larch from WC; 18 Alaska cedar from WC
  !         19 western juniper from WC juniper; 20 bigleaf maple from WC
  !         21 vine maple from WC bigleaf maple; 22 red alder from WC; 23 paper birch from WC
  !         24 golden chinkapin from WC; 25 Pacific dogwood from WC; 26 quaking aspen from WC
  !         27 black cottonwood from WC; 28 Oregon white oak from WC
  !         29 cherry and plum species from WC bitter cherry; 30 willow species from WC
  !         31 other softwoods - use mountain hemlock; 32 other hardwoods - use aspen
  integer, parameter :: EQEC(32) = (/&
       &14,36,36,26,14,25,7,15,20,36,19,19,4,9,24,25,29,2,4,3,&
       &3,5,6,24,35,23,23,8,35,20,19,23/)

  !     EM SPECIES:
  !         1 whitebark pine; 2 larch; 3 Douglas-fir; 4 limber pine, use IE limber pine
  !         5 subalpine larch, use IE subalpine larch; 6 Rocky Mountain juniper, use ie RM juniper
  !         7 lodgepole pine; 8 Englemann spruce; 9 subalpine fir; 10 ponderosa pine
  !         11 green ash, use IE cottonwood; 12 quaking aspen, use IE quaking aspen
  !         13 black cottonwood, use IE cottonwood; 14 balsam poplar, use IE cottonwood
  !         15 plains cottonwood, use IE cottonwood; 16 narrowleaf cottonwood, use IE cottonwood
  !         17 paper birch, use IE paper birch; 18 other softwoods - use western juniper
  !         19 other hardwoods, use IE cottonwood
  integer, parameter :: EQEM(19) = (/&
       &9,36,36,9,29,4,7,15,20,36,17,23,17,17,17,17,6,4,17/)

  !     IE SPECIES:
  !         1 white pine; 2 western larch; 3 Douglas-fir; 4 grand fir; 5 western hemlock
  !         6 western redcedar; 7 lodgepole pine; 8 Engelmann spruce; 9 subalpine fir
  !         10 ponderosa pine; 11 mountain hemlock; 12 whitebark pine; 13 limber pine
  !         14 subalpine larch; 15 pinyon pine, uses pinus spp.; 16 Rocky Mountain juniper
  !         17 pacific yew; 18 quaking aspen; 19 cottonwood, uses populus spp.; 20 mountain maple
  !         21 paper birch; 22 other hardwoods; 23 other softwoods
  integer, parameter :: EQIE(23) = (/&
       &14,36,36,25,19,14,7,15,20,36,19,9,9,29,9,4,4,23,17,19,&
       &6,5,19/)

  !     KT SPECIES:
  !         1 white pine; 2 western larch; 3 Douglas-fir; 4 grand fir; 5 western hemlock
  !         6 western redcedar; 7 lodgepole pine; 8 Engelmann spruce; 9 subalpine fir
  !         10 ponderosa pine; 11 other (mountain hemlock)
  integer, parameter :: EQKT(11) = (/&
       &14,36,36,25,19,14,7,15,20,36,19/)

  !     LS SPECIES: SEE THE LS BLKDAT SPECIES LIST
  integer, parameter :: EQLS(68) = (/&
       &19,9,22,22,24,4,8,10,11,10,4,18,17,17,14,18,19,10,7,9,&
       &10,11,12,10,17,12,14,4,21,19,24,21,21,21,24,17,16,16,19,18,&
       &23,19,6,20,20,20,16,28,13,13,24,19,9,19,15,20,17,22,18,12,&
       &24,19,19,20,19,19,14,19/)

  !     NC SPECIES:
  !         1 other conifers - use Douglas-fir; 2 sugar pine; 3 Douglas-fir; 4 white fir
  !         5 madrone; 6 incense cedar; 7 California black oak; 8 tanoak; 9 red fir
  !         10 ponderosa pine; 11 other hardwood - use tanoak
  !         12 coast redwood - use giant sequoia/redwood from WS
  integer, parameter :: EQNC(12) = (/&
       &36,38,36,27,34,34,9,30,18,36,30,39/)

  !     NE SPECIES: SEE THE NE BLKDAT SPECIES LIST
  integer, parameter :: EQNE(108) = (/&
       &10,10,4,13,8,11,13,22,24,30,12,4,4,17,12,18,19,9,19,16,&
       &19,24,35,9,9,7,12,14,10,10,9,8,6,12,23,16,22,19,22,4,&
       &21,21,14,18,16,20,15,15,23,19,19,18,29,9,19,21,21,23,24,19,&
       &20,15,20,28,24,25,21,23,24,23,19,15,29,29,15,20,21,20,20,16,&
       &18,19,22,9,18,15,29,12,20,28,19,14,19,29,18,10,11,24,13,24,&
       &29,29,9,20,17,16,29,24/)

  !     OC SPECIES:
  !         1 Port Orford cedar (pc); 2 incense cedar (ic); 3 western redcedar (rc)
  !         4 grand fir (gf); 5 California red fir (rf); 6 Shasta red fir (sh); 7 Douglas-fir (df)
  !         8 western hemlock (wh); 9 mountain hemlock (mh); 10 whitebark pine (wb)
  !         11 knobcone pine (kp); 12 lodgepole pine (lp); 13 Coulter pine (cp) ! PP
  !         14 limber pine (lm); 15 Jeffrey pine (jp); 16 sugar pine (sp)
  !         17 western white pine (wp); 18 ponderosa pine (pp); 19 Monterey pine (mp) ! KP
  !         20 gray pine (gp); 21 western juniper (ju); 22 Brewer spruce (br) ! white spruce
  !         23 giant sequoia (gs); 24 Pacific yew (py); 25 other softwoods (os) ! PP
  !         26 coast live oak (lo); 27 canyon live oak (cy); 28 blue oak (bl)
  !         29 Engelmann oak (eo); 30 Oregon white oak (wo); 31 California black oak (bo)
  !         32 valley white oak (vo); 33 interior live oak (io); 34 bigleaf maple (bm)
  !         35 California buckeye (bu) ! Ohio buckeye; 36 red alder (ra); 37 pacific madrone (ma)
  !         38 golden chinkapin (gc); 39 pacific dogwood (dg); 40 oregon ash (fl) ! Ash spp
  !         41 walnut (wn); 42 tanoak (to); 43 California sycamore (sy) ! American sycamore
  !         44 quaking aspen (as); 45 black cottonwood (cw); 46 willow (wi) ! Salix spp.
  !         47 California nutmeg (cn) ! PY; 48 California laurel (cl)
  !         49 other hardwoods (oh) ! BO; 50 redwood (rw)
  integer, parameter :: EQOC(50) = (/&
       &39,34,14,25,18,18,36,14,19,9,9,7,36,9,37,38,14,36,9,12,&
       &4,4,39,4,36,29,3,12,33,8,9,22,13,3,15,5,34,24,35,21,&
       &20,30,12,23,23,20,4,5,9,39/)

  !     ON SPECIES: SEE THE ON BLKDAT SPECIES LIST
  integer, parameter :: EQON(72) = (/&
       &19,9,22,22,24,4,8,10,11,10,4,18,17,17,14,18,19,10,7,9,&
       &10,11,12,10,17,12,14,4,21,19,24,21,21,21,24,17,16,16,19,18,&
       &23,19,6,20,20,20,16,28,13,13,24,19,9,19,15,20,17,22,18,12,&
       &24,19,19,20,19,19,14,19,19,24,4,11/)

  !     OP SPECIES:
  !         1 Pacific silver fir; 2 white fir; 3 grand fir; 4 subalpine fir
  !         5 California red fir/Shasta red fir; 6 Sitka spruce; 7 noble fir
  !         8 Alaska cedar/western larch; 9 incense cedar; 10 Engelmann spruce; 11 lodgepole pine
  !         12 Jeffrey pine; 13 sugar pine; 14 western white pine; 15 ponderosa pine
  !         16 Douglas-fir; 17 coast redwood; 18 western redcedar; 19 western hemlock
  !         20 mountain hemlock; 21 bigleaf maple; 22 red alder; 23 pacific madrone; 24 tanoak
  !         25 giant chinkapin; 26 quaking aspen; 27 black cottonwood
  !         28 Oregon white oak/California black oak; 29 juniper; 30 subalpine larch
  !         31 whitebark pine; 32 knobcone pine; 33 Pacific yew; 34 Pacific dogwood
  !         35 hawthorn (r4 definition); 36 bitter cherry; 37 willow; 38 ---; 39 other (AS)
  integer, parameter :: EQOP(39) = (/&
       &26,27,25,20,18,6,24,2,39,15,7,37,38,14,36,36,39,14,19,19,&
       &3,5,34,24,24,23,23,8,4,29,9,9,4,35,17,35,20,0,23/)

  !     PN SPECIES:
  !         1 Pacific silver fir; 2 white fir; 3 grand fir; 4 subalpine fir
  !         5 California red fir/Shasta red fir; 6 Sitka spruce; 7 noble fir
  !         8 Alaska cedar/western larch; 9 incense cedar; 10 Engelmann spruce; 11 lodgepole pine
  !         12 Jeffrey pine; 13 sugar pine; 14 western white pine; 15 ponderosa pine
  !         16 Douglas-fir; 17 coast redwood; 18 western redcedar; 19 western hemlock
  !         20 mountain hemlock; 21 bigleaf maple; 22 red alder; 23 white alder/pacific madrone
  !         24 paper birch; 25 giant chinkapin/tanoak; 26 quaking aspen; 27 black cottonwood
  !         28 Oregon white oak/California black oak; 29 juniper; 30 subalpine larch
  !         31 whitebark pine; 32 knobcone pine; 33 Pacific yew; 34 Pacific dogwood
  !         35 hawthorn (r4 definition); 36 bitter cherry; 37 willow; 38 ---; 39 other (AS)
  integer, parameter :: EQPN(39) = (/&
       &26,27,25,20,18,6,24,2,39,15,7,37,38,14,36,36,39,14,19,19,&
       &3,5,34,6,24,23,23,8,4,29,9,9,4,35,17,35,20,0,23/)

  !     SN SPECIES: SEE THE SN BLKDAT SPECIES LIST
  integer, parameter :: EQSN(90) = (/&
       &30,17,13,14,16,31,14,28,19,24,35,24,30,12,4,21,18,8,13,7,&
       &10,12,15,12,9,9,19,16,15,14,20,20,4,21,21,14,18,17,17,17,&
       &21,20,20,15,20,18,15,12,19,12,22,17,9,18,16,16,15,17,12,19,&
       &18,9,19,19,23,23,16,15,18,16,25,21,15,28,21,16,23,24,22,28,&
       &19,14,17,18,10,10,11,17,24,24/)

  !     SO SPECIES:
  !         1 white pine; 2 sugar pine; 3 Douglas-fir; 4 white fir/grand fir - use white fir
  !         5 mountain hemlock; 6 incense cedar (use NI western redcedar); 7 lodgepole pine
  !         8 Englemann spruce; 9 red fir/subalpine fir - use red fir; 10 ponderosa pine
  !         11 western juniper; 12 grand fir; 13 subalpine fir; 14 pacific silver fir
  !         15 noble fir; 16 whitebark pine; 17 western larch; 18 western redcedar
  !         19 western hemlock; 20 pacific yew; 21 white alder; 22 red alder; 23 big leaf maple
  !         24 quaking aspen; 25 black cottonwood; 26 bitter cherry; 27 oregon white oak
  !         28 willow; 29 giant chinkapin; 30 curl-leaf mt. mahog.- used WC-other (AS)
  !         31 birch-leaf mt. mahog.- used WC-other (AS); 32 other softwoods- used Douglas-fir
  !         33 other hardwoods- used WC-other (AS)
  integer, parameter :: EQSO(33) = (/&
       &14,38,36,27,19,34,7,15,18,36,4,25,20,26,24,9,36,14,19,4,&
       &35,5,3,23,23,35,8,20,24,23,23,36,23/)

  !     TT SPECIES:
  !         1 whitebark pine; 2 limber pine; 3 Douglas-fir; 4 singleleaf pinyon - use Pinus sp
  !         5 blue spruce; 6 quaking aspen; 7 lodgepole pine; 8 Engelmann spruce; 9 subalpine fir
  !         10 ponderosa pine - use CI (NI) PP; 11 Utah juniper - use UT western juniper
  !         12 Rocky Mountain juniper - use UT western juniper
  !         13 bigtooth maple - use SO big leaf maple
  !         14 Rocky Mountain maple - use IE MM (NI-mtn. hemlock)
  !         15 narrowleaf cottonwood - use CR Populus sp
  !         16 curlleaf mountain-mahogany - use SO (WC-other(AS))
  !         17 other softwoods - use whitebark pine; 18 other hardwoods - use CR Populus sp
  integer, parameter :: EQTT(18) = (/&
       &9,9,36,9,15,23,7,15,20,36,4,4,3,19,17,23,9,17/)

  !     UT SPECIES:
  !         1 whitebark pine; 2 limber pine; 3 Douglas-fir; 4 white fir; 5 blue spruce
  !         6 quaking aspen; 7 lodgepole pine; 8 Engelmann spruce; 9 subalpine fir
  !         10 ponderosa pine; 11 common pinyon - use Pinus sp; 12 western juniper
  !         13 Gambel oak - use Quercus sp; 14 singleleaf pinyon - use Pinus sp
  !         15 Rocky Mtn juniper - use western juniper; 16 Utah juniper - use western juniper
  !         17 GB bristlecone pine - from CR, uses Pinus sp
  !         18 narrowleaf cottonwood - from CR, uses Populus sp
  !         19 Fremont cottonwood - from CR, uses Populus sp
  !         20 curl-leaf mt. mahog.- from SO, uses WC-other (AS)
  !         21 bigtooth maple - from SO, uses big leaf maple
  !         22 box elder - from CR, uses Populus sp; 23 other softwoods - use whitebark pine
  !         24 other hardwoods - use Quercus sp
  integer, parameter :: EQUT(24) = (/&
       &9,9,36,27,10,23,7,15,20,36,9,4,24,9,4,4,9,17,17,23,&
       &3,17,9,24/)

  !     WC SPECIES:
  !         1 Pacific silver fir; 2 white fir; 3 grand fir; 4 subalpine fir
  !         5 California red fir/Shasta red fir; 6 ---; 7 noble fir; 8 Alaska cedar/western larch
  !         9 incense cedar; 10 Engelmann spruce/Sitka spruce - use ES; 11 lodgepole pine
  !         12 Jeffrey pine; 13 sugar pine; 14 western white pine; 15 ponderosa pine
  !         16 Douglas-fir; 17 coast redwood; 18 western redcedar; 19 western hemlock
  !         20 mountain hemlock; 21 bigleaf maple; 22 red alder; 23 white alder/pacific madrone
  !         24 paper birch; 25 giant chinkapin/tanoak; 26 quaking aspen; 27 black cottonwood
  !         28 Oregon white oak/California black oak; 29 juniper; 30 subalpine larch
  !         31 whitebark pine; 32 knobcone pine; 33 Pacific yew; 34 Pacific dogwood
  !         35 hawthorn (r4 definition); 36 bitter cherry; 37 willow; 38 ---; 39 other (AS)
  integer, parameter :: EQWC(39) = (/&
       &26,27,25,20,18,0,24,2,39,15,7,37,38,14,36,36,39,14,19,19,&
       &3,5,34,6,24,23,23,8,4,29,9,9,4,35,17,35,20,0,23/)

  !     WS SPECIES:
  !         1 sugar pine; 2 Douglas-fir; 3 white fir; 4 giant sequoia; 5 incense cedar
  !         6 Jeffrey pine; 7 California red fir; 8 ponderosa pine; 9 lodgepole pine CA
  !         10 whitebark pine use whitebark pine CA
  !         11 western white pine use western white pine CA
  !         12 singleleaf pinyon use Pinus sp. (as done in UT)
  !         13 Pacific silver fir use Pacific silver fir SO; 14 knobcone pine CA
  !         15 foxtail pine use knobcone pine CA
  !         16 Coulter pine use ponderosa pine (as done in CA)
  !         17 limber pine use knobcone pine CA; 18 Monterey pine use Monterey pine CA
  !         19 gray or California foothill pine use gray pine CA
  !         20 washoe pine use knobcone pine CA; 21 GB bristlecone pine use UT GB = CR Pinus sp
  !         22 bigcone Douglas-fir use Douglas-fir; 23 redwood use giant sequoia
  !         24 mountain hemlock use CA mountain hemlock; 25 western juniper use CA western juniper
  !         26 Utah juniper use CA western juniper; 27 California juniper CA western juniper
  !         28 California live oak use CA California live oak
  !         29 canyon live oak use CA canyon live oak; 30 blue oak use CA blue oak
  !         31 California black oak use corkbark oak
  !         32 California white oak/valley oak use CA valley oak
  !         33 interior live oak use CA interior live oak; 34 tanoak
  !         35 giant chinkapin use CA/SO giant chinkapin; 36 quaking aspen use CA/SO quaking aspen
  !         37 California-laurel use CA California-laurel
  !         38 Pacific madrone use CA/NC Pacific madrone
  !         39 Pacific dogwood use CA Pacific dogwood; 40 bigleaf maple use CA/SO bigleaf maple
  !         41 curl-leaf mt. mahog use SO MC = WC-other (AS)
  !         42 other softwoods use lodgepole pine; 43 other hardwoods use corkbark oak
  integer, parameter :: EQWS(43) = (/&
       &38,36,27,39,34,37,18,36,7,9,14,9,26,9,9,36,9,9,12,9,&
       &9,36,39,19,4,4,4,29,3,12,9,22,13,30,24,23,5,34,35,3,&
       &23,7,9/)

  select case (VARACD)
  case ('AK')
     IEQ = EQAK(ISP)
  case ('BC')
     IEQ = EQBC(ISP)
  case ('BM')
     IEQ = EQBM(ISP)
  case ('CA')
     IEQ = EQCA(ISP)
  case ('CI')
     IEQ = EQCI(ISP)
  case ('CR')
     IEQ = EQCR(ISP)
  case ('CS')
     IEQ = EQCS(ISP)
  case ('EC')
     IEQ = EQEC(ISP)
  case ('EM')
     IEQ = EQEM(ISP)
  case ('IE')
     IEQ = EQIE(ISP)
  case ('KT')
     IEQ = EQKT(ISP)
  case ('LS')
     IEQ = EQLS(ISP)
  case ('NC')
     IEQ = EQNC(ISP)
  case ('NE')
     IEQ = EQNE(ISP)
  case ('OC')
     IEQ = EQOC(ISP)
  case ('ON')
     IEQ = EQON(ISP)
  case ('OP')
     IEQ = EQOP(ISP)
  case ('PN')
     IEQ = EQPN(ISP)
  case ('SN')
     IEQ = EQSN(ISP)
  case ('SO')
     IEQ = EQSO(ISP)
  case ('TT')
     IEQ = EQTT(ISP)
  case ('UT')
     IEQ = EQUT(ISP)
  case ('WC')
     IEQ = EQWC(ISP)
  case ('WS')
     IEQ = EQWS(ISP)
  case default
     IEQ = EQIE(ISP)
  end select

  FMBRKT = DBH*B1(IEQ)

  if ((VARACD .eq. 'CS' .and. ISP .eq. 3) .or.&
       &(VARACD .eq. 'SN' .and. ISP .eq. 5)) then
     FMBRKT = (.07 + .09*DBH*2.54 - .0001*DBH*DBH*2.54*2.54)/2.54
     FMBRKT = MAX(0.,FMBRKT)
  end if

  return
end function FMBRKT
