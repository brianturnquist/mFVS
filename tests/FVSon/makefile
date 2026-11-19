# $Id: makefile 734 2013-03-22 22:39:10Z ncrookston.fs@gmail.com $

all : Hardwood ONFocused ONRestart ONLimit ONCrnmult

Hardwood :
	@echo "Test Hardwood (1 stands)"
	rm -f FVSOutHardwood.db
	rm -f Hardwood.out Hardwood.sng Hardwood.trl
	-../../bin/FVSon --keywordfile=Hardwood.key
	grep -v "\-999" Hardwood.sum      > Hardwood.tmp
	grep -v "\-999" Hardwood.sum.save > Hardwood.tmp.save
	-diff -w Hardwood.tmp Hardwood.tmp.save
	rm Hardwood.tmp Hardwood.tmp.save
	@echo "Hardwood output DB dump"
	rm -f FVSOutHardwoodDBdump.txt
	@echo "Hardwood output DB dump" > FVSOutHardwoodDBdump.txt
	-sqlite3 FVSOutHardwood.db ".tables" >> FVSOutHardwoodDBdump.txt
	@echo "FVS_Cases" >> FVSOutHardwoodDBdump.txt
	-sqlite3 FVSOutHardwood.db ".header on" ".mode csv" \
  "select StandID \
   from FVS_Cases" >> FVSOutHardwoodDBdump.txt
	@echo "FVS_Summary_East_Metric" >> FVSOutHardwoodDBdump.txt
	-sqlite3 FVSOutHardwood.db ".header on" ".mode col" \
  "select count(*)  \
   from FVS_Summary_East_Metric" >> FVSOutHardwoodDBdump.txt
	@echo "---FVS_CutList_East_Metric---" >> FVSOutHardwoodDBdump.txt
	-sqlite3 FVSOutHardwood.db ".header on" ".mode csv" \
  "select count(*) \
   from FVS_CutList_East_Metric" >> FVSOutHardwoodDBdump.txt
	-sqlite3 FVSOutHardwood.db ".header on" ".mode csv" \
  "select SpeciesFVS, count(SpeciesFVS), round(sum(TPH),2) \
   from FVS_CutList_East_Metric \
   group by SpeciesFVS \
   order by SpeciesFVS" >> FVSOutHardwoodDBdump.txt
	@echo "---FVS_TreeList_East_Metric---" >> FVSOutHardwoodDBdump.txt
	-sqlite3 FVSOutHardwood.db ".header on" ".mode csv" \
  "select count(*) \
   from FVS_TreeList_East_Metric" >> FVSOutHardwoodDBdump.txt
	-sqlite3 FVSOutHardwood.db ".header on" ".mode csv" \
  "select SpeciesFVS, count(SpeciesFVS), round(sum(TPH),2) \
   from FVS_TreeList_East_Metric \
   group by SpeciesFVS \
   order by SpeciesFVS" >> FVSOutHardwoodDBdump.txt
	@echo "---End of DB dump---" >> FVSOutHardwoodDBdump.txt
	-diff -w FVSOutHardwoodDBdump.txt FVSOutHardwoodDBdump.txt.save
	@echo "Test Hardwood (1 stand) ended"

# SP19 focused Canadian decks. Outputs are compared by tests/parity/check_suite.py.

ONFocused :
	@echo "Test ONFocused (2 stands, fire and volume options)"
	rm -f ONFocused.db ONFocused.out ONFocused.sum ONFocused.sng ONFocused.trl
	-../../bin/FVSon --keywordfile=ONFocused.key
	@echo "Test ONFocused ended"

ONRestart :
	@echo "Test ONRestart (straight run, then stop in 2019 and restart)"
	rm -f ONRestart.db ONRestart_full.db ONRestart.out ONRestart_full.out ONRestart.sum ONRestart_full.sum ONRestart.stop
	-../../bin/FVSon --keywordfile=ONRestart.key
	-mv ONRestart.sum ONRestart_full.sum
	-mv ONRestart.out ONRestart_full.out
	-mv ONRestart.db ONRestart_full.db
	-../../bin/FVSon --keywordfile=ONRestart.key --stoppoint=2,2019,ONRestart.stop
	-../../bin/FVSon --restart=ONRestart.stop
	-diff -w ONRestart_full.sum ONRestart.sum
	@echo "Test ONRestart ended"

ONLimit :
	@echo "Test ONLimit (5828 tree records)"
	rm -f ONLimit.db ONLimitOut.db ONLimit.out ONLimit.sum
	sqlite3 ONLimit.db "ATTACH 'FVSDataHardwood.db' AS s;   CREATE TABLE FVS_StandInit AS SELECT * FROM s.FVS_StandInit;   CREATE TABLE FVS_TreeInit AS SELECT t.* FROM s.FVS_TreeInit t,   (WITH RECURSIVE r(k) AS (SELECT 1 UNION ALL SELECT k+1 FROM r WHERE k<62) SELECT k FROM r)   ORDER BY k, t.Tree_ID;   UPDATE FVS_TreeInit SET Tree_ID = rowid, Tree_Count = Tree_Count/62.0;"
	-../../bin/FVSon --keywordfile=ONLimit.key
	@echo "Test ONLimit ended"

ONCrnmult :
	@echo "Test ONCrnmult (bug 44)"
	rm -f ONCrnmult.db ONCrnmult.out ONCrnmult.sum
	-../../bin/FVSon --keywordfile=ONCrnmult.key
	@echo "Test ONCrnmult ended"
