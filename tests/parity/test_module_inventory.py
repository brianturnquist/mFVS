"""Check the live-source contracts that the MOD03/MOD04 index must retain."""

import unittest

from module_inventory import build_index


class ModuleInventoryChecks(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.index = build_index()
        cls.units = cls.index["units"]

    def unit(self, source: str, name: str) -> dict:
        return next(u for u in self.units if u["source"] == source and u["name"] == name)

    def test_live_sources_have_unique_units_and_families(self) -> None:
        expected = {p for p, f in self.index["files"].items() if p.endswith(".f90") and not f["include"]}
        self.assertEqual({u["source"] for u in self.units}, expected)
        self.assertEqual(len({u["id"] for u in self.units}), len(self.units))
        self.assertTrue(all(u["family"] for u in self.units))

    def test_every_common_layout_has_types_and_a_reviewed_owner(self) -> None:
        for block in self.index["common_layouts"]:
            with self.subTest(block=block["name"], source=block["source"]):
                self.assertTrue(block["proposed_owner"])
                self.assertTrue(all(block["declarations"].values()))

    def test_native_entry_include_contracts_remain_separate(self) -> None:
        entries = [e for e in self.unit("vbase/dgf.f90", "DGF")["entries"] if e["name"] == "DGCONS"]
        self.assertEqual(len(entries), 2)
        native = next(e for e in entries if e["arguments"])
        self.assertEqual(native["arguments"], ["IDTYPE"])
        self.assertEqual(native["variants"], ["em"])
        self.assertNotIn("em", next(e for e in entries if not e["arguments"])["variants"])

    def test_types_are_not_mutable_module_objects(self) -> None:
        for name, source in (("CHARMOD", "charmod"), ("CLKCOEF_MOD", "clkcoef_mod"), ("MRULES_MOD", "mrules_mod")):
            self.assertEqual(self.unit(f"volume/NVEL/{source}.f90", name)["persistent_locals"], [])
        debug = next(m for m in self.index["module_state"] if m["module"] == "DEBUG_MOD")
        self.assertIn("DEBUG", debug["declarations"])
        self.assertNotIn("MODEL", debug["declarations"])

    def test_statement_functions_and_entry_dummies_are_scoped(self) -> None:
        logs = self.unit("ak/logs.f90", "LOGS")
        self.assertIn("DIBAHI", {f["name"] for f in logs["statement_functions"]})
        self.assertTrue(any(c["name"] == "DIBAHI" and c["boundary"] == "statement-function" for c in logs["calls"]))
        crown = self.unit("canada/bc/crown.f90", "CRNMD")
        self.assertFalse(any(c["name"] == "BB" for c in crown["calls"]))  # COMMON array
        stub = self.unit("base/exmist.f90", "EXMIST")
        self.assertEqual(sum(c["name"] == "CBUFF" for c in stub["calls"]), 2)
        self.assertTrue(all(c["boundary"] == "entry-dummy" for c in stub["calls"] if c["name"] == "CBUFF"))

    def test_zero_argument_and_foreign_calls_are_indexed(self) -> None:
        calls = [c for u in self.units for c in u["calls"]]
        self.assertTrue(any(c["name"] == "PUTSTD" and not c["arguments"] for c in calls))
        self.assertTrue(any(c["name"] == "CFIM_DRIVER" and c["boundary"] == "boundary:CFIM" for c in calls))
        self.assertTrue(any(c["name"] == "FM_FOFEM" and c["boundary"] == "boundary:FOFEM" for c in calls))
        unresolved = [(u["id"], c["name"]) for u in self.units for c in u["calls"]
                      if not c["definitions"] and not c["boundary"]]
        self.assertEqual(unresolved, [])

    def test_pilot_is_checked_with_an_external_wrapper(self) -> None:
        # MOD07: the checked function lives in ISTFNB_MOD; the external keeps
        # the legacy symbol by importing it; SUMHED, the only internal caller,
        # imports the module function. Neither keeps persistent state.
        checked = self.unit("base/istfnb_mod.f90", "ISTFNB")
        self.assertEqual(checked["parent"], "base/istfnb_mod.f90:8:ISTFNB_MOD")
        self.assertEqual(checked["declarations"]["STRING"], "character(len=*), intent(in) :: STRING")
        wrapper = self.unit("base/istfnb.f90", "ISTFNB")
        self.assertEqual(wrapper["uses"], ["use ISTFNB_MOD, only: ISTFNB_CHECKED => ISTFNB"])
        for pilot in (checked, wrapper):
            self.assertEqual(pilot["family"], "MOD10:helpers")
            self.assertEqual(pilot["arguments"], ["STRING"])
            self.assertEqual(pilot["common"], [])
            self.assertEqual(pilot["persistent_locals"], [])
            self.assertEqual(pilot["entries"], [])
            self.assertEqual(len(pilot["variants"]), 24)
        sumhed = self.unit("vbase/sumhed.f90", "SUMHED")
        self.assertIn("use ISTFNB_MOD, only: ISTFNB", sumhed["uses"])
        callers = [(u["source"], c) for u in self.units for c in u["calls"] if c["name"] == "ISTFNB"]
        self.assertEqual(len(callers), 1)
        self.assertEqual(callers[0][0], "vbase/sumhed.f90")
        self.assertEqual(callers[0][1]["arguments"], ["ITITLE"])


if __name__ == "__main__":
    unittest.main()
