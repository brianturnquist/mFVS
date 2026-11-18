"""Negative controls for MOD02's corrected/immediate comparison contract."""

import json
import math
from contextlib import closing
from pathlib import Path
import sqlite3
import tempfile
import unittest

from check_suite import compare, restart_difference


class CorrectedReferenceChecks(unittest.TestCase):
    def setUp(self) -> None:
        self.temporary = tempfile.TemporaryDirectory(prefix="mfvs-corrected-controls-")
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)
        self.left, self.right = self.root / "left", self.root / "right"
        for store in (self.left, self.right):
            (store / "files").mkdir(parents=True)
            (store / "files/model.out").write_text("MODEL VALUE 10.0\n")
            (store / "files/state.stop").write_bytes(bytes(160))
            with closing(sqlite3.connect(store / "files/model.db")) as con, con:
                con.execute("CREATE TABLE model_values(value REAL)")
                con.execute("INSERT INTO model_values VALUES(10.0)")
            (store / "outputs.json").write_text(json.dumps(["model.out", "model.db", "state.stop"]))

    def differences(self) -> list[str]:
        return compare(self.left, self.right, "ie", original_defects=False)

    def test_equal_outputs(self) -> None:
        self.assertEqual(self.differences(), [])

    def test_model_text_mutation(self) -> None:
        (self.right / "files/model.out").write_text("MODEL VALUE 11.0\n")
        self.assertTrue(self.differences())

    def test_sqlite_one_ulp_mutation(self) -> None:
        with closing(sqlite3.connect(self.right / "files/model.db")) as con, con:
            con.execute("UPDATE model_values SET value=?", (math.nextafter(10.0, math.inf),))
        self.assertTrue(self.differences())

    def test_missing_output(self) -> None:
        (self.right / "outputs.json").write_text(json.dumps(["model.out", "state.stop"]))
        (self.right / "files/model.db").unlink()
        self.assertTrue(self.differences())

    def test_corrected_decay_is_exact(self) -> None:
        left, right = self.left / "files/state.stop", self.right / "files/state.stop"
        data = bytearray(left.read_bytes())
        data[64] = 3
        left.write_bytes(data)
        # This is the documented original-side allowance, not permission to
        # mask the same changed value in a corrected or immediate reference.
        self.assertIsNone(restart_difference(left, right, "ie"))
        self.assertTrue(self.differences())

    def test_corrected_tt_location_is_exact(self) -> None:
        left, right = self.left / "files/state.stop", self.right / "files/state.stop"
        data = bytearray(right.read_bytes())
        data[108:112] = (1).to_bytes(4, "little")
        right.write_bytes(data)
        self.assertIsNone(restart_difference(left, right, "tt"))
        self.assertIsNotNone(restart_difference(left, right, "tt", original_defects=False))

    def test_run_identifier_only(self) -> None:
        for store, tail in ((self.left, "1"), (self.right, "2")):
            (store / "files/state.stop").write_bytes(
                b"header 00000000-0000-4000-8000-00000000000" + tail.encode() + b" value")
        self.assertEqual(self.differences(), [])


if __name__ == "__main__":
    unittest.main()
