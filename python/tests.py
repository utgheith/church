import unittest
from church import true, false, Pair, pair, fst, snd

class TestPair(unittest.TestCase):
    def test_pair(self) -> None:
        p: Pair[int, str] = pair(1, "hello")
        self.assertEqual(fst(p), 1)
        self.assertEqual(snd(p), "hello")

class TestBoolean(unittest.TestCase):
    def test_true(self) -> None:
        self.assertEqual(true(1, 2), 1)
        self.assertEqual(true("a", "b"), "a")
        self.assertEqual(true(True, False), True)

    def test_false(self) -> None:
        self.assertEqual(false(1, 2), 2)
        self.assertEqual(false("a", "b"), "b")
        self.assertEqual(false(True, False), False)

if __name__ == "__main__":
    unittest.main()