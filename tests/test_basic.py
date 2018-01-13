from nose.plugins.skip import SkipTest
from nose.tools import assert_raises, nottest

#@SkipTest
class TestBasic:

    def setUp(self):
        pass

    def test_something(self):
        assert 1 + 1 == 2

    def tearDown(self):
        pass
