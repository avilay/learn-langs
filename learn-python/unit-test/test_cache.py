from cache import use_cache
from unittest.mock import patch


# Mock a specific method to return a fixed value regardless of the input
@patch('cache.Cache.read')
def test_use_cache_1(cache_mock_read):
    cache_mock_read.return_value = 100
    use_cache()
    assert True


# Mock a specific method to return a series of values in order of calling
# but keep ignoring the input
@patch('cache.Cache.read')
def test_use_cache_2(cache_mock_read):
    cache_mock_read.side_effect = [5, 4, 3, 2]
    use_cache()
    assert True


# Mock a specific method to return values based on inputs
@patch('cache.Cache.read')
def test_use_cache_3(cache_mock_read):
    def mock_read(key):
        if key == 'key one':
            return 100
        elif key == 'key two':
            return 200
        else:
            return 500

    cache_mock_read.side_effect = mock_read
    use_cache()


# Mock the full object
@patch('cache.Cache')
def test_use_cache_4(mock_cache):
    obj = mock_cache.return_value
    obj.read.return_value = 100
    obj.read_all.return_value = 'ALL'
    use_cache()
    print(obj._capacity)

