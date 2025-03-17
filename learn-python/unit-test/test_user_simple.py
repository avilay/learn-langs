# TEST - Add a valid bookmark
# First get all existing bookmarks and check that valid bookmark is not already present
# Add a valid bookmark. It should return True
# Get bookmarks again and check for presence of valid bookmark

# TEST - Add dead link
# First get all existing bookmarks and check that invalid bookmark is not present
# Add an invalid bookmark. It should return False
# Get bookmarks again and check for absence of valid bookmark

# TEST - Add a non-url string
# Throws a MissingSchema exception

import pytest
from requests.exceptions import MissingSchema
from user import User


def get_test_user():
    user = User(username='happy', password='orange')
    user._bookmarks.append('http://www.avilaylabs.net')
    user._bookmarks.append('http://www.microsoft.com')
    return user


def test_live_link():
    user = get_test_user()
    live_link = 'http://www.google.com'
    if live_link in user.bookmarks:
        return
    assert user.add_bookmark(live_link)
    assert live_link in user.bookmarks


def test_dead_link():
    user = get_test_user()
    dead_links = [
        'http://avilayparekh.com',  # The domain does not exist
        'http://avilaylabs.net/funnypage'  # The URL does not exist
    ]
    for dead_link in dead_links:
        assert not user.add_bookmark(dead_link)
        assert dead_link not in user.bookmarks


def test_bad_link():
    user = get_test_user()
    bad_link = 'hahaha'
    with pytest.raises(MissingSchema):
        user.add_bookmark(bad_link)

