from requests.exceptions import MissingSchema
import pytest
from data_gen import *  # NOQA


def test_live_link(user, live_link):
    if live_link in user.bookmarks:
        return
    assert user.add_bookmark(live_link)
    assert live_link in user.bookmarks  # def test_dead_link():


def test_dead_link(user, dead_links):
    for dead_link in dead_links:
        assert not user.add_bookmark(dead_link)
        assert dead_link not in user.bookmarks


def test_bad_link(user, bad_link):
    with pytest.raises(MissingSchema):
        user.add_bookmark(bad_link)
