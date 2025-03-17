import pytest
from user import User


class DataGenerator:
    def __init__(self):
        self._user = User(username='happy', password='orange')
        self._user._bookmarks.append('http://www.avilaylabs.net')
        self._user._bookmarks.append('http://www.microsoft.com')
        self._dead_links = [
            'http://avilayparekh.com',  # The domain does not exist
            'http://avilaylabs.net/funnypage'  # The URL does not exist
        ]
        self._live_link = 'http://www.google.com'
        self._bad_link = 'hahaha'

    def get_user(self):
        return self._user

    def get_dead_links(self):
        return self._dead_links

    def get_live_link(self):
        return self._live_link

    def get_bad_link(self):
        return self._bad_link


data_gen = DataGenerator()


@pytest.fixture
def user():
    return data_gen.get_user()


@pytest.fixture
def dead_links():
    return data_gen.get_dead_links()


@pytest.fixture
def live_link():
    return data_gen.get_live_link()


@pytest.fixture
def bad_link():
    return data_gen.get_bad_link()