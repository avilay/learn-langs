import requests


class User:
    def __init__(self, **kwargs):
        self.username = kwargs['username']
        self.password = kwargs['password']
        self._bookmarks = []

    @property
    def bookmarks(self):
        return self._bookmarks

    def _validate(self, link):
        try:
            res = requests.get(link)
            return res.status_code == 200
        except requests.exceptions.ConnectionError:
            return False

    def add_bookmark(self, link):
        if self._validate(link):
            self.bookmarks.append(link)
            return True
        else:
            return False

    def search(self, filter):
        matches = []
        for bookmark in self._bookmarks:
            if bookmark.find(filter) > -1:
                matches.append(bookmark)
        return matches


def main():
    user = User(username='happy', password='orange')
    print(user._validate('http://avilayparekh.com'))


if __name__ == '__main__':
    main()