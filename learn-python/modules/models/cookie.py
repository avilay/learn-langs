
class Cookie(object):
    def __init__(self, flavor, size):
        self.flavor = flavor
        self.size = size

    def debug(self):
        print('My __name__ is ' + __name__)