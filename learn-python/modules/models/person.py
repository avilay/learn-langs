import os
from models.cookie import Cookie


class Person(object):
    def __init__(self):
        self.fname = ''
        self.lname = ''
        self.twitter_handle = ''


if __name__ == '__main__':
    print('My current working dir is ' + os.getcwd())
    c = Cookie('choclate', 10)
    c.debug()

