"""
$ python asserts.py user
Will result in the AssertionError as expected. However, running the command like -

$ python -OO asserts.py user
Will not throw any errors because -OO is running Python in optimized mode and that will remove
all asserts from the code.
"""

import sys
from collections import namedtuple

User = namedtuple('User', ['is_admin'])


def system_shutdown(user):
    assert user.is_admin, 'Only admin can shutdown the system!'
    print('System shutdown initiated')


def main():
    try:
        user = User(is_admin=sys.argv[1] == 'admin')
        system_shutdown(user)
    except AssertionError as ae:
        print(ae)


if __name__ == '__main__':
    main()
