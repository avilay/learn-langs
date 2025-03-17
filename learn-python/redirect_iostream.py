import sys
from io import StringIO
import logging


def main():
    # Setting the log config before I munge the stream objects will not work
    # because when basicConfig creates a StreamHandler it has already taken a reference
    # of the real streams.
    # logging.basicConfig(level=logging.DEBUG)
    real_stderr = sys.stderr
    my_stderr = StringIO()
    try:
        sys.stderr = my_stderr
        # Setting the log config here will work as expected because now when StreamHandler goes to take a reference
        # of the streams, it gets my fake streams instead.
        logging.basicConfig(level=logging.DEBUG)
        logger = logging.getLogger()
        logger.info('Hello World')
        print('Something else')
    finally:
        sys.stderr = real_stderr
        output = my_stderr.getvalue()
        my_stderr.close()
    print('\nFrom the redirected stream')
    print(output)


if __name__ == '__main__':
    main()
