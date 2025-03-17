import logging
import json
import sys
import cookie
from desserts.ladoo import Ladoo

def simple_config():
    lvl = sys.argv[1] if len(sys.argv) > 1 else 'info'
    loglevel = getattr(logging, lvl.upper())
    logformat = '%(asctime)s:%(levelname)s:%(name)s:%(message)s'
    logging.basicConfig(format=logformat, level=loglevel, filename='example.log')


def advanced_config(rootnamespace):
    lvl = sys.argv[1] if len(sys.argv) > 1 else 'info'
    loglevel = getattr(logging, lvl.upper())
    # logformat = '%(asctime)s:%(levelname)s:%(name)s:%(message)s'
    logformat = dict(
        timestamp='%(asctime)s',
        level='%(levelname)s',
        module='%(name)s',
        message='%(message)s'
    )
    formatter = logging.Formatter(json.dumps(logformat))

    fh = logging.FileHandler('example.log')
    # fh.setLevel(loglevel)
    fh.setLevel(logging.DEBUG)
    fh.setFormatter(formatter)

    # Note, different handlers can have different levels and formatters
    ch = logging.StreamHandler()
    # ch.setLevel(loglevel)
    ch.setLevel(logging.DEBUG)
    ch.setFormatter(formatter)

    logger = logging.getLogger(rootnamespace)
    logger.setLevel(logging.DEBUG)  # The root log level has to be set. The other handlers will log if level is higher than root
    logger.addHandler(fh)
    logger.addHandler(ch)


def main_simple():
    simple_config()
    logger = logging.getLogger(__name__)
    logger.info('Starting')
    cookie.bake(5)
    logger.info('Done')


def main_advanced():
    advanced_config('desserts')
    advanced_config(__name__)
    print(__name__)
    logger = logging.getLogger(__name__)
    # logger.info('Starting')
    logging.info('Starting')
    ld = Ladoo()
    ld.bake(3)
    logger.info('Baked')


if __name__ == '__main__': main_advanced()