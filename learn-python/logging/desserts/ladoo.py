import logging


class Ladoo(object):
    def __init__(self):
        self._logger = logging.getLogger(__name__)

    def bake(self, num):
        self._logger.info('Starting to bake {} ladoo.'.format(num))
        try:
            f = open('nonexistantfile.txt')
            f.write('cookies baked.')
        except FileNotFoundError as fnfe:
            self._logger.error('Oops! something went wrong! {}'.format(fnfe), exc_info=fnfe)