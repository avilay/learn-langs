import logging


class Barfi(object):
    def __init__(self):
        self._logger = logging.getLogger(__name__)

    def bake(self, num):
        self._logger.info('Starting to bake {} barfi.'.format(num))
        try:
            f = open('nonexistantfile.txt')
            f.write('cookies baked.')
        except Exception as e:
            self._logger.error('Oops! something went wrong! {}'.format(e.message), exc_info=e)