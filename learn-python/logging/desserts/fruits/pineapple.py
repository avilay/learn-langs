import logging


class Pineapple(object):
    def __init__(self):
        self._logger = logging.getLogger(__name__)

    def cut(self, pcs):
        self._logger.info('Starting to cut {} pineapple.'.format(pcs))
        try:
            f = open('nonexistantfile.txt')
            f.write('cookies baked.')
        except Exception as e:
            self._logger.error('Oops! something went wrong! {}'.format(e.message), exc_info=e)
