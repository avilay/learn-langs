import logging


def simple():
    logformat = '%(asctime)s:%(levelname)s:%(reqid)s:%(name)s:%(message)s'
    logging.basicConfig(format=logformat, level=logging.DEBUG)
    logger = logging.getLogger(__name__)
    logger.info('Starting', extra={'reqid': '111222'})
    logger.debug('Ending', extra={'reqid': '222333'})


def default_adapter():
    logformat = '%(asctime)s:%(levelname)s:%(reqid)s:%(name)s:%(message)s'
    logging.basicConfig(format=logformat, level=logging.DEBUG)
    logger = logging.getLogger(__name__)
    adapter = logging.LoggerAdapter(logger, dict(reqid='111222'))
    adapter.info('Starting')
    adapter.debug('Ending')    


if __name__ == '__main__':
    default_adapter()   
