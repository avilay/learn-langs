import logging


def bake(num):
    logger = logging.getLogger(__name__)
    logger.info('Starting to bake {} cookies.'.format(num))
    try:
        f = open('nonexistantfile.txt')
        f.write('cookies baked.')
    except Exception as e:
        logger.error('Oops! something went wrong! {}'.format(e.message), exc_info=e)