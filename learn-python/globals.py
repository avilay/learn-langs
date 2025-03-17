import logging

logging.basicConfig()
logger = logging.getLogger(__name__)
logger.setLevel(logging.INFO)


def main():
    logger.info('This is info log')
    logger.info('This is debug log')
    logger.debug('This is first debug log')
    logger.setLevel(logging.DEBUG)
    logger.debug('This is second debug log')


if __name__ == '__main__':
    main()
