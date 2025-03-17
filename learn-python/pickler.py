import pickle


class Cookie(object):
    def __init__(self, flavor, size):
        self.flavor = flavor
        self.size = size

    def debug(self):
        print('My __name__ is ' + __name__)


def persist():
    chcochip = Cookie('chocolate chip', 20)
    oatraisin = Cookie('oatmeal raisin', 10)
    snkdood = Cookie('snicker doodle', 25)
    cookies = [chcochip, oatraisin, snkdood]
    with open('cookies.pkl', 'wb') as f:
        pickle.dump(cookies, f, pickle.HIGHEST_PROTOCOL)


def reload():
    # cookies = None
    with open('cookies.pkl', 'rb') as f:
        cookies = pickle.load(f)
    for cookie in cookies:
        print(cookie.flavor, cookie.size)


def main():
    # persist()
    reload()


if __name__ == '__main__':
    main()
