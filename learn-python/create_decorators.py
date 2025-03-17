from functools import wraps


def logthis(func):
    @wraps(func)
    def wrapper(*args, **kwargs):
        all_args = ','.join([str(arg) for arg in args])
        all_kwargs = ','.join(['{}-{}'.format(k, v) for k, v in kwargs.items()])
        print('Calling {}'.format(func.__name__), all_args, all_kwargs)
        return func(*args, **kwargs)
    return wrapper


@logthis
def add(a, b):
    return a + b


@logthis
def foo(hsh):
    newhsh = {}
    for k, v in hsh.items():
        newhsh[k] = v + 10
    return newhsh


def check(func):
    @wraps(func)
    def wrapper(*args, **kwargs):
        this = args[0]
        flavor = args[1]
        if not flavor.startswith(this._flavor_prefix):
            raise RuntimeError(f'flavor must start with {this._flavor_prefix}')
        return func(*args, **kwargs)
    return wrapper


class ChocolateCookie:
    def __init__(self, calories):
        self._flavor_prefix = 'Chocolate'
        self._flavor = None
        self._calories = calories

    @check
    def set_flavor(self, flavor):
        self._flavor = flavor

    def __repr__(self):
        return f'Flavor: {self._flavor}, Calories: {self._calories}'


def main():
    # c = add(10, 20)
    # print('Sum is {}'.format(c))

    # newhsh = foo({'a': 10, 'b': 20})
    # for k, v in newhsh.items():
    #     print(k, v)
    cookie = ChocolateCookie(200)
    try:
        cookie.set_flavor('Oatmeal Raisin')
    except RuntimeError as re:
        print(re)
    cookie.set_flavor('Chocolate Chip')
    print(cookie)


if __name__ == '__main__':
    main()
