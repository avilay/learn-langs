import pprint


class MetaCookie(type):
    def __new__(cls, name, bases, namespace):
        print('MetaCookie::__new__')
        print('cls = ', cls)
        print('name = ', name)
        print('bases = ', bases)
        print('namespace -')
        pprint.pprint(namespace)
        print('\n\n')
        new_cls = type.__new__(cls, name, bases, namespace)
        return new_cls


class Cookie(metaclass=MetaCookie):
    def __init__(self, flavor, calories):
        print('Cookie::__init__')
        self.flavor = flavor
        self.calories = calories

    def servings(self):
        return self.calories / 100


class ChocolateChipCookie(Cookie):
    def __init__(self, calories, price):
        print('ChocolateChipCookie::__init__')
        super().__init__('chocolate_chip', calories)
        self.price = price

    def bake(self):
        print('Baking chocolate chip cookies!')
