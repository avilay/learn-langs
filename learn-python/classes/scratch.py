class MyClass:
    def __new__(cls, *args, **kwargs):
        print('MyClass.__new__')
        # obj = super().__new__(cls, *args, **kwargs)
        # return obj
        return 1

    def __init__(self):
        print('MyClass.__init__')


obj = MyClass()
print(type(obj))
