#%%
class Callme:
    def __init__(self):
        self._state = 10

    def __call__(self, v=1):
        print('Callme::__call__: ', self._state)
        self._state += v


cm = Callme()
cm(2)  # Note how the object cm is being used as a callable
cm()  # And each invocation is mutating the state of the object
