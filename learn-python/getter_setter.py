class Cookie:
    def __init__(self, flavor):
        self._flavor = flavor

    def bake(self):
        print("Baking {} cookie".format(self._flavor))

    @property
    def flavor(self):
        return self._flavor

    @flavor.setter
    def flavor(self, flvr):
        self._flavor = flvr


cc = Cookie("chocolate chip")
cc.bake()
print(cc.flavor)  # Note I did not have to invoke the falvor method with cc.flavor()
cc.flavor = "Oatmeal Raisin"  # Note I did not have to invoke the flavor setter with cc.set(v)
cc.bake()
