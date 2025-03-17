
#%%
class Cake:
    def bake(self):
        print('Cake::bake')

def cut(self):
    print("cutting a something with icing and frosting")
    self.bake()
    print(self.icing)
    print(self.frosting)


truffle = Cake()
truffle.icing = "chocolate"
truffle.frosting = "vanilla"
Cake.cut_cake = cut
truffle.cut_cake()
