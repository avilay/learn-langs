# To convert an int to a string representation of its binary digits
i = 8
bits = f'{i:b}'
print(type(bits), bits)


# To convert a string representation of binary digits to int
bits = '1011'
i = int(bits, 2)
print(type(i), i)


# To convert an int to bytes
i = 8
bits = int(i).to_bytes(length=1, byteorder='big')
print(type(bits), bits)


# To convert bytes to int
bits = b'\x10'
i = int.from_bytes(bits, byteorder='big')
print(type(i), i)
