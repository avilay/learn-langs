# Use while
#%%
s1 = 'This is a string'
s2 = 'This is a very long string'
i = 0
s = s2
while i < len(s):
    if i > 15:
        break
    print(s[i])
    i += 1
else:
    print('\nelse will not be executed if break is used to break out of the loop')
#%%

# Use for
#%%
s1 = 'This is a string with a z'
s2 = 'This is a string'
s = s2
for c in s:
    print(c)
    if c == 'z':
        break
else:
    print('\nelse will not be executed if break is used to break out of the loop')
#%%

# A traditional for(int i = 0; i < 5; i++) {} can be written like this -
#%%
for x in range(5):
    print(x)


for i, x in enumerate(range(10, 15)):
    print(i, x)
#%%
