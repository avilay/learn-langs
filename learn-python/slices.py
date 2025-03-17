
#%%
ary = list(range(10))

# ary[n:m] will create a new list copying elements from the original list from index n to m-1
ary_copy = ary[2:7]
print(ary_copy, ary)
ary_copy[0] = 999
print(ary_copy, ary)

# ary[n:m:s] will get every s'kipped element starting at index n and ending at m-1
print(ary[2:7:3])
