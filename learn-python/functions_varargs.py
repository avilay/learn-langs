
#%%
def posvarargs(arg1, *args, arg2=2.17):
    print(arg1, args, arg2)


# Because arg2 is after *args, it has to be passed via keyword.
# In the call below, 3.14 is taken as part of *args
posvarargs(10, 'hello', 'world', 3.14)

# Here I am passing arg2 as a keyword
posvarargs(10, 'hello', 'world', arg2=3.14)


# When specifying keyword varargs, the usual rule of cannot specify positional after keyword applies
# def keyvarargs(arg1, **kwargs, *arg) --> is not valid
# def keyvarargs(arg1, **kwargs, arg2) --> is not valid
# Pretty much no other argument is allowed once I have used **kwargs
#%%
def keyvarargs(arg1, **kwargs):
    print(arg1, kwargs)


keyvarargs(10, greeting='hello', constnum=3.14)
keyvarargs(arg1=10, greeting='hello', constnum=3.14)


#%%
def poskeyvarargs(arg1, *args, **kwargs):M
    print(arg1, args, kwargs)


poskeyvarargs(10, 'hello', 'world', constnum=3.14)
poskeyvarargs(10, greeting='hello', constnum=3.14)
poskeyvarargs(arg1=10, greeting='hello', constnum=3.14)
