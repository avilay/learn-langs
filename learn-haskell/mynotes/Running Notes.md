# Running Notes

GHCUp installs the haskell toolchain which consists of -

* ghc - the compiler
* cabal-install - the blessed package manager
* HLS - the Haskell Language Server
* stack - the other package manager



When install GHCUp, it will ask me if I want to have `stack` integrated with GHCUp, I should answer yes. It will do what is described in [User Guide - GHCup (haskell.org)](https://www.haskell.org/ghcup/guide/#stack-integration).



When installing the Haskell extension for VSCode, I should go the settings and set the "Manage HLS" to "GHCUp" instead of the default "Path". This will mean VSCode will now use GHCUp for finding and using the Haskell toolchain. This also means that all cabal or stack projects will be automatically setup. I don't have to install any packages in the *.cabal file manually using `cabal install` or `stack build` or anything like that. VSCode will do all that for me. See [Installation - GHCup (haskell.org)](https://www.haskell.org/ghcup/install/#vscode-integration)



Cabal's default config file is `~/.cabal/confg` it contains the repo (equivalent to condo-forge, pypi, etc.).



All about packages.

[5.9. Packages — Glasgow Haskell Compiler 9.10.1 User's Guide](https://downloads.haskell.org/ghc/latest/docs/users_guide/packages.html#)



```
Warning: The libraries were installed by creating a global GHC environment
file at:
/Users/avilay/.ghc/aarch64-darwin-9.4.8/environments/default

The presence of such an environment file is likely to confuse or break other
tools because it changes GHC's behaviour: it changes the default package set
in ghc and ghci from its normal value (which is "all boot libraries"). GHC
environment files are little-used and often not tested for.

Furthermore, management of these environment files is still more difficult
than it could be; see e.g. https://github.com/haskell/cabal/issues/6481 .

Double-check that creating a global GHC environment file is really what you
wanted! You can limit the effects of the environment file by creating it in a
specific directory using the --package-env flag. For example, use:

cabal install --lib <packages...> --package-env .

to create the file in the current directory.
(base) [local] ॐ  ~ $ ghci
Loaded package environment from /Users/avilay/.ghc/aarch64-darwin-9.4.8/environments/default
```

> It seems that on my Mac `$XDG_DATA_HOME/ghc/arch-os-version` is `~/.ghc/aarch64-darwin-9.4.8`. 

GHC only knows about packages that are *installed*. Installed packages live in package databases. To see which packages are currently available, use the `ghc-pkg list` command. Each installed package has a UUID. To list the installed package ID along with the package name and version run `ghc-pkg list -v`.

When not using cabal, packages can be exposed or hidden. Only exposed packages can be imported. A broken package is one if it is missing from the package database or one of its dependencies is broken. When using cabal the available packages are determined by the *.cabal file.

A package database is where the details about installed packages are stored. It is a directory, usually called `package.conf.d`, that contains a file for each package, together with a binary cache of the package data in the file `package.cache`. Managing package databases is done using the `ghc-pkg` tool. There can be multiple package databases, at compile/run time GHC stacks these and uses them to locate installed packages. There are two package databases in particular -

* Global package database which comes with the GHC installation, e.g., `/usr/lib/ghc-6.12.1/package.conf.d` or `/Users/avilay/.ghcup/ghc/9.4.8/lib/ghc-9.4.8/lib/package.conf.d` on my MacOSX.
* User package database at `$XDG_DATA_HOME/ghc/arch-os-version/package.conf.d`.

I can also set the envvar `GHC_PACKAGE_PATH` where I can specify the paths of the packages that will stacked. The global and user package dbs are ignored in this case.

A package environment file is a file that tells ghc precisely which packages should be visible. It can be used to create environments for GHC or GHCI that are local to a shell session or to some file system location. There is no way to reload the environment file in GHCI, if it changes, GHCI must be restarted. The file contains package IDs and optionally package databases. Package environments are either referenced by their full path, or by their name which is appended to `$XDG_DATA_HOME/ghc/arch-os-version/environments/<name>`. There is a `default` environment but it is best not to mess with it. There are ways to create new environments using `cabal`. I can start GHC/I with a specific enviornment using hte `-package-env <file>|<name>` option. I can also set the `GHC_ENVIRONMENT` to specify the path or the name. GHC/I will look for the package environment in the following locations -

1. File `<file>` if I use the `-package-env <file>` option.
2. File `$XDG_DATA_HOME/ghc/arch-os-version/environments/<name>` if I use the `-package-env <name>` option.
3. File `<file>` if the environment variable `GHC_ENVIRONMENT` is set to `<file>`.
4. File `$XDG_DATA_HOME/ghc/arch-os-version/environments/<name>` if the envvar `GHC_ENVIRONMENT` is set to `<name>`.
5. File `.ghc.environment.arch-os-version` if it exists in the current directory or any parent directory excep the user home.
6. File `$XDG_DATA_HOME/ghc/arch-os-version/environments/default` if it exists.



 In most cases I should not need to install ad-hoc packages. I'll be specifying my project dependencies in a *.cabal file and I can use the `cabal repl` command to launch GHCI with all of those packages. However, in case I do need to install a standalone package, it is best I create a custom package env and install stuff there.

```shell
cabal install --lib haskell-say
```

Will add this to the default GHC environment. I can use the same `-package-env <file>|<name>` flag here to specify a local environment file. E.g.,

```shell
cabal install --lib haskell-say --package-env local.env
```

Another useful trick is to ask cabal to create a fake/temp environment -

```shell
cabal repl --build-depends haskell-say
```

[3. cabal-install Configuration and Commands — Cabal 3.12.1.0 User's Guide](https://cabal.readthedocs.io/en/stable/cabal-config-and-commands.html)



# foldl in terms of foldr

```haskell
foldl _ result [] = result
foldl acc result (x:xs) =
	let result' = acc result x
	 in foldl acc result' xs
	 

foldr _ result [] = result
foldr acc result (x:xs) =
  let result' = foldr acc result xs
   in acc x result'
```



`foldr` is just replacing the `cons` function with the `acc` function -

```haskell
foldr acc x0 [x1, x2, x3]
= acc x1 (foldr acc x0 [x2, x3])
= acc x1 x230
= x1230

foldr acc x0 [x2, x3]
= acc x2 (foldr acc x0 [x3])
= acc x2 x30 = x230

foldr acc x0 [x3]
= acc x3 (foldr acc x0 [])
= acc x3 x0 = x30

foldr acc x0 [] = x0

foldr acc x0 [x1, x2, x3]
= acc x1 (acc x2 (acc x3 x0))

(cons x1 (cons x2 (cons x3 x0)))
= (acc x1 (acc x2 (acc x3 x0)))
```



```haskell
foldl acc x0 [x1, x2, x3]
= foldl acc (acc x0 x1) [x2, x3] = foldl acc x01 [x2, x3]
= foldl acc (acc x01 x2) [x3] = foldl acc x012 [x3]
= foldl acc (acc x012 x3) [] = foldl acc x0123 []
= x0123

foldl acc x0 [x1, x2, x3]
= acc (acc (acc x0 x1) x2) x3
```



To implement `foldl` in terms of `foldr`, `foldr` needs to accumulate functions as nested functions and then apply the final function to the inital value.

```haskell
foldl acc result xs = (foldlr acc' id xs) result 
 where
   acc' x f r = f (acc r x)
```

`acc'` is a function that can be applied to `r`, this will apply the original `acc` to the baked-in `x` and the newly supplied `r` and then bake it in for the next level of function.



```haskell
foldl acc x0 [x1, x2, x3] = (foldlr acc' id [x1, x2, x3]) x0  
```

`foldr` is baking in the input list into partial function calls

```haskell
EXP = foldr acc' id [x1, x2, x3] = acc' x1 (acc' x2 (acc' x3 id))

acc' x3 id = F3
EXP = acc' x1 (acc' x2 F3)

acc' x2 F3 = F2
EXP = acc' x1 F2

acc' x1 F2 = F1
EXP = F1

-- F1 is just a giant nested function
foldl acc x0 [x1, x2, x3] = (foldlr acc' id [x1, x2, x3]) x0
= F1 x0
= acc' x1 F2 x0 = F2 (acc x0 x1) 
-- because of lazy eval the arg won't get evaluated just yet

-- substituting the value of F2
= acc' x2 F3 (acc x0 x1) = F3 (acc (acc x0 x1) x2)

-- substituting the value of F3
acc' x3 id (acc (acc x0 x1) x2) = id (acc (acc (acc x0 x1) x2) x3)
= acc (acc (acc x0 x1) x2) x3
```



