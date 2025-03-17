See [Packaging Python Projects](https://packaging.python.org/en/latest/tutorials/packaging-projects/) and [setuptools](https://setuptools.pypa.io/en/latest/) for the latest and greatest.

# Archive
The following content is now obsolete. 

To generate a distributable -
$ python setup.py sdist

If you don’t supply an explicit list of files (or instructions on how to generate one), the sdist command puts a minimal default set into the source distribution:

* all Python source files implied by the py_modules and packages options
* all C source files mentioned in the ext_modules or libraries options
* scripts identified by the scripts option See Installing Scripts.
* anything that looks like a test script: test/test*.py (currently, the Distutils don’t do anything with test scripts except include them in source distributions, but in the future there will be a standard for testing Python module distributions)
* README.txt (or README), setup.py (or whatever you called your setup script), and setup.cfg
* all files that matches the package_data metadata. See Installing Package Data.
* all files that matches the data_files metadata. See Installing Additional Files.

To install while developing -
$ pip install -e .

$ pip install -e path/to/SomeProject
$ pip install -e git+ssh://repo/my_project.git#egg=SomeProject
$ pip install --download <DIR> -r requirements.txt
$ pip install --no-index --find-links=[file://]<DIR> -r requirements.txt
$ pip install --no-index --find-links=file:///local/dir/ SomePackage
$ pip install ./downloads/SomePackage-1.0.4.tar.gz
$ pip install http://my.package.repo/SomePackage-1.0.4.zip
$ pip install --index-url http://my.package.repo/simple/ SomePackage


Files
=====
setup.py
setup.cfg
README.rst (auto generate this based on README.md)
MANIFEST.in (do not foresee needing this)

Setup args
==========
name
version
description
long_description
url
author
license
classifiers
keywords
packages=find_packages(exclude=['contrib', 'docs', 'tests*'])
install_requires
extras_require = {
        'dev': ['check-manifest'],
        'test': ['coverage'],
    }
package_data --> optional
include_package_data=True --> instead of package_data
data_files --> use this for conf files
entry_points={
        'console_scripts': [
            'foo = my_package.some_module:main_func',
            'bar = other_module:some_func',
        ]


Difference between install_requires and extras_require
======================================================
When pip is installing the package, it is usually installing for the end user of the package, and as such will check the install_requires dependencies. However, for people developing the package there might be extra dependencies like the test framework being used that should be checked for. These dependencies are specified in teh extras_require. The keys of the dict are args given to pip. E.g.,
$ pip install -e . dev

Versioning my distributable package
===================================
1.2.0.dev1  # Development release
1.2.0a1     # Alpha Release
1.2.0b1     # Beta Release
1.2.0rc1    # RC Release
1.2.0       # Final Release
1.2.0.post1 # Post Release


Result of using entry_point
===========================
When this project is installed on non-Windows platforms (pip install), a set of foo and bar scripts will be installed that import main_func and some_func from the specified modules. The functions you specify are called with no arguments, and their return value is passed to sys.exit(), so you can return an errorlevel or message to print to stderr. However, this does not mean that main_func and some_func cannot accept arguments. If argsparse is used, then it reads in the argument directly from sys.args variable, so everything just works.

On Windows, a set of foo.exe, bar.exe, and baz.exe launchers are created, alongside a set of foo.py, bar.py, and baz.pyw files. The .exe wrappers find and execute the right version of Python to run the .py or .pyw file.


Difference between package_data, include_package_data, and data_files
=====================================================================
By default setup.py will only copy my-pack/*.py files inside a python package directory into the dist package. If I want it to include other files like my-pack/users.csv, for example, I have to explicitly ask setup.py to copy it via the setup.package_data argument. In the destination installation, the file will appear exactly like it was in the source, i.e., in dist-packages/my-dist/my-pack/uses.csv.

Usually, I would want to copy all non-*.py files in a directory like my-pack/data. In this case, instead of specifying a package_data value with a glob, I can use the setup.include_package_data = True, to include all non-*.py inside my python package.

If I have some non *.py files that need to copied to a directory other than the installation directory on the destination computer, then I use the data_files argument. For example I have my-pack/my-pack.ini config file that needs to be copied to /etc/my-dist/my-pack.ini on the destination computer. Here I cannot use package_data because it will copy files in the exact same place as it was in the source tree. For this I use data_files.


Difference between requirements.txt and setup.install_requires
==============================================================
Whereas install_requires defines the dependencies for a single project, Requirements Files are often used to define the requirements for a complete python environment.

Whereas install_requires requirements are minimal, requirements files often contain an exhaustive listing of pinned versions for the purpose of achieving repeatable installations of a complete environment.

Whereas install_requires requirements are “Abstract”, requirements files often contain pip options like --index-url or --find-links to make requirements “Concrete”. [1]

Whereas install_requires metadata is automatically analyzed by pip during an install, requirements files are not, and only are used when a user specifically installs them using pip install -r.

References
==========
http://pythonhosted.org/setuptools/index.html
https://github.com/pypa/sampleproject
https://docs.python.org/3.4/distutils/setupscript.html
https://pip.pypa.io/en/latest/reference/pip_install.html
https://packaging.python.org/en/latest/distributing.html
http://docutils.sourceforge.net/docs/user/rst/quickstart.html
https://github.com/jeffknupp/sandman
https://pypi.python.org/pypi?%3Aaction=list_classifiers