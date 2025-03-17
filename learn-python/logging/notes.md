# Notes
## How do log levels work?
Lets say we have the following setup -

main.py
```python
import logging
import pkg_one.module

logging.basicConfig(level=logging.ERROR)
```

pkg_one/module.py
```python
import logging

logger = logging.getLogger(__name__)
```

This effectively means:
```python
1. main: import logging
2. pkg_one.module: import logging
3. pkg_one.module: logger = logging.getLogger(__name__)
4. main: logging.basicConfig(level=logging.ERROR)
```

In line 3, the logger's name is `pkg_one.module`. The system will go looking for any of its ancestor who has its level set and apply it to this logger. In this case, the system reaches the root but does not find any loggers with their level set. Root logger always has its level set to `WARNING`. So that is what is applied to `<logger pkg_one.module>`. Later in line 4, the root logger's level is set to `ERROR`. This means that any inherited loggers, like our `<logger pkg_one.module>`, will inherit the new level.

While this is the ideal scenario, it is possible for the package author to do evil things like set the log level inside its module, like so -

pkg_one/module.py
```python
import logging

logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)
```

Now, the effective code reads -
```python
1. main: import logging
2. pkg_one.module: import logging
3. pkg_one.module: logger = logging.getLogger(__name__)
4. pkg_one.module: logger.setLevel(logging.DEBUG)
5. main: logging.basicConfig(level=logging.ERROR)
```

Because the level of the module's logger was explicitly set to `DEBUG`, setting the root logger's level does not alter anything!

A good practice for library authors is to **never** set the log level in their modules. Let the main module set the level.

### How to deal with such evil libraries?
We will have to explicitly set the log level of that particular logger, setting the log level of any of its ancestor won't work. So our main will look like this -

main.py
```python
import logging
import pkg_one.module

logging.basicConfig(level=logging.ERROR)
logging.getLogger("pkg_one.module").setLevel(logging.ERROR)
```

And now the effective code reads like this -
```python
1. main: import logging
2. pkg_one.module: import logging
3. pkg_one.module: logger = logging.getLogger(__name__)
4. pkg_one.module: logger.setLevel(logging.DEBUG)
5. main: logging.basicConfig(level=logging.ERROR)
6. main: logging.getLogger("pkg_one.module").setLevel(logging.ERROR)
```

Because line 6 comes after line 4, we have reset the evil logging level.
