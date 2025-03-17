import ConfigParser

config = ConfigParser.SafeConfigParser()

# config.add_section('Section1')
# config.set('Section1', 'an_int', '15')
# config.set('Section1', 'a_bool', 'true')
# config.set('Section1', 'a_float', '3.145')
# config.set('Section1', 'baz', 'fun')
# config.set('Section1', 'bar', 'Python')
# config.set('Section1', 'foo', '%(bar)s is %(baz)s')
# with open('example.cfg', 'wb') as configfile:
# 	config.write(configfile)

config.read('example.cfg')
print('an_int: {}'.format(config.getint('Section1', 'an_int')))
print('foo: {}'.format(config.get('Section1', 'foo')))
print('oof: {}'.format(config.get('Section2', 'oof')))
