project(
  'meson-unity-1',
  'c',
  default_options: [
    'c_std=c99',
    'warning_level=3',
  ],
)

lib_src = files('src/add.c')
app_src = lib_src + files('src/main.c')

##
# Enable double floating point assertions:
#
# https://github.com/ThrowTheSwitch/Unity/blob/master/docs/UnityAssertionsReference.md#double-if-enabled
#
add_global_arguments('-DUNITY_INCLUDE_DOUBLE', language : 'c')

executable(
  'main',
  app_src,
)

unity_subproject = subproject('unity')
unity_dependency = unity_subproject.get_variable('unity_dep')

test_src = files('test/main.spec.c', 'test/add.spec.c')

incdir = include_directories('include')

my_test = executable('main_test',
  lib_src + test_src,
  dependencies: [ unity_dependency ],
  include_directories: incdir,
)

test('test_suite', my_test)
