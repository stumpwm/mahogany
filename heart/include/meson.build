subdir('hrt')

exclude_dirs = []
exclude_files = ['config.h.in']

if not get_option('debug_utils')
  exclude_dirs += [
    'debug',
  ]
endif

install_subdir(
  'hrt',
  install_dir: get_option('includedir'),
  exclude_files: exclude_files,
  exclude_directories: exclude_dirs,
)
