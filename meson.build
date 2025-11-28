project(
  'mahogany',
  'c',
  version: '0.1',
  license: 'GPL',
  default_options: [
    'c_std=c11',
    'warning_level=2',
    'werror=true',
  ],
)

subdir('heart')

lisp_conf_data = configuration_data()
lisp_conf_data.set('SOURCE_ROOT', meson.source_root())
lisp_conf_data.set('BUILD_ROOT', meson.build_root())

lisp_make_file = configure_file(
  input: 'make-mahogany.lisp.in',
  output: 'make-mahogany.lisp',
  configuration: lisp_conf_data,
)

lisp_inputs = [
  lib_hrt,
]

lisp_c = get_option('lisp')

lisp = find_program(lisp_c)

if lisp_c == 'sbcl'
  execute_lisp = [lisp, '--non-interactive', '--load', lisp_make_file]
elif lisp_c == 'ccl'
  sh = find_program('sh')
  execute_lisp = [lisp, '-b', '--load', lisp_make_file, '--']
else
  error('Don\'t know how to  build using lisp implementation ' + lisp_c)
endif

mahogany_target = custom_target(
  'mahogany',
  command: execute_lisp + 'build',
  build_always_stale: true,
  build_by_default: true,
  install: true,
  output: 'mahogany',
  input: lisp_inputs,
  install_dir: get_option('bindir'),
)

libhrt_path = lib_hrt.full_path()

mahogany_run_env = {
  'LD_LIBRARY_PATH': libhrt_path.substring(0, -11)
}

run_target(
  'run',
  command: mahogany_target,
  env: mahogany_run_env,
)

run_target(
  'mahogany-test',
  command: execute_lisp + 'test',
  depends: [lib_hrt],
)
