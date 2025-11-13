project(
  'heart',
  'c',
  version: '0.1',
  license: 'GPL',
  default_options: [
    'c_std=c11',
    'warning_level=2',
    'werror=true',
  ],
)

add_project_arguments(
  [
    '-DWLR_USE_UNSTABLE',
    '-D_POSIX_C_SOURCE=200112L',
    '-Wno-unused-parameter',
    '-Wno-unused-result',
  ],
  language: 'c',
)

cc = meson.get_compiler('c')

wlroots_version = ['>=0.19.0', '<0.20.0']

wayland_server = dependency('wayland-server')
wayland_protos = dependency('wayland-protocols', version: '>=1.14')
xkbcommon      = dependency('xkbcommon')
xcb            = dependency('xcb', required: get_option('xwayland'))
wlroots        = dependency(
  ['wlroots-0.19', 'wlroots'],
  version: wlroots_version,
  required: false,
)

if not wlroots.found()
  wlroots_proj = subproject(
    'wlroots',
    default_options: ['examples=false'],
    required: true,
    version: wlroots_version,
  )
  wlroots = wlroots_proj.get_variable('wlroots')
  wlroots_conf = wlroots_proj.get_variable('conf_data')
  wlroots_has_xwayland = wlroots_conf.get('WLR_HAS_XWAYLAND') == 1
else
  wlroots_has_xwayland = wlroots.get_pkgconfig_variable('have_xwayland') == 'false'
endif



if get_option('xwayland').enabled() and not wlroots_has_xwayland
  error('Cannot enable Xwayland in heart: wlroots has been built without Xwayland support')
endif

have_xwayland = xcb.found() and wlroots_has_xwayland

features = {
  'xwayland': have_xwayland,
}

hrt_source_files = []

subdir('include')
subdir('protocols')
subdir('src')

hrt_deps = [
  server_protos,
  wayland_server,
  wlroots,
  xkbcommon,
]

hrt_inc = include_directories('include')
proto_inc = include_directories('protocols')

hrt_vars = {}
foreach name, have : features
  hrt_vars += { 'have_' + name.underscorify(): have.to_string() }
endforeach

lib_hrt = library(
  meson.project_name(), hrt_source_files,
  dependencies: hrt_deps,
  include_directories: [hrt_inc, proto_inc],
  install: true
)

heart = declare_dependency(
  link_with: lib_hrt,
  dependencies: hrt_deps,
  include_directories: hrt_inc,
)

pkg = import('pkgconfig')
pkg.generate(
  lib_hrt,
  version: meson.project_version(),
  filebase: meson.project_name(),
  name: meson.project_name(),
  description: 'Core package for mahogany wm',
  variables: hrt_vars,
)

if get_option('example')
  subdir('example')
endif

summary(features, bool_yn: true)
