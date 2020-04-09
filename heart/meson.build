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

wayland_server = dependency('wayland-server')
wayland_protos = dependency('wayland-protocols', version: '>=1.14')
xkbcommon      = dependency('xkbcommon')
xcb            = dependency('xcb', required: get_option('xwayland'))

wlroots_version = ['>=0.10.0', '<0.11.0']
wlroots_proj = subproject(
	'wlroots',
	default_options: ['examples=false'],
	required: false,
	version: wlroots_version,
)
if wlroots_proj.found()
	wlroots = wlroots_proj.get_variable('wlroots')
	wlroots_conf = wlroots_proj.get_variable('conf_data')
	wlroots_has_xwayland = wlroots_conf.get('WLR_HAS_XWAYLAND') == 1
else
	wlroots = dependency('wlroots', version: wlroots_version)
	wlroots_has_xwayland = cc.get_define('WLR_HAS_XWAYLAND', prefix: '#include <wlr/config.h>', dependencies: wlroots) == '1'
endif

if get_option('xwayland').enabled() and not wlroots_has_xwayland
	error('Cannot enable Xwayland in heart: wlroots has been built without Xwayland support')
endif

have_xwayland = xcb.found() and wlroots_has_xwayland

conf_data = configuration_data()
conf_data.set10('HAVE_XWAYLAND', have_xwayland)

heart_inc = include_directories('include')

subdir('include')
subdir('protocols')
subdir('src')
