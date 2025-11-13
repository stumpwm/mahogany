header_defines = configuration_data()
foreach name, have : features
	header_defines.set10('HRT_HAS_' + name.underscorify().to_upper(), have)
endforeach

conf_h = configure_file(
  input: 'config.h.in',
  output: 'config.h',
  configuration: header_defines,
)

install_headers(conf_h, subdir: 'hrt')
