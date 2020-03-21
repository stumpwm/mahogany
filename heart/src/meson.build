heart_deps = [
  server_protos,
  wayland_server,
  wlroots,
  xkbcommon,
]

executable('heart',
	   files(
	     'main.c',
	     'server.c',
	     'output.c',
	     'input.c',
	     'keyboard.c',
	     'cursor.c',
	   ),
	   include_directories: [heart_inc],
	   dependencies: heart_deps,
	  )
