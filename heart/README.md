This folder contains a potential C backend to a Wayland compositor
written in a language besides C.

Its task is to setup the initial state of the compositor, render the
output, and initially handle new connections to the compositor. If
needed, the backend will also wrap some wlroots functions so that less
foreign code needs to be called from the other language.

The foreign code will interface with this library through a series of
callbacks, which are organized based on their purpose.

## Output callbacks
This set of callbacks handles when outputs are added and
removed. Outputs usually correspond to physical monitors, but they can
also be VNC connections or other means of displaying a chunk of
pixels.

+ **output_added**: Called when a new output is added.
+ **output_removed**: Called when an output is removed.

## Seat (input) callbacks
This set of callbacks handles input events

+ **handle_key_event**: When a key is pressed, this callback is
  triggered.
+ **handle_cursor_key**: When a mouse button is pressed, this event is
  triggered.
+ **handle_cursor_axis**: When a mouse wheel is scrolled, this event is
  triggered.
+ **handle_cursor_motion**: When a mouse is moved, this callback is
  triggered.

## Client callbacks
This set of callbacks handle client related callbacks, such as when a
surface is added or removed. In the backend, this may be either a new
XWayland window, or a client implementing the XDG shell protocol.
