#include <hrt/hrt_input.h>

#include <wlr/types/wlr_xcursor_manager.h>

void hrt_seat_set_cursor_img(struct hrt_seat *seat, char *img_name) {
  seat->cursor_image = img_name;
  wlr_xcursor_manager_set_cursor_image(seat->xcursor_manager,
				       seat->cursor_image, seat->cursor);
}
