#include <hrt/hrt_input.h>

#include <wlr/types/wlr_cursor.h>

void hrt_seat_set_cursor_img(struct hrt_seat *seat, char *img_name) {
    seat->cursor_image = img_name;
    wlr_cursor_set_xcursor(seat->cursor, seat->xcursor_manager,
                           seat->cursor_image);
}
