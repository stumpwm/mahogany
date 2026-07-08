/**
 * This file is copied almost verbatim from the sway window manager.

 Copyright (c) 2016-2017 Drew DeVault

 Permission is hereby granted, free of charge, to any person obtaining a copy of
 this software and associated documentation files (the "Software"), to deal in
 the Software without restriction, including without limitation the rights to
 use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 of the Software, and to permit persons to whom the Software is furnished to do
 so, subject to the following conditions:

 The above copyright notice and this permission notice shall be included in all
 copies or substantial portions of the Software.

 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 SOFTWARE.
 */

#include <stdlib.h>
#include <wlr/util/addon.h>
#include "scene_descriptor.h"
#include "wlr/util/log.h"

struct scene_descriptor {
	void *data;
	struct wlr_addon addon;
};

static const struct wlr_addon_interface addon_interface;

static struct scene_descriptor *scene_node_get_descriptor(
		struct wlr_scene_node *node, enum hrt_scene_descriptor_type type) {
	struct wlr_addon *addon = wlr_addon_find(&node->addons, (void *)type, &addon_interface);
	if (!addon) {
		return NULL;
	}

	struct scene_descriptor *desc = wl_container_of(addon, desc, addon);
	return desc;
}

static void descriptor_destroy(struct scene_descriptor *desc) {
	wlr_addon_finish(&desc->addon);
	free(desc);
}

void *scene_descriptor_try_get(struct wlr_scene_node *node,
		enum hrt_scene_descriptor_type type) {
	struct scene_descriptor *desc = scene_node_get_descriptor(node, type);
	if (!desc) {
		return NULL;
	}

	return desc->data;
}

void scene_descriptor_destroy(struct wlr_scene_node *node,
		enum hrt_scene_descriptor_type type) {
	struct scene_descriptor *desc = scene_node_get_descriptor(node, type);
	if (!desc) {
		return;
	}
	descriptor_destroy(desc);
}

static void addon_handle_destroy(struct wlr_addon *addon) {
	struct scene_descriptor *desc = wl_container_of(addon, desc, addon);
	descriptor_destroy(desc);
}

static const struct wlr_addon_interface addon_interface = {
	.name = "hrt_scene_descriptor",
	.destroy = addon_handle_destroy,
};

bool scene_descriptor_assign(struct wlr_scene_node *node,
		enum hrt_scene_descriptor_type type, void *data) {
	struct scene_descriptor *desc = calloc(1, sizeof(*desc));
	if (!desc) {
    wlr_log(WLR_ERROR, "Could not allocate a scene descriptor");
		return false;
	}

	wlr_addon_init(&desc->addon, &node->addons, (void *)type, &addon_interface);
	desc->data = data;
    return true;
}
