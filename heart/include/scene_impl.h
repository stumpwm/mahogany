#pragma once

#include "hrt/hrt_scene.h"

struct hrt_scene_root *hrt_scene_root_create(struct wlr_scene_tree *scene);

struct hrt_scene_group *hrt_scene_group_create(struct hrt_scene_root *parent);
