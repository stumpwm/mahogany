ccl = cat $(1) | ccl -b

sbcl = sbcl --non-interactive --load $(1)

LISP=sbcl

ROOT := $(shell pwd)
BUILD_DIR := $(shell pwd)/build
# In order to not watch heart build files but still detect fresh builds,
# use files in the internal directory as as placeholder:
CACHE := $(BUILD_DIR)/internal

# We actually want to watch the build output (build/include/hrt), but those files
# might not exist
HRT_INCLUDES = $(shell find $(ROOT)/heart/include/hrt/ -type f)
# WLR_INCLUDES = $(shell find $(ROOT)/heart/subprojects/wlroots/include/wlr/ -type f)

$(BUILD_DIR)/mahogany: $(BUILD_DIR)/heart/lib64/libheart.so lisp/bindings/hrt-bindings.lisp lisp/bindings/wlr-bindings.lisp build-mahogany.lisp FORCE
	$(call $(LISP), build-mahogany.lisp)

lisp/bindings/hrt-bindings.lisp: $(ROOT)/lisp/bindings/hrt-bindings.yml $(HRT_INCLUDES)
	PKG_CONFIG_PATH=$(BUILD_DIR)/lib/pkgconfig cl-bindgen b lisp/bindings/hrt-bindings.yml

lisp/bindings/wlr-bindings.lisp: $(ROOT)/lisp/bindings/wlr-bindings.yml
	PKG_CONFIG_PATH=$(BUILD_DIR)/lib/pkgconfig cl-bindgen b lisp/bindings/wlr-bindings.yml

$(BUILD_DIR)/heart/lib64/libheart.so: $(CACHE)/wlroots-configured FORCE
	ninja -C $(BUILD_DIR)/heart
	# FIXME?: move the api headers into a separate directory and just use those instead of calling install:
	ninja -C $(BUILD_DIR)/heart install > $(BUILD_DIR)/install_output.txt

$(CACHE)/wlroots-configured:
	mkdir -p $(BUILD_DIR)/heart && meson setup $(BUILD_DIR)/heart heart/ -Dprefix=$(BUILD_DIR) -Dlibdir=lib
	mkdir -p $(CACHE)
	touch $(CACHE)/wlroots-configured

run: $(BUILD_DIR)/mahogany
	LD_LIBRARY_PATH=build/lib/ ./build/mahogany

clean: FORCE
	ninja -C $(BUILD_DIR)/heart clean
	rm -f $(BUILD_DIR)/mahogany
	rm -rf $(BUILD_DIR)/lib64
	rm -rf $(BUILD_DIR)/include
	rm -rf $(BUILD_DIR)/install_output.txt

test: $(BUILD_DIR)/heart/lib64/libheart.so
	$(call $(LISP),run-tests.lisp)

FORCE: ;
