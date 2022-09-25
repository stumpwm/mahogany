ccl = cat $(1) | ccl -b

sbcl = sbcl --non-interactive --load $(1)

LISP=sbcl

BUILD_DIR := $(shell pwd)/build
# In order to not watch heart build files but still detect fresh builds,
# use files in the internal directory as as placeholder:
CACHE := $(BUILD_DIR)/internal

$(BUILD_DIR)/mahogany: $(BUILD_DIR)/heart/lib64/libheart.so build-mahogany.lisp FORCE
	$(call $(LISP), build-mahogany.lisp)

$(BUILD_DIR)/heart/lib64/libheart.so: $(CACHE)/wlroots-configured FORCE
	ninja -C $(BUILD_DIR)/heart
	# FIXME?: move the api headers into a separate directory and just use those instead of calling install:
	ninja -C $(BUILD_DIR)/heart install > $(BUILD_DIR)/install_output.txt

$(CACHE)/wlroots-configured:
	mkdir -p $(BUILD_DIR)/heart && meson setup $(BUILD_DIR)/heart heart/ -Dprefix=$(BUILD_DIR)
	mkdir -p $(CACHE)
	touch $(CACHE)/wlroots-configured

run: $(BUILD_DIR)/mahogany
	LD_LIBRARY_PATH=build/lib64/:build/lib/ ./build/mahogany

clean: FORCE
	ninja -C $(BUILD_DIR)/heart clean
	rm $(BUILD_DIR)/mahogany

test: $(BUILD_DIR)/heart/lib64/libheart.so
	$(call $(LISP),run-tests.lisp)

FORCE: ;
