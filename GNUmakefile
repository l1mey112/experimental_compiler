CC := gcc
CFLAGS := -Wall -Wextra -MMD
DEBUG_FLAGS := -g
PROD_FLAGS := -O2

SRC_DIR := .
BUILD_DIR := build
TARGET := hasc

SRC_FILES := $(wildcard $(SRC_DIR)/*.c)
OBJ_FILES := $(patsubst $(SRC_DIR)/%.c,$(BUILD_DIR)/%.o,$(SRC_FILES))
DEP_FILES := $(OBJ_FILES:.o=.d)

.PHONY: all debug prod clean

all: debug

debug: CFLAGS += $(DEBUG_FLAGS)
debug: $(TARGET)

prod: CFLAGS += $(PROD_FLAGS)
prod: $(TARGET)

$(TARGET): $(OBJ_FILES)
	$(CC) $(CFLAGS) $^ -o $@

$(BUILD_DIR)/%.o: $(SRC_DIR)/%.c | $(BUILD_DIR)
	$(CC) $(CFLAGS) -c $< -o $@

$(BUILD_DIR):
	mkdir -p $@

clean:
	rm -rf $(BUILD_DIR)

-include $(DEP_FILES)