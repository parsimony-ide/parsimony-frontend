SOURCES=src/cpp/parser.cpp src/cpp/inference.cpp src/cpp/bind.cpp
PREJS=src/cpp/pre.js
TARGET_DIR=resources/public/js/compiled
TEST_OUT_DIR=$(TARGET_DIR)/test/out
DEV_OUT_DIR=$(TARGET_DIR)/dev/out

TOUCH_THESE=test/parsimony/asm_parser_test.cljs test/parsimony/asm_inference_test.cljs

TARGET=$(TARGET_DIR)/asm_impl.js

INCLUDE_DIR=include

CC=emcc

DEBUG_CC_OPTS=-O0 --memory-init-file 0 -I$(INCLUDE_DIR) -DDEBUG
PRODUCTION_CC_OPTS=-O3 --memory-init-file 0 -I$(INCLUDE_DIR)
CC_OPTS=$(PRODUCTION_CC_OPTS)
#CC_OPTS=$(DEBUG_CC_OPTS)
512M=536870912

all: $(TARGET)

$(TARGET): $(SOURCES)
	mkdir -p $(TARGET_DIR)
	mkdir -p $(TEST_OUT_DIR)
	mkdir -p $(DEV_OUT_DIR)
	$(CC) $(CC_OPTS) --bind --pre-js $(PREJS) -s TOTAL_MEMORY=$(512M) -o $(TARGET) $(SOURCES)
	cp $(TARGET) $(TEST_OUT_DIR)
	cp $(TARGET) $(DEV_OUT_DIR)
	touch $(TOUCH_THESE)
