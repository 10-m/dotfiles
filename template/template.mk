include ./c_mk.inc

MODULE_NAME = opemng

.PHONY: all START DONE clean

all: START $(OPEMNG_OBJ_FILE) DONE

START:
	@echo -e "\n\n--------------------------------| compiling $(MODULE_NAME) |-----------------------------\n\n"

DONE:
	@echo -e "\n\n----------------------------| $(MODULE_NAME) compilation done |--------------------------\n\n"

$(OPEMNG_OBJ_FILE) : $(OPEMNG_OBJ_FILES)
	$(LINKER) $(LINK_FLAGS) $(LDFLAGS) -o $(OPEMNG_OBJ_FILE) $(OPEMNG_OBJ_FILES)

$(OPEMNG_OBJ_DIR)/opemngglob.o : $(OPEMNG_SRC_DIR)/opemngglob.c $(OPEMNG_DEP_FILES)
	$(CC) $(CC_FLAGS) -DOAM $(INCLUDES) $< -o $@
	@echo -e $(DELIM)

$(OPEMNG_OBJ_DIR)/opemnginit.o : $(OPEMNG_SRC_DIR)/opemnginit.c $(OPEMNG_DEP_FILES)
	$(CC) $(CC_FLAGS) -DOAM $(INCLUDES) $< -o $@
	@echo -e $(DELIM)

$(OPEMNG_OBJ_DIR)/opemngtask.o : $(OPEMNG_SRC_DIR)/opemngtask.c $(OPEMNG_DEP_FILES)
	$(CC) $(CC_FLAGS) -DOAM $(INCLUDES) $< -o $@
	@echo -e $(DELIM)

$(OPEMNG_OBJ_DIR)/opemngmsg.o : $(OPEMNG_SRC_DIR)/opemngmsg.c $(OPEMNG_DEP_FILES)
	$(CC) $(CC_FLAGS) -DOAM $(INCLUDES) $< -o $@
	@echo -e $(DELIM)

$(OPEMNG_OBJ_DIR)/opemngstat.o : $(OPEMNG_SRC_DIR)/opemngstat.c $(OPEMNG_DEP_FILES)
	$(CC) $(CC_FLAGS) -DOAM $(INCLUDES) $< -o $@
	@echo -e $(DELIM)

$(OPEMNG_OBJ_DIR)/opemngsftp.o : $(OPEMNG_SRC_DIR)/opemngsftp.c $(OPEMNG_DEP_FILES)
	$(CC) $(CC_FLAGS) -DOAM $(INCLUDES) $< -o $@
	@echo -e $(DELIM)

$(OPEMNG_OBJ_DIR)/opemngstm.o : $(OPEMNG_SRC_DIR)/opemngstm.c $(OPEMNG_DEP_FILES)
	$(CC) $(CC_FLAGS) -DOAM $(INCLUDES) $< -o $@
	@echo -e $(DELIM)

$(OPEMNG_OBJ_DIR)/opemngutl.o : $(OPEMNG_SRC_DIR)/opemngutl.c $(OPEMNG_DEP_FILES)
	$(CC) $(CC_FLAGS) -DOAM $(INCLUDES) $< -o $@
	@echo -e $(DELIM)

$(OPEMNG_OBJ_DIR)/opemngalm.o : $(OPEMNG_SRC_DIR)/opemngalm.c $(OPEMNG_DEP_FILES)
	$(CC) $(CC_FLAGS) -DOAM $(INCLUDES) $< -o $@
	@echo -e $(DELIM)

ifeq ($(OPEMNG_UT),yes)
$(OPEMNG_OBJ_DIR)/opemngstub.o : $(OPEMNG_DIR)/test/src/opemngstub.c $(OPEMNG_DEP_FILES)
	$(CC) $(CC_FLAGS) -DOAM $(INCLUDES) $< -o $@
	@echo -e $(DELIM)
endif # ifeq ($(OPEMNG_UT),yes)

clean:
	rm -f $(OPEMNG_OBJ_DIR)/*.o
