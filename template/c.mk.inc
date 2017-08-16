nclude ../make.h

# opemng src dir and files
OPEMNG_SRC_DIR = $(OPEMNG_DIR)/src
OPEMNG_SRC_FILES = $(wildcard $(OPEMNG_SRC_DIR)/*.c)

# opemng objs
OPEMNG_OBJ_FILES := $(OPEMNG_SRC_FILES:.c=.o)
OPEMNG_OBJ_FILES := $(notdir $(OPEMNG_OBJ_FILES))
OPEMNG_OBJ_FILES := $(addprefix $(OPEMNG_OBJ_DIR)/, $(OPEMNG_OBJ_FILES))

# opemng inc
INCLUDES = $(FSAP_INC_OPT) -I$(OPEMNG_INC) -I$(OAM_COMMON_INC) -I$(EL_INC) \
           -I$(ALMMNG_INC) -I$(GPSCTL_INC) -I$(RFCTL_INC) -I$(BBCTL_INC) \
           -I$(GWCTL_INC) -I$(CFGMNG_INC) -I$(VGDB_INC)
OPEMNG_DEP_FILES = \
	Makefile \
	make.h \
	$(wildcard $(OPEMNG_INC)/*.h) \
	$(wildcard $(OPEMNG_INC)/*.h) \
	$(wildcard $(OAM_COMMON_INC)/*.h) \
	$(wildcard $(EL_INC)/*.h)

# Unit Test
ifeq ($(OPEMNG_UT),yes)
INCLUDES += -I$(OPEMNG_DIR)/test/inc
CC_FLAGS += -include $(OPEMNG_DIR)/test/inc/opemngstub.h -DOPEMNG_UT
OPEMNG_DEP_FILES += $(wildcard $(OPEMNG_DIR)/test/inc/*.h)

OPEMNG_UT_SRC_FILES = $(wildcard $(OPEMNG_DIR)/test/src/*.c)
OPEMNG_UT_OBJ_FILES := $(OPEMNG_UT_SRC_FILES:.c=.o)
OPEMNG_UT_OBJ_FILES := $(notdir $(OPEMNG_UT_OBJ_FILES))
OPEMNG_UT_OBJ_FILES := $(addprefix $(OPEMNG_OBJ_DIR)/, $(OPEMNG_UT_OBJ_FILES))

OPEMNG_SRC_FILES += $(OPEMNG_UT_SRC_FILES)
OPEMNG_OBJ_FILES += $(OPEMNG_UT_OBJ_FILES)
endif # ifeq ($(OPEMNG_UT),yes)
