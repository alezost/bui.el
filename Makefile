# This is not a full-featured Makefile and it is not intended to be used
# to install bui package to your system.  Its only purpose is to
# byte-compile *.el files (using "make") to make sure that there are no
# compilation warnings.

EMACS = emacs

TOP := $(dir $(lastword $(MAKEFILE_LIST)))

LOAD_PATH = -L $(TOP)

EMACS_BATCH = $(EMACS) -batch -Q $(LOAD_PATH)

ELS =						\
  bui-utils.el					\
  bui-button.el					\
  bui-history.el				\
  bui-entry.el					\
  bui-core.el					\
  bui-info.el					\
  bui-list.el					\
  bui.el					\
  examples/buffers.el

ELCS = $(ELS:.el=.elc)

all: $(ELCS)

%.elc: %.el
	@printf "Compiling $<\n"
	@$(EMACS_BATCH) --eval "\
	  (when (file-exists-p \"$@\")\
	    (delete-file \"$@\"))" \
	-f batch-byte-compile $<

clean:
	@printf "Removing *.elc...\n"
	@$(RM) $(ELCS)
