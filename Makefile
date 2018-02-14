WANTED_BIN = funEvalExpr
GENERATED_BIN = funEvalExpr-exe.exe

STACK = stack

BUILD = build

CP = cp

INSTALL = install
INSTALL_FLAG = --local-bin-path .

CLEAN = clean

RM = rm -rf

all:
	$(STACK) $(BUILD)
	$(STACK) $(INSTALL) $(INSTALL_FLAG)
	$(CP) $(GENERATED_BIN) $(WANTED_BIN)

clean:
	$(STACK) $(CLEAN)

fclean: clean
	$(RM) $(GENERATED_BIN)
	$(RM) $(WANTED_BIN)

re: fclean all

.PHONY: all clean fclean re
