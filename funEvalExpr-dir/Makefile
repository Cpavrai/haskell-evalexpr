BIN = .stack-work/install/1e01afab/bin/funEvalExpr-exe

ifeq ($(OS),Windows_NT)
	EXEC = ../funEvalExpr.exe
	BIN = .stack-work/install/1e01afab/bin/funEvalExpr-exe.exe
else
	EXEC = ../funEvalExpr
endif


CP = cp
RM = rm -f

BUILD = stack install


all: $(EXEC)

$(EXEC):
	$(BUILD)
	$(CP) $(BIN) $(EXEC)

clean:
	$(RM) $(EXEC)

re: clean all