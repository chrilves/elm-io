# Usage:
# make        # compile all binary
# make clean  # remove ALL binaries and objects

.PHONY = all clean

ELM = elm                        # compiler to use

SRCS := $(wildcard */*.elm)
BINS := $(SRCS:%.elm=%.html)

all: ${BINS}

%.html: %.elm
	${ELM} make $< --output=$@

clean:
	@echo "Cleaning up..."
	rm -rvf elm-stuff/ ${BINS}
