PICPREFIX=./picc
include ./Makefile.common

MODEL=PIC12F675

PROG=relay-test
OBJS=$(PROG).c

all: $(PROG).hex

$(PROG).hex: $(OBJS)
	$(PICL) $(OBJS) -O$@

$(PROG)_b.hex: $(OBJS)
	$(PICL) $(OBJS) -D$(MODEL)
	#$(PICL) $(OBJS) -DCHIP_B=1 -O$@

install: $(PROG).hex
	pk2prep
	pk2cmd -M -P $(MODEL) -T -F $< 
	#pk2cmd -M -P 12F675 -T -F $< 
	#pk2 -device 16F887 -write $< 

on: $(PROG).hex
	pk2cmd -P $(MODEL) -on

off: $(PROG).hex
	pk2cmd -P $(MODEL) -off
