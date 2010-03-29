PICPREFIX=../picc
include ./Makefile.common

MODEL=PIC16F887

PROG=micro
OBJS=$(PROG).c

all: $(PROG).hex

$(PROG).hex: $(OBJS)
	$(PICL) $(OBJS) -O$@

$(PROG)_b.hex: $(OBJS)
	$(PICL) $(OBJS) -D$(MODEL)
	#$(PICL) $(OBJS) -DCHIP_B=1 -O$@

install: $(PROG).hex
	./pk2prep
	pk2cmd -M -P $(MODEL) -T -F $< 

serial: 
	stty raw clocal 57600 cs8 -parenb parodd cstopb -echo < /dev/ttyUSB0 
