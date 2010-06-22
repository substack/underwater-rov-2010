all: rov

serial:
	sudo chgrp plugdev /dev/bus/usb -R
	sudo chmod g+rw /dev/bus/usb -R
	stty raw clocal 57600 cs8 -parenb parodd cstopb -echo /dev/ttyUSB0

rov:
	ghc --make Dashboard.hs -threaded -o Dashboard
