#include <pic.h>
#include "util.h"

#define CMD_PRELUDE 0x53
#define CMD_SET_A 0x41

void main() {
    TRISA = 0;
    TRISB = 0;
    TRISC = 1 << 7; // RC7 to 1 for serial RX
    TRISD = 0;
    TRISE = 0;
    
    // No analog
    ANSEL = 0;
    
    // Everything off
    PORTA = 0x00;
    PORTB = 0x00;
    PORTC = 0x00;
    PORTE = 0x00;
    PORTD = 0x00;
    
    init();
    
    while (1) {
        byte i;
        for (i = 0; i <= 0xf; i++) {
            serial_tx(i);
        }
    }
    
    while (1) {
        byte prelude, cmd, data;
        prelude = serial_rx();
        if (prelude == CMD_PRELUDE) {
            cmd = serial_rx();
            if (cmd == CMD_SET_A) {
                PORTA = serial_rx();
                wait_dsec(1);
            }
        }
    }
}
