#include <pic.h>
#include "util.h"

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
        PORTA = (1 << 0) | (1 << 3) | (1 << 4);
        wait_csec(1);
        PORTA = (1 << 1) | (1 << 2) | (1 << 5);
        wait_csec(1);
    }
}
