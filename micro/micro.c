#include <pic.h>
#include "util.h"

#define CMD_PRELUDE 0x50
#define CMD_SET_MOTORS 0x40
#define CMD_SET_SERVO_0 0x41
#define CMD_SET_SERVO_1 0x42
#define CMD_OK 0x80

static byte SERVO_0 = 0;
static byte SERVO_1 = 0;

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
    
    while (0) {
        byte i, j;
        for (i = 0; i < 10; i++) {
            for (j = 0; j < 10; j++) {
                wait_msecf(1,128);
            }
        }
        
        serial_tx(97);
    }
    
    while (1) {
        byte b, i, j;
        byte cmd = serial_rx();
        switch (cmd) {
            case CMD_SET_MOTORS :
                PORTA = serial_rx();
                serial_tx(CMD_OK);
                break;
            case CMD_SET_SERVO_0 :
                SERVO_0 = serial_rx();
                serial_tx(CMD_OK);
                break;
            case CMD_SET_SERVO_1 :
                SERVO_1 = serial_rx();
                serial_tx(CMD_OK);
                break;
        }
        
        PORTC = 0x1;
        wait_msecf(1,SERVO_0);
        PORTC = 0x2;
        wait_msecf(1,SERVO_1);
        PORTC = 0;
    }
}
