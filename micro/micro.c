#include <pic.h>
#include "util.h"

#define CMD_PRELUDE 0x50
#define CMD_SET_MOTORS 0x40
#define CMD_SET_SERVO_0 0x41
#define CMD_SET_SERVO_1 0x42
#define CMD_OK 0x80

byte motor_bit(byte t, byte x) {
    byte y = x % 128;
    byte h = x < 128;
    if (x < 64) {
        return ((t % (128 / y)) != 0) << h;
    }
    else {
        return (t % (128 / (127 - y)) == 0) << h;
    }
}
        
void main() {
    byte SERVO_0 = 0, SERVO_1 = 0;
    byte MOTOR_L = 0, MOTOR_R = 0, MOTOR_V = 0;
    byte T = 0;
    
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
        byte cmd = serial_rx();
        
        switch (cmd) {
            case CMD_SET_MOTORS :
                MOTOR_L = serial_rx();
                MOTOR_R = serial_rx();
                MOTOR_V = serial_rx();
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
        
        PORTA
            = (motor_bit(T,MOTOR_L) << 0)
            | (motor_bit(T,MOTOR_R) << 2)
            | (motor_bit(T,MOTOR_V) << 4)
        ;
        
        T ++;
        
        PORTC = 0x1;
        wait_msecf(1,SERVO_0);
        PORTC = 0x2;
        wait_msecf(1,SERVO_1);
        PORTC = 0;
    }
}
