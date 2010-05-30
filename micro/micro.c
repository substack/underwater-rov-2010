#include <pic.h>
#include "util.h"

#define CMD_PRELUDE 0x50
#define CMD_SET_MOTORS 0x40
#define CMD_OK 0x80
#define CMD_GET_TEMP 0x81

void main() {
    byte SERVO_0 = 0, SERVO_1 = 0;
    byte MOTOR_L = 0, MOTOR_R = 0, MOTOR_V = 0;
    short BUCKET_L = 0, BUCKET_R = 0, BUCKET_V = 0;
    
    init();
    
    while (1) {
        byte cmd = serial_rx();
        byte power;
        
        switch (cmd) {
            case CMD_SET_MOTORS :
                MOTOR_L = serial_rx();
                MOTOR_R = serial_rx();
                MOTOR_V = serial_rx();
                SERVO_0 = serial_rx();
                SERVO_1 = serial_rx();
                serial_tx(CMD_OK);
                break;
            case CMD_GET_TEMP :
                // read temperature data from RE2 (ANS7)
                serial_tx(read_analog(7));
                break;
        }
        
        BUCKET_L += MOTOR_L - 127;
        BUCKET_R += MOTOR_R - 127;
        BUCKET_V += MOTOR_V - 127;
        
        power = 0;
        
        if (BUCKET_L >= 127) {
            BUCKET_L -= 127;
            power = power | (1 << 0);
        }
        else if (BUCKET_L < -127) {
            BUCKET_L += 127;
            power = power | (1 << 1);
        }
        
        if (BUCKET_R >= 127) {
            BUCKET_R -= 127;
            power = power | (1 << 2);
        }
        else if (BUCKET_R < -127) {
            BUCKET_R += 127;
            power = power | (1 << 3);
        }
        
        if (BUCKET_V >= 127) {
            BUCKET_V -= 127;
            power = power | (1 << 4);
        }
        else if (BUCKET_V < -127) {
            BUCKET_V += 127;
            power = power | (1 << 5);
        }
        
        PORTA = power;
        
        PORTC = 0x1;
        wait_msecf(1,SERVO_0);
        PORTC = 0x2;
        wait_msecf(1,SERVO_1);
        PORTC = 0;
    }
}
