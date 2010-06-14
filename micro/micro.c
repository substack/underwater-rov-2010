#include <pic.h>
#include "util.h"

#define CMD_SET_MOTORS 0x40

#define set_servo(n,power) \
    PORTC = n; \
    wait_msecf(1,power); \
    PORTC = 0;

void main() {
    byte SERVO_0 = 0, SERVO_1 = 0;
    byte MOTOR_L = 0, MOTOR_R = 0, MOTOR_V = 0;
    short BUCKET_L = 0, BUCKET_R = 0, BUCKET_V = 0;
    byte T = 0;
    
    init();
    
    while (1) {
        byte cmd = serial_rx();
        byte power, s0, s1;
        
        switch (cmd) {
            case CMD_SET_MOTORS :
                MOTOR_L = serial_rx();
                MOTOR_R = serial_rx();
                MOTOR_V = serial_rx();
                
                s0 = serial_rx();
                s1 = serial_rx();
                
                set_servo(1 << 0, SERVO_0);
                set_servo(1 << 1, SERVO_1);
                
                SERVO_0 = s0;
                SERVO_1 = s1;
                
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
        wait_msec(4);
    }
}
