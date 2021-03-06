// Based on serial.c code by Orion Sky Lawlor (olawlor@acm.org, 2004/1/14)
#include <pic.h>
typedef unsigned char byte;

__IDLOC(1);
__CONFIG(INTIO & WDTDIS & MCLRDIS & BORDIS & UNPROTECT & PWRTEN);

// wait for integer seconds
void busywait_sec(byte sec) {
    byte i, j, k;
    for (i = sec * 4; i != 0; i--) // seconds
        for (j = 250; j != 0; j--) // quarters of seconds
            for (k = 250; k != 0; k--) NOP(); // 1 ms
}

// wait for integer deciseconds
void busywait_dsec(byte dsec) {
    byte i,j,k;
    for (i = dsec; i != 0; i--) // tenths of seconds
        for (j = 100; j != 0; j--) // 10 ms = 0.01 sec
            for (k = 250; k != 0; k--) NOP(); // 1 ms
}

// wait for integer centiseconds
void busywait_csec(byte csec) {
    byte i,j,k;
    for (i = csec; i != 0; i--) // hundredths of seconds
        for (j = 10; j != 0; j--) // 10 ms = 0.01 sec
            for (k = 250; k != 0; k--) NOP(); // 1 ms
}

// wait for integer milliseconds
void busywait_msec(byte msec) {
    byte i,j;
    for (i = msec; i != 0; i--) // 10 ms = 0.01 sec
        for (j = 250; j != 0; j--) NOP(); // 1 ms
}

void main(void) {
    TRISIO = 0x0; // all 6 pins are outputs
    
    CMCON = 0x07; /* comparator off */
    WPU=0; /* disable weak pull-up on all pins*/
    
    //        4 MHz  | analog pin 1
    //ANSEL = (1 << 4) | (1 << 0);
    ANSEL = 0x00;
    
    // ADCON0
    CHS0 = CHS1 = 0; // choose A/D input pin
    ADFM = 0; // left-justified
    VCFG = 0; // vdd reference voltage
    ADON = 1; // the converter module is operating
    
    OPTION = 0x80; // disable weak pull-up globally
    TMR0 = 0;
    
    /* Turn on interrupts on GPIO0 */
    IOCB=0x01; /* interrupt on pin 0 change */
    GPIO = 0x0; /* clear output pins */
    GPIF=0; /* clear interrupt flag */
    GPIE=1; /* enable GPIO interrupts */
    
    GIE=0; /* turn off interrupts */
    busywait_msec(1); /* wait for lines to settle before sending */
    
    TRISIO |= (1 << 2);
    
    while (1) {
        GPIO = 0xff;
        // 4ms half-duty cycle is the lower bound for prop motion
        // with vanilla thruster and plastic propeller
        busywait_dsec(3);
        GPIO = 0x00;
        busywait_dsec(1);
    }
}
