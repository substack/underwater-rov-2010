/**
  Test routine for serial.c interface.
  Orion Sky Lawlor, olawlor@acm.org, 2004/1/14
*/
#include "pickit.h"
//#include "serial.h"

__IDLOC(1);
__CONFIG(INTIO & WDTDIS & MCLRDIS & BORDIS & UNPROTECT & PWRTEN);

/* Busy-wait for this fraction of a millisecond.
  FIXME: using timer here would use less power and generate less heat
*/
void busywait_subms(byte fract) {
    // Wait for one millisecond (depends on compiler, optimizer)
    byte c;
    for (c=fract;c!=0;c--) { NOP(); }
}

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
    for (i = dsec; i != 0; i--) // hundredths of seconds
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

/* Run this GPIO line (expressed as a bit mask)
   at this fractional power.  0 sends a 1ms pulse,
   255 sends a 2ms pulse. */
void fire_pwm(byte fract, byte pin) {
    GPIO |= pin; /* turn pin on */
    busywait_subms(250); /* wait 1ms always */
    busywait_subms(fract); /* wait variable delay */
    GPIO &= ~pin; /* turn pin off again */
}
void fire_pwmc(byte fract, byte pin) {
    //PORTC |= pin; /* turn pin on */
    busywait_subms(250); /* wait 1ms always */
    busywait_subms(fract); /* wait variable delay */
    //PORTC &= ~pin; /* turn pin off again */
}

void main(void) {
    load_osccal();
    TRISIO = 0x0;
    //TRISC = 0; /* all six port-c pins are outputs */
    
    CMCON = 0x07; /* comparator off */
    WPU=0; /* disable weak pull-up on all pins*/
    
    //*
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
    busywait(250); /* wait for lines to settle before sending */
    
    TRISIO |= (1 << 2);
    
    while (1) {
        //GPIO |= (1 << 2);
        GPIO = 0xff;
        busywait_sec(1); // 1 second
        //GPIO &= ~(1 << 2);
        GPIO = 0x00;
        busywait_sec(1); // 1 second
    }
}
