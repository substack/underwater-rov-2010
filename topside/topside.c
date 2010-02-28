#include <pic.h>
typedef unsigned char byte;

// Set up magical safety stuff
__IDLOC(1);
__CONFIG(INTIO & WDTDIS & MCLRDIS & BORDIS & UNPROTECT & PWRTEN & LVPDIS);

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

void main() {
    TRISA = 0;
    TRISB = 0;
    TRISC = 1 << 7; // RC7 to 1 for serial RX
    TRISD = 0;
    TRISE = 0;
    
    // No analog
    ANSEL = 0;
    
    // Turn off interrupts
    // perl -pe'($_)=/(\w+IE\b)/ and ($_="    $_ = 0;\n")' pic16f887.h
    RBIE = RABIE = T0IE = PEIE = GIE = TMR1IE = TMR2IE = CCP1IE = SSPIE = TXIE =
    RCIE = ADIE = CCP2IE = ULPWUIE = BCLIE = EEIE = C1IE = C2IE = OSFIE = 0;
    
    // Everything off
    PORTA = 0x00;
    PORTB = 0x00;
    PORTC = 0x00;
    PORTE = 0x00;
    PORTD = 0x00;
    
    while (1) {
        PORTA = (1 << 0) | (1 << 3) | (1 << 4);
        busywait_csec(1);
        PORTA = (1 << 1) | (1 << 2) | (1 << 5);
        busywait_csec(1);
    }
}

