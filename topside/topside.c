#include <pic.h>

// Set up magical safety stuff
__IDLOC(1);
__CONFIG(
    INTIO & WDTDIS & MCLRDIS & BORDIS & UNPROTECT & PWRTEN & LVPDIS
);

void main() {
    // Most tri-states to 0
    TRISA = TRISB = TRISD = TRISE = 0;
    TRISC = 1 << 7; // RC7 to 1 for serial RX
    
    // No analog
    ANSEL = 0;
    
    // Turn off interrupts
    // perl -pe'($_)=/(\w+IE\b)/ and ($_="    $_ = 0;\n")' pic16f887.h
    RBIE = RABIE = T0IE = PEIE = GIE = TMR1IE = TMR2IE = CCP1IE = SSPIE = TXIE =
    RCIE = ADIE = CCP2IE = ULPWUIE = BCLIE = EEIE = C1IE = C2IE = OSFIE = 0;
    
    // Turn off motors and LEDs
    PORTA = PORTB = PORTC = 0x0;
    PORTE = ~0x0;
    RD0 = RD1 = 0;
    
    while (1) {
        NOP();
    }
}

