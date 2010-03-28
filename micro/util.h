#ifndef PIC_UTIL_H
#define PIC_UTIL_H
#include <pic.h>

typedef unsigned char byte;
// Safety settings
__IDLOC(1);
__CONFIG(INTIO & WDTDIS & MCLRDIS & BORDIS & UNPROTECT & PWRTEN & LVPDIS);

// wait for integer deciseconds
void wait_dsec(byte dsec) {
    byte i,j,k;
    for (i = dsec; i != 0; i--) // tenths of seconds
        for (j = 100; j != 0; j--) // 10 ms = 0.01 sec
            for (k = 250; k != 0; k--) NOP(); // 1 ms
}

// wait for integer centiseconds
void wait_csec(byte csec) {
    byte i,j,k;
    for (i = csec; i != 0; i--) // hundredths of seconds
        for (j = 10; j != 0; j--) // 10 ms = 0.01 sec
            for (k = 250; k != 0; k--) NOP(); // 1 ms
}

void serial_init(void) {
    IRCF0 = IRCF1 = IRCF2 = 1;
    BRG16=1;
    BRGH=1; /* now baud rate will equal Fosc/(4*(n+1)) */
    //SPBRG = 34; /* or 16 for 117kbps (at 8MHz internal clock) */
    SPBRG = 34; /* or 16 for 117kbps (at 8MHz internal clock) */
    SPBRGH=0; /* high 8 bits of baud divisor */
    // async operation, RS-232 inversion
    SYNC=0;
    SPEN=1;
    TXEN=1;
    CREN=1;
    SCKP=0; // RS-232 style data inversion
    
    ADDEN=0; /* don't do address detection */
    ABDEN=0; /* don't do auto-baud-detect */
}

void init(void) {
    // Turn off interrupts
    // perl -pe'($_)=/(\w+IE\b)/ and ($_="    $_ = 0;\n")' pic16f887.h
    RBIE = RABIE = T0IE = PEIE = GIE = TMR1IE = TMR2IE = CCP1IE = SSPIE = TXIE =
    RCIE = ADIE = CCP2IE = ULPWUIE = BCLIE = EEIE = C1IE = C2IE = OSFIE = 0;
    
    serial_init();
}

void serial_tx(byte b) {
    while (!TXIF) NOP();
    TX9D = 0; // no parity
    TXREG = b;
    // inexplicably, tx doesn't work unless after transmission at least 4
    // hundreths of a second elapse between calls
    wait_csec(5);
}

//static bit rcv, should; /* parity bit */
byte serial_rx(void) {
    int count=0;
    int r;
    while (!RCIF) {}
    
    if (OERR) {
        CREN=0; CREN=1;
    }
    if (FERR) {
        int q = RCREG;
        FERR = 0; /* framing error--e.g., cable unplugged */
        //PORTE = ~0x1;
    }
    
    //rcv=RX9D; /* extract parity bit first */
    r=RCREG; /* pull received data */
    return r;
}

#endif
