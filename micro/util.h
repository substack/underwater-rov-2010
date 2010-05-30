#ifndef PIC_UTIL_H
#define PIC_UTIL_H
#include <pic.h>

typedef unsigned char byte;
// Safety settings
__IDLOC(1);
__CONFIG(INTIO & WDTDIS & MCLRDIS & BORDIS & UNPROTECT & PWRTEN & LVPDIS);

// wait for dsec deciseconds
void wait_dsec(byte dsec) {
    byte i,j,k;
    for (i = dsec; i != 0; i--) // tenths of seconds
        for (j = 100; j != 0; j--) // 10 ms = 0.01 sec
            for (k = 250; k != 0; k--) NOP(); // 1 ms
}

// wait for csec centiseconds
void wait_csec(byte csec) {
    byte i,j,k;
    for (i = csec; i != 0; i--) // hundredths of seconds
        for (j = 10; j != 0; j--) // 10 ms = 0.01 sec
            for (k = 250; k != 0; k--) NOP(); // 1 ms
}

// wait for msec milliseconds
void wait_msec(byte msec) {
    byte i;
    for (; msec != 0; msec--)
        for (i = 250; i != 0; i--) NOP(); // 1 ms
}

// wait for msec milliseconds plus (frac/256) milliseconds
void wait_msecf(byte msec, byte frac) {
    byte i, j, msec_, frac_;
    for (i = 0; i < 2; i++) {
        for (msec_ = msec; msec_ != 0; msec_--)
            for (j = 250; j != 0; j--) NOP(); // 1 ms
        for (frac_ = frac; frac_ != 0; frac_--) NOP(); // 1 ms
    }
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
    
    TRISC = 1 << 7; // RC7 to 1 for serial RX
}

byte read_analog(byte ans) {
    switch (ans) {
        case 0 : TRISA0 = 1; break;
        case 1 : TRISA1 = 1; break;
        case 2 : TRISA2 = 1; break;
        case 3 : TRISA3 = 1; break;
        case 4 : TRISA5 = 1; break;
        case 5 : TRISE0 = 1; break;
        case 6 : TRISE1 = 1; break;
        case 7 : TRISE2 = 1; break;
    }
    ADFM = 0;
    VCFG0 = 0;
    VCFG1 = 0;
    ADIE = 0;
    ANSEL = 1 << ans;
    
    // page 106 from the spec sheet
    //       ADON=1     GODONE=1   ADCS1=1    CH0,CH1,CH2,CH3
    ADCON0 = (1 << 0) | (1 << 1) | (1 << 7) | (ans << 2);
    while (GODONE) {}
    return ADRESH;
}

void init(void) {
    // Turn off interrupts
    // perl -pe'($_)=/(\w+IE\b)/ and ($_="    $_ = 0;\n")' pic16f887.h
    RBIE = RABIE = T0IE = PEIE = GIE = TMR1IE = TMR2IE = CCP1IE = SSPIE = TXIE =
    RCIE = ADIE = CCP2IE = ULPWUIE = BCLIE = EEIE = C1IE = C2IE = OSFIE = 0;
    
    // Everything off
    PORTA = 0x00;
    PORTB = 0x00;
    PORTC = 0x00;
    PORTD = 0x00;
    PORTE = 0x00;
    
    TRISA = 0;
    TRISB = 0;
    TRISC = 0;
    TRISD = 0;
    TRISE = 0;
    ANSEL = 0; // No analog
    
    serial_init();
}

void serial_tx(byte b) {
    while (!TXIF) NOP();
    TX9D = 0; // no parity
    TXREG = b;
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
