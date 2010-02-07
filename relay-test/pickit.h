/*
Little utility routines for running the 
Microchip(tm) PICkit(tm) FLASH Starter Kit
USB device programmer.

Orion Sky Lawlor, olawlor@acm.org, 2003/8/4 (public domain)
*/
#ifndef __OSL_PIC_PICKIT_H
#define __OSL_PIC_PICKIT_H

#include <pic.h>

/* Check for 14-pin chips: comes in from Orion's Makefile.common */
#if defined(PIC16F676)||defined(PIC16F688)||defined(PIC16F675)

/* Tack on 14-pin registers on top of existing 12F regs */
#define TRISA TRISIO 
#define PORTA GPIO 
/* Port C control (from Microchip docs) */
static volatile       unsigned char     PORTC    @ 0x07;
static          bank1 unsigned char     TRISC   @ 0x87;
static volatile       unsigned char     ADCON1  @ 0x9F;
#define WPUA WPU /* (there is no WPUC) */

#endif

typedef unsigned char byte;

/**
 Busywait for this long (FIXME: should use sleep & timer).
 WARNING: Comes out at *half* the requested delay with picl 9.60!
*/
void busywait(byte ms) {
	for (;ms!=0;ms--) 
	{ // Wait for one millisecond (depends on compiler, optimizer)
		byte c;
		for (c=250;c!=0;c--) { NOP(); }
	}
}

/**
 Call the OSCCAL routine loaded up at address 0x3ff, 
 and copy that value to the clock rate register, OSCCAL.
 The factory actually writes the routine at 0x3ff, although
 you can tweak it yourself later if you prefer (by writing address 0x3fff).
 
 This is ONLY needed (and possible) on the 12F675 and 16F676 chips.
*/
void load_osccal(void) {
#ifdef PIC16F676
	typedef byte (*osccal_fn)(void);
	osccal_fn fn=(osccal_fn)0x3ff;
	OSCCAL=fn();
#endif
}

// Turn all hardware off (e.g., at startup)
void clear(void) {
	TRISIO = 0xFF;
	GPIO = 0; 
	VRCON = 0; 
	CMCON = 0x07;
	TMR0 = 0;
	OPTION = 0x80;
	ANSEL = 0x31;
	ADCON0 = 0x01;
	T0IE = 0;
	ADIE = 0;
	GIE = 0;
	T0IF = 0;
	ADIF = 0;
	GPIF = 0;
}

#endif
