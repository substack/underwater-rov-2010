#!/usr/bin/env perl
use warnings;
use strict;
use Time::HiRes;

sub read_test {
    open my $fh, "<", "/dev/ttyUSB0";
    while (1) {
        my $byte = getc $fh;
        print unpack "B*", $byte;
        print "\n";
    }
    close $fh;
}

sub write_test {
    open my $fh, ">", "/dev/ttyUSB0";
    for (0 .. 10) {
        print $fh $_ for 0x53, 0x41, (1 << 1) | (1 << 2) | (1 << 5);
        sleep 0.2;
        print $fh $_ for 0x53, 0x41, (1 << 0) | (1 << 3) | (1 << 4);
    }
    close $fh;
}

read_test();
