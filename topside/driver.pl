#!/usr/bin/env perl
use warnings;
use strict;
use Time::HiRes;

my %CMD = (
    PRELUDE => 0x50,
    SET_MOTORS => 0x40,
    OK => 0x80,
);

open my $fh, "+<", "/dev/ttyUSB0";

while (1) {
    my $byte = pack "H*", map /(\w+)/, scalar <STDIN>;
    my $msg = $CMD{PRELUDE} . $CMD{SET_MOTORS} . $byte;
    do {
        #print "Sending: ", (join " ", (unpack "B*", $msg) =~ m/(.{8})/g), "\n";
        print $fh $byte;
    } until ord getc $fh == $CMD{OK};
    print "OK\n";
}

END {
    close $fh;
}
