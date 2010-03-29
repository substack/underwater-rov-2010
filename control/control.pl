#!/usr/bin/env perl
# ROV control script
use 5.10.0;
use warnings;
use strict;
$|++;

use ROV;
my $rov = ROV->new(
    @ARGV ? (serial => shift) : ()
);

#use Term::ReadKey;
#ReadMode 4;

use Gamepad;
Gamepad->new(
    hook => sub {
        my $gamepad = shift;
        my $lx = $gamepad->axis('left')->{x};
        my $ly = $gamepad->axis('left')->{y};
        my $ry = $gamepad->axis('right')->{y};
        
        $rov->motors({
            vertical => $ry,
            left => ($ly - $lx) / ($ly + $lx) * ($ly - $lx),
            right => ($ly - $lx) / ($ly + $lx) * ($lx - $ly),
        });
        $rov->send($rov->motor_byte);
    },
    on_down => sub { print "down!" },
    on_up => sub { print "up!" },
)->run;

#END { 
#    ReadMode 0;
#    print "\r\n";
#}

