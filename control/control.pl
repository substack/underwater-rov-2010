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
use Time::HiRes;
Gamepad->new(
    hook => sub {
        my $gamepad = shift;
        my $lx = $gamepad->axis('left')->{x};
        my $ly = $gamepad->axis('left')->{y};
        my $ry = $gamepad->axis('right')->{y};
        
        my $theta = 2 * atan2 1 - $lx, -$ly;
        my $d = sqrt $lx ** 2 + $ly ** 2;
        
        $rov->motors({
            vertical => $ry,
            left => ((sin $theta) + (cos $theta)) * $d,
            right => ((sin $theta) - (cos $theta)) * $d,
        });
        my $byte = $rov->motor_byte;
        print "$byte\n";
        $rov->send($byte);
        print +(unpack "H*", $rov->recv), "\n";
    },
    on_down => sub { print "down!" },
    on_up => sub { print "up!" },
)->run;

END { 
    #ReadMode 0;
    #print "\r\n";
    $rov->send(0);
}
