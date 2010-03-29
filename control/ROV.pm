use MooseX::Declare;

class ROV {
    has 'serial' => (
        isa => 'Str',
        is => 'ro',
        default => '/dev/ttyUSB0',
    );
    
    has 'fh' => (
        is => 'ro',
        default => sub {
            system "stty raw clocal 57600 cs8 -parenb parodd cstopb -echo < /dev/ttyUSB0";
            open my $fh, "+<", shift->serial;
            select $fh;
            $|++;
            select STDOUT;
            return $fh;
        },
    );
    
    has 'motors' => (
        is => 'rw',
        isa => 'HashRef',
        default => sub { +{
            vertical => 0,
            left => 0,
            right => 0,
        } },
    );
    
    method motor_byte {
        my $byte = 0;
        
        for my $i (0 .. 2) {
            my $motor = (qw/vertical left right/)[$i];
            my $v = $self->motors->{$motor};
            $byte |= ((abs $v > rand) << (($i * 2) + ($v > 0)));
        }
        
        return $byte;
    }
    
    method send($byte) {
        my $fh = $self->fh;
        print $fh $byte;
    }
    
    method recv {
        my $fh = $self->fh;
        getc $fh;
    }
}

no Moose;
