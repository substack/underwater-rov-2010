use MooseX::Declare;
use Linux::Joystick;
use Time::HiRes;

class Gamepad {
    has 'axes' => (
        isa => 'HashRef',
        is => 'ro',
        default => sub { +{
            map { $_ => { x => 0, y => 0 } } qw/left right/
        } },
    );
    
    method axis(Str $key) { $self->axes->{$key} }
    
    has 'buttons' => (
        isa => 'HashRef',
        is => 'ro',
        default => sub { +{ map { $_ => 0 } 1 .. 8, qw/left right up down/ } },
    );
    
    method button(Str $key) { $self->buttons->{$key} }
    
    has 'js' => (
        isa => 'Linux::Joystick',
        is => 'ro',
        default => sub { Linux::Joystick->new(
            blocking => 1,
            device => 1,
        ) },
    );
    
    has 'hook' => (
        isa => 'CodeRef',
        is => 'rw',
        default => sub { sub {} }
    );
    
    has 'on_down' => (
        isa => 'CodeRef',
        is => 'rw',
        default => sub { sub {} }
    );
    
    has 'on_up' => (
        isa => 'CodeRef',
        is => 'rw',
        default => sub { sub {} }
    );
    
    method run {
        print "Ready\n";
        while (1) {
            while (defined (my $ev = $self->js->nextEvent)) {
                $self->handle($ev);
            }
            $self->hook->($self);
        }
    }
    
    method handle($ev) {
        if ($ev->isAxis) {
            my $xy = {
                0 => 'x',
                1 => 'y',
                2 => 'y',
                3 => 'x',
            }->{$ev->axis};
            my $lr = $ev->axis < 2 ? 'left' : 'right';
            
            $self->axes->{$lr}{$xy} = $ev->axisValue / (2**15 - 1);
        }
        elsif ($ev->isButton) {
            my $key = {
                (map { $_ => $_ - 1 } 1 .. 8),
                left => 9, right => 10,
                up => 11, down => 12,
            }->{ $ev->{button} };
            $self->buttons->{$key} = $ev->buttonDown;
            
            my $bev = $ev->buttonDown ? $self->on_down : $self->on_up;
            ($bev->{$key} // sub {})->();
        }
    }
}

no Moose;