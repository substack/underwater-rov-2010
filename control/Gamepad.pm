use MooseX::Declare;
use Linux::Joystick;

class Gamepad {
    has 'state' => (
        isa => 'HashRef',
        is => 'ro',
        default => sub { +{
            axes => [ map { $_ => 0 } qw/left right/ ],
            buttons => [ map { $_ => 0 } 1 .. 8, qw/left right up down/ ]
        } },
    );
    
    has 'js' => (
        isa => 'Linux::Joystick',
        is => 'ro',
        default => sub { Linux::Joystick->new(
            nonblocking => 1,
            device => 0,
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
    
    method run () {
        while (1) {
            my $ev = $self->js->nextEvent // return;
            $self->handle($ev);
            $self->hook->();
        }
    }
    
    method handle ($ev) {
        if ($ev->isAxis) {
            $self->state->{$ev->axis} = $ev->axisValue;
        }
        elsif ($ev->isButton) {
            my $key = {
                (map { $_ => $_ - 1 } 1 .. 8),
                left => 9, right => 10,
                up => 11, down => 12,
            }->{ $ev->{button} };
            $self->state->{buttons}{$key} = $ev->buttonDown;
            
            my $bev = $ev->buttonDown ? $self->on_down : $self->on_up;
            ($bev->{$key} // sub {})->();
        }
    }
}

no Moose;
