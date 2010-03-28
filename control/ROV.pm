use MooseX::Declare;

class ROV {
    has 'dev' => (
        isa => 'Str',
        is => 'ro',
        default => '/dev/ttyUSB0',
    );
}

no Moose;
