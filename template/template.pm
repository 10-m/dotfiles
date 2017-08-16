package xxxx;

use version;
use strict;
use warnings;
use parent qw(Exporter Class::Accessor);

our $AUTOLOAD;
our $VERSION   = qv('0.0.1');
our @EXPORT    = qw();
our @EXPORT_OK = qw();

__PACKAGE__->follow_best_practice;
__PACKAGE__->mk_accessors(qw(name salary age));

my %_default = ( name => 'No Name', salary => 0 );

sub new {
    my ( $class, @args ) = @_;
    my %arg = ref $args[0] eq 'HASH' ? %{ $args[0] } : @args;
    my $obj = {};
    $obj = $class->SUPER::new(%arg) if $class->can('SUPER::new');
    my $self = bless { %_default, %{$obj} }, $class;
    # no need to inherit
    # my $slef = bless { %_default, %arg }, $class;
    return $self;
}

sub init {
    my $self = shift;
    $self->SUPER::init() if $self->can('SUPER::init');
    $self->{$_} = $_default{$_} for keys %_default;
    return $self;
}

sub clone {
    my $self  = shift;
    my $class = ref $self;
    return bless { %{$self} }, $class;
}

1;
__END__

my $obj = xxxx->new({name => 'AAA'});
my $obj = xxxx->new(name => 'AAA');

# uniq name
my $package = PACKAGE;
$self->{$package . "::var_name"};
