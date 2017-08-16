#!/usr/bin/perl
use strict;
use warnings;
use autodie;
use Carp;
use Data::Dumper;

sub D { print Dumper @_; }

############################################
# Usage      : Config::Auto->get_defaults ()
# Purpose    : Defaults for 'new'
# Returns    : A hash of defaults
# Parameters : none
# Throws     : no exceptions
# Comments   : No corresponding attribute,
#            : gathers data from each
#            : attr_def attribute
# See Also   : $self->set_default()

run_tests() if $ENV{HARNESS_ACTIVE};

sub run_tests {
    exit;
}
__END__

=head1 NAME %file%
