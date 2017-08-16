#!/usr/bin/perl
use strict;
use warnings;
use Test::More;
use feature 'say';
use Data::Dumper;

sub D { print Dumper @_; }

package ObjectToTest;
    sub new {
        my ($class, @args) = @_;
        bless \@args, $class;
    }

    sub revert {
        reverse $_[1];
    }
1;

package main;

subtest 'instance is returned' => sub {

    # Arrange
    my $object = ObjectToTest->new;

    # Act
    my $reverted = $object->revert('abc');

    # Assert
    is( $reverted, 'cba' );
};

done_testing;
