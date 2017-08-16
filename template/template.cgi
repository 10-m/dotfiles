#!/usr/bin/perl -T
use strict;
use warnings;
use utf8;
use CGI;
use CGI::Carp qw(fatalsToBrowser);
use CGI::Pretty;

my $q = CGI->new();
print $q->header( -charset => 'UTF-8' );
print $q->start_html(
    -lang  => 'ja',
    -title => ' T I T L E '
);

print $q->end_html;
