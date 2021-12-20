#!/usr/bin/perl

use strict;
use warnings;

sub random_pwd {
    my $length = shift;
    my @chars = map(chr, (33 .. 126));
    return join '', @chars[ map rand @chars, 0 .. $length ];
}

my $length = shift;
my $amount = shift;
my $i;
for ($i = 0; $i < $amount; $i++){
    print random_pwd($length), "\n";
}