#!/usr/bin/perl

use strict;
use warnings;

sub test { 
    print "computed\n";
    #print join(", ", @_), "\n";
    my $a = shift;
    my $b = shift;
    return $a + $b;
}

sub fibonacci {
    my ($n) = @_;
    print "fibonacci($n)\n";
    return 1 if $n == 1 or $n == 2;
    return fibonacci($n-1) + fibonacci($n-2);
}

sub memoize {
    my ($func) = @_;
 
    my $original = \&{$func};
    my %known_results;
    my $sub = sub {
        my @lc_args = @_;
        my $lc_name =  join(", ", @lc_args);
        if (not exists $known_results{$lc_name}) {
            $known_results{$lc_name} = $original->(@lc_args);
        }
        return $known_results{$lc_name};
    };
    no strict 'refs';
    no warnings 'redefine';
    *{$func} = $sub;
}

memoize('test');
print test(1, 2), "\n";
print test(1, 2), "\n";
print test(3, 3), "\n";

memoize('fibonacci');
 print fibonacci(6);