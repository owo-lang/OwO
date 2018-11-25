#!/usr/bin/perl
use strict;
use warnings FATAL => 'all';
use Term::ANSIColor;
use v5.10;

my $version = 'owo --version';
say "$version: @{[ `$version` ]}";
my @failure = ();

foreach my $fixture (map {substr $_, 0, -1} split(/[ \t\n]+/, `ls -G -d */`)) {
    say colored("Fixture $fixture:", 'cyan');
    foreach $_ (split(/[ \t\n]+/, `ls -G $fixture/*.owo`)) {
        say colored("  Case $_:", 'yellow');
        my $diff = `owo --dump-tokens -c $_ | diff - @{[ s/\.owo/\.out/rg ]}`;
        if (length $diff) {
            push @failure, $_;
            say colored(join("\n", map {"    $_"} split(/\n/, $diff)), 'red');
        } else {
            say colored('    Passed!', 'green');
        }
    }
}

say '';
if (scalar @failure) {
    my $pretty = join("\n  ", @failure);
    say colored("Failing tests:\n  $pretty", 'red');
    die;
} else {
    say colored('All tests passed!', 'green');
}
