#!/usr/bin/perl
use strict;
use warnings FATAL => 'all';
use Term::ANSIColor;
use v5.10;

my $version = 'owo --version';
say "$version: @{[ `$version` ]}";
my @failure = ();
my $success = 0;
my $noTerm = scalar @ARGV && $ARGV[0] eq '--no-terminal';

sub ntr {return colored $_[0], 'green';}
sub red {return colored $_[0], 'red';}
sub redy {return colored $_[0], 'bold red';}

foreach my $fixture (map {substr $_, 0, -1} split /[ \t\n]+/, `ls -G -d */`) {
    say "Fixture $fixture:";
    `touch $fixture.flags`;
    my $flags = `cat $fixture.flags`;
    foreach my $case (split /[ \t\n]+/, `ls -G $fixture/*.owo`) {
        say " Case $case:";
        my $out = $case =~ s/\.owo/\.out/rg;
        `touch $out`;
        my $diff = `owo $flags -c $case | diff - $out`;
        if (length $diff) {
            push @failure, $case;
            map {say red("  $_")} split /\n/, $diff;
            print colored("  Replace the golden value (y/N)? ", 'cyan');
            !$noTerm && getc eq 'y' ? `owo $flags -c $case > $out`
                : say colored('  Leave it alone.', 'bold yellow');
        } else {
            say ntr('  Passed!');
            $success++;
        }
    }
}

my $failed = scalar @failure;
say 'Result: ', $failed ? redy('FAILED.') : ntr('ok.'),
    ntr(" $success passed,"),
    colored(" $failed failed.", $failed ? 'bold red' : 'white');
if ($failed) {
    my $pretty = join "\n ", @failure;
    say red("Failing tests:\n $pretty");
    die;
}
