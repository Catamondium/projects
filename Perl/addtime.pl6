#!/usr/bin/perl6
use strict;
use worries;

class Time {
	has Int $.hrs;
	has Int $.mins;

	method Str (-->Str) {
		sprintf "%02d:%02d", $!hrs, $!mins;
	}

	method Int (-->Int) {
		($!hrs*60) + $!mins;
	}
}

sub pTime (Str $s) {
	my @subs = $s.split(':');
	Time.new: :hrs(@subs[0].Int), :mins(@subs[1].Int);
}

multi infix:<+>(Time $t, Int $elapse) {
	my $tot = Int($t) + Int($elapse);
	Time.new: :hrs(($tot / 60).floor), :mins($tot % 60);
}

my %*SUB-MAIN-OPTS = :named-anywhere(True);
sub MAIN(Str $start, Str $mins, Bool :$quiet=False) {
	my $begin = pTime $start;
	my $elapse = $mins.contains(':') ?? Int(pTime($mins)) !! Int($mins);

	if $quiet {
		print "{$begin + $elapse}\n";
	} else {
		printf "Start:\t{$begin}\t%+d\nEnd:\t{$begin + $elapse}\n", $elapse
	}
}
