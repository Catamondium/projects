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

multi sub trait_mod:<is>(Routine $f, :$debug!) {
	$f.wrap: sub (|args) {
		my \ret := callwith(|args);
		say "{$f.name}({\args.gist}) -> {\ret.gist}";
		|ret;
	}
}

sub pTime (Str $s) is debug {
	my @subs = $s.split(':');
	Time.new: :hrs(@subs[0].Int), :mins(@subs[1].Int);
}

multi infix:<+>(Time $t, Int $elapse) {
	my $tot = Int($t) + Int($elapse);
	Time.new: hrs => ($tot / 60).floor, mins => $tot % 60;
}

my %*SUB-MAIN-OPTS = :named-anywhere(True);
sub MAIN(Str $time, Int $elapse, Bool :$quiet=False) {
	say $time;
	say $elapse;
	say $quiet;
}
