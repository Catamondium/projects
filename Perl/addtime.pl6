#!/usr/bin/perl6
use strict;
use worries;

class Time {
	has Int $.hrs;
	has Int $.mins;

	multi method fromStr (Str $s) {
		my @subs = $s.split(":");
		self.bless(hrs=>@subs[0].Numeric, mins=>@subs[1].Numeric);
	}

	multi method Str (-->Str) {
		sprintf "%02d:%02d", $!hrs, $!mins;
	}

	multi method Int (-->Int) {
		($!hrs*60) + $!mins;
	}
}

multi infix:<+>(Time $t, Int $elapse) {
	my $tot = Int($t) + $elapse;
	Time.new(hrs => ($tot / 60).floor, mins => $tot % 60);
}

my $t = Time.fromStr("1");
say ~$t;
