#!/usr/bin/env perl
# Wall-clock one subprocess for the benchmark driver (ADR-0075 §5): spawn the argv command with
# stdout/stderr discarded, print elapsed seconds. Whole-process timing by design — no guest clock.
# Exits 111 when the child fails, so the driver never records a crashed run as a timing.
use strict;
use warnings;
use Time::HiRes qw(time);

my $t0 = time;
my $rc = system("sh", "-c", 'exec "$@" >/dev/null 2>&1', "sh", @ARGV);
exit 111 if $rc != 0;
printf "%.4f\n", time - $t0;
