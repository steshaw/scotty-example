#!/usr/bin/perl

use Sys::CpuAffinity;

$num_cpus = Sys::CpuAffinity::getNumCpus();

print "num-cpus = $num_cpus\n";
