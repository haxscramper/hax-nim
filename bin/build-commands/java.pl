#!/usr/bin/env perl
# -*- coding: utf-8 -*-
# perl

use warnings;
use strict;

# Command line argument parsing
use Pod::Usage;
use Getopt::Long;
use Data::Dumper;
use File::Which;
use File::Copy;
use Storable qw(dclone);

# Allow to use true and false as named constants
use constant false => 0;
use constant true  => 1;

# Allow to use functions with parameter signatures
use v5.20;
use feature qw(signatures);
no warnings qw(experimental::signatures);

sub command_exists($comm) {
    return `which $comm 2> /dev/null` ne "";
}

# Pretty message printing
my $pretty_msg = command_exists("colecho");
sub log1($message)  {
    if ($pretty_msg) { system("colecho -- \"$message\"");
    } else { say "  - $message"; } }
sub err1($message)  {
    if ($pretty_msg) { system("colecho -e:2 -- \"$message\"");
    } else { say "!!! $message"; } }
sub info1($message) {
    if ($pretty_msg) { system("colecho -i:1 -- \"$message\"");
    } else { say "--> $message"; } }
sub warn1($message) {
    if ($pretty_msg) { system("colecho -w:1 -- \"$message\"");
    } else { say "=>> $message"; } }



sub get_balance ($line) {
    my @open_matches = ($line =~ /\{/g);
    my @clos_matches = ($line =~ /\}/g);
    my $balance = @open_matches - @clos_matches;
    return $balance;
}

my @imports;
my @classes;
my $main_class = "";
say "Arguments:", @ARGV;
my $input_file = $ARGV[0];

{
    my %class;
    my $paren_balance = 0;
    my $class_started = false;

    open my $fh, "$input_file";
    while (my $line = <$fh>) {
        chomp $line;
        #$line =~ s/^\s*(.*?)\s*$/$1/;
        if ($line =~ /import (.*)$/) {
            push @imports, "import $1";
        } elsif ($line =~ /.*?class (\w+)/ and not $class_started) {
            %class = (
                name => $1,
                start => $.,
                body => [ ],
            );
            $class_started = true;
        }

        if ($class_started) {

            if ($line =~ /public static void main.*/) {
                $main_class = $class{'name'};
            }

            my $add = get_balance($line);
            # printf "%-50s --> $paren_balance/$add\n", $line;
            $paren_balance += $add;
            push (@{ $class{'body'} },  "$line");
            if ($paren_balance == 0) {
                $class_started = false;
                $class{'end'} = $.;
                my $copy = dclone(\%class);
                push @classes, $copy;
            }
        }
    }
}

my $dir = ".$input_file.d";

rmdir "$dir";
mkdir "$dir";
chdir "$dir";

unlink glob "*.class";
unlink glob "*.java";
info1("Removed old files");

copy "../$input_file", "$input_file" or die "No input file '$input_file'";

sub writeFile($file, $string) {
    open (FH, ">", $file);
    print FH $string;
    close FH;
}

for my $class (@classes) {
    writeFile(
        $class->{'name'} . ".java",
          join("\n", @imports) . "\n"x2 . join("\n", @{ $class->{'body'} }))
}

info1("Main class is $main_class");
info1("Generated files:");
unlink "$input_file";

sub system1($command) {
    say $command;
    system($command);
}

log1($_) for glob "*.java";
system1("javac $_") for glob "*.java";
writeFile("manifest.txt", "Main-Class: $main_class\n");
system1("jar cfvm out.jar manifest.txt " . join(" ", map { "'$_'" } glob "*.class"));
