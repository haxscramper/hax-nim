#!/usr/bin/env perl
# -*- coding: utf-8 -*-
# perl
use warnings;
use strict;

use File::Copy;
use Data::Dumper;
use Storable qw(dclone);

# Allow to use true and false as named constants
use constant false => 0;
use constant true  => 1;

# Allow to use functions with parameter signatures
use v5.20;
use feature qw(signatures);
no warnings qw(experimental::signatures);
# Pretty message printing
sub log1  ($message, $ind = 0) { system("colecho -I:$ind -- '$message'"); }
sub err1  ($message, $ind = 0) { system("colecho -e:2 -I:$ind -- '$message'"); }
sub info1 ($message, $ind = 0) { system("colecho -i:1 -I:$ind -- '$message'"); }
sub warn1 ($message, $ind = 0) { system("colecho -w:1 -I:$ind -- '$message'"); }



sub get_balance ($line) {
    my @open_matches = ($line =~ /\{/g);
    my @clos_matches = ($line =~ /\}/g);
    my $balance = @open_matches - @clos_matches;
    return $balance;
}

my @imports;
my @classes;
my $main_class;
my $input_file = "main.java";

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
            push (@{ $class{'body'} }, $line);
            if ($paren_balance == 0) {
                $class_started = false;
                $class{'end'} = $.;
                my $copy = dclone(\%class);
                push @classes, $copy;
            }
        }
    }
}

my $dir = "$input_file.d";

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
unlink "main.java";

sub system1($command) {
    say $command;
    system($command);
}

log1($_, 2) for glob "*.java";
system1("javac $_") for glob "*.java";
writeFile("manifest.txt", "Main-Class: $main_class\n");
system1("jar cfvm out.jar manifest.txt " . join(" ", map { "'$_'" } glob "*.class"));
