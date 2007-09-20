use strict;
use warnings;

use inc::Module::Install;

name 'HTML-TableParser';

all_from 'lib/HTML/TableParser.pm';

requires 'HTML::Entities';
requires 'HTML::Parser' => 3.26;
build_requires 'Test::More' => 0.32;

auto_install;
WriteAll;

