use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    'NAME'		=> 'HTML::TableParser',
    'VERSION_FROM'	=> 'TableParser.pm', # finds $VERSION
    'PREREQ_PM'		=> {
			    HTML::Entities => 0,
			    HTML::Parser => 3.25,
			    Test::More => 0.32,
			   }, # e.g., Module::Name => 1.1
    ($] >= 5.005 ?    ## Add these new keywords supported since 5.005
      (ABSTRACT_FROM => 'TableParser.pm', # retrieve abstract from module
       AUTHOR     => 'djerius@cpan.org') : ()),
);
