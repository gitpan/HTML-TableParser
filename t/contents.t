use strict;
use warnings;

use Test::More tests => 113;

use IO::File;
use File::Basename;

BEGIN { use_ok( 'HTML::TableParser' ); }

our $create = 0;

our $header;
our @columns;

our @parse_data;


my $fh;

sub start
{
  @parse_data = ();
}

sub start_create
{
  @parse_data = ();
  my ( $tbl_id, $line_no, $udata ) = @_;
  $fh = IO::File->new( $udata->{data}, 'w' ) 
    or die( "unable to create $udata->{data}\n" );
}

sub end_create
{
  $fh->close;
}

sub row
{
  my ( $tbl_id, $line_no, $data, $udata ) = @_;

  my $data_s = join('\t', @$data);

  print $fh $data_s, $;
    if $create;

  push @parse_data, $data_s;
}

sub header
{
  my ( $tbl_id, $line_no, $col_names, $udata ) = @_;

  $header = $col_names;

  if ( $create )
  {
    open FILE, ">$udata->{hdr}" or die;
    print FILE "$_\n" foreach @$col_names;
    close FILE;

    @columns = @$col_names;
  }

}

our @data_t = qw( Default Chomp Trim Decode );

for my $html ( <data/*.html> )
{
  ( my $hdrfile = $html ) =~ s/.html/.hdr/;

  my %datafile;

  ( $datafile{$_} = $html ) =~ s/.html/.$_.data/
    foreach @data_t;

  my %req = ( hdr => \&header, row => \&row,
	      udata => { hdr => $hdrfile }
	      );

  my %data;
  unless( $create )
  {
    open FILE, $hdrfile or die;
    @columns = <FILE>;
    chomp(@columns);

    foreach my $type ( @data_t )
    {
      open FILE, $datafile{$type} 
	or die( "couldn't open $datafile{$type}\n");
      local $/ = $; ;
      $data{$type} = [<FILE>];
      chomp(@{$data{$type}});
    }
    $req{start} = \&start;
  }
  else
  {
    $req{start} = \&start_create;
    $req{end} = \&end_create;
  }

  foreach my $type ( @data_t )
  {
    my %attr = $type eq 'Default' ? () : ( $type => 1 );

    $req{udata}{data} = $datafile{$type};
    {
      local $req{id} = 1;
      my $p = HTML::TableParser->new( [ \%req ], \%attr );
      $p->parse_file( $html ) || die;
      ok( eq_array( $header, \@columns ), "$html id" );
      $data{$type} = [@parse_data] if $create;
      ok( eq_array( $data{$type}, \@parse_data ), "$html($type) id data" );
    }

    {
      local $req{cols} = [ $columns[0] ];
      my $p = HTML::TableParser->new( [ \%req ], \%attr );
      $p->parse_file( $html ) || die;
      ok( eq_array( $header, \@columns ), "$html cols" );
      ok( eq_array( $data{$type}, \@parse_data ), "$html($type) cols data" );
    }

    {
      my $re = $columns[-1];
      substr($re, -1, 1, '');
      local $req{colre} = [ $re ];
      my $p = HTML::TableParser->new( [ \%req ], \%attr );
      $p->parse_file( $html ) || die;
      ok( eq_array( $header, \@columns ), "$html colre" );
      ok( eq_array( $data{$type}, \@parse_data ), "$html($type) colre data" );
    }

    {
      $header = undef;
      local $req{cols} = [ "this column doesn't exist" ];
      my $p = HTML::TableParser->new( [ \%req ], \%attr );
      $p->parse_file( $html ) || die;
      ok( !defined $header, "$html($type) cols: no match" );
    }
  }

}
