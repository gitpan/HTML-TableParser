use strict;
use warnings;

use Test::More tests => 121;

use IO::File;
use File::Basename;

BEGIN { use_ok( 'HTML::TableParser' ); }

require 't/common.pl';

our $verbose = 0;
our $create = 0;

our $header;
our $columns;

our @parse_data;


my $fh;

sub start
{
  my ( $tbl_id, $line_no, $udata ) = @_;

  print STDERR "start: $tbl_id\n" if $verbose;

  die( "whoops! we're already in the middle of a table!\n" )
    if defined @parse_data;
  @parse_data = ();
}

sub start_create
{
  @parse_data = ();
  my ( $tbl_id, $line_no, $udata ) = @_;

  print STDERR "start_create: $tbl_id\n" if $verbose;


  $fh = IO::File->new( $udata->{data}, 'w' ) 
    or die( "unable to create $udata->{data}\n" );
}

sub end_create
{
  my ( $tbl_id, $line_no, $udata ) = @_;

  print STDERR "end_create: $tbl_id\n" if $verbose;

  $fh->close;
}

sub row
{
  my ( $tbl_id, $line_no, $data, $udata ) = @_;

  print STDERR "row: $tbl_id\n" if $verbose;

  my $data_s = join("\t", @$data);

  print $fh $data_s, $;
    if $create;

  push @parse_data, $data_s;
}

sub header
{
  my ( $tbl_id, $line_no, $col_names, $udata ) = @_;

  print STDERR "header: $tbl_id\n" if $verbose;

  $header = $col_names;

  if ( $create )
  {
    open FILE, ">$udata->{hdr}" or die;
    print FILE "$_\n" foreach @$col_names;
    close FILE;

    @$columns = @$col_names;
  }

}

our @data_t = qw( Default Chomp Trim Decode );

for my $html ( <data/*.html> )
{
  ( my $hdrfile = $html ) =~ s/.html/.hdr/;


  my %req = ( hdr => \&header, row => \&row,
	      udata => { hdr => $hdrfile }
	      );

  my $data;
  unless( $create )
  {
    ($columns, $data ) = read_table_data( $html, \@data_t );

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

    ( my $datafile = $html ) =~ s/.html/.$type.data/;
    $req{udata}{data} = $datafile;
    
    {
      local $req{id} = 1;
      my $p = HTML::TableParser->new( [ \%req ], \%attr );
      undef @parse_data;
      $p->parse_file( $html ) || die;
      ok( eq_array( $header, $columns ), "$html id" );
      $data->{$type} = [@parse_data] if $create;
      ok( eq_array( $data->{$type}, \@parse_data ), "$html($type) id data" );
    }

    {
      local $req{cols} = [ $columns->[0] ];
      my $p = HTML::TableParser->new( [ \%req ], \%attr );
      undef @parse_data;
      $p->parse_file( $html ) || die;
      ok( eq_array( $header, $columns ), "$html cols" );
      ok( eq_array( $data->{$type}, \@parse_data ), "$html($type) cols data" );
    }

    {
      my $re = $columns->[-1];
      substr($re, -1, 1, '');
      local $req{colre} = [ $re ];
      undef @parse_data;
      my $p = HTML::TableParser->new( [ \%req ], \%attr );
      $p->parse_file( $html ) || die;
      ok( eq_array( $header, $columns ), "$html colre" );
      ok( eq_array( $data->{$type}, \@parse_data ), "$html($type) colre data" );
    }

    {
      $header = undef;
      local $req{cols} = [ "this column doesn't exist" ];
      undef @parse_data;
      my $p = HTML::TableParser->new( [ \%req ], \%attr );
      $p->parse_file( $html ) || die;
      ok( !defined $header, "$html($type) cols: no match" );
    }
  }

}


# table2.html has an embedded table.  check that out now.
{
  my $html = 'data/table2.html';
  my $fakehtml = 'data/table2-1.html';
  my $hdrfile = 'data/table2-1.hdr';

  my %req = ( hdr => \&header, row => \&row,
	      udata => { hdr => $hdrfile }
	      );

  my $data;
  my $datafile;
  unless( $create )
  {
    ($columns, $data, $datafile ) = read_table_data( $fakehtml, \@data_t );

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

    ( my $datafile = $fakehtml ) =~ s/.html/.$type.data/;

    $req{udata}{data} = $datafile;
    
    {
      local $req{id} = 1.1;
      my $p = HTML::TableParser->new( [ \%req ], \%attr );
      undef @parse_data;
      $p->parse_file( $html ) || die;
      ok( eq_array( $header, $columns ), "$fakehtml id" );
      $data->{$type} = [@parse_data] if $create;
      ok( eq_array( $data->{$type}, \@parse_data ), "$fakehtml($type) id data" );
    }
  }
}
