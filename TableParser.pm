package HTML::TableParser::Table;
use 5.006;
use strict;
use warnings;

use HTML::Entities;

# This class is used to keep track of information related to a table and
# to create the information passed back to the user callbacks.  It is
# in charge of marshalling the massaged header and row data to the user
# callbacks.

# An instance is created when the controlling TableParser class finds
# a <table> tag.  The object is given an id based upon which table it
# is to work on.  Its methods are invoked from the TableParser
# callbacks when they run across an appropriate tag (tr, th, td).  The
# object is destroyed when the matching </table> tag is found.

# Since tables may be nested, multiple TableParser::Table objects may
# exist simultaneously.  TableParser uses two pieces of information
# held by this class -- ids and process.  The first is an array
# of table ids, one element per level of table nesting.  The second is
# a flag indicating whether this table is being processed (i.e. it
# matches a requested table) or being ignored.  Since TableParser
# uses the ids information from an existing table to initialize a new
# table, it first creates an empty sentinel (place holder) table
# (by calling the TableParser::Table constructor with no arguments)

# The class handles missing </tr>, </td>, and </th> tags.  As such
# (especially when handling multi-row headers) user callbacks may
# be slightly delayed (and data cached).  It also handles rows
# with overlapping columns

sub new
{
  my $this = shift;

  my $class = ref($this) || $this;

  my $self = {
	      data 	=> [[]],	# row data (for overlapping rows)
	      row	=> undef,	# row info
	      col	=> undef,	# column info
	      hdr	=> undef,	# accumulated header info
	      hdr_row 	=> 0,		# index of header row
	      hdr_line	=> undef,	# line in file of header row
	      in_hdr	=> 0,		# are we in a header row?
	      prev_hdr	=> 0,		# was the previous row a header row?
	      line	=> undef,	# line in file of current row
	      start_line => undef,	# line in file of table start
	      req	=> undef,	# the matching table request
	     };

  bless $self, $class;

  my ( $parser, $ids, $reqs, $line ) = @_;

  $self->{parser} = $parser;
  $self->{start_line} = $line;

  # if called with no args, create an empty, placeholder object
  unless ( defined $ids )
  {
    $self->{ids} = [ 0 ];
    $self->{process} = 0;
  }

  else
  {
    $ids->[-1]++;
    $self->{ids} = [ @$ids, 0 ];
    $self->{id} =  join( '.', grep { $_ != 0 } @{$ids} );

    $self->{reqs} = $reqs;

    # are we interested in this table?
    $self->match_id();

    # inform user of table start.  note that if we're looking for
    # for column name matches, we don't want to do the callback;
    # in that case $self->{req} isn't set and callback() won't
    # actually make the call.
    $self->callback( 'start', $self->{start_line} ) if $self->{process};
  }

  $self;
}

# see if the table id matches a requested table.  sets $self->{process}
# and $self->{req}.
sub match_id
{
  my $self = shift;

  $self->{process} = 0;

  for my $req ( @{$self->{reqs}} )
  {

    # if the id's match, or the request id is DEFAULT, we match
    if ( defined $req->{id} && 
	( $self->{id} eq $req->{id} || $req->{id} eq 'DEFAULT' ) )
    {
      next if $self->{req}{match} && ! $self->{req}{MultiMatch};
      $self->match_req( $req );
      last;
    }

    # we're going for a header match; must delay decision until
    # header is read in.  must set {process} so we don't turn
    # off parser, but don't set {req}, so don't try doing callbacks.
    elsif (
	   ( exists $req->{cols_hash} && keys %{$req->{cols_hash}} ) ||
	   ( exists $req->{colre}       && @{$req->{colre}}  )
	  )
    {
      next if $self->{req}{match} && ! $self->{req}{MultiMatch};
      $self->{process} = 1;
      $self->{req} = undef;
      last;
    }
  }
}

# we've pulled in a header; does it match against one of the requests?
sub match_hdr
{
  my ( $self, @cols ) = @_;

  for my $req ( @{$self->{reqs}} )
  {

    # need to do this so fix_texts picks up the request's data edit
    # attributes
    $self->{req} = $req;

    my @fix_cols = @cols;
    $self->fix_texts(\@fix_cols);

    $self->{req} = undef;

    foreach  (@fix_cols )
    {
      if ( exists $req->{cols_hash}{$_} )
      {
	$self->match_req( $req );
	return 1;
      }

      foreach my $re ( @{$req->{colre}} )
      {
	if ( /$re/ )
	{
	  $self->match_req( $req );
	  return 1;
	}
      }
    }
  }

  0;
}

sub match_req
{
  my ( $self, $req ) = @_;

  if ( $req->{class} )
  {
#    no strict 'refs';
    my $new = $req->{new};
    $self->{obj} = $req->{class}->$new( $req->{id}, $req->{udata} );
  }
  elsif ( $req->{obj} )
  {
    $self->{obj} = $req->{obj};
  }

  $self->{process} = 1;
  $self->{req} = $req;
  $self->{req}{match}++;
}


# generic call back interface.  handle method calls as well as
# subroutine calls.
sub callback
{
  my $self = shift;
  my $method = shift;

  return unless 
    defined $self->{req} && exists $self->{req}->{$method};

  my $req = $self->{req};
  my $call = $req->{$method};

  if ( 'CODE' eq ref $call )
  {
    $call->( $self->{id}, @_, $req->{udata} );
  }
  else
  {
    $self->{obj}->$call( $self->{id}, @_, $req->{udata} );
  }
}


# handle <th>
sub start_header
{
  my $self = shift;
  my ( undef, $line ) = @_;

  $self->{in_hdr}++;
  $self->{prev_hdr}++;
  $self->{hdr_line} = $line;
  $self->start_column( @_ );
}


# handle </th>
sub end_header
{
  my $self = shift;
  $self->end_column();
}

# handle <td>
sub start_column
{
  my $self = shift;
  my ( $attr, $line ) =  @_;

  # end last column if not explicitly ended. perform check here
  # to avoid extra method call
  $self->end_column() if defined $self->{col};

  # we really shouldn't be here if a row hasn't been started
  unless ( defined $self->{row} )
  {
    $self->callback( 'warn', "<td> or <th> without <tr> at line $line\n" );
    $self->start_row( {}, $line );
  }

  $self->{col} = { attr =>  { %$attr}  };
  $self->{col}{attr}{colspan} ||= 1;
  $self->{col}{attr}{rowspan} ||= 1;
}

# handle </td>
sub end_column
{
  my $self = shift;

  return unless defined $self->{col};

  $self->{col}{text} = defined $self->{text} ? $self->{text} : '' ;

  push @{$self->{row}}, $self->{col};

  $self->{col} = undef;
  $self->{text} = undef;
}

sub start_row
{
  my $self = shift;
  my ( $attr, $line ) = @_;

  # end last row if not explicitly ended
  $self->end_row();

  $self->{row} = [];
  $self->{line} = $line;
}


sub end_row
{
  my $self = shift;

  return unless defined $self->{row};

  # perhaps an unfinished row. first finish column
  $self->end_column();

  # if we're in a header, deal with overlapping cells differently
  # then if we're in the data section
  if ( $self->{in_hdr} )
  {

    my $cn = 0;
    my $rn = 0;
    foreach my $col ( @{$self->{row}} )
    {
      # do this just in case there are newlines and we're concatenating
      # column names later on.  causes strange problems.  besides,
      # column names should be regular
      $col->{text} =~ s/^\s+//;
      $col->{text} =~ s/\s+$//;

      # need to find the first undefined column
      $cn++ while defined $self->{hdr}[$cn][$self->{hdr_row}];

      # note that header is stored as one array per column, not row!
      for ( my $cnn = 0 ; $cnn < $col->{attr}{colspan} ; $cnn++, $cn++ )
      {
	$self->{hdr}[$cn] ||= [];
	$self->{hdr}[$cn][$self->{hdr_row}] = $col->{text};
	
	# put empty placeholders in the rest of the rows
	for ( my $rnn = 1 ; $rnn < $col->{attr}{rowspan} ; $rnn++ )
	{
	  $self->{hdr}[$cn][$rnn + $self->{hdr_row}] = '';
	}
      }
    }

    $self->{hdr_row}++;
  }
  else
  {
    my $cn = 0;
    my $rn = 0;
    foreach my $col ( @{$self->{row}} )
    {
      # need to find the first undefined column
      $cn++ while defined $self->{data}[0][$cn];

      for ( my $cnn = 0 ; $cnn < $col->{attr}{colspan} ; $cnn++, $cn++ )
      {
	for ( my $rnn = 0 ; $rnn < $col->{attr}{rowspan} ; $rnn++ )
	{
	  $self->{data}[$rnn] ||= [];
	  $self->{data}[$rnn][$cn] = $col->{text};
	}
      }
    }
  }

  # if we're one row past the header, we're done with the header
  $self->finish_header()
    if ! $self->{in_hdr} && $self->{prev_hdr};

  # output the data if we're not in a header
  $self->callback( 'row', $self->{line}, 
		   $self->fix_texts( shift @{$self->{data}} ) )
      unless $self->{in_hdr};

  $self->{in_hdr} = 0;
  $self->{row} = undef;
}

# collect the possible multiple header rows into one array and
# send it off
sub finish_header
{
  my $self = shift;

  return unless $self->{hdr};

  my @header = map { join( ' ', grep { defined $_ && $_ ne '' } @{$_}) }
                        @{ $self->{hdr} };

  # if we're trying to match header columns, check that here.
  if ( defined $self->{req} )
  {
    fix_texts( \@header );
    $self->callback( 'hdr',  $self->{hdr_line}, \@header );
  }

  else
  {
    if ( $self->match_hdr( @header ) )
    {
      # haven't done this callback yet...
      $self->callback( 'start', $self->{start_line} );

      fix_texts( \@header );
      $self->callback( 'hdr',  $self->{hdr_line}, \@header );
    }

    # no match.  reach up to the controlling parser and turn off
    # processing of this table. this is kind of kludgy!
    else
    {
      $self->{parser}->process(0);
    }
  }


  $self->{hdr} = undef;
  $self->{prev_hdr} = undef;
  $self->{hdr_row} = 0;
}

DESTROY
{
  my $self = shift;

  # if we're actually parsing this table, do something.
  if ( $self->{process} )
  {
    # just in case
    $self->end_row();

    # just in case there's no table body
    $self->finish_header();

    $self->callback( 'end', $self->{line} );
  }
}

sub fix_texts
{
  my ( $self, $texts  ) = @_;

  for ( @$texts )
  {
    chomp $_ 
      if $self->{req}{Chomp};

    if ( $self->{req}{Trim} )
    {
      s/^\s+//;
      s/\s+$//;
    }

    decode_entities( $_ )
      if $self->{req}{Decode};
  }

  $texts;
}

sub text
{
  my $self = shift;

  $self->{text} = shift;
}

sub id  { $_[0]->{id} }
sub ids { $_[0]->{ids} }
sub process { $_[0]->{process} }

package HTML::TableParser;

use 5.006;
use strict;
use warnings;

use Carp;
use HTML::Parser;

our @ISA = qw(HTML::Parser);

our $VERSION = '0.1';

# Preloaded methods go here.

our %Attr =  ( Trim => 0,
	       Decode => 1,
	       Chomp => 0,
	       MultiMatch => 0,
	     );
our @Attr = keys %Attr;

sub new
{
  my $class = shift;

  my $reqs = shift;

  my $self = $class->SUPER::new
               (
		api_version => 3,
		unbroken_text => 1,
		start_h  => [ 'start', 'self, tagname, attr, line' ],
		end_h    => [ 'end',   'self, tagname, attr, line' ],
	       );

  croak( "must specify a table request" )
    unless  defined $reqs and 'ARRAY' eq ref $reqs;

  my $attr = shift || {};

  my @notvalid = grep { ! exists $Attr{$_} } keys %$attr;
  croak ("Invalid attribute(s): '", join(" ,'", @notvalid ), "'\n" )
    if @notvalid;

  my %attr = ( %Attr, %$attr );

  $self->{reqs} = tidy_reqs( $reqs, \%attr );

  $self->{Tables} = [ HTML::TableParser::Table->new() ];

  # by default we're not processing anything
  $self->process(0);

  $self;
}


our @ReqAttr = ( qw( cols colre id class obj start end hdr row warn udata ),
		 keys %Attr );
our %ReqAttr = map { $_ => 1 } @ReqAttr;

sub tidy_reqs
{
  my ( $reqs, $attr ) = @_;

  my @reqs;

  my $nreq = 0;
  for my $req ( @$reqs )
  {
    my %req;

    $nreq++;

    my @notvalid = grep { ! exists $ReqAttr{$_} } keys %$req;
    croak ("table request $nreq: invalid attribute(s): '",
	   join(" ,'", @notvalid ), "'\n" )
      if @notvalid;

    my $id = 0;

    if ( defined $req->{cols} )
    {
      my $cols;

      if ( 'ARRAY' eq ref $req->{cols} )
      {
	$cols = $req->{cols};
      }
      elsif ( ! ref $req->{cols} )
      {
	$cols = [ $req->{cols} ];
      }
      else
      {
	croak( "table request $nreq: cols must be a scalar or arrayref\n" );
      }

      $req{cols_hash} = { map { $_ => 1 } @{$cols} };
      $id++ if @{$cols};
    }
    else
    {
      $req{cols_hash} = {};
    }

    unless ( defined $req->{colre} )
    {
      $req{colre} = [];
    }
    else
    {

      if ( 'ARRAY' eq ref $req->{colre} )
      {
	$req{colre} = $req->{colre};
      }
      elsif ( ! ref $req->{colre} )
      {
	$req{colre} = [ $req->{colre} ];
      }
      else
      {
	croak( "table request $nreq: colre must be a scalar or arrayref\n" );
      }

      $id++ if @{$req{colre}};
    }

    if ( exists $req->{id} )
    {
      $id++;
      $req{id} = $req->{id};
    }

    croak( "table request $nreq: must specify at least one id method" )
      unless $id;

    $req{obj} = $req->{obj}
      if exists $req->{obj};

    $req{class} = $req->{class}
      if exists $req->{class};

    for my $method ( qw( start end hdr row warn new ) )
    {
      if ( exists $req->{$method} && 'CODE' eq ref $req->{$method} )
      {
	$req{$method} = $req->{$method};
      }

      elsif ( exists $req{obj} || exists $req{class})
      {
	my $thing = exists $req{obj} ? $req{obj} : $req{class};

	if ( exists $req->{$method} )
	{
	  croak( "table request $nreq: can't have object & non-scalar $method" )
	    if ref $req->{$method};

	  my $call = $req->{$method};

	  croak( "table request $nreq: class doesn't have method $call" )
	    if ( exists $req->{obj} && ! $req->{obj}->can( $call ) )
	      || !UNIVERSAL::can( $thing, $call );
	}
	else
	{
	  $req{$method} = $method
	    if UNIVERSAL::can( $thing, $method );
	}
      }
      elsif( exists $req->{$method} )
      {
	croak( "invalid callback for $method" );
      }
    }

    # last minute cleanups for things that don't fit in the above loop
    croak( "must specify valid constructor for class $req->{class}\n" )
      if exists $req{class} && ! exists $req{new};


    $req{udata} = undef;
    $req{udata} = exists $req->{udata} ? $req->{udata} : undef;

    $req{match} = 0;

    @req{@Attr} = @Attr{@Attr};

    $req{$_} = $attr->{$_}
      foreach grep { defined $attr->{$_} } @Attr;

    $req{$_} = $req->{$_}
      foreach grep { defined $req->{$_} } @Attr;

    push @reqs, \%req;
  }

  \@reqs;
}


sub process
{
  my ($self, $state) = @_;

  my $ostate = $self->{process} || 0;

  if ( $state )
  {
    $self->report_tags( qw( table th td tr ) );
    $self->handler( 'text'   => 'text',  'self, text, line' );
  }

  else
  {
    $self->report_tags( qw( table  ) );
    $self->handler( 'text' => '' );
  }

  $self->{process} = $state;
  $ostate;
}


our %trans = ( tr => 'row',
	       th => 'header',
	       td => 'column' );

sub start
{
  my $self = shift;
  my $tagname = shift;

  if ( 'table' eq $tagname )
  {
    $self->start_table( @_ );
  }

  else
  {
    my $method = 'start_' . $trans{$tagname};

    $self->{Tables}[-1]->$method(@_);
  }
}


sub end
{
  my $self = shift;
  my $tagname = shift;

  if ( 'table' eq $tagname )
  {
    $self->end_table(  @_ );
  }

  else
  {
    my $method = 'end_' . $trans{$tagname};

    $self->{Tables}[-1]->$method(@_);
  }
}


sub start_table
{
  my ( $self, $attr, $line ) = @_;

  my $otbl = $self->{Tables}[-1];

  my $tbl = HTML::TableParser::Table->new( $self, 
					   $self->{Tables}[-1]->ids,
					   $self->{reqs}, $line );

  $self->process( $tbl->process );

  push @{$self->{Tables}}, $tbl;
}


sub end_table
{
  my ( $self, $attr, $line ) = @_;

  my $tbl = pop @{$self->{Tables}};
  undef $tbl;

  $self->process( $self->{Tables}[-1]->process );
}


sub text
{
  my ( $self, $text, $line ) = @_;

  $self->{Tables}[-1]->text( $text );
}




1;
__END__

=pod


=head1 NAME

HTML::TableParser - Extract data from an HTML table

=head1 SYNOPSIS

  use HTML::TableParser;
  $p = HTML::TableParser->new( \@reqs, \%attr );
  $p->parse_file( 'foo.html' );

  @reqs = (
	   {
	    id => 1,                      # table id
	    cols => [ 'Object Type' ],  # column name exact match
	    colre => [ qr/object/ ],      # column name RE match
	    obj => $obj,                  # method callbacks
	   },
	   {
	    id => 1.1,                    # id for embedded table
	    hdr => \&header,              # function callback
	    row => \&row,                 # function callback
	    start => \&start,             # function callback
	    end => \&end,                 # function callback
	    udata => { Snack => 'Food' }, # arbitrary user data
	   }

	  );

  # create parser object
  $p = HTML::TableParser->new( \@reqs, 
		   { Decode => 1, Trim => 1, Chomp => 1 } );
  $p->parse_file( 'foo.html' );


  # function callbacks
  sub start {
    my ( $id, $line, $udata ) = @_;
    #...
  }

  sub end {
    my ( $id, $line, $udata ) = @_;
    #...
  }

  sub header {
    my ( $id, $line, $cols, $udata ) = @_;
    #...
  }

  sub row  {
    my ( $id, $line, $cols, $udata ) = @_;
    #...
  }

=head1 DESCRIPTION

B<HTML::TableParser> uses B<HTML::Parser> to extract
data from an HTML table.  The data is returned via a series of user
defined callback functions or methods.  Specific tables may be
selected either by a unique table id or by matching against the column
names.  Multiple tables may be parsed simultaneously in the document.

=head2 Table Selection

There are several ways to indicate which tables in the HTML document
you want to extract data from:

=over 8

=item id

Each table is given a unique id relative to its parent based upon its
order and nesting. The first top level table has id C<1>, the second
C<2>, etc.  The first table nested in table C<1> has id C<1.1>, the
second C<1.2>, etc.  The first table nested in table C<1.1> has id
C<1.1.1>, etc.

=item column name exact match

exact matches against one or more column names

=item column name RE match

matches column names against one or more regular expressions.

=back

=head2 Data Extraction

As the parser traverses the table, it will pass data to user provided
callback functions or methods after it has digested particular
structures in the table.  All functions are passed the table id (as
described above), the line number in the HTML source where the table
was found, and a reference to any table specific user provided data.

=over 8

=item Table Start

The B<start> callback is invoked when a matched table has been found.

=item Table End

The B<end> callback is invoked after a matched table has been parsed.

=item Header

The B<hdr> callback is invoked after the table header has been read in.
Some tables do not use the B<E<lt>thE<gt>> tag to indicate a header, so this
function may not be called.  It is passed the column names.

=item Row

The B<row> callback is invoked after a row in the table has been read.
It is passed the column data.

=item Warn

The B<warn> callback is invoked when a non-fatal error occurs during
parsing.  Fatal errors croak.

=item New

This is the class method to call to create a new object when
B<HTML::TableParser> is supposed to create new objects upon table
start.

=back

Callbacks may be functions or methods or a mixture of both.
In the latter case, an object must be passed to the constructor.

=head2 Callback API

The callbacks are invoked as follows:

  start( $tbl_id, $line_no, $udata );

  end( $tbl_id, $line_no, $udata );

  hdr( $tbl_id, $line_no, \@col_names, $udata );

  row( $tbl_id, $line_no, \@data, $udata );

  warn( $tbl_id, $message, $udata );

  new( $tbl_id, $udata );

=head2 Data Cleanup

There are several cleanup operations that may be performed:

=over 8

=item Chomp

B<chomp()> the data

=item Decode

Run the data through B<HTML::Entities::decode>.

=item Trim

remove leading and trailing white space.

=back

=head2 Data Organization

Column names are derived from cells delimited by the B<E<lt>thE<gt>> and
B<E<lt>/thE<gt>> tags. Some tables have header cells which span one or
more columns or rows to make things look nice.  B<HTML::TableParser>
determines the actual number of columns used and provides column
names for each column, repeating names for spanned columns and
concatenating spanned rows and columns.  For example,  if the
table header looks like this:

 +----+--------+----------+-------------+-------------------+
 |    |        | Eq J2000 |             | Velocity/Redshift |
 | No | Object |----------| Object Type |-------------------|
 |    |        | RA | Dec |             | km/s |  z  | Qual |
 +----+--------+----------+-------------+-------------------+

The columns will be:

  No
  Object
  Eq J2000 RA
  Eq J2000 Dec
  Object Type
  Velocity/Redshift km/s
  Velocity/Redshift z
  Velocity/Redshift Qual

Row data are derived from cells delimited by the B<E<lt>tdE<gt>> and 
B<E<lt>/tdE<gt>> tags.  Cells which span more than one column or row are
handled correctly, i.e. the values are duplicated in the appropriate
places.

=head1 METHODS

=over 8

=item new

   $p = HTML::TableParser->new( \@reqs, \%attr );

This is the class constructor.  It is passed a list of table requests
as well as attributes which specify defaults for common operations.

B<Table Requests>

A table request is a hash whose elements select the
identification method, the callbacks, and any table-specific data
cleanup.

Elements used to identify the table are

=over 8

=item id

a scalar containing the table id to match.  If it is the string
'DEFAULT' it will match for every table.

=item cols

an arrayref containing the column names to match, or a scalar
containing a single column name

=item colre

an arrayref containing the regular expressions to match, or a scalar
containing a single reqular expression

=back

More than one of these may be used for a single table request, and
a request may match more than one table.  By default a request is
used only once; set the C<MultiMatch> attribute to enable multiple
matches per request.

Callback functions are specified with the callback attributes
C<start>, C<end>, C<hdr>, C<row>, and C<warn>.  They should be set to
code references, i.e.

  %table_req = ( ..., start => \&start_func, end => \&end_func )

To use methods, specify the object with the C<obj> key, and
the method names via the callback attributes, which should be set
to strings.  If you don't specify method names they will default to (you
guessed it) C<start>, C<end>, C<hdr>, C<row>, and C<warn>.

  $obj = SomeClass->new();
  # ...
  %table_req_1 = ( ..., obj => $obj );
  %table_req_2 = ( ..., obj => $obj, start => 'start',
                             end => 'end' );

You can also have B<HTML::TableParser> create a new object for you
for each table by specifying the C<class> attribute.  By default
the constructor is assumed to be the class B<new()> method; if not,
specify it using the C<new> attribute:

  use MyClass;
  %table_req = ( ..., class => 'MyClass', new => 'mynew' );

To use a function instead of a method for a particular callback,
set the callback attribute to a code reference:

  %table_req = ( ..., obj => $obj, end => \&end_func );

You don't have to provide all the callbacks.  You should not use both
C<obj> and C<class> in the same table request.

You can specify arbitrary data to be passed to the callback functions
via the C<udata> attribute:

  %table_req = ( ..., udata => \%hash_of_my_special_stuff )

Data cleanup operations may be specified uniquely for each table. The
available keys are C<Chomp>, C<Decode>, C<Trim>.

B<Attributes>

The C<%attr> hash provides default values for some of the table
request attributes, namely the data cleanup operations ( C<Chomp>,
C<Decode>, C<Trim> ), and the multi match attribute C<MultiMatch>,
i.e.,

  $p = HTML::TableParser->new( \@reqs, { Chomp => 1 } );

will set B<Chomp> on for all of the table requests, unless overriden
by them.

B<Decode> defaults to on; all of the others default to off.


=item parse_file

This is the same function as in B<HTML::Parser>.

=item parse

This is the same function as in B<HTML::Parser>.

=back


=head1 AUTHOR

Diab Jerius (djerius@cfa.harvard.edu)

=head1 SEE ALSO

L<HTML::Parser>, L<HTML::TableExtract>.

=cut
