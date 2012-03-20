#!/usr/bin/perl

use strict;

use IPC::Open2;
use IO::Handle;

use Irssi;
use Irssi::Irc;
use Irssi::TextUI;

use vars qw($VERSION %IRSSI);

$VERSION = '0.3.1';
%IRSSI = (
    authors     => 'Nathan Coad',
    contact     => 'encode1@gmail.com',
    name        => 'aspeller',
    description     => 'Checks your typing for spelling mistakes',
    license     => 'GPLv2',
    url         => 'http://www.nathancoad.net',
    changed     => '19/02/05',
);

###############################################################################
#INSTRUCTIONS FOR USE
#
#First of all, define the location to aspell either using the global variable
#	$program below, or type "/set aspell_path <path_to_aspell>" in Irssi.
#	
#To disable this script, type "/set aspell_enable OFF" in Irssi.
#
#To send a command to aspell, use the "/aspell" command.  This will pass 
#	the first argument (delimted by whitespace) to aspell.  A list of useful 
#	commands is as follows:
#		*word  	Add a word to the personal dictionary
#		&word 	Insert the all-lowercase version of the word in the personal 
#				dictionary
#		@word 	Accept the word, but leave it out of the dictionary
#		# 	Save the current personal dictionary
#
#To add a word to aspell's personal dictionary, you can also use the
#	"/aspell_add <word_to_insert>" command.
#
#To get aspell to make some suggestions about a mispelled word, use the
#	"/suggest" command.  The format of this command is
#	
#			/suggest <word_spelt_incorrectly>
#		
#To unload this script, you MUST execute the "/aspell_shutdown" command in
#	Irssi.  This will remove the pipe used to communicate with aspell. 
#	Failure to unload the script using this command will result in zombie 
#	processes.
#
###############################################################################
#
#TODO:
#investigate the possibility of getting aspell to ignore nicks
#my $length = Irssi::parse_special ( "\$[-!0]\@L" );
#
#CHANGELOG:
#0.2.0
#	- Changed things totally.  Now uses a statusbar to highlight spelling errors
#0.2.2
#	- Fixed parsing for * at beginning of input string
#0.2.3
#	- Fixed some escaping problems with input and output text
#	- Currently leading special characters and the presence of $ and % screw thigns up
#0.2.4
#	- Special leading characters should be fixed
#0.2.5
#	- Improved removal of special leading characters
#0.3.0
#	- Altered the statusbar so its prefixed with the channel name rather than Aspell
#	- Added a hack so the statusbar displays the end of the text if you've typed more than the screen's width
#0.3.1
#	- Fixed a flaw that made the statusbar redraw constantly

#define global vars
#see http://irssi.org/?page=docs&doc=formats for a list of possible colours
my $aspell_colour = '%G';
my $hilighted_colour = '%r%8';

#This is set to 15 by default in irssi.  If this number doesnt seem right, have a look in ~/.irssi/config at this line:
#    prompt = "{prompt $[.15]itemname}";
#The number in that line should be the number that $max_window_name_length is set to
my $max_window_name_length = 15;

my $program = "/usr/bin/aspell";
my $program_args = " -a -p .aspeller --sug-mode=ultra --lang=en_GB";


#explanation of args:
#	-a puts aspell in pipe mode.  THIS IS ESSENTIAL
#	-p .aspeller sets aspell to use the personal dictionary contained in .aspeller.
#		feel free to change the name of this dictionary.
#	--sug-mode=ultra tells aspell not to bother looking too hard for the correct word,
#		since we currently ignore this anyway.  Suggestion mode can be one of:
#		ultra, fast, normal, bad-spellers
my $debug = 0;

##YOU SHOULDN'T NEED TO MODIFY ANYTHING BEYOND THIS POINT
use vars qw($global_text $last_text $text_length $server $window $speed $g_window_name $enabled $g_offset @results);
use vars qw($read $write $pid);

sub init()
{
	#organise program path
	if ($program ne Irssi::settings_get_str('aspell_path')) {
		if ($debug) {print "Aspell location redefined in user settings";}
		$program = Irssi::settings_get_str('aspell_path');
	}
	
	#append flags to aspell
	$program .= $program_args;
	if ($debug) {print "Aspell is now " . $program;}

	#open aspell for reading and writing
	#and check to make sure it didnt fail
	eval {
		$pid = open2(*READ, *WRITE, $program);
	};
	if ($@) { 
		if ($@ =~ /^open2/) {
			warn "open2 failed: $!\n$@\n";
			return;
		}
		die;            # reraise unforeseen exception
	}
	
	#setup global variables holding the filehandles
	$read = *READ;
	$write = *WRITE;
	if ($debug) {print "The pid of aspell is " . $pid;}
	
	#get the identifier line that aspell always outputs	
	my $idn_line = <READ>;
	chomp $idn_line;
	print $idn_line;

	#put aspell in terse mode
	print WRITE "!\n";

	#get user-defined settings
	$speed = Irssi::settings_get_int('aspell_speed');
	$enabled = Irssi::settings_get_bool('aspell_enable');

	print "Aspeller version $VERSION loaded. Please type /aspell_shutdown to close this script";

	#get the current window name into the global variable
	get_window_name();
}

sub main()
{
	#the program's main loop, triggered every timeout
	#if the script is disabled, return now
	if (! $enabled) {return;}
	
	#get the current line of input text
	my $text = Irssi::parse_special('$L');
	$text_length = length($text);
	
	#if it hasnt changed, redraw the status bar and return
	if ($text eq $last_text) { return; }
	
	if ($debug) {print "Last text (".$last_text."), curr text (".$text;}

	$g_offset = 0;

	if (check_command($text)) {
		#dont do anything
	} else {
		#process the input text for spelling mistakes
		aspell_interface(escape_ize($text));
		$global_text = (colourize(escape_ize($text)));
		
		#$global_text = (colourize($text));
	}
	
	#do some final stuff
	#a nasty little hack, especially if we end up cutting in the middle of a colour code
	my $width = Irssi::active_win()->{width} - length($g_window_name) -1;
	if ($text_length > $width) {
		$global_text = substr($global_text,- $width,$width);
	} else {
		$global_text = $global_text;
	}
	
	#display the info on the statusbar
	Irssi::statusbar_items_redraw('aspeller');
	$last_text = $text;
}

sub check_command()
#checks if the input text is a command
#returns 1 if it is
{
	my $text = @_[0];
	if ($text =~ /^\// && $text !~ /^\/me/) {
		return 1;
	}
	return 0;
}

sub aspell_interface($)
#handles interactions with aspell
{
	my ($text) = @_;
	#my ($line, $pid);
	*READ = $read;
	*WRITE = $write;
	#reset the global array
	@results = ();

	#format the text ready for output to aspell
	$text = prepare_text($text);

	#check to make sure we should output to aspell
	if ($text eq "" || $text eq "\n") {
		if ($debug) {print "no text to input";}
		return;
	} elsif ($debug) {
		print $text;
	}

	#send aspell the line of text
	print WRITE $text;

	#check for output until a newline
	while(<READ>) {
		if ($_ eq "\n") {
			last;
		}
		@results[$#results + 1] = $_;
	}	
}

sub prepare_text($)
#prepares the text for sending to aspell
#adds a trailing newline
#and removes the following characters from the beginning: *, &, @, +, -, ~, #, !, %, and ^
{
	my $text = @_[0];
	my $char;
	my $found = 1;
	my @bad_chars = ('\*','&', '@', '\+', '-', '~', '#', '!', '%', '\^','\$\$');
	if ($text !~ /\n$/) {
		$text .= "\n";
	}
	while ($found == 1) {
		$found = 0;
		foreach $char (@bad_chars) {
			#print "Searching for (".$char.")";
			#$g_offset = 0;
			while ($text =~ s/^$char//) { 
				$g_offset++;
				$found = 1;
				#if ($g_offset < -20) {last;}
			}
#		print "Offset reached " . $g_offset;
		}
	}
	#sleep 2;
	if ($debug) {print "Filtered text (".$text.")";}
	return $text;
}

sub colourize()
#handle the colouration of the output text
#returns the coloured text
{
	my ($text) = @_;
	my $correction_counter = $g_offset;
	if ($#results == -1) {
		#then there were no typos
		#so just return the original text
		return $text;
	} else {
		my $text2 = $text;
		foreach (@results) {
			if ($debug) {print "Result: ".$_;}
			$text2 = colourize_text($text2,$_,$correction_counter);
			#add one for every character inserted to highlight the misspelt word
			$correction_counter += 6;
			#print "Correction counter is " . $correction_counter;
		}
		return $text2;
		#alert($window, $text2);
		#Irssi::signal_continue($text, $server, $window);
	}
}

sub get_window_name()
#returns the window name (up to $max_window_name_length characters long)
{
	my $awin = Irssi::active_win();
	my $name;
	
	#get the name
	$_ = $awin->{active}->{type};
	if ($_ eq 'CHANNEL' || $_ eq 'QUERY') {
		$name = $awin->{active}->{visible_name};
	} else {
		$name = $awin->{name};
	}

	#if the string is too long, get rid of some
	if (length($name) > $max_window_name_length) {
		$name = substr($name,0,$max_window_name_length);
	}

	#return the name, complete with the brackets
	$g_window_name =  '[' . $name . ']';
}

sub colourize_text($$$)
#take the aspell output, and colour the misspelt word
{
	#setup variables
	my ($text, $result,$offset) = @_;
	my @output;
	my ($in_counter,$out_counter,$start,$length,$i);
	$i = 0;
	#split input text into an array
	my @input = split(//,$text);

	#get the place of the mispelt word
	if ($result =~ /\&\s([^\s]+)\s\d+\s(\d+)/) {
		#get the values ready
		$length = $1;
		$start = $2;
	} elsif ($result =~ /\#\s([^\s]+)\s(\d+)/) {
		#get the values ready
		$length = $1;
		$start = $2;
	}

	if ($debug) {
		print "Start plus offset is " . ($start + $offset);
		print $length;
		print $start;
	}
	
	#copy input before the spelling mistake
	for($i; $i < ($start + $offset); $i++) {
		$output[$out_counter++] = $input[$in_counter++];
	}

	if ($debug) {print "Before the spelling mistake " . join("",@output);}
	
	$output[$out_counter++] = $hilighted_colour;
	$length = length($length) + $out_counter - 1;
	
	#copy word
	for($i; $i < ($length); $i++) {
		$output[$out_counter++] = $input[$in_counter++];
	}

	if ($debug) {print "After the word " . join("",@output);}

	$output[$out_counter++] = '%n';

	#copy rest of $text
	for($i; $i <= ($#input); $i++) {
		$output[$out_counter++] = $input[$in_counter++];
	}

	if ($debug) {
		$_ = join("",@output);
		print "Final colouring: (".$_;
	}
	
	return join("",@output);
}

sub escape_ize()
#escape $ and %, since they're used for special things by irssi
{
	my $text = @_[0];
	
	#have to do something here
	#but i cant figure out an easy way of doing it
	
	#while ($text =~ s/\$/\$\$/) {$g_offset--;}
	#while ($text =~ s/\%/\%\%/) {$g_offset--;}
	$text =~ s/\$/\$\$/g;
	$text =~ s/\%/\%\%/g;

	return $text;
}

sub alert($$)
#sends the user a prompt in the channel window
{
	my ($window, $msg) = @_;
	
	if ($window eq 0) {
		#handle the situation that $window isnt defined
		print $msg;
	} else {
		$window->print($msg);
	}
}

###############################################################################
##User Commands

sub set()
{
	#send some command directly to aspell
    my ($arguments, $server, $witem) = @_;
	my $output;
	*WRITE = $write;
	*READ = $read;
	my @arg_list = split(/\s+/, $arguments);

	print WRITE $arg_list[0] . "\n";
	$output = "Sent '" . $arg_list[0] . "' to aspell.";
	#my $response = <READ>;
	#print $response;
	alert($witem, $output);	
}

sub add()
{
	#use the aspell command & to add the word to aspell's personal dictionary
    my ($arguments, $server, $witem) = @_;
	my @arg_list = split(/\s+/, $arguments);
	my ($input, $output);
	*WRITE = $write;
	*READ = $read;
	$input = prepare_text($arg_list[0]);
	print WRITE '&' . $input;
	print WRITE "#\n";
	chomp $input;
	$output = "Added '" . $input . "' to personal aspell dictionary.";
	alert($witem, $output);	
}

sub suggest()
{
	#setup variables
	my ($arguments, $server, $witem) = @_;
	my ($output, $response, @temp);
	*WRITE = $write;
	*READ = $read;
	#split the arguments variable into an array of arguments
	my @arg_list = split(/\s+/, $arguments);

	#THIS IS EVIL. do not uncomment this
	#aspell 0.50.3 does not respond to this command
	#so irssi will be blocked waiting for a response
	#	
	#if ($debug) {
	#	print WRITE '$$cr sug-mode' . "\n";
	#	$response = <READ>;
	#	print $response;	
	#}

	#set aspell to normal mode to get spelling suggestions
	#this can be changed to ultra, fast, normal or bad-spellers
	print WRITE '$$cs sug-mode, normal' . "\n";

	#check the first word
	aspell_interface($arg_list[0]);

	#reset aspell's sugestion mode
	print WRITE '$$cs sug-mode, ultra' . "\n";

	#if the word is spelt correctly, there will be no results
	if ($#results == -1) {
		#$output = "The spelling of " . $hilighted_colour . $arg_list[0] . " is correct";
		$output = "The spelling of " . $aspell_colour . $arg_list[0] . "%n is correct";
		alert($witem, $output);
		return;
	}
	
	#otherwise, format the results and output them
	@temp = split(/:/, $results[0]);
	chomp $temp[1];
	$output = $aspell_colour . "Suggestions for '" . $arg_list[0] . "': " . $temp[1];
	alert($witem, $output);
}

sub shutdown()
{
	my ($arguments, $server, $witem) = @_;
	#get the filehandles
	*READ = $read;
	*WRITE = $write;
	#tell the user what we're doing
	print "Shutting down the connection to aspell";
	#save the user dictionary
	print WRITE "#\n";
	#clean up the pipe handles	
	close(WRITE);
	close(READ);
	waitpid($pid,0);
	print "Aspell connection shut down";
	print "Unloading script " . $IRSSI{'name'};
	Irssi::command("statusbar aspeller disable");
	$_ = "script unload " . $IRSSI{'name'};
	Irssi::command($_);
}

###############################################################################
##Stuff to handle signals and settings

sub aspeller()
#the actual function that gets called to redraw the statusbar
{
	my ($item, $get_size_only) = @_;
	my $statusbar_prefix = $aspell_colour . $g_window_name . ' %n';
	$item->default_handler($get_size_only, $statusbar_prefix . $global_text, undef, 1);
}

sub window_changed()
#update the status bar to reflect the changed window
{
	#get the name of the new window
	get_window_name();
	#and redraw the statusbar
	Irssi::statusbar_items_redraw('aspeller');
}

sub aspellerUpdate()
#called if any settings are modified
#updates things to reflect new settings
{
	#this bit adapted from Demonen's scroller script
	if ($speed != Irssi::settings_get_int('aspell_speed')) {
		Irssi::timeout_remove($speed);
		if (Irssi::settings_get_int('aspell_speed') < 10){
			Irssi::settings_set_int('aspell_speed', 10);
			print "Sorry, minimum delay for timeouts in irssi is 10 ms.  Delay set to 10 ms.";
		}
		$speed = Irssi::timeout_add(Irssi::settings_get_int('aspell_speed'), 'main' , undef);
		#since this is the setting that was changed, return
		return;
	}
	$enabled = Irssi::settings_get_bool('aspell_enable');
	if (! $enabled) {
		Irssi::command("statusbar aspeller disable");
	} else {
		Irssi::command("statusbar aspeller enable");
	}
}

###############################################################################
##SIGNALS

#add settings
Irssi::settings_add_str('aspeller', 'aspell_path', $program);
Irssi::settings_add_bool('aspeller', 'aspell_enable', 1);
Irssi::settings_add_int('aspeller','aspell_speed',50);

#setup Irssi signals
Irssi::timeout_add(Irssi::settings_get_int('aspell_speed'), 'main' , undef);
Irssi::signal_add('setup changed', 'aspellerUpdate');
Irssi::signal_add_last('window changed', 'window_changed');
Irssi::signal_add_last('window item changed', 'window_changed');

#setup the statusbar
Irssi::statusbar_item_register('aspeller', undef, 'aspeller');
Irssi::command("statusbar aspeller enable");
Irssi::command("statusbar aspeller add aspeller");

#add commands
Irssi::command_bind('aspell_shutdown','shutdown');
Irssi::command_bind('aspell','set');
Irssi::command_bind('aspell_add','add');
Irssi::command_bind('suggest','suggest');

#run init routine
init();
