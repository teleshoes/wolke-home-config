#!/usr/bin/perl
use strict;
use warnings;
use List::Util qw(max);

my $inputFile = "googleplay_shards_of_honor.m4a.orig";

my $artist = "Lois McMaster Bujold, Grover Gardner";
my $album = "Shards of Honor";

# chapter names by number
#   custom chapter names appear in filename,
#     lowercase,
#     with non-word chars replaced with '_',
#     multiple '_'s replaced with a single '_',
#     and leading/trailing '_'s removed
#   default chapter names are "Chapter #", and "ch#" in the filename
my %customChapterNames = (
  16 => 'Aftermaths',
);

# min silence to detect for later use, calculated and stored in a file
my $silenceDetectMinMillis = 1000;

#
my $longBreaksMinSilenceS = 2.8;
my $shortBreaksMinSilenceS = 1.5;

# shift each break to this many seconds before the silence ends
my $leadingSilenceS = 0.5;

# smallest duration for a part file, excepting full chapters that are shorter
my $minPartSeconds = 300;

# set higher to find more fake chapter breaks, 0 for no fakes
#   ignore any long break that would result in a chapter shorter than this value
my $shortestChapterLenS = 1140;

# long breaks that are not in fact chapter breaks
my %hardcodedFakeChapterBreakEnds = map {$_ => 1} qw(
  22554.2 29676.8 31169.8
);

sub getChapterBreaks($);
sub parseChapterBreaksIntoChapters($);
sub getFilesToCreate($$);
sub getSilences($);

sub main(@){
  my @silences = getSilences($silenceDetectMinMillis);
  my @chapterBreaks = getChapterBreaks [@silences];

  for my $chapterBreak(@chapterBreaks){
    ### uncomment to find fake chapter breaks
    ###   listen to each, if it doesnt start with 'chapter whatever',
    ###     copy the 2nd number, the silence end, to hardcodedFakeChapterBreakEnds
    #print "$$chapterBreak{start}  $$chapterBreak{end}  $$chapterBreak{dur}\n";
    #system "mpv", "-ss", $$chapterBreak{end}, $inputFile and die;
  }

  my @chapters = parseChapterBreaksIntoChapters [@chapterBreaks];

  my @filesToCreate = getFilesToCreate [@chapters], [@silences];

  my $fileNumDigs = length max(map {$$_{fileNum}} @filesToCreate);
  my $chapterNumDigs = length max(map {$$_{chapterNum}} @filesToCreate);
  my $partNumDigs = length max(map {$$_{partNum}} @filesToCreate);

  for my $file(@filesToCreate){
    my $fileNumFmt = sprintf "%0${fileNumDigs}d", $$file{fileNum};
    my $partNumFmt = sprintf "%0${partNumDigs}d", $$file{partNum};
    my $chapterNumFmt = sprintf "%0${chapterNumDigs}d", $$file{chapterNum};

    my $chapterName;
    my $cleanChapterName;
    if(defined $customChapterNames{$$file{chapterNum}}){
      $chapterName = $customChapterNames{$$file{chapterNum}};
      $cleanChapterName = lc $chapterName;
      $cleanChapterName =~ s/[^a-zA-Z0-9]+/_/g;
      $cleanChapterName =~ s/^_//;
      $cleanChapterName =~ s/_$//;
    }else{
      $chapterName = "Chapter $chapterNumFmt";
      $cleanChapterName = "ch$chapterNumFmt";
    }

    my $fileNamePrefix = "${fileNumFmt}_${cleanChapterName}_pt${partNumFmt}";

    my $fileNameWav = "$fileNamePrefix.wav";
    my $fileNameOgg = "$fileNamePrefix.ogg";
    my @ffmpegCmd = ("ffmpeg",
      "-i", $inputFile,
      "-ss", $$file{start},
      "-t", $$file{dur},
      "-bitexact",
      $fileNameWav,
    );
    print "@ffmpegCmd\n";
    #system @ffmpegCmd unless -e $fileNameWav;

    my @oggencCmd = ("oggenc",
      "--title", "$chapterName - pt$partNumFmt",
      "--artist", $artist,
      "--album", $album,
      $fileNameWav,
    );
    my @oggencCmdFmt = map {"\"$_\""} @oggencCmd;
    print "@oggencCmdFmt\n";
    #system @oggencCmd unless -e $fileNameOgg;
  }

  #system "md5sum *.wav > wav-md5sums";
  #system "duration -s *.wav > wav-duration";
  #system "du -b *.wav > wav-filesize";
}

sub getChapterBreaks($){
  my @silences = @{$_[0]};
  my @longBreaks = grep {$$_{dur} >= $longBreaksMinSilenceS} @silences;

  my @chapterBreaks;
  my $prevChapterStart = 0;
  for my $break(@longBreaks){
    my $chapterStart = $$break{end};
    if($chapterStart - $prevChapterStart < $shortestChapterLenS){
      next; # fake chapter break
    }elsif(defined $hardcodedFakeChapterBreakEnds{$chapterStart}){
      next; # fake chapter break
    }else{
      push @chapterBreaks, $break;
      $prevChapterStart = $chapterStart;
    }
  }
  return @chapterBreaks;
}

sub parseChapterBreaksIntoChapters($){
  my @chapterBreaks = @{$_[0]};
  my @chapters;
  my $prevChapterEnd = 0;
  for my $chapterBreak(@chapterBreaks){
    my $chapterStart = $prevChapterEnd;
    my $chapterEnd = $$chapterBreak{end} - $leadingSilenceS; #minus seconds before end of silence
    push @chapters, {
      start => $chapterStart,
      end   => $chapterEnd,
    };
    $prevChapterEnd = $chapterEnd;
  }

  return @chapters;
}

sub getFilesToCreate($$){
  my @chapters = @{$_[0]};
  my @silences = @{$_[1]};

  my @filesToCreate;
  my $fileNum = 1;

  my $chapterNum = 1;
  for my $chapter(@chapters){
    my $chapterStart = $$chapter{start};
    my $chapterEnd = $$chapter{end};
    my @shortBreaksInChapter =
      grep {$$_{dur} >= $shortBreaksMinSilenceS and $$_{start} < $chapterEnd} @silences;

    my $partNum = 1;
    my $curPartStart = $chapterStart;
    for my $break(@shortBreaksInChapter){
      my $targetEnd = $$break{end} - $leadingSilenceS; #minus seconds before end of silence
      if($targetEnd - $curPartStart < $minPartSeconds){
        # this part would be too short
        next;
      }elsif($chapterEnd - $targetEnd < $minPartSeconds){
        # the next part would be too short
        next;
      }else{
        push @filesToCreate, {
          start => $curPartStart,
          end => $targetEnd,
          dur => $targetEnd - $curPartStart,
          chapterNum => $chapterNum,
          partNum => $partNum,
          fileNum => $fileNum,
        };
        $fileNum++;
        $partNum++;
        $curPartStart = $targetEnd;
      }
    }
    push @filesToCreate, {
      start => $curPartStart,
      end => $chapterEnd,
      dur => $chapterEnd - $curPartStart,
      chapterNum => $chapterNum,
      partNum => $partNum,
      fileNum => $fileNum,
    };
    $fileNum++;
    $chapterNum++;
  }

  return @filesToCreate;
}

sub getSilences($){
  my ($intervalMillis) = @_;
  my $silenceDetectOutputFile = "silence-detect-interval-$intervalMillis.out";
  if(not -e $silenceDetectOutputFile){
    print "DETECTING SILENCES >= ${intervalMillis}ms\n";
    system "silence-detect --interval $intervalMillis $inputFile | tee $silenceDetectOutputFile";
  }else{
    print "SKIPPING DETECTING SILENCES\n";
  }
  my @lines = `cat $silenceDetectOutputFile`;
  my @silences;
  for my $line(@lines){
    if($line =~ /^\s*(\d+|\d*\.\d+)\s+(\d+|\d*\.\d+)\s+(\d+|\d*\.\d+)\s*$/){
      push @silences, {
        start => $1,
        end => $2,
        dur => $3,
      };
    }else{
      die "ERROR: invalid line $line";
    }
  }
  return @silences;
}

&main(@ARGV);
