#!/usr/bin/perl
use strict;
use warnings;
use List::Util qw(max);

my $DEFAULT_OPTS = {
  artist                        => "",
  album                         => "",
  customChapterNames            => {},
  shortestChapterLenMillis      => 300000,
  fakeChapterBreakEndsSeconds   => {},
  silenceDetectMinMillis        => 1000,
  leadingSilenceMillis          => 500,
  longBreaksMinSilenceMillis    => 2800,
  shortBreaksMinSilenceMillis   => 1500,
  minPartMillis                 => 300000,
};

my $MODE_SIMULATE = "simulate";
my $MODE_RUN = "run";
my $MODE_FIND_FAKE_CHAPTER_BREAKS = "find-fake-chapter-breaks";

my $usage = "Usage:
  $0 [OPTS] INPUT_FILE
    -detect silences in INPUT_FILE using `silence-detect`
    -split into chapters by long breaks
    -split chapters into breaks by short breaks
    -write ffmpeg + oggenc commands to generate 1 file per part

  $0 [OPTS] INPUT_FILE --run
    same as `$0 [OPTS] INPUT_FILE`,
      except also RUN the ffmpeg+oggenc commands,
      unless the target files exist already

  $0 [OPTS] INPUT_FILE --find-fake-chapter-breaks
    -detect silences in INPUT_FILE using `silence-detect`
    -split into chapters by long breaks
    -for each chapter found:
      -print the silence info for that break as in silence-detect
      -play the original file using `mpv` at the chapter start

  OPTS
    -h | --help
      show this message

    --artist=ARTIST
      add this tag to oggenc (--title is calculated from chapter/part)

    --album=ALBUM
      add this tag to oggenc (--title is calculated from chapter/part)

    --custom-chapter-name=CHAPTER_NUM:CHAPTER_NAME
      (this arg can be given more than once)
      (default <CHAPTER_NAME> in title is \"Chapter <CHAPTER_NUM>\")
      (default <CHAPTER_NAME> in filename is \"ch<CHAPTER_NUM>\")

      for chapter# <CHAPTER_NUM>, use <CHAPTER_NAME> as part of oggenc --title
      for chapter# <CHAPTER_NUM>, use <CHAPTER_NAME> as part of the filename, after:
        -converting to lowercase
        -replacing non-word chars with '_'
        -replacing multiple '_'s in sequence with a single '_'
        -removing leading/trailing '_'s

    --shortest-chapter-len-millis=SHORTEST_CHAPTER_LEN_MILLIS
      (default is $$DEFAULT_OPTS{shortestChapterLenMillis})

      ignore any long break that would result in a chapter shorter than
      <SHORTEST_CHAPTER_LEN_MILLIS>ms
        -the higher this number, the more fake chapter breaks are found
        -set to 0 to assume all chapter breaks found are valid

    --fake-chapter-break-end-seconds=FAKE_CHAPTER_BREAK_END_SECONDS
      (this arg can be given more than once)

      long breaks that are NOT chapter breaks:
        do not treat any long break that ends at PRECISELY this value as a chapter
      note that this is the second value printed by `silence-detect`

    --silence-detect-min-millis=SILENCE_DETECT_MIN_MILLIS
      (default is $$DEFAULT_OPTS{silenceDetectMinMillis})

      minimum silence period in milliseconds to pass to `silence-detect`

    --leading-silence-millis=LEADING_SILENCE_MILLIS
      (default is $$DEFAULT_OPTS{leadingSilenceMillis}

      split parts at <LEADING_SILENCE_MILLIS>ms before the end of the silence
      be sure to use a value smaller than --short-breaks-min-silence-millis

    --long-breaks-min-silence-millis=LONG_BREAKS_MIN_SILENCE_MILLIS
      (default is $$DEFAULT_OPTS{longBreaksMinSilenceMillis})

      split file into chapters using silences that are
        at least <LONG_BREAKS_MIN_SILENCE_MILLIS>ms

    --short-breaks-min-silence-millis=SHORT_BREAKS_MIN_SILENCE_MILLIS
      (default is $$DEFAULT_OPTS{shortBreaksMinSilenceMillis})

      split chapters into parts using silences that are
        at least <SHORT_BREAKS_MIN_SILENCE_MILLIS>ms

    --min-part-millis=MIN_PART_MILLIS
      (default is $$DEFAULT_OPTS{minPartMillis}

      the minimum length of a generated story part,
        EXCEPT for full chapters that are smaller
";

sub getChapterBreaks($$);
sub parseChapterBreaksIntoChapters($$$);
sub getFilesToCreate($$$);
sub getSilences($$);

sub main(@){
  my $opts = {%$DEFAULT_OPTS};
  while(@_ > 0 and $_[0] =~ /^-/){
    my $arg = shift;
    if($arg =~ /^(-h|--help)$/){
      print $usage;
      exit 0;
    }elsif($arg =~ /^--artist=(.+)$/){
      $$opts{artist} = $1;
    }elsif($arg =~ /^--album=(.+)$/){
      $$opts{album} = $1;
    }elsif($arg =~ /^--custom-chapter-name=(\d+):(.+)$/){
      $$opts{customChapterNames}{$1} = $2;
    }elsif($arg =~ /^--shortest-chapter-len-millis=(\d+)$/){
      $$opts{shortestChapterLenMillis} = $1;
    }elsif($arg =~ /^--fake-chapter-break-end-seconds=(\d+|\d*\.\d+)$/){
      $$opts{fakeChapterBreakEndsSeconds}{$1} = 1;
    }elsif($arg =~ /^--silence-detect-min-millis=(\d+)$/){
      $$opts{silenceDetectMinMillis} = $1;
    }elsif($arg =~ /^--leading-silence-millis=(\d+)$/){
      $$opts{leadingSilenceMillis} = $1;
    }elsif($arg =~ /^--long-breaks-min-silence-millis=(\d+)$/){
      $$opts{longBreaksMinSilenceMillis} = $1;
    }elsif($arg =~ /^--short-breaks-min-silence-millis=(\d+)$/){
      $$opts{shortBreaksMinSilenceMillis} = $1;
    }elsif($arg =~ /^--min-part-millis=(\d+)$/){
      $$opts{minPartMillis} = $1;
    }else{
      die "$usage\nERROR: unknown or invalid arg $arg\n";
    }
  }

  my $mode;

  my $inputFile;
  if(@_ == 1 and -f $_[0]){
    $inputFile = $_[0];
    $mode = $MODE_SIMULATE;
  }elsif(@_ == 2 and -f $_[0] and $_[1] =~ /^--run$/){
    $inputFile = $_[0];
    $mode = $MODE_RUN;
  }elsif(@_ == 2 and -f $_[0] and $_[1] =~ /^--find-fake-chapter-breaks$/){
    $inputFile = $_[0];
    $mode = $MODE_FIND_FAKE_CHAPTER_BREAKS;
  }else{
    die "$usage\nERROR: missing input file, or invalid command\n";
  }

  my @silences = getSilences($opts, $inputFile);
  my @chapterBreaks = getChapterBreaks $opts, [@silences];

  if($mode eq $MODE_FIND_FAKE_CHAPTER_BREAKS){
    for my $chapterBreak(@chapterBreaks){
      print "\n\n\nif the long-break now playing is not a chapter break, add:\n";
      print "--fake-chapter-break-end-seconds=$$chapterBreak{end}\n";
      print " ($$chapterBreak{start}  $$chapterBreak{end}  $$chapterBreak{dur})\n";
      print "Press q for next (Ctrl+C in mpv will abort all)\n";
      system "mpv", "-ss", $$chapterBreak{end}, $inputFile
        and die; #Ctrl+C kills
    }
    exit 0;
  }

  my $inputFileDurationS = `duration -s -n "$inputFile"`;

  my @chapters = parseChapterBreaksIntoChapters $opts, $inputFileDurationS, [@chapterBreaks];

  my @filesToCreate = getFilesToCreate $opts, [@chapters], [@silences];

  my $fileNumDigs = length max(map {$$_{fileNum}} @filesToCreate);
  my $chapterNumDigs = length max(map {$$_{chapterNum}} @filesToCreate);
  my $partNumDigs = length max(map {$$_{partNum}} @filesToCreate);

  for my $file(@filesToCreate){
    my $fileNumFmt = sprintf "%0${fileNumDigs}d", $$file{fileNum};
    my $partNumFmt = sprintf "%0${partNumDigs}d", $$file{partNum};
    my $chapterNumFmt = sprintf "%0${chapterNumDigs}d", $$file{chapterNum};

    my $chapterName;
    my $cleanChapterName;
    if(defined $$opts{customChapterNames}{$$file{chapterNum}}){
      $chapterName = $$opts{customChapterNames}{$$file{chapterNum}};
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
    if($mode eq $MODE_SIMULATE){
      print "@ffmpegCmd\n";
    }elsif($mode eq $MODE_RUN){
      print "@ffmpegCmd\n";
      system @ffmpegCmd unless -e $fileNameWav;
    }

    my @oggencCmd = ("oggenc",
      "--title", "$chapterName - pt$partNumFmt",
      "--artist", $$opts{artist},
      "--album", $$opts{album},
      $fileNameWav,
    );
    my @oggencCmdFmt = map {"\"$_\""} @oggencCmd;
    if($mode eq $MODE_SIMULATE){
      print "@oggencCmdFmt\n";
    }elsif($mode eq $MODE_RUN){
      print "@oggencCmdFmt\n";
      system @oggencCmd unless -e $fileNameOgg;
    }
  }

  #system "md5sum *.wav > wav-md5sums";
  #system "duration -s *.wav > wav-duration";
  #system "du -b *.wav > wav-filesize";
}

sub getChapterBreaks($$){
  my ($opts, $silences) = @_;

  my $longBreaksMinSilenceS = $$opts{longBreaksMinSilenceMillis} / 1000.0;
  my $shortestChapterLenS = $$opts{shortestChapterLenMillis} / 1000.0;

  my @longBreaks = grep {$$_{dur} >= $longBreaksMinSilenceS} @$silences;

  my @chapterBreaks;
  my $prevChapterStart = 0;
  for my $break(@longBreaks){
    my $chapterStart = $$break{end};
    if($chapterStart - $prevChapterStart < $shortestChapterLenS){
      next; # fake chapter break
    }elsif(defined $$opts{fakeChapterBreakEndsSeconds}{$chapterStart}){
      next; # fake chapter break
    }else{
      push @chapterBreaks, $break;
      $prevChapterStart = $chapterStart;
    }
  }
  return @chapterBreaks;
}

sub parseChapterBreaksIntoChapters($$$){
  my ($opts, $inputFileDurationS, $chapterBreaks) = @_;
  my $leadingSilenceS = $$opts{leadingSilenceMillis} / 1000.0;

  my @chapters;
  my $prevChapterEnd = 0;
  for my $chapterBreak(@$chapterBreaks){
    my $chapterStart = $prevChapterEnd;
    my $chapterEnd = $$chapterBreak{end} - $leadingSilenceS; #minus seconds before end of silence
    push @chapters, {
      start => $chapterStart,
      end   => $chapterEnd,
    };
    $prevChapterEnd = $chapterEnd;
  }
  push @chapters, {
    start => $prevChapterEnd,
    end => $inputFileDurationS,
  };

  return @chapters;
}

sub getFilesToCreate($$$){
  my ($opts, $chapters, $silences) = @_;

  my $leadingSilenceS = $$opts{leadingSilenceMillis} / 1000.0;
  my $shortBreaksMinSilenceS = $$opts{shortBreaksMinSilenceMillis} / 1000.0;
  my $minPartS = $$opts{minPartMillis} / 1000.0;

  my @filesToCreate;
  my $fileNum = 1;

  my $chapterNum = 1;
  for my $chapter(@$chapters){
    my $chapterStart = $$chapter{start};
    my $chapterEnd = $$chapter{end};
    my @shortBreaksInChapter =
      grep {$$_{dur} >= $shortBreaksMinSilenceS and $$_{start} < $chapterEnd} @$silences;

    my $partNum = 1;
    my $curPartStart = $chapterStart;
    for my $break(@shortBreaksInChapter){
      my $targetEnd = $$break{end} - $leadingSilenceS; #minus seconds before end of silence
      if($targetEnd - $curPartStart < $minPartS){
        # this part would be too short
        next;
      }elsif($chapterEnd - $targetEnd < $minPartS){
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

sub getSilences($$){
  my ($opts, $inputFile) = @_;
  my $intervalMillis = $$opts{silenceDetectMinMillis};
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
