#!/usr/bin/perl
use strict;
use warnings;

my $URL_REGEX = "https?://68.media.tumblr.com";
my $SIZE_CACHE_FILE = "size-cache";
my $HIGH_RES = 1280;

my $CMD_CURL = "curl-tor";

sub getSize($);
sub readSizeCache($);
sub writeSizeCache($$);
sub run(@);

my $usage = "Usage:
  $0 -h|--help
    print this message and exit

  $0 [OPTS]
    parse all HTML files in the current dir
    find tumblr-ish URLS and attempt to download
    either them or high-res (${HIGH_RES}px) versions of them

    if file 'xskip' exists, parse each line to find a prefix 'tumblr_<POST>'
    skip symlinking (but not download or order#) for each matching post

  OPTS
    -u|--urls
      just print the parsed URLs in order and exit

    -s|--sizes
      just fetch, cache, and print all sizes

    -r|--reverse
      process URLs in the reverse order
";

sub main(@){
  my $urlsOnly = 0;
  my $sizesOnly = 0;
  my $reverse = 0;
  while(@_ > 0){
    my $arg = shift;
    if($arg =~ /^(-h|--help)$/){
      die $usage;
    }elsif($arg =~ /^(-u|--urls)$/){
      $urlsOnly = 1;
    }elsif($arg =~ /^(-s|--sizes)$/){
      $sizesOnly = 1;
    }elsif($arg =~ /^(-r|--reverse)$/){
      $reverse = 1;
    }
  }

  my @htmlFiles = `ls *.html | sort -g`;
  my $html = '';
  for my $htmlFile(@htmlFiles){
    chomp $htmlFile;
    $html .= `cat $htmlFile`;
  }
  my @imgUrls = $html =~ m/($URL_REGEX\/[^"&]*)["&]/g;

  my @xskipPostsArr = `cat xskip 2>/dev/null`;
  @xskipPostsArr = grep {$_ =~ /^tumblr(_[a-zA-Z0-9]+)_/} @xskipPostsArr;
  @xskipPostsArr = map {$_ =~ /^tumblr(_[a-zA-Z0-9]+)_/; $1} @xskipPostsArr;
  my %xskipPosts = map {$_ => 1} @xskipPostsArr;

  my %xskipFileIds;

  my %alreadySeenUrls;

  my %fileIdOrder;

  my $order = 0;
  my %filesById;
  for my $imgUrl(@imgUrls){
    next if $imgUrl =~ /birthday\w+\.jpg/;
    next if $imgUrl =~ /avatar_\w+\.png/;
    next if defined $alreadySeenUrls{$imgUrl};
    $alreadySeenUrls{$imgUrl} = 1;
    if($imgUrl !~ /
      ^ $URL_REGEX
      (?:   \/ [0-9a-f]+     )?
      \/  (    tumblr)
          ( _  [a-zA-Z0-9]+)
      (?: ( _r \d+)          )?
      (?: ( _  \d+)          )?
          (\.  \w+)
      $/x){
      die "invalid url: $imgUrl\n";
    }
    my ($prefix, $post, $rnum, $res, $suffix) = ($1, $2, $3, $4, $5);
    $rnum = '' if not defined $rnum;
    $res = '' if not defined $res;
    my $fileName = "$prefix$post$rnum$res$suffix";
    my $fileId = "$post-$rnum-$suffix";
    my $file = {
      url           => $imgUrl,
      fileName      => $fileName,
      fileId        => $fileId,
      res           => $res,
    };
    if(defined $xskipPosts{$post}){
      $xskipFileIds{$fileId} = 1;
    }
    $filesById{$fileId} = [] if not defined $filesById{$fileId};
    push @{$filesById{$fileId}}, $file;
    if(not defined $fileIdOrder{$fileId}){
      $order++;
      $fileIdOrder{$fileId} = $order;
    }
  }

  my @keys = sort keys %xskipFileIds;

  my @fileIds = sort keys %filesById;
  @fileIds = reverse @fileIds if $reverse;

  if($urlsOnly){
    for my $fileId(@fileIds){
      for my $file(@{$filesById{$fileId}}){
        print "$$file{url}\n";
      }
    }
    exit 0;
  }

  my $symlinkPrefix = "order-";
  run "rm -f $symlinkPrefix*";

  my $count = 0;
  my $total = 0 + keys %filesById;

  my $countDigits = length $total;

  for my $fileId(@fileIds){
    $count++;
    print "\n$count / $total\n";
    my $maxRes = undef;
    my $maxResFile = undef;
    for my $file(@{$filesById{$fileId}}){
      my $fileRes = $$file{res};
      $fileRes =~ s/\D//g;
      if(not defined $maxRes or $maxRes < $fileRes){
        $maxRes = $fileRes;
        $maxResFile = $file;
      }
    }

    my $url = $$maxResFile{url};
    my $fileName = $$maxResFile{fileName};
    my $fileIdOrder = $fileIdOrder{$$maxResFile{fileId}};

    my $highResUrl = $url;
    my $highResFileName = $fileName;
    $highResUrl =~ s/_\d+\.(\w+)$/_$HIGH_RES.$1/;
    $highResFileName =~ s/_\d+\.(\w+)$/_$HIGH_RES.$1/;

    if($sizesOnly){
      getSize $url;
      getSize $highResUrl;
      next;
    }

    if(not -f $highResFileName){
      my $normalSize = getSize $url;
      my $highResSize = getSize $highResUrl;
      if($normalSize < $highResSize){
        run $CMD_CURL, $highResUrl, "--silent", "--show-error", "-o", $highResFileName;
        if($? != 0){
          print "\n";
          run "rm", $highResFileName;
          die "error running $CMD_CURL";
        }
        my $highResFileContents = `cat $highResFileName`;
        if($highResFileContents =~ /<Code>AccessDenied<\/Code>/){
          run "rm", $highResFileName;
        }
      }
    }

    if(not -f $highResFileName and not -f $fileName){
      run $CMD_CURL, $url, "--silent", "--show-error", "-o", $fileName;
      if($? != 0){
        print "\n";
        run "rm", $fileName;
        die "error running $CMD_CURL\n";
      }
    }

    my $target;
    $target = $highResFileName if -f $highResFileName;
    $target = $fileName if -f $fileName;

    if(defined $target and not defined $xskipFileIds{$fileId}){
      my $suffix = $1 if $target =~ /(\.\w+)$/;
      $suffix = "" if not defined $suffix;

      my $orderFmt = sprintf "%0${countDigits}d", $fileIdOrder;
      my $symlink = "$symlinkPrefix$orderFmt$suffix";
      run "ln", "-s", $target, $symlink;
    }
  }
}

sub getSize($){
  my ($url) = @_;
  my $cacheSize = readSizeCache $url;
  return $cacheSize if defined $cacheSize;
  my $urlFmt = $url;
  $urlFmt =~ s/\W+/_/g;
  my $tmpFile = "/tmp/dltumblr-headers-$urlFmt-" . time;
  run $CMD_CURL, $url, "--silent", "--show-error", "-I", "-o", $tmpFile;
  if($? != 0){
    print "\n";
    run "rm", $tmpFile;
    die "error running $CMD_CURL\n";
  }
  my $headers = `cat $tmpFile`;
  run "rm", $tmpFile;
  my $size;
  if($headers =~ /Content-Length: (\d+)/){
    $size = $1;
  }else{
    die "invalid headers: $headers\n";
  }
  writeSizeCache $url, $size;
  return $size;
}
sub readSizeCache($){
  my ($url) = @_;
  my @lines = `cat $SIZE_CACHE_FILE 2>/dev/null`;
  for my $line(@lines){
    if($line =~ /^(\d+) ($url)$/){
      return $1;
    }
  }
  return undef;
}
sub writeSizeCache($$){
  my ($url, $size) = @_;
  open FH, ">> $SIZE_CACHE_FILE" or die "could not write size cache\n$!\n";
  print FH "$size $url\n";
  close FH;
}


sub run(@){
  print "@_\n";
  system @_;
}

&main(@ARGV);
