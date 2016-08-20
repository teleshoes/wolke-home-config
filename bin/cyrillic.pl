#!/usr/bin/perl
use strict;
use warnings;
use Term::ReadKey;

$SIG{'INT'} = sub{system "clear"; ReadMode 0; exit 0};

our $keyDelay = 0.3;

sub shuffle(@);
sub getSomeInput();
sub getSomeKeys();

my $cyr = "
А | а | а            | ah
Б | б | бэ           | beh
В | в | вэ           | veh
Г | г | гэ           | geh
Д | д | дэ           | deh
Е | е | е            | yeh
Ё | ё | ё            | yaw
Ж | ж | жэ           | zheh
З | з | зэ           | zeh
И | и | и            | ee
Й | й | и краткое    | y
К | к | ка           | kah
Л | л | эл or эль    | el
М | м | эм           | em
Н | н | эн           | en
О | о | о            | aw
П | п | пэ           | peh
Р | р | эр           | errr
С | с | эс           | ess
Т | т | тэ           | teh
У | у | у            | oo
Ф | ф | эф           | eff
Х | х | ха           | chhha
Ц | ц | це           | tseh
Ч | ч | че           | cheh
Ш | ш | ша           | sha
Щ | щ | ща           | shcha
Ъ | ъ | твёрдый знак | hard/un-palate
Ы | ы | ы            | oo-ee
Ь | ь | мягкий знак  | soft/palate
Э | э | э            | eh
Ю | ю | ю            | ee-oo
Я | я | я            | yah
";

sub main(@){
  my @alphabet;
  for my $line(split /\n/, $cyr){
    next if $line =~ /^\s*$/;
    my ($big, $small, $name, $eng) = split / \| /, $line;
    $big =~ s/^\s*(.*?)\s*$/$1/;
    $small =~ s/^\s*(.*?)\s*$/$1/;
    $name =~ s/^\s*(.*?)\s*$/$1/;
    $eng =~ s/^\s*(.*?)\s*$/$1/;
    push @alphabet, [$big, $eng];
  }

  @alphabet = shuffle @alphabet;
  my $letterIndex = 0;
  my $partIndex = 0;
  my $repaint = 1;
  while(1){
    my @letter = @{$alphabet[$letterIndex]};
    my $s = "$letter[$partIndex]\n";
    print $s if $repaint;
    $repaint = 0;
    for my $key(@{getSomeKeys()}){
      $key = uc $key;
      if($key eq 'A'){
        $partIndex--;
        $partIndex = 0 if $partIndex < 0;
        $repaint = 1;
      }elsif($key eq 'D'){
        $partIndex++;
        $partIndex = $#letter if $partIndex > $#letter;
        $repaint = 1;
      }elsif($key eq 'W'){
        $letterIndex--;
        $letterIndex = 0 if $letterIndex < 0;
        $repaint = 1;
      }elsif($key eq 'S'){
        $letterIndex++;
        $letterIndex = $#alphabet if $letterIndex > $#alphabet;
        $repaint = 1;
      }elsif($key eq 'Q'){
        exit 0;
      }
    }
  }
}

sub shuffle(@){
  my @ordered = @_;
  my @shuffled = ();
  while (@ordered) {
    my $i = int(rand() * @ordered);
    push @shuffled, $ordered[$i];
    splice(@ordered, $i, 1);
  }
  return @shuffled;
}

sub getSomeInput(){
  ReadMode 3;
  my @bytes;
  my $start = time;

  while(1){
    my $byte = ReadKey($keyDelay);
    last if not defined $byte and time - $start > $keyDelay;
    push @bytes, $byte if defined $byte;
  }
  ReadMode 0;
  return \@bytes;
}

#assumes utf8
sub getSomeKeys(){
  my $enter = 'ENTER';
  my $bkspc = 'BACKSPACE';
  my @cmds = (
    ['[', 'A'], 'UP',
    ['[', 'B'], 'DOWN',
    ['[', 'C'], 'RIGHT',
    ['[', 'D'], 'LEFT',
    ['O', 'H'], 'HOME',
    ['O', 'F'], 'END',
    ['[', '2', '~'], 'INSERT',
    ['[', '3', '~'], 'DELETE',
    ['[', '5', '~'], 'PGUP',
    ['[', '6', '~'], 'PGDN',
  );

  my @keys;
  my @bytes = @{getSomeInput()};
  for(my $i=0; $i<@bytes; $i++){
    if(ord $bytes[$i] == 0x1b){
      my $k1 = $i+1<=$#bytes ? $bytes[$i+1] : '';
      my $k2 = $i+2<=$#bytes ? $bytes[$i+2] : '';
      my $k3 = $i+3<=$#bytes ? $bytes[$i+3] : '';
      for(my $c=0; $c<@cmds; $c+=2){
        my @cmdArr= @{$cmds[$c]};
        my $cmd= $cmds[$c+1];
        if(@cmdArr == 2 and $cmdArr[0] eq $k1 and $cmdArr[1] eq $k2){
          push @keys, $cmd;
          $i+=2;
          last;
        }elsif(@cmdArr == 3 and
               $cmdArr[0] eq $k1 and
               $cmdArr[1] eq $k2 and
               $cmdArr[2] eq $k3){
          push @keys, $cmd;
          $i+=3;
          last;
        }
      }
    }elsif($bytes[$i] eq "\n"){
      push @keys, $enter;
    }elsif(ord $bytes[$i] == 0x7f){
      push @keys, $bkspc;
    }elsif(ord $bytes[$i] >= 0xc2 and ord $bytes[$i] <= 0xdf){
      my $b1 = $bytes[$i];
      my $b2 = $i+1<=$#bytes ? $bytes[$i+1] : '';
      my $key = "$b1$b2";
      $i+=1;
      utf8::decode($key);
      push @keys, $key; 
    }elsif(ord $bytes[$i] >= 0xe0 and ord $bytes[$i] <= 0xef){
      my $b1 = $bytes[$i];
      my $b2 = $i+1<=$#bytes ? $bytes[$i+1] : '';
      my $b3 = $i+2<=$#bytes ? $bytes[$i+2] : '';
      my $key = "$b1$b2$b3";
      $i+=2;
      utf8::decode($key);
      push @keys, $key;
    }else{
      push @keys, $bytes[$i];
    }
  }
  return \@keys;
}


&main(@ARGV);
