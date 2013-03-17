unit uCHXConst;

{$mode objfpc}{$H+}

interface

const
  KLinuxDirSeparator = '/';
  KWinDirSeparator = '\';

  // WordDelimiters except utf8 bit mask (Dirty way ^_^)
  kCUUTF8Delimiters: set of char = [#0..#127] - ['a'..'z', 'A'..'Z', '1'..'9', '0'];


implementation

end.

