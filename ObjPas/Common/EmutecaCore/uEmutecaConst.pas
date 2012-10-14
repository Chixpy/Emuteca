unit uEmutecaConst;

{$mode objfpc}{$H+}

interface

const
  // Praying for no emulator use these exit codes.
  CGMExecErrorNoGame = 300;
  CGMDecompressError = 301;

  CGMGameSubFolder = 'Game';

  CEmuDir = '%EMUDIR%';
  CRomDir = '%ROMDIR%';
  CCurrentDir = '%CURRENTDIR%';

  CROMPath = '%ROM%';
  CROMName = '%ROMNAME%';
  CROMNameNoExt = '%ROMNAMENOEXT%';
  CROMExt = '%ROMEXT%';
  CROMNull = '%_%';

  CGAMEGROUPKEY = 'Group: ';
  
  kCUVirtualFolderExt = '.(folder)';
  //< Virtual extension used for folders y some contexts
  kCUVirtualGroupExt = '.(group)';
  //< Virtual extension used for groups filenames
  kCUVirtualGameExt = '.(game)';
  //< Virtual extension used for game filenames  

implementation

end.

