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

implementation

end.

