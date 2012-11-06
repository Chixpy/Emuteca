{ This file is part of Emuteca

  Copyright (C) 2006-2013 Chixpy

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 3 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}

{ Unit of Emuteca core contants. }
unit uEmutecaConst;

{$mode objfpc}{$H+}

interface

const
  // Praying for no emulator use these exit codes.
  kEmutecaExecErrorNoGame = 300;
  {< Error code when game is not found.}
  kEmutecaDecompressError = 301;
  {< Error decompressing archive.}

  kEmutecaGameSubFolder = 'Game';
  {< Subfolder in temp directory, where games will be uncrompressed}

  // Keys in config and command line

  kEmutecaEmuDirKey = '%EMUDIR%';
  {< Emulator's directory key.}
  kEmutecaRomDirKey = '%ROMDIR%';
  {< ROM's directory key.}
  kEmutecaCurrentDirKey = '%CURRENTDIR%';
  {< Current directory key}

  kEmutecaROMPathKey = '%ROM%';
  {< ROM directory.}
  kEmutecaROMFileNameKey = '%ROMNAME%';
  {< ROM filename.}
  kEmutecaROMFileNameNoExtKey = '%ROMNAMENOEXT%';
  {< ROM filename without extension.}
  kEmutecaROMFileExtKey = '%ROMEXT%';
  {< ROM file extension.}
  kEmutecaNullKey = '%_%';
  {< Null value (empty string is substituted with '%ROMNAME%').}

  kGroupSectionKey = 'Group: ';
  {< Key for group sections in database files.}
  
  kEmutecaVirtualFolderExt = '.(folder)';
  {< Virtual extension used for folders y some contexts.}
  kEmutecaVirtualGroupExt = '.(group)';
  {< Virtual extension used for groups filenames.}
  kEmutecaVirtualGameExt = '.(game)';
  {< Virtual extension used for game filenames.}

implementation

end.

