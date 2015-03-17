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

unit uConst;

{$mode objfpc}{$H+}

interface

const
  // Script engine const (These may be in uEmutecaConst or uEmutecaRscStr )
  kFSMUnitsFolder = 'Common/';
  {< Folder for common units. }
  kFSMDataSection = 'SCRIPTDATA';
  {< Section for reading script information. }

  // Copyright and legal non translatable strings
  // --------------------------------------------
  kCopyrightString = '(C) 2006-2012 Chixpy';
  {< Copyright. }
  kLicenseString = 'GNU GPL v3';
  {< License. }

  // File extensions
  // ---------------
  kFileExtensionGameDB = '.edb';
  {< File extension used for game databases. }
  kFileExtensionScript = '.pas';
  {< File extension used for script files. }
  kFileExtensionEmuDB = '.ini';
  {< File extension used for emu database files. }
  kFileExtensionSystemDB = '.ini';
  {< File extension used for emulator databases. }
  kFileExtensionText = '.txt';
  {< Default file extension for text files}

  // File masks for filters
  // ----------------------
  kFileMaskAllFiles = AllFilesMask;
  {< Mask for all files. }
  kFileMaskGameDB = '*' + kFileExtensionGameDB;
  {< Mask for game databases. }
  kFileMaskScript = '*' + kFileExtensionScript;
  {< Mask for script files. }
  kFileMaskEmuDB = '*' + kFileExtensionEmuDB;
  {< Mask for script files. }
  kFileMaskSystemDB = '*' + kFileExtensionSystemDB;
  {< Mask for emulator databases. }
  kFileMaskText = '*' + kFileExtensionSystemDB;
  {< Mask for text files. }

  // Config sections and keys
  // ------------------------

  // Default images section
  kConfigSectionImages = 'Images';
  kConfigKeyImagesFolder = 'ImagesFolder';
  {}kConfigKeyDefaultImagesSubfolder = 'DefaultImagesSubfolder';
  {  }kConfigKeyDefaultSystemImage = 'DefaultSystemImage';
  {  }kConfigKeyDefaultSystemIcon = 'DefaultSystemIcon';
  {  }kConfigKeyDefaultEmulatorImage ='DefaultEmulatorImage';
  {  }kConfigKeyDefaultEmulatorIcon = 'DefaultEmulatorIcon';
  {  }kConfigKeyDefaultGameImage = 'DefaultGameImage';
  {  }kConfigKeyDefaultGameIcon = 'DefaultGameIcon';
  {}kConfigKeyFlagsSubfolder = 'FlagsSubfolder';
  {}kConfigKeyVIIconsSubfolder = 'VIIconsSubfolder';
  {}kConfigKeyIconsSubfolder =  'IconsSubfolder';
  {  }kConfigKeyIconsIniFile = 'IconsIniFile';

  // Configuration and data section
  kConfigSectionConfig = 'Config';
  kConfigKeyHelpFolder = 'HelpFolder';
  kConfigKeySearchFile = 'SearchFile';
  kConfigKeyDataFolder = 'DataFolder';
  kConfigKeyEmulatorsIniFile = 'EmulatorsIniFile';
  kConfigKeySystemsIniFile = 'SystemsIniFile';



  // Other constants (maybe they can be variables...)
  // ---------------
  kSimilarityThresold = 25;
  {< Thresold used for fuzzy matching }

implementation

end.

