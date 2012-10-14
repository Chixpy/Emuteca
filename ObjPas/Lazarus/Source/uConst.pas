unit uConst;

{$mode objfpc}{$H+}

interface

const
  // Script engine const (These may be in uEmutecaConst or uEmutecaRscStr )
  kFSMUnitsFolder = 'Common/';
  {< Folder for common units }
  kFSMDataSection = 'SCRIPTDATA';
  {< Section for reading script information }

  // Copyright and legal non translatable strings
  // --------------------------------------------
  kCopyrightString = '(C) 2006-2012 Chixpy';
  {< Copyright }
  kLicenseString = 'GNU GPL v3';
  {< License }

  // File extensions
  // ---------------
  kFileExtensionGameDB = '.edb';
  {< File extension used for game databases }
  kFileExtensionScript = '.pas';
  {< File extension used for script files }
  kFileExtensionEmuDB = '.ini';
  {< File extension used for c}
  kFileExtensionSystemDB = '.ini';
  {< File extension used for emulator databases }

  // File masks for filters
  // ----------------------
  kFileMaskAllFiles = AllFilesMask;
  {< Mask for all files }
  kFileMaskGameDB = '*' + kFileExtensionGameDB;
  {< Mask for game databases }
  kFileMaskScript = '*' + kFileExtensionScript;
  {< Mask for script files }
  kFileMaskEmuDB = '*' + kFileExtensionEmuDB;
  {< Mask for script files }
  kFileMaskSystemDB = '*' + kFileExtensionSystemDB;
  {< Mask for emulator databases }

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

  // Configuration and data
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

