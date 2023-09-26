unit uEmutecaConst;

{< Constants unit of Emuteca Core.

  This file is part of Emuteca Core.

  Copyright (C) 2011-2018 Chixpy
}
{$mode objfpc}{$H+}

interface

const
  // Form and frame names
  krsFmtWindowCaption = '%0:s: %1:s';
  {< Window caption format
    %0:s = Application.Title (derived from rsFmtApplicationTitle).
    %1:s = Window caption.
  }

  krsTempGameSubFolder = 'Soft/';
  {< Subfolder in temp directory, where games will be decompressed.

    With ending folder separator.
  }


  // File extensions
  // ---------------
  krsFileExtGroup = '.egl';
  {< Extension for group lists. }
  krsFileExtSoft = '.csv';
  {< Extension for soft lists. }
  krsFileExtINI = '.ini';
  {< Extension for ini databases (Systems, Emulators). }
  krsFileExtScript = '.pas';
  {< Extension for script files. }
  krsFileExtTXT = '.txt';
  {< Extension for generic text files. }

  // File masks for filters
  // ----------------------
  krsFileMaskGroup = '*' + krsFileExtGroup;
  {< File mask for group lists. }
  krsFileMaskSoft = '*' + krsFileExtSoft;
  {< File mask for soft lists. }
  krsFileMaskINI = '*' + krsFileExtINI;
  {< File mask for ini databases (Systems, Emulators). }
  krsFileMaskScript = '*' + krsFileExtScript;
  {< File mask for script files. }
  krsFileMaskTXT = '*' + krsFileExtTXT;
  {< File mask for generic text files. }

  // EXIT CODES for handling some errors
  // -----------------------------------
  // Praying for no emulator use these exit codes.
  kErrorRunSoftUnknown = -300;
  {< Run Software: Unknown error. }
  kErrorRunSoftNoSoft = kErrorRunSoftUnknown - 1;
  {< Run Software: Error code when soft = nil. }
  kErrorRunSoftNoSystem = kErrorRunSoftNoSoft - 1;
  {< Run Software: Fatal Error No system cached ¿¿??. }
  kErrorRunSoftNoEmu = kErrorRunSoftNoSystem - 1;
  {< Run Software: Error code when Emulator = nil. }
  kErrorRunSoftNoSoftFile = kErrorRunSoftNoEmu - 1;
  {< Run Software: Error code when soft file is not found. }
  kErrorRunSoftNoEmuFile = kErrorRunSoftNoSoftFile - 1;
  {< Run Software: Error code when emulator exe is not found. }
  kError7zDecompress = -400;
  {< Base error const decompressing archive. }

  // CSV list headers
  // ----------------
  krsCSVStatsHeader = '"Last Time","Times Played","Playing Time"';

  krsCSVSoftHeader = '"Group","SHA1","ID","Folder","FileName",' +
    '"Title","[Removed]","Sort Title","Version","Year","Publisher",' +
    '"Zone","DumpStatus","Dump Info","Fixed","Trainer","Translation",' +
    '"Pirate","Cracked","Modified","Hack","Extra Parameters"';
  krsCSVSoftStatsHeader = krsCSVSoftHeader + ',' + krsCSVStatsHeader;
  krsCSVGroupHeader = '"ID","Title","Sort Title","Year","Developer",' +
    '"[Removed]"';
  krsCSVGroupStatsHeader = krsCSVGroupHeader + ',' + krsCSVStatsHeader;


  // IniKeys
  // -------
  // Shared
  krsIniKeyID = 'ID';
  krsIniKeyTitle = 'Title';
  krsIniKeySortTitle = 'SortTitle';
  krsIniKeyFileName = 'FileName';
  krsIniKeyYear = 'Year';
  krsIniKeyEnabled = 'Enabled';
  krsIniKeyWorkingFolder = 'WorkingFolder';
  krsIniKeyIcon = 'Icon';
  krsIniKeyImage = 'Image';
  krsIniKeyDeveloper = 'Developer';
  krsIniKeyExtensions = 'Extensions';

  // System
  // Shared Keys: krsIniKeyID, krsIniKeyTitle, krsIniKeyFileName,
  //   krsIniKeyWorkingFolder, krsIniKeyIcon, krsIniKeyImage,
  //   krsIniKeyExtensions
  krsIniKeyCoreIDs = 'CoreIDs';
  krsIniKeyBaseFolder = 'BaseFolder';
  krsIniKeyMainEmulator = 'MainEmulator';
  krsIniKeyOtherEmulators = 'OtherEmulators';
  krsIniKeyBackImage = 'BackImage';
  krsIniKeySoftIcon = 'SoftIcon';
  krsIniKeyIconFolder = 'IconFolder';
  krsIniKeyLogoFolder = 'LogoFolder';
  krsIniKeyImageFolders = 'ImageFolders';
  krsIniKeyImageCaptions = 'ImageCaptions';
  krsIniKeyText = 'Text';
  krsIniKeyTextFolders = 'TextFolders';
  krsIniKeyTextCaptions = 'TextCaptions';
  krsIniKeyMusicFolders = 'MusicFolders';
  krsIniKeyMusicCaptions = 'MusicCaptions';
  krsIniKeyVideoFolders = 'VideoFolders';
  krsIniKeyVideoCaptions = 'VideoCaptions';
  krsIniKeySoftExportKey = 'SoftExportKey';
  krsIniKeyExtractAll = 'ExtractAll';
  krsIniKeyMergeableGroups = 'MergeableGroups';

  // Group
  // Shared Keys: krsIniKeyID, krsIniKeyTitle, krsIniKeySortTitle,
  //   krsIniKeyYear, krsIniKeyFileName, krsIniKeyDeveloper


  // Soft
  // Shared Keys: krsIniKeyID, krsIniKeyTitle, krsIniKeySortTitle,
  //   krsIniKeyYear, krsIniKeyFileName
  krsIniKeySHA1 = 'SHA1';
  krsIniKeyGroup = 'Group';
  krsIniKeyTranslitTitle = 'TranslitTitle';
  krsIniKeyVersion = 'Version';
  krsIniKeyPublisher = 'Publisher';
  krsIniKeyZone = 'Zone';
  krsIniKeyDumpInfo = 'DumpInfo';
  krsIniKeyDumpStatus = 'DumpStatus';
  krsIniKeyFixed = 'Fixed';
  krsIniKeyTrainer = 'Trainer';
  krsIniKeyTranslation = 'Translation';
  krsIniKeyPirate = 'Pirate';
  krsIniKeyCracked = 'Cracked';
  krsIniKeyModified = 'Modified';
  krsIniKeyHack = 'Hack';
  krsIniKeyFolder = 'Folder';

  // Emulator
  // Shared Keys: krsIniKeyEnabled, krsIniKeyTitle, krsIniKeyWorkingFolder,
  //   krsIniKeyIcon, krsIniKeyImage, krsIniKeyDeveloper, krsIniKeyExtensions
  krsIniKeyParameters = 'Parameters';
  krsIniKeyCoreIDKey = 'CoreIDKey';
  krsIniKeyCoreIDParamFmt = 'CoreIDParamFmt';
  krsIniKeyExtensionParamFmt = 'ExtensionParamFmt';
  krsIniKeyExtraParamFmt = 'ExtraParamFmt';
  krsIniKeyExitCode = 'ExitCode';
  krsIniKeyExeFile = 'ExeFile';
  krsIniKeyWebPage = 'WebPage';
  krsIniKeyInfoFile = 'InfoFile';

  // Playing Stats
  krsIniKeyPlayingTime = 'PlayingTime';
  krsIniKeyTimesPlayed = 'TimesPlayed';
  krsIniKeyLastTime = 'LastTime';

  // Enumerated, sets, etc.
  // ----------------------
  // Constants for krsIniKeySoftExportKey
  krsSEKCRC32 = 'CRC32';
  krsSEKSHA1 = 'SHA1';
  krsSEKFileName = 'FileName';
  krsSEKCustom = 'Custom';

  // Constant for DumpStatus, fixed (for icon filenames)
  krsEDSFavorite = 'Favorite';
  krsEDSGood = 'GoodDump';
  //krsEDSAlternate = 'Alternate';
  krsEDSUnknown = 'Unknown';
  krsEDSOverDump = 'OverDump';
  krsEDSBadDump = 'BadDump';
  krsEDSUnderDump = 'UnderDump';
  krsEDSKeepValue = 'KeepValue';

  // Constant for DumpStatus, fixed (for databases)
  krsEDSFavoriteKey = '!';
  krsEDSGoodKey = '';
  //krsEDSAlternateKey = 'a';
  krsEDSUnknownKey = '?';
  krsEDSOverDumpKey = 'o';
  krsEDSBadDumpKey = 'b';
  krsEDSUnderDumpKey = 'u';
  krsImportKeepValueKey = '@';

  // Dirs
  krsTemp7zCacheFolder = 'SHA1Cache/';

type
  TEmutecaSoftExportKey = (TEFKSHA1, TEFKCRC32, TEFKFileName, TEFKCustom);
  TEmutecaDumpStatus = (edsFavorite, edsGood, {edsAlternate,} edsUnknown,
    edsOverDump, edsBadDump, edsUnderDump,  edsKeepValue);

  TEmutecaProgressCallBack = function(const aAction, aInfo: string;
    const aValue, aMaxValue: int64; const IsCancelable: boolean): boolean of
    object;
{< Callback funtion to show progress }

const
  EmutecaSoftExportKeyStrK: array [TEmutecaSoftExportKey] of string =
    (krsSEKSHA1, krsSEKCRC32, krsSEKFileName, krsSEKCustom);
  //< Strings for FileKeys (fixed constants, used for ini files, etc. )

  EmutecaDumpStatusKey: array [TEmutecaDumpStatus] of string =
    (krsEDSFavoriteKey, krsEDSGoodKey, {krsEDSAlternateKey,} krsEDSUnknownKey,
    krsEDSOverDumpKey, krsEDSBadDumpKey, krsEDSUnderDumpKey,
    krsImportKeepValueKey);
  //< Keys for DumpStatus, used in IniFiles
  EmutecaDumpStatusStrK: array [TEmutecaDumpStatus] of string =
    (krsEDSFavorite, krsEDSGood, {krsEDSAlternate,} krsEDSUnknown,
    krsEDSOverDump, krsEDSBadDump, krsEDSUnderDump,  krsEDSKeepValue);
//< Strings for DumpStatus (fixed constants, used for icon filenames, etc. )


// Nothing to implement... :-D
implementation

end.
{
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
