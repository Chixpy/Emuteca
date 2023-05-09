unit uPSI_uEmutecaConst;

{< Exports of uEmutecaConst for Pascal Script engine of Emuteca.

  ----

  This file is part of Emuteca Core.

  Copyright (C) 2011-2018 Chixpy

  This source is free software; you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 3 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
  more details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by
  writing to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
  Boston, MA 02111-1307, USA.
}
{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, uPSComponent, uPSRuntime, uPSCompiler,
  // Emuteca Core units
  uEmutecaConst;

procedure SIRegister_uEmutecaConst(CL: TPSPascalCompiler);
procedure RIRegister_uEmutecaConst_Routines(S: TPSExec);

implementation

procedure SIRegister_uEmutecaConst(CL: TPSPascalCompiler);
begin
  CL.AddConstantN('krsFmtWindowCaption', 'String').SetString(
    krsFmtWindowCaption);
  CL.AddConstantN('krsTempGameSubFolder', 'String').SetString(
    krsTempGameSubFolder);

  // File extensions
  // ---------------
  CL.AddConstantN('krsFileExtGroup', 'String').SetString(krsFileExtGroup);
  CL.AddConstantN('krsFileExtSoft', 'String').SetString(krsFileExtSoft);
  CL.AddConstantN('krsFileExtINI', 'String').SetString(krsFileExtINI);
  CL.AddConstantN('krsFileExtScript', 'String').SetString(krsFileExtScript);
  CL.AddConstantN('krsFileExtTXT', 'String').SetString(krsFileExtTXT);

  // File masks for filters
  // ----------------------
  CL.AddConstantN('krsFileMaskGroup', 'String').SetString(krsFileMaskGroup);
  CL.AddConstantN('krsFileMaskSoft', 'String').SetString(krsFileMaskSoft);
  CL.AddConstantN('krsFileMaskINI', 'String').SetString(krsFileMaskINI);
  CL.AddConstantN('krsFileMaskScript', 'String').SetString(krsFileMaskScript);
  CL.AddConstantN('krsFileMaskTXT', 'String').SetString(krsFileMaskTXT);

  // EXIT CODES for handling some errors
  // -----------------------------------
  CL.AddConstantN('kErrorRunSoftUnknown', 'LongInt').SetInt(
    kErrorRunSoftUnknown);
  CL.AddConstantN('kErrorRunSoftNoSoft', 'LongInt').SetInt(
    kErrorRunSoftNoSoft);
  CL.AddConstantN('kErrorRunSoftNoEmu', 'LongInt').SetInt(kErrorRunSoftNoEmu);
  CL.AddConstantN('kErrorRunSoftNoSoftFile', 'LongInt').SetInt(
    kErrorRunSoftNoSoftFile);
  CL.AddConstantN('kErrorRunSoftNoEmuFile', 'LongInt').SetInt(
    kErrorRunSoftNoEmuFile);
  CL.AddConstantN('kError7zDecompress', 'LongInt').SetInt(kError7zDecompress);

  // CSV list headers
  // ----------------
  CL.AddConstantN('krsCSVStatsHeader', 'String').SetString(krsCSVStatsHeader);
  CL.AddConstantN('krsCSVSoftHeader', 'String').SetString(krsCSVSoftHeader);
  CL.AddConstantN('krsCSVSoftStatsHeader', 'String').SetString(
    krsCSVSoftStatsHeader);
  CL.AddConstantN('krsCSVGroupHeader', 'String').SetString(krsCSVGroupHeader);
  CL.AddConstantN('krsCSVGroupStatsHeader', 'String').SetString(
    krsCSVGroupStatsHeader);

  // IniKeys
  // -------
  // Shared
  CL.AddConstantN('krsIniKeyID', 'String').SetString(krsIniKeyID);
  CL.AddConstantN('krsIniKeyTitle', 'String').SetString(krsIniKeyTitle);
  CL.AddConstantN('krsIniKeySortTitle', 'String').SetString(
    krsIniKeySortTitle);
  CL.AddConstantN('krsIniKeyFileName', 'String').SetString(krsIniKeyFileName);
  CL.AddConstantN('krsIniKeyYear', 'String').SetString(krsIniKeyYear);
  CL.AddConstantN('krsIniKeyEnabled', 'String').SetString(krsIniKeyEnabled);
  CL.AddConstantN('krsIniKeyWorkingFolder', 'String').SetString(
    krsIniKeyWorkingFolder);
  CL.AddConstantN('krsIniKeyIcon', 'String').SetString(krsIniKeyIcon);
  CL.AddConstantN('krsIniKeyImage', 'String').SetString(krsIniKeyImage);
  CL.AddConstantN('krsIniKeyDeveloper', 'String').SetString(
    krsIniKeyDeveloper);
  CL.AddConstantN('krsIniKeyExtensions', 'String').SetString(
    krsIniKeyExtensions);

  // System
  // Shared Keys: krsIniKeyID, krsIniKeyTitle, krsIniKeyFileName,
  //   krsIniKeyWorkingFolder, krsIniKeyIcon, krsIniKeyImage,
  //   krsIniKeyExtensions
  CL.AddConstantN('krsIniKeyBaseFolder', 'String').SetString(
    krsIniKeyBaseFolder);
  CL.AddConstantN('krsIniKeyMainEmulator', 'String').SetString(
    krsIniKeyMainEmulator);
  CL.AddConstantN('krsIniKeyOtherEmulators', 'String').SetString(
    krsIniKeyOtherEmulators);
  CL.AddConstantN('krsIniKeyBackImage', 'String').SetString(
    krsIniKeyBackImage);
  CL.AddConstantN('krsIniKeyIconFolder', 'String').SetString(
    krsIniKeyIconFolder);
  CL.AddConstantN('krsIniKeyImageFolders', 'String').SetString(
    krsIniKeyImageFolders);
  CL.AddConstantN('krsIniKeyImageCaptions', 'String').SetString(
    krsIniKeyImageCaptions);
  CL.AddConstantN('krsIniKeyText', 'String').SetString(krsIniKeyText);
  CL.AddConstantN('krsIniKeyTextFolders', 'String').SetString(
    krsIniKeyTextFolders);
  CL.AddConstantN('krsIniKeyTextCaptions', 'String').SetString(
    krsIniKeyTextCaptions);
  CL.AddConstantN('krsIniKeyMusicFolders', 'String').SetString(
    krsIniKeyMusicFolders);
  CL.AddConstantN('krsIniKeyMusicCaptions', 'String').SetString(
    krsIniKeyMusicCaptions);
  CL.AddConstantN('krsIniKeyVideoFolders', 'String').SetString(
    krsIniKeyVideoFolders);
  CL.AddConstantN('krsIniKeyVideoCaptions', 'String').SetString(
    krsIniKeyVideoCaptions);
  CL.AddConstantN('krsIniKeySoftExportKey', 'String').SetString(
    krsIniKeySoftExportKey);
  CL.AddConstantN('krsIniKeyExtractAll', 'String').SetString(
    krsIniKeyExtractAll);

  // Group
  // Shared Keys: krsIniKeyID, krsIniKeyTitle, krsIniKeySortTitle,
  //   krsIniKeyYear, krsIniKeyFileName, krsIniKeyDeveloper


  // Soft
  // Shared Keys: krsIniKeyID, krsIniKeyTitle, krsIniKeySortTitle,
  //   krsIniKeyYear, krsIniKeyFileName
  CL.AddConstantN('krsIniKeySHA1', 'String').SetString(krsIniKeySHA1);
  CL.AddConstantN('krsIniKeyGroup', 'String').SetString(krsIniKeyGroup);
  CL.AddConstantN('krsIniKeyTranslitTitle', 'String').SetString(
    krsIniKeyTranslitTitle);
  CL.AddConstantN('krsIniKeyVersion', 'String').SetString(krsIniKeyVersion);
  CL.AddConstantN('krsIniKeyPublisher', 'String').SetString(
    krsIniKeyPublisher);
  CL.AddConstantN('krsIniKeyZone', 'String').SetString(krsIniKeyZone);
  CL.AddConstantN('krsIniKeyDumpInfo', 'String').SetString(krsIniKeyDumpInfo);
  CL.AddConstantN('krsIniKeyDumpStatus', 'String').SetString(
    krsIniKeyDumpStatus);
  CL.AddConstantN('krsIniKeyFixed', 'String').SetString(krsIniKeyFixed);
  CL.AddConstantN('krsIniKeyTrainer', 'String').SetString(krsIniKeyTrainer);
  CL.AddConstantN('krsIniKeyTranslation', 'String').SetString(
    krsIniKeyTranslation);
  CL.AddConstantN('krsIniKeyPirate', 'String').SetString(krsIniKeyPirate);
  CL.AddConstantN('krsIniKeyCracked', 'String').SetString(krsIniKeyCracked);
  CL.AddConstantN('krsIniKeyModified', 'String').SetString(krsIniKeyModified);
  CL.AddConstantN('krsIniKeyHack', 'String').SetString(krsIniKeyHack);
  CL.AddConstantN('krsIniKeyFolder', 'String').SetString(krsIniKeyFolder);

  // Emulator
  // Shared Keys: krsIniKeyEnabled, krsIniKeyTitle, krsIniKeyWorkingFolder,
  //   krsIniKeyIcon, krsIniKeyImage, krsIniKeyDeveloper, krsIniKeyExtensions
  CL.AddConstantN('krsIniKeyParameters', 'String').SetString(
    krsIniKeyParameters);
  CL.AddConstantN('krsIniKeyExtraParamFmt', 'String').SetString(
    krsIniKeyExtraParamFmt);
  CL.AddConstantN('krsIniKeyExitCode', 'String').SetString(krsIniKeyExitCode);
  CL.AddConstantN('krsIniKeyExeFile', 'String').SetString(krsIniKeyExeFile);
  CL.AddConstantN('krsIniKeyWebPage', 'String').SetString(krsIniKeyWebPage);
  CL.AddConstantN('krsIniKeyInfoFile', 'String').SetString(krsIniKeyInfoFile);

  // Playing Stats
  CL.AddConstantN('krsIniKeyPlayingTime', 'String').SetString(
    krsIniKeyPlayingTime);
  CL.AddConstantN('krsIniKeyTimesPlayed', 'String').SetString(
    krsIniKeyTimesPlayed);
  CL.AddConstantN('krsIniKeyLastTime', 'String').SetString(krsIniKeyLastTime);

  // Enumerated, sets, etc.
  // ----------------------
  // Constants for krsIniKeySoftExportKey
  CL.AddConstantN('krsSEKCRC32', 'String').SetString(krsSEKCRC32);
  CL.AddConstantN('krsSEKSHA1', 'String').SetString(krsSEKSHA1);
  CL.AddConstantN('krsSEKFileName', 'String').SetString(krsSEKFileName);
  CL.AddConstantN('krsSEKCustom', 'String').SetString(krsSEKCustom);

  // Constant for DumpStatus, fixed (for example, icon filenames)
  CL.AddConstantN('krsEDSFavorite', 'String').SetString(krsEDSFavorite);
  CL.AddConstantN('krsEDSGood', 'String').SetString(krsEDSGood);
//  CL.AddConstantN('krsEDSAlternate', 'String').SetString(krsEDSAlternate);
  CL.AddConstantN('krsEDSOverDump', 'String').SetString(krsEDSOverDump);
  CL.AddConstantN('krsEDSBadDump', 'String').SetString(krsEDSBadDump);
  CL.AddConstantN('krsEDSUnderDump', 'String').SetString(krsEDSUnderDump);
  CL.AddConstantN('krsEDSUnknown', 'String').SetString(krsEDSUnknown);
  CL.AddConstantN('krsEDSKeepValue', 'String').SetString(krsEDSKeepValue);

  // Constant for DumpStatus, fixed (for databases)
  CL.AddConstantN('krsEDSFavoriteKey', 'String').SetString(krsEDSFavoriteKey);
  CL.AddConstantN('krsEDSGoodKey', 'String').SetString(krsEDSGoodKey);
//  CL.AddConstantN('krsEDSAlternateKey', 'String').SetString(
//    krsEDSAlternateKey);
  CL.AddConstantN('krsEDSOverDumpKey', 'String').SetString(krsEDSOverDumpKey);
  CL.AddConstantN('krsEDSBadDumpKey', 'String').SetString(krsEDSBadDumpKey);
  CL.AddConstantN('krsEDSUnderDumpKey', 'String').SetString(
    krsEDSUnderDumpKey);
  CL.AddConstantN('krsEDSUnknownKey', 'String').SetString(krsEDSUnknownKey);

  // Key when importing to keep current value
  CL.AddConstantN('krsImportKeepValueKey', 'String').SetString(
    krsImportKeepValueKey);

  // Internal folders
  CL.AddConstantN('krsTemp7zCacheFolder', 'String').SetString(
    krsTemp7zCacheFolder);

  // TYPES
  // =====
  CL.AddTypeS('TEmutecaSoftExportKey',
    '(TEFKSHA1, TEFKCRC32, TEFKFileName, TEFKCustom)');
  CL.AddTypeS('TEmutecaDumpStatus',
    '(edsFavorite, edsGood, edsOverDump, edsBadDump, edsUnderDump, edsUnknown, edsKeepValue)');

  CL.AddTypeS('TEmutecaProgressCallBack',
    'function(const Title, Info1, Info2: string; const Value, MaxValue: int64) : boolean');


  {
  const
    EmutecaSoftExportKeyStrK: array [TEmutecaSoftExportKey] of string =
      (krsSEKSHA1, krsSEKCRC32, krsSEKFileName, krsSEKCustom);
    //< Strings for FileKeys (fixed constants, used for ini files, etc. )

    EmutecaDumpStatusKey: array [TEmutecaDumpStatus] of string =
      (krsEDSFavoriteKey, krsEDSGoodKey, krsEDSOverDumpKey,
      krsEDSBadDumpKey, krsEDSUnderDumpKey, krsEDSUnknownKey, krsImportKeepValueKey);
    //< Keys for DumpStatus, used in IniFiles
    EmutecaDumpStatusStrK: array [TEmutecaDumpStatus] of string =
      (krsEDSFavorite, krsEDSGood, krsEDSOverDump,
      krsEDSBadDump, krsEDSUnderDump, krsEDSUnknown, krsEDSKeepValue);
  //< Strings for DumpStatus (fixed constants, used for icon filenames, etc. )
  }
end;

procedure RIRegister_uEmutecaConst_Routines(S: TPSExec);
begin

end;

end.
