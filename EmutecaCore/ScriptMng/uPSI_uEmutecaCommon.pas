unit uPSI_uEmutecaCommon;

interface

uses
  SysUtils, Classes, uPSComponent, uPSRuntime, uPSCompiler,
  uEmutecaCommon;

procedure SIRegister_uEmutecaCommon(CL: TPSPascalCompiler);
procedure RIRegister_uEmutecaCommon_Routines(S: TPSExec);

implementation

procedure SIRegister_uEmutecaCommon(CL: TPSPascalCompiler);
begin
  // CONSTANTS
  // =========
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
  CL.AddConstantN('kErrorRunSoftUnknown', 'LongInt').SetInt(kErrorRunSoftUnknown);
  CL.AddConstantN('kErrorRunSoftNoSoft', 'LongInt').SetInt(kErrorRunSoftNoSoft);
  CL.AddConstantN('kErrorRunSoftNoEmu', 'LongInt').SetInt(kErrorRunSoftNoEmu);
  CL.AddConstantN('kErrorRunSoftNoSoftFile', 'LongInt').SetInt(kErrorRunSoftNoSoftFile);
  CL.AddConstantN('kErrorRunSoftNoEmuFile', 'LongInt').SetInt(kErrorRunSoftNoEmuFile);
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
  CL.AddConstantN('krsIniKeyParameters', 'String').SetString(krsIniKeyParameters);
  CL.AddConstantN('krsIniKeyExtraParamFmt', 'String').SetString(krsIniKeyExtraParamFmt);
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
  CL.AddConstantN('krsEDSVerified', 'String').SetString(krsEDSVerified);
  CL.AddConstantN('krsEDSGood', 'String').SetString(krsEDSGood);
  CL.AddConstantN('krsEDSAlternate', 'String').SetString(krsEDSAlternate);
  CL.AddConstantN('krsEDSOverDump', 'String').SetString(krsEDSOverDump);
  CL.AddConstantN('krsEDSBadDump', 'String').SetString(krsEDSBadDump);
  CL.AddConstantN('krsEDSUnderDump', 'String').SetString(krsEDSUnderDump);
  CL.AddConstantN('krsEDSUnknown', 'String').SetString(krsEDSUnknown);
  CL.AddConstantN('krsEDSKeepValue', 'String').SetString(krsEDSKeepValue);

    // Constant for DumpStatus, fixed (for databases)
  CL.AddConstantN('krsEDSVerifiedKey', 'String').SetString(krsEDSVerifiedKey);
  CL.AddConstantN('krsEDSGoodKey', 'String').SetString(krsEDSGoodKey);
  CL.AddConstantN('krsEDSAlternateKey', 'String').SetString(krsEDSAlternateKey);
  CL.AddConstantN('krsEDSOverDumpKey', 'String').SetString(krsEDSOverDumpKey);
  CL.AddConstantN('krsEDSBadDumpKey', 'String').SetString(krsEDSBadDumpKey);
  CL.AddConstantN('krsEDSUnderDumpKey', 'String').SetString(krsEDSUnderDumpKey);
  CL.AddConstantN('krsEDSUnknownKey', 'String').SetString(krsEDSUnknownKey);

  // Key when importing to keep current value
  CL.AddConstantN('krsImportKeepValueKey', 'String').SetString(krsImportKeepValueKey);

  // Internal folders
  CL.AddConstantN('krsTemp7zCacheFolder', 'String').SetString(krsTemp7zCacheFolder);

  // RESOURCE STRINGS
  // ================

  // Misc
  // ----
  CL.AddConstantN('rsNever', 'String').SetString(rsNever);
  CL.AddConstantN('rsFileAlreadyAdded', 'String').SetString(
    rsFileAlreadyAdded);
  CL.AddConstantN('rsCleaningSystemData', 'String').SetString(
    rsCleaningSystemData);

  // Lists
  // -----
  CL.AddConstantN('rsLoadingSystemList', 'String').SetString(
    rsLoadingSystemList);
  CL.AddConstantN('rsSavingSystemList', 'String').SetString(
    rsSavingSystemList);
  CL.AddConstantN('rsImportingSystemList', 'String').SetString(
    rsImportingSystemList);
  CL.AddConstantN('rsExportingSystemList', 'String').SetString(
    rsExportingSystemList);
  CL.AddConstantN('rsLoadingGroupList', 'String').SetString(
    rsLoadingGroupList);
  CL.AddConstantN('rsSavingGroupList', 'String').SetString(rsSavingGroupList);
  CL.AddConstantN('rsImportingGroupList', 'String').SetString(
    rsImportingGroupList);
  CL.AddConstantN('rsExportingGroupList', 'String').SetString(
    rsExportingGroupList);
  CL.AddConstantN('rsLoadingSoftList', 'String').SetString(rsLoadingSoftList);
  CL.AddConstantN('rsSavingSoftList', 'String').SetString(rsSavingSoftList);
  CL.AddConstantN('rsImportingSoftList', 'String').SetString(
    rsImportingSoftList);
  CL.AddConstantN('rsExportingSoftList', 'String').SetString(
    rsExportingSoftList);
  CL.AddConstantN('rsLoadingEmulatorList', 'String').SetString(rsLoadingEmulatorList);
  CL.AddConstantN('rsSavingEmulatorList', 'String').SetString(rsSavingEmulatorList);
  CL.AddConstantN('rsImportingEmulatorList', 'String').SetString(rsImportingEmulatorList);
  CL.AddConstantN('rsExportingEmulatorList', 'String').SetString(
    rsExportingEmulatorList);

  // Importing/Exporting Warnings
  CL.AddConstantN('rsImportingNoSHA1', 'String').SetString(rsImportingNoSHA1);
  CL.AddConstantN('rsExportingNoSHA1', 'String').SetString(rsExportingNoSHA1);

  // File mask descriptions
  // ----------------------
  CL.AddConstantN('rsFileMaskDescGroup', 'String').SetString(
    rsFileMaskDescGroup);
  CL.AddConstantN('rsFileMaskDescSoft', 'String').SetString(
    rsFileMaskDescSoft);
  CL.AddConstantN('rsFileMaskDescINI', 'String').SetString(rsFileMaskDescINI);
  CL.AddConstantN('rsFileMaskDescScript', 'String').SetString(
    rsFileMaskDescScript);
  CL.AddConstantN('rsFileMaskDescTXT', 'String').SetString(rsFileMaskDescTXT);

  // Strings for DumpStatus, translatable
  // ------------------------------------
  CL.AddConstantN('rsEDSVerified', 'String').SetString(rsEDSVerified);
  CL.AddConstantN('rsEDSGood', 'String').SetString(rsEDSGood);
  CL.AddConstantN('rsEDSAlternate', 'String').SetString(rsEDSAlternate);
  CL.AddConstantN('rsEDSOverDump', 'String').SetString(rsEDSOverDump);
  CL.AddConstantN('rsEDSBadDump', 'String').SetString(rsEDSBadDump);
  CL.AddConstantN('rsEDSUnderDump', 'String').SetString(rsEDSUnderDump);
  CL.AddConstantN('rsEDSUnknown', 'String').SetString(rsEDSUnknown);
  CL.AddConstantN('rsEDSKeepValue', 'String').SetString(rsEDSKeepValue);

  // TYPES
  // =====
  CL.AddTypeS('TEmutecaSoftExportKey',
    '(TEFKSHA1, TEFKCRC32, TEFKFileName, TEFKCustom)');
  CL.AddTypeS('TEmutecaDumpStatus',
    '(edsVerified, edsGood, edsAlternate, edsOverDump, edsBadDump, edsUnderDump, edsUnknown, edsKeepValue)');

  CL.AddTypeS('TEmutecaProgressCallBack',
    'function(const Title, Info1, Info2: string; const Value, MaxValue: int64) : boolean');

  {
  const
    EmutecaSoftExportKeyStrK: array [TEmutecaSoftExportKey] of string =
      (krsSEKSHA1, krsSEKCRC32, krsSEKFileName, krsSEKCustom);
    //< Strings for FileKeys (fixed constants, used for ini files, etc. )

    EmutecaDumpStatusKey: array [TEmutecaDumpStatus] of string =
      (krsEDSVerifiedKey, krsEDSGoodKey, krsEDSAlternateKey, krsEDSOverDumpKey,
      krsEDSBadDumpKey, krsEDSUnderDumpKey, krsEDSUnknownKey, krsImportKeepValueKey);
    //< Keys for DumpStatus, used in IniFiles
    EmutecaDumpStatusStrK: array [TEmutecaDumpStatus] of string =
      (krsEDSVerified, krsEDSGood, krsEDSAlternate, krsEDSOverDump,
      krsEDSBadDump, krsEDSUnderDump, krsEDSUnknown, krsEDSKeepValue);
  //< Strings for DumpStatus (fixed constants, used for icon filenames, etc. )
    EmutecaDumpStatusStr: array [TEmutecaDumpStatus] of string =
      (rsEDSVerified, rsEDSGood, rsEDSAlternate, rsEDSOverDump,
      rsEDSBadDump, rsEDSUnderDump, rsEDSUnknown, rsEDSKeepValue);
    //< Strings for DumpStatus (localizable)
  }


  // FUNCTION / PROCEDURES
  // =====================

  CL.AddDelphiFunction(
    'function Str2SoftExportKey(aString: string): TEmutecaSoftExportKey');
  CL.AddDelphiFunction(
    'function SoftExportKey2StrK(aSOK: TEmutecaSoftExportKey): string');

  CL.AddDelphiFunction(
    'function Key2DumpSt(aString: string): TEmutecaDumpStatus');
  CL.AddDelphiFunction(
    'function DumpSt2Key(aEDS: TEmutecaDumpStatus): string');
  CL.AddDelphiFunction(
    'function DumpSt2Str(aEDS: TEmutecaDumpStatus): string');
  CL.AddDelphiFunction(
    'function DumpSt2StrK(aEDS: TEmutecaDumpStatus): string');

  CL.AddDelphiFunction(
    'procedure EmuTKSearchAllRelatedFiles(OutFileList: TStrings; ' +
    'aFolder: string; aFileName: string; Extensions: TStrings; ' +
    'SearchInComp: boolean; DecompressFolder: string)');
  CL.AddDelphiFunction(
    'function EmuTKSearchFirstRelatedFile(aFolder: string; ' +
    'aFileName: string; Extensions: TStrings; SearchInComp: boolean; ' +
    'AutoDecompress: boolean; DecompressFolder: string): string');
  CL.AddDelphiFunction(
    'procedure EmuTKSearchAllFilesByNameExtCT(aFileList: TStrings; ' +
    'aBaseFileName: string; aExtList: string)');
  CL.AddDelphiFunction(
    'procedure EmuTKSearchAllFilesByNameExtSL(aFileList: TStrings; ' +
    'aBaseFileName: string; aExtList: TStrings)');
  CL.AddDelphiFunction(
    'function EmuTKSearchFirstFileByNameExtCT(aBaseFileName: string; ' +
    'aExtList: string): string');
  CL.AddDelphiFunction(
    'function EmuTKSearchFirstFileByNameExtSL(aBaseFileName: string; ' +
    'aExtList: TStrings): string');
end;

(* === run-time registration functions === *)
(*----------------------------------------------------------------------------*)
procedure RIRegister_uEmutecaCommon_Routines(S: TPSExec);
begin
  S.RegisterDelphiFunction(@Str2SoftExportKey, 'Str2SoftExportKey', cdRegister);
  S.RegisterDelphiFunction(@SoftExportKey2StrK, 'SoftExportKey2StrK', cdRegister);
  S.RegisterDelphiFunction(@Key2DumpSt, 'Key2DumpSt', cdRegister);
  S.RegisterDelphiFunction(@DumpSt2Key, 'DumpSt2Key', cdRegister);
  S.RegisterDelphiFunction(@DumpSt2Str, 'DumpSt2Str', cdRegister);
  S.RegisterDelphiFunction(@DumpSt2StrK, 'DumpSt2StrK', cdRegister);
  S.RegisterDelphiFunction(@EmuTKSearchAllRelatedFiles,
    'EmuTKSearchAllRelatedFiles', cdRegister);
  S.RegisterDelphiFunction(@EmuTKSearchFirstRelatedFile,
    'EmuTKSearchFirstRelatedFile', cdRegister);
  S.RegisterDelphiFunction(@EmuTKSearchAllFilesByNameExtCT,
    'EmuTKSearchAllFilesByNameExtCT', cdRegister);
  S.RegisterDelphiFunction(@EmuTKSearchAllFilesByNameExtSL,
    'EmuTKSearchAllFilesByNameExtSL', cdRegister);
  S.RegisterDelphiFunction(@EmuTKSearchFirstFileByNameExtCT,
    'EmuTKSearchFirstFileByNameExtCT', cdRegister);
  S.RegisterDelphiFunction(@EmuTKSearchFirstFileByNameExtSL,
    'EmuTKSearchFirstFileByNameExtSL', cdRegister);
end;

end.
