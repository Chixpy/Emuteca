{ Commons unit of Emuteca.

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
unit uEmutecaCommon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazFileUtils, LazUTF8,
  // CHX units
  uCHX7zWrapper, uCHXStrUtils, uCHXFileUtils;

const
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
  kErrorRunSoftNoSoft = -301;
  {< Run Software: Error code when soft = nil. }
  kErrorRunSoftNoEmu = -302;
  {< Run Software: Error code when Emulator = nil. }
  kErrorRunSoftNoSoftFile = -303;
  {< Run Software: Error code when soft file is not found. }
  kErrorRunSoftNoEmuFile = -305;
  {< Run Software: Error code when emulator exe is not found. }
  kError7zDecompress = -400;
  {< Base error const decompressing archive. }

  // CSV list headers
  // ----------------
  krsCSVStatsHeader = '"Last Time","Times Played","Playing Time"';

  krsCSVSoftHeader = '"Group","SHA1","ID","Folder","FileName",' +
    '"Title","Transliterated Name","Sort Title","Version","Year","Publisher",'
    +
    '"Zone","DumpStatus","Dump Info","Fixed","Trainer","Translation",' +
    '"Pirate","Cracked","Modified","Hack","Extra Parameters"';
  krsCSVSoftStatsHeader = krsCSVSoftHeader + ',' + krsCSVStatsHeader;
  krsCSVGroupHeader = '"ID","Title","Sort Title","Year","Developer",' +
    '"Media file"';
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
  krsIniKeyBaseFolder = 'BaseFolder';
  krsIniKeyMainEmulator = 'MainEmulator';
  krsIniKeyOtherEmulators = 'OtherEmulators';
  krsIniKeyBackImage = 'BackImage';
  krsIniKeySoftIcon = 'SoftIcon';
  krsIniKeyIconFolder = 'IconFolder';
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
  krsEDSVerified = 'Verified';
  krsEDSGood = 'GoodDump';
  krsEDSAlternate = 'Alternate';
  krsEDSOverDump = 'OverDump';
  krsEDSBadDump = 'BadDump';
  krsEDSUnderDump = 'UnderDump';
  krsEDSUnknown = 'Unknown';
  krsEDSKeepValue = 'KeepValue';

  // Constant for DumpStatus, fixed (for databases)
  krsEDSVerifiedKey = '!';
  krsEDSGoodKey = '';
  krsEDSAlternateKey = 'a';
  krsEDSOverDumpKey = 'o';
  krsEDSBadDumpKey = 'b';
  krsEDSUnderDumpKey = 'u';
  krsEDSUnknownKey = '?';

  // Key for import file to keep current value
  krsImportKeepValueKey = '@';

  // Dirs
  krsTemp7zCacheFolder = 'SHA1Cache/';

resourcestring

  // Misc
  rsNever = 'Never';
  rsFileAlreadyAdded = 'This file is already added.';
  rsCleaningSystemData = 'Cleaning system data...';

  // List action
  rsLoadingSystemList = 'Loading system list...';
  rsSavingSystemList = 'Saving system list...';
  rsImportingSystemList = 'Importing system list...';
  rsExportingSystemList = 'Exporting system list...';
  rsLoadingGroupList = 'Loading group list...';
  rsSavingGroupList = 'Saving group list...';
  rsImportingGroupList = 'Importing group list...';
  rsExportingGroupList = 'Exporting group list...';
  rsLoadingSoftList = 'Loading soft list...';
  rsSavingSoftList = 'Saving soft list...';
  rsImportingSoftList = 'Importing soft list...';
  rsExportingSoftList = 'Exporting soft list...';
  rsLoadingEmulatorList = 'Loading emulator list...';
  rsSavingEmulatorList = 'Saving emulator list...';
  rsImportingEmulatorList = 'Importing emulator list...';
  rsExportingEmulatorList = 'Exporting emulator list...';

  // Importing/Exporting Warnings
  rsImportingNoSHA1 =
    'Warning: Some info could not be imported because some files haven''t got SHA1 cached.'
    + LineEnding + '(%2:d/%3:d) %0:s%1:s';
  rsExportingNoSHA1 =
    'Warning: We can''t export because not all files have SHA1 cached.' +
    LineEnding + '(%2:d/%3:d) %0:s%1:s';

  // File mask descriptions
  // ----------------------
  rsFileMaskDescGroup =
    'Group file list (' + krsFileMaskGroup + ')';
  {< Description of file mask for group lists. }
  rsFileMaskDescSoft =
    'Soft file list (' + krsFileMaskSoft + ')';
  {< Description of file mask for soft lists. }
  rsFileMaskDescINI = 'Soft file list (' + krsFileMaskINI + ')';
  {< Description of file mask for ini databases (Systems, Emulators, Export/Import, ...). }
  rsFileMaskDescScript =
    'Soft file list (' + krsFileMaskScript + ')';
  {< Description of file mask for script files. }
  rsFileMaskDescTXT = 'Soft file list (' + krsFileMaskTXT + ')';
  {< Description of file mask for generic text files. }

  // Strings for DumpStatus, translatable
  // ------------------------------------
  rsEDSVerified = 'Verified';
  rsEDSGood = 'GoodDump';
  rsEDSAlternate = 'Alternate';
  rsEDSOverDump = 'OverDump';
  rsEDSBadDump = 'BadDump';
  rsEDSUnderDump = 'UnderDump';
  rsEDSUnknown = 'Unknown';
  rsEDSKeepValue = 'Keep value'; // Only for imports


  // Example frames text
  // -------------------

  rsUnknown = 'Unknown';

  // Formated statistics
  rsFmtNGroups = '%0:d groups';
  rsFmtNVersions = '%0:d versions.';
  rsFmtNItems = '%1:d visible of %0:d items.';
  {<
     %0:d = Number of items.
     %1:d = Number of visible items.
  }

  rsFmtNTimes = '%0:d times.';
  {< %0:d = Number of times. }


type
  TEmutecaSoftExportKey = (TEFKSHA1, TEFKCRC32, TEFKFileName, TEFKCustom);
  TEmutecaDumpStatus = (edsVerified, edsGood, edsAlternate, edsOverDump,
    edsBadDump, edsUnderDump, edsUnknown, edsKeepValue);

  TEmutecaProgressCallBack = function(const aAction, aInfo: string;
    const aValue, aMaxValue: int64; const IsCancelable: boolean): boolean of
    object;
{< Callback funtion to show progress }

const
  EmutecaSoftExportKeyStrK: array [TEmutecaSoftExportKey] of string =
    (krsSEKSHA1, krsSEKCRC32, krsSEKFileName, krsSEKCustom);
  //< Strings for FileKeys (fixed constants, used for ini files, etc. )

  EmutecaDumpStatusKey: array [TEmutecaDumpStatus] of string =
    (krsEDSVerifiedKey, krsEDSGoodKey, krsEDSAlternateKey, krsEDSOverDumpKey,
    krsEDSBadDumpKey, krsEDSUnderDumpKey, krsEDSUnknownKey,
    krsImportKeepValueKey);
  //< Keys for DumpStatus, used in IniFiles
  EmutecaDumpStatusStrK: array [TEmutecaDumpStatus] of string =
    (krsEDSVerified, krsEDSGood, krsEDSAlternate, krsEDSOverDump,
    krsEDSBadDump, krsEDSUnderDump, krsEDSUnknown, krsEDSKeepValue);
  //< Strings for DumpStatus (fixed constants, used for icon filenames, etc. )
  EmutecaDumpStatusStr: array [TEmutecaDumpStatus] of string =
    (rsEDSVerified, rsEDSGood, rsEDSAlternate, rsEDSOverDump,
    rsEDSBadDump, rsEDSUnderDump, rsEDSUnknown, rsEDSKeepValue);
  //< Strings for DumpStatus (localizable)


function Str2SoftExportKey(aString: string): TEmutecaSoftExportKey;
function SoftExportKey2StrK(aSOK: TEmutecaSoftExportKey): string;

function Key2DumpSt(aString: string): TEmutecaDumpStatus;
function DumpSt2Key(aEDS: TEmutecaDumpStatus): string;
function DumpSt2Str(aEDS: TEmutecaDumpStatus): string;
function DumpSt2StrK(aEDS: TEmutecaDumpStatus): string;

procedure EmuTKSearchAllRelatedFiles(OutFileList: TStrings;
  aFolder: string; aFileName: string; Extensions: TStrings;
  SearchInComp: boolean; AutoDecompress: boolean; DecompressFolder: string);

function EmuTKSearchFirstRelatedFile(aFolder: string;
  aFileName: string; Extensions: TStrings; SearchInComp: boolean;
  AutoDecompress: boolean; DecompressFolder: string): string;

procedure EmuTKSearchAllFilesByNameExtCT(aFileList: TStrings;
  aBaseFileName: string; aExtList: string);
procedure EmuTKSearchAllFilesByNameExtSL(aFileList: TStrings;
  aBaseFileName: string; aExtList: TStrings);

function EmuTKSearchFirstFileByNameExtCT(aBaseFileName: string;
  aExtList: string): string;
function EmuTKSearchFirstFileByNameExtSL(aBaseFileName: string;
  aExtList: TStrings): string;

implementation

procedure EmuTKSearchAllFilesByNameExtCT(aFileList: TStrings;
  aBaseFileName: string; aExtList: string);
var
  aTempSL: TStringList;
begin
  aTempSL := TStringList.Create;
  try
    aTempSL.CommaText := aExtList;
    EmuTKSearchAllFilesByNameExtSL(aFileList, aBaseFileName, aTempSL);
  finally
    aTempSL.Free;
  end;
end;

procedure EmuTKSearchAllFilesByNameExtSL(aFileList: TStrings;
  aBaseFileName: string; aExtList: TStrings);
var
  i: integer;
  aFile: string;
begin
  i := 0;
  while i < aExtList.Count do
  begin
    aFile := aExtList[i];
    if (aFile <> '') and (aFile[1] <> ExtensionSeparator) then
      aFile := ExtensionSeparator + aFile;
    aFile := aBaseFileName + aFile;
    if FileExistsUTF8(aFile) then
      aFileList.Add(aFile);
    Inc(i);
  end;
end;

function EmuTKSearchFirstFileByNameExtCT(aBaseFileName: string;
  aExtList: string): string;
var
  aTempSL: TStringList;
begin
  aTempSL := TStringList.Create;
  try
    aTempSL.CommaText := aExtList;
    Result := EmuTKSearchFirstFileByNameExtSL(aBaseFileName, aTempSL);
  finally
    aTempSL.Free;
  end;
end;

function EmuTKSearchFirstFileByNameExtSL(aBaseFileName: string;
  aExtList: TStrings): string;
var
  i: integer;
  aFile: string;
begin
  Result := '';
  i := 0;
  while (Result = '') and (i < aExtList.Count) do
  begin
    aFile := aExtList[i];
    if (aFile <> '') and (aFile[1] <> ExtensionSeparator) then
      aFile := ExtensionSeparator + aFile;
    aFile := aBaseFileName + aFile;
    if FileExistsUTF8(aFile) then
      Result := aFile;
    Inc(i);
  end;
end;

function Str2SoftExportKey(aString: string): TEmutecaSoftExportKey;
begin
  // In Emuteca <= 0.7, True => CRC32 / False => FileName
  aString := UTF8UpperCase(aString);

  // I don't like this "else if" format but it's clearer...
  if (aString = UTF8UpperCase(krsSEKCRC32)) or
    (StrToBoolDef(aString, False)) then
    Result := TEFKCRC32
  else if (aString = UTF8UpperCase(krsSEKFileName)) or
    (not StrToBoolDef(aString, True)) then
    Result := TEFKFileName
  else if (aString = UTF8UpperCase(krsSEKSHA1)) then
    Result := TEFKSHA1
  else if (aString = UTF8UpperCase(krsSEKCustom)) then
    Result := TEFKCustom
  else // Default
    Result := TEFKSHA1;
end;

function SoftExportKey2StrK(aSOK: TEmutecaSoftExportKey): string;
begin
  Result := EmutecaSoftExportKeyStrK[aSOK];
end;

function Key2DumpSt(aString: string): TEmutecaDumpStatus;
begin
  aString := UTF8Trim(UTF8LowerString(aString));

  if (aString = DumpSt2Key(edsGood)) then // krsedsGoodKey = ''
    Result := edsGood
  else if (aString[1] = DumpSt2Key(edsVerified)) then
    Result := edsVerified
  else if (aString[1] = DumpSt2Key(edsAlternate)) then
    Result := edsAlternate
  else if (aString[1] = DumpSt2Key(edsOverDump)) then
    Result := edsOverDump
  else if (aString[1] = DumpSt2Key(edsBadDump)) then
    Result := edsBadDump
  else if (aString[1] = DumpSt2Key(edsUnderDump)) then
    Result := edsUnderDump
  else if (aString[1] = DumpSt2Key(edsUnknown)) then
    Result := edsUnknown
  else if (aString[1] = DumpSt2Key(edsKeepValue)) then
    Result := edsKeepValue
  else
    Result := edsUnknown;
end;

function DumpSt2Key(aEDS: TEmutecaDumpStatus): string;
begin
  Result := EmutecaDumpStatusKey[aEDS];
end;

function DumpSt2Str(aEDS: TEmutecaDumpStatus): string;
begin
  Result := EmutecaDumpStatusStr[aEDS];
end;

function DumpSt2StrK(aEDS: TEmutecaDumpStatus): string;
begin
  Result := EmutecaDumpStatusStrK[aEDS];
end;

procedure EmuTKSearchAllRelatedFiles(OutFileList: TStrings;
  aFolder: string; aFileName: string; Extensions: TStrings;
  SearchInComp: boolean; AutoDecompress: boolean; DecompressFolder: string);
var
  CompressedArchives: TStringList;
  i: integer;
begin
  aFolder := SetAsFolder(aFolder);
  DecompressFolder := SetAsFolder(DecompressFolder);

  if (aFileName = '') or (aFolder = '') or
    (not DirectoryExistsUTF8(aFolder)) or (Extensions = nil) or
    (Extensions.Count = 0) then
    Exit;

  if not assigned(OutFileList) then
    OutFileList := TStringList.Create;

  // 1. Basic search
  // Folder/aFileName.mext
  EmuTKSearchAllFilesByNameExtSL(OutFileList, aFolder + aFileName, Extensions);

  // 2. Search in folder
  // Folder/aFileName/[*]/*.mext
  FindAllFiles(OutFileList, aFolder + SetAsFolder(aFileName),
    FileMaskFromStringList(Extensions), True);

  if not SearchInComp then
    Exit; // If we don't want to search in CompArchives then Exit.


  if AutoDecompress then
  begin

  if DecompressFolder = '' then
    Exit;
  // 3. Search in zip files
  //   Folder/aFileName.zip/*.mext
  //   Extract to DecompressFolder/LastSubFolder(Folder)/aFileName/*.mext)
  DecompressFolder := SetAsFolder(DecompressFolder) +
    SetAsFolder(ExtractFileName(ExcludeTrailingPathDelimiter(aFolder))) +
    SetAsFolder(aFileName);

  // 3.a. If not DecompressFolder exists, then search Folder/aFileName.zip/*.mext
  //   and extract to DecompressFolder
  if not DirectoryExistsUTF8(DecompressFolder) then
  begin
    CompressedArchives := TStringList.Create;
    try
      EmuTKSearchAllFilesByNameExtCT(CompressedArchives, aFolder + aFileName,
        w7zGetFileExts);

      i := 0;
      while i < CompressedArchives.Count do
      begin
        w7zExtractFile(CompressedArchives[i], AllFilesMask, DecompressFolder,
          False, '');
        Inc(i);
      end;
    finally
      FreeAndNil(CompressedArchives);
    end;
  end;

  // 3.b. Actually searching in DecompressFolder
  FindAllFiles(OutFileList, DecompressFolder,
    FileMaskFromStringList(Extensions), True);

  // If something is found then Exit
  if OutFileList.Count > 0 then
    Exit;

  //// 4. If nothing found, search ONLY ONE from every compressed archive.
  //// Folder/*.zip/aFileName.mext

  // COMENTED: Too sloooow..., keeped for reference

  //CompressedArchives := TStringList.Create;
  //try
  //  FindAllFiles(CompressedArchives, aFolder,
  //    FileMaskFromCommaText(w7zGetFileExts), True);

  //  i := 0;
  //  while i < CompressedArchives.Count do
  //  begin
  //    w7zExtractFile(CompressedArchives[i], aFileName + '.*',
  //      DecompressFolder + ExtractFileName(CompressedArchives[i]),
  //      False, '');
  //    Inc(i);
  //  end;
  //finally
  //  FreeAndNil(CompressedArchives);
  //end;

  //FindAllFiles(OutFileList, DecompressFolder,
  //  FileMaskFromStringList(Extensions), True);

  end
  else
  begin // Autodecompress = False
   // We don't want to auto decompress it only check if it exists.

    // TODO: It's a copy of EmuTKSearchFirstRelatedFile, must adapted to
    //   search all files found.

    // 3. Without extracting
    //CompressedArchives := TStringList.Create;
    //TempStrLst := TStringList.Create;
    //try
    //  EmuTKSearchAllFilesByNameExtCT(CompressedArchives, aFolder + aFileName,
    //    w7zGetFileExts);
    //
    //  i := 0;
    //  while (i < CompressedArchives.Count) and (Result = '') do
    //  begin
    //    TempStrLst.Clear;
    //    w7zListFiles(CompressedArchives[i], TempStrLst, True, '');
    //
    //    // Testing if a valid file is found
    //    Result := SearchComprFile(TempStrLst, '', Extensions);
    //    if Result <> '' then
    //      Result := SetAsFolder(CompressedArchives[i]) + Result;
    //
    //    Inc(i);
    //  end;
    //finally
    //  TempStrLst.Free;
    //  FreeAndNil(CompressedArchives);
    //end;
    //if Result <> '' then
    //  Exit;

    //// 4. Without extracting

    // COMENTED: Too sloooow..., keeped for reference

    //CompressedArchives := TStringList.Create;
    //TempStrLst := TStringList.Create;
    //try
    //  EmuTKSearchAllFilesByNameExtCT(CompressedArchives, aFolder,
    //    w7zGetFileExts);

    //  i := 0;
    //  while i < CompressedArchives.Count do
    //  begin
    //    TempStrLst.Clear;
    //    w7zListFiles(CompressedArchives[i], TempStrLst, True, '');

    //    Result := SearchComprFile(TempStrLst, aFileName, Extensions);
    //    if Result <> '' then
    //      Result := SetAsFolder(CompressedArchives[i]) + Result;

    //    Inc(i);
    //  end;
    //finally
    //  TempStrLst.Free;
    //  FreeAndNil(CompressedArchives);
    //end;
    //Result := SearchFirstFileInFolderByExtSL(DecompressFolder, Extensions);
  end;
end;

function EmuTKSearchFirstRelatedFile(aFolder: string;
  aFileName: string; Extensions: TStrings; SearchInComp: boolean;
  AutoDecompress: boolean; DecompressFolder: string): string;

  function SearchComprFile(aCompFileList: TStrings; aFileName: string;
    Extensions: TStrings): string;
  var
    i, j: integer;
    TempStr, aExt: string;
  begin
    Result := '';
    i := 0;
    while (i < aCompFileList.Count) and (Result = '') do
    begin
      TempStr := ExtractFileNameOnly(aCompFileList[i]);
      // aFileName = '' -> Any file with valid extension
      if (aFileName = '') or
        (CompareFilenamesIgnoreCase(TempStr, aFileName) = 0) then
      begin
        TempStr := ExtractFileExt(aCompFileList[i]);
        j := 0;
        while (j < Extensions.Count) and (Result = '') do
        begin
          aExt := Extensions[j];
          if (aExt <> '') and (aExt[1] <> ExtensionSeparator) then
            aExt := ExtensionSeparator + aExt;
          if UTF8CompareText(TempStr, aExt) = 0 then
            Result := aCompFileList[i];
          Inc(j);
        end;
      end;
      Inc(i);
    end;
  end;

var
  CompressedArchives: TStringList;
  TempStrLst: TStringList;
  i: integer;
begin
  Result := '';

  aFolder := SetAsFolder(aFolder);
  aFileName := RemoveFromBrackets(ExtractFileNameOnly(aFileName));

  if (aFileName = '') or (aFolder = '') or
    (not DirectoryExistsUTF8(aFolder)) or (not assigned(Extensions)) or
    (Extensions.Count = 0) then
    Exit;

  // 1. Basic search
  // Folder/aFileName.mext
  Result := EmuTKSearchFirstFileByNameExtSL(aFolder + aFileName, Extensions);
  if Result <> '' then
    Exit;

  // 2. Search in folder
  // Folder/aFileName/[*]/*.mext
  Result := SearchFirstFileInFolderByExtSL(aFolder + SetAsFolder(aFileName), Extensions);
  if Result <> '' then
    Exit;

  if not SearchInComp then
    Exit;

  if AutoDecompress then
  begin
    if DecompressFolder = '' then
      Exit;
    // 3. Search in zip files
    //   Folder/aFileName.zip/*.mext
    //   Extract to DecompressFolder/LastSubFolder(Folder)/aFileName/*.mext)
    DecompressFolder := SetAsFolder(DecompressFolder) +
      SetAsFolder(ExtractFileName(ExcludeTrailingPathDelimiter(aFolder))) +
      SetAsFolder(aFileName);

    // 3.a. If not DecompressFolder exists, then search Folder/aFileName.zip/*.mext
    //   and extract to DecompressFolder (WE EXTRACT ALL FILES)
    if not DirectoryExistsUTF8(DecompressFolder) then
    begin
      CompressedArchives := TStringList.Create;
      try
        EmuTKSearchAllFilesByNameExtCT(CompressedArchives, aFolder + aFileName,
          w7zGetFileExts);

        i := 0;
        while i < CompressedArchives.Count do
        begin
          w7zExtractFile(CompressedArchives[i], AllFilesMask, DecompressFolder,
            False, '');
          Inc(i);
        end;
      finally
        FreeAndNil(CompressedArchives);
      end;
    end;

    // 3.b. Actually searching the file in CacheFolder
    Result := SearchFirstFileInFolderByExtSL(DecompressFolder, Extensions);
    if Result <> '' then
      Exit;


    //// 4. If nothing found, search ONLY ONE from every compressed archive.
    //// Folder/*.zip/aFileName.mext

    // COMENTED: Too sloooow..., keeped for reference

    //CompressedArchives := TStringList.Create;
    //try
    //  FindAllFiles(CompressedArchives, aFolder,
    //    FileMaskFromCommaText(w7zGetFileExts), True);

    //  i := 0;
    //  while i < CompressedArchives.Count do
    //  begin
    //    w7zExtractFile(CompressedArchives[i], aFileName + '.*',
    //      DecompressFolder + ExtractFileName(CompressedArchives[i]),
    //      False, '');
    //    Inc(i);
    //  end;
    //finally
    //  FreeAndNil(CompressedArchives);
    //end;
    //Result := SearchFirstFileInFolderByExtSL(DecompressFolder, Extensions);
  end
  else
  begin // We don't want to auto decompress it only check if it exists.

    // 3. Without extracting
    CompressedArchives := TStringList.Create;
    TempStrLst := TStringList.Create;
    try
      EmuTKSearchAllFilesByNameExtCT(CompressedArchives, aFolder + aFileName,
        w7zGetFileExts);

      i := 0;
      while (i < CompressedArchives.Count) and (Result = '') do
      begin
        TempStrLst.Clear;
        w7zListFiles(CompressedArchives[i], TempStrLst, True, '');

        // Testing if a valid file is found
        Result := SearchComprFile(TempStrLst, '', Extensions);
        if Result <> '' then
          Result := SetAsFolder(CompressedArchives[i]) + Result;

        Inc(i);
      end;
    finally
      TempStrLst.Free;
      FreeAndNil(CompressedArchives);
    end;
    if Result <> '' then
      Exit;

    //// 4. Without extracting

    // COMENTED: Too sloooow..., keeped for reference

    //CompressedArchives := TStringList.Create;
    //TempStrLst := TStringList.Create;
    //try
    //  EmuTKSearchAllFilesByNameExtCT(CompressedArchives, aFolder,
    //    w7zGetFileExts);

    //  i := 0;
    //  while i < CompressedArchives.Count do
    //  begin
    //    TempStrLst.Clear;
    //    w7zListFiles(CompressedArchives[i], TempStrLst, True, '');

    //    Result := SearchComprFile(TempStrLst, aFileName, Extensions);
    //    if Result <> '' then
    //      Result := SetAsFolder(CompressedArchives[i]) + Result;

    //    Inc(i);
    //  end;
    //finally
    //  TempStrLst.Free;
    //  FreeAndNil(CompressedArchives);
    //end;
    //Result := SearchFirstFileInFolderByExtSL(DecompressFolder, Extensions);

  end;
end;

end.
