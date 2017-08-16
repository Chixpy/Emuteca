unit uEmutecaCommon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazFileUtils, LazUTF8,
  u7zWrapper,
  uCHXStrUtils, uCHXFileUtils;

const
  rsFmtWindowCaption = '%0:s: %1:s';
  {<
    %0:s = Application.Title (derived from rsFmtApplicationTitle).
    %1:s = Window caption.
  }

  krsEmutecaTempGameSubFolder = 'Soft/';
  {< Subfolder in temp directory, where games will be decompressed.

    With directory separator.
  }


  // File extensions
  // ---------------
  krsEmutecaGroupFileExt = '.egl';
  {< Extension for group lists. }
  krsEmutecaSoftFileExt = '.csv';
  {< Extension for soft lists. }
  krsEmutecaINIFileExt = '.ini';
  {< Extension for ini databases (Systems, Emulators, Export/Import, ...). }
  krsEmutecaScriptFileExt = '.pas';
  {< Extension for script files. }
  krsEmutecaTXTFileExt = '.txt';
  {< Extension for generic text files. }

  // File masks for filters
  // ----------------------
  krsEmutecaGroupFileMask = '*' + krsEmutecaGroupFileExt;
  {< File mask for group lists. }
  krsEmutecaSoftFileMask = '*' + krsEmutecaSoftFileExt;
  {< File mask for soft lists. }
  krsEmutecaINIFileMask = '*' + krsEmutecaINIFileExt;
  {< File mask for ini databases (Systems, Emulators, Export/Import, ...). }
  krsEmutecaScriptFileMask = '*' + krsEmutecaScriptFileExt;
  {< File mask for script files. }
  krsEmutecaTXTFileMask = '*' + krsEmutecaTXTFileExt;
  {< File mask for generic text files. }



  // EXIT CODES for handling some errors
  // -----------------------------------
  // Praying for no emulator use these exit codes.
  kEmutecaExecErrorNoGame = -300;
  {< Error code when game is not found. }
  kEmutecaDecompressError = -301;
  {< Error decompressing archive. }

  // List headers
  // ------------
  krsCSVStatsHeader = '"Last Time","Times Played","Playing Time"';

  krsCSVSoftHeader = '"Group","SHA1","ID","Folder","FileName",' +
    '"Title","TransliteratedName","SortTitle","Version","Year","Publisher",' +
    '"Zone","DumpStatus","DumpInfo","Fixed","Trainer","Translation",' +
    '"Pirate","Cracked","Modified","Hack"';
  krsCSVSoftStatsHeader = krsCSVSoftHeader + ',' + krsCSVStatsHeader;

  // IniKeys
  // -------
  // Shared
  krsIniKeyID = 'ID';
  krsIniKeyTitle = 'Title';
  krsIniKeyFileName = 'FileName';
  krsIniKeyYear = 'Year';
  krsIniKeyEnabled = 'Enabled';

  // System
  krsIniKeyExtensions = 'Extensions';
  krsIniKeyBaseFolder = 'BaseFolder';
  krsIniKeyWorkingFolder = 'WorkingFolder';
  krsIniKeyMainEmulator = 'MainEmulator';
  krsIniKeyOtherEmulators = 'OtherEmulators';
  krsIniKeyIcon = 'Icon';
  krsIniKeyImage = 'Image';
  krsIniKeyBackImage = 'BackImage';
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
  // Constants for SoftExportKey
  krsCRC32 = 'CRC32';
  krsSHA1 = 'SHA1';
  krsFileName = 'FileName';
  krsCustom = 'Custom';

  // Group
  krsIniKeyDeveloper = 'Developer';

  // Soft
  krsIniKeySHA1 = 'SHA1';
  krsIniKeyGroup = 'Group';
  krsIniKeyTranslitTitl = 'TranslitTitle';
  krsIniKeySortTitle = 'SortTitle';
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
  // Constant for DumpStatus, fixed (for filenames)
  krsedsVerified = 'Verified';
  krsedsGood = 'GoodDump';
  krsedsAlternate = 'Alternate';
  krsedsOverDump = 'OverDump';
  krsedsBadDump = 'BadDump';
  krsedsUnderDump = 'UnderDump';

  // Playing Stats
  krsIniKeyPlayingTime = 'PlayingTime';
  krsIniKeyTimesPlayed = 'TimesPlayed';
  krsIniKeyLastTime = 'LastTime';

resourcestring
  rsNever = 'Never';

  rsLoadingSystemList = 'Loading system list...';
  rsImportingSystemList = 'Importing system list...';
  rsSavingSystemList = 'Saving system list...';
    rsLoadingSoftList = 'Loading soft list...';
  rsImportingSoftList = 'Importing soft list...';
  rsSavingSoftList = 'Saving soft list...';
    rsLoadingGroupList = 'Loading group list...';
  rsImportingGroupList = 'Importing group list...';
  rsSavingGroupList = 'Saving group list...';

  // File masks for filters
  // ----------------------
  rsEmutecaGroupFileMaskDesc =
    'Group file list (' + krsEmutecaGroupFileMask + ')';
  {< Description of file mask for group lists. }
  rsEmutecaSoftFileMaskDesc =
    'Soft file list (' + krsEmutecaSoftFileMask + ')';
  {< Description of file mask for soft lists. }
  rsEmutecaINIFileMaskDesc = 'Soft file list (' + krsEmutecaINIFileMask + ')';
  {< Description of file mask for ini databases (Systems, Emulators, Export/Import, ...). }
  rsEmutecaScriptFileMaskDesc =
    'Soft file list (' + krsEmutecaScriptFileMask + ')';
  {< Description of file mask for script files. }
  rsEmutecaTXTFileMaskDesc = 'Soft file list (' + krsEmutecaTXTFileMask + ')';
  {< Description of file mask for generic text files. }

  // Strings for DumpStatus, translatable
  // ------------------------------------
  rsedsVerified = 'Verified';
  rsedsGood = 'GoodDump';
  rsedsAlternate = 'Alternate';
  rsedsOverDump = 'OverDump';
  rsedsBadDump = 'BadDump';
  rsedsUnderDump = 'UnderDump';
  rsFileAlreadyAdded = 'This file is already added.';


type
  TEmutecaSoftExportKey = (TEFKSHA1, TEFKCRC32, TEFKFileName, TEFKCustom);
  TEmutecaDumpStatus = (edsVerified, edsGood, edsAlternate, edsOverDump,
    edsBadDump, edsUnderDump);

  TEmutecaProgressCallBack = function(const Title, Info1, Info2: string;
    const Value, MaxValue: int64): boolean of object;
{< Callback funtion to show progress }

const
  EmutecaSoftExportKeyStrK: array [TEmutecaSoftExportKey] of string =
    (krsSHA1, krsCRC32, krsFileName, krsCustom);
  //< Strings for FileKeys (fixed constants, used for ini files, etc. )

  EmutecaDumpStatusKey: array [TEmutecaDumpStatus] of string =
    ('!', '', 'a', 'o', 'b', 'u');
  //< Keys for DumpStatus, used in IniFiles
  EmutecaDumpStatusStr: array [TEmutecaDumpStatus] of string =
    (rsedsVerified, rsedsGood, rsedsAlternate, rsedsOverDump,
    rsedsBadDump, rsedsUnderDump);
  //< Strings for DumpStatus (localizable)
  EmutecaDumpStatusStrK: array [TEmutecaDumpStatus] of string =
    (krsedsVerified, krsedsGood, krsedsAlternate, krsedsOverDump,
    krsedsBadDump, krsedsUnderDump);
  //< Strings for DumpStatus (fixed constants, used for icon filenames, etc. )


function Str2EmutecaSoftExportKey(aString: string): TEmutecaSoftExportKey;
function EmutecaSoftExportKey2StrK(aSOK: TEmutecaSoftExportKey): string;
//< Same as Result := EmutecaSoftExportKeyStrK[aSOK];

function Key2EmutecaDumpSt(aString: string): TEmutecaDumpStatus;
function EmutecaDumpSt2Key(aEDS: TEmutecaDumpStatus): string;
//< Same as Result := EmutecaDumpStatusKey[aEDS];
function EmutecaDumpSt2Str(aEDS: TEmutecaDumpStatus): string;
//< Same as Result := EmutecaDumpStatusStr[aEDS];
function EmutecaDumpSt2StrK(aEDS: TEmutecaDumpStatus): string;
//< Same as Result := EmutecaDumpStatusStrK[aEDS];


procedure EmuTKSearchAllRelatedFiles(OutFileList: TStrings;
  aFolder: string; aFileName: string; Extensions: TStrings;
  SearchInComp: boolean; DecompressFolder: string);

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

function Str2EmutecaSoftExportKey(aString: string): TEmutecaSoftExportKey;
begin
  // In Emuteca <= 0.7, True => CRC32 / False => FileName
  aString := UTF8UpperCase(aString);

  // I don't like this "else if" format but it's clearer...
  if (aString = UTF8UpperCase(krsCRC32)) or
    (StrToBoolDef(aString, False)) then
    Result := TEFKCRC32
  else if (aString = UTF8UpperCase(krsFileName)) or
    (not StrToBoolDef(aString, True)) then
    Result := TEFKFileName
  else if (aString = UTF8UpperCase(krsSHA1)) then
    Result := TEFKSHA1
  else if (aString = UTF8UpperCase(krsCustom)) then
    Result := TEFKCustom
  else // Default
    Result := TEFKSHA1;
end;

function EmutecaSoftExportKey2StrK(aSOK: TEmutecaSoftExportKey): string;
begin
  Result := EmutecaSoftExportKeyStrK[aSOK];
end;

function Key2EmutecaDumpSt(aString: string): TEmutecaDumpStatus;
begin
  aString := UTF8Trim(UTF8LowerString(aString));

  if (aString = EmutecaDumpSt2Key(edsGood)) then // krsedsGoodKey = ''
    Result := edsGood
  else if (aString[1] = EmutecaDumpSt2Key(edsVerified)) then
    Result := edsVerified
  else if (aString[1] = EmutecaDumpSt2Key(edsAlternate)) then
    Result := edsAlternate
  else if (aString[1] = EmutecaDumpSt2Key(edsOverDump)) then
    Result := edsOverDump
  else if (aString[1] = EmutecaDumpSt2Key(edsBadDump)) then
    Result := edsBadDump
  else if (aString[1] = EmutecaDumpSt2Key(edsUnderDump)) then
    Result := edsUnderDump
  else
    Result := edsGood;
end;

function EmutecaDumpSt2Key(aEDS: TEmutecaDumpStatus): string;
begin
  Result := EmutecaDumpStatusKey[aEDS];
end;

function EmutecaDumpSt2Str(aEDS: TEmutecaDumpStatus): string;
begin
  Result := EmutecaDumpStatusStr[aEDS];
end;

function EmutecaDumpSt2StrK(aEDS: TEmutecaDumpStatus): string;
begin
   Result := EmutecaDumpStatusStrK[aEDS];
end;

procedure EmuTKSearchAllRelatedFiles(OutFileList: TStrings;
  aFolder: string; aFileName: string; Extensions: TStrings;
  SearchInComp: boolean; DecompressFolder: string);
var
  CompressedArchives: TStringList;
  i: integer;
begin
  aFolder := SetAsFolder(aFolder);
  DecompressFolder := SetAsFolder(DecompressFolder);
  aFileName := RemoveFromBrackets(ExtractFileNameOnly(aFileName));

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
  FindAllFiles(OutFileList, aFolder + aFileName,
    FileMaskFromStringList(Extensions), True);

  if not SearchInComp then
    Exit; // If we don't want to search in CompArchives

  if DecompressFolder = '' then
    Exit;
  // 3. Search in zip files
  //   Folder/aFileName.zip/*.mext
  //   Extract to DecompressFolder/LastSubFolder(Folder)/aFileName/*.mext)
  DecompressFolder := DecompressFolder +
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

  // If Found something then Exit
  if OutFileList.Count > 0 then
    Exit;

  // 4. If nothing found, search ONLY ONE from every compressed archive.
  // Folder/*.zip/aFileName.mext

  CompressedArchives := TStringList.Create;
  try
    FindAllFiles(CompressedArchives, aFolder,
      FileMaskFromCommaText(w7zGetFileExts), True);

    i := 0;
    while i < CompressedArchives.Count do
    begin
      w7zExtractFile(CompressedArchives[i], aFileName + '.*',
        DecompressFolder + ExtractFileName(CompressedArchives[i]),
        False, '');
      Inc(i);
    end;
  finally
    FreeAndNil(CompressedArchives);
  end;

  FindAllFiles(OutFileList, DecompressFolder,
    FileMaskFromStringList(Extensions), True);
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
  DecompressFolder := SetAsFolder(DecompressFolder);
  aFileName := RemoveFromBrackets(ExtractFileNameOnly(aFileName));

  if (aFileName = '') or (aFolder = '') or
    (not DirectoryExistsUTF8(aFolder)) or (Extensions = nil) or
    (Extensions.Count = 0) then
    Exit;

  // 1. Basic search
  // Folder/aFileName.mext
  Result := EmuTKSearchFirstFileByNameExtSL(aFolder + aFileName, Extensions);
  if Result <> '' then
    Exit;

  // 2. Search in folder
  // Folder/aFileName/[*]/*.mext
  Result := SearchFirstFileInFolderByExtSL(aFolder + aFileName, Extensions);
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
    DecompressFolder := DecompressFolder +
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


    // 4. If nothing found, search ONLY ONE from every compressed archive.
    // Folder/*.zip/aFileName.mext
    CompressedArchives := TStringList.Create;
    try
      FindAllFiles(CompressedArchives, aFolder,
        FileMaskFromCommaText(w7zGetFileExts), True);

      i := 0;
      while i < CompressedArchives.Count do
      begin
        w7zExtractFile(CompressedArchives[i], aFileName + '.*',
          DecompressFolder + ExtractFileName(CompressedArchives[i]),
          False, '');
        Inc(i);
      end;
    finally
      FreeAndNil(CompressedArchives);
    end;
    Result := SearchFirstFileInFolderByExtSL(DecompressFolder, Extensions);
  end
  else
  begin // We don't want to auto decompress it only if exists.

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
        w7zListFiles(CompressedArchives[i], TempStrLst, True, True, '');

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

    // 4. Without extracting
    CompressedArchives := TStringList.Create;
    TempStrLst := TStringList.Create;
    try
      EmuTKSearchAllFilesByNameExtCT(CompressedArchives, aFolder,
        w7zGetFileExts);

      i := 0;
      while i < CompressedArchives.Count do
      begin
        TempStrLst.Clear;
        w7zListFiles(CompressedArchives[i], TempStrLst, True, True, '');

        Result := SearchComprFile(TempStrLst, aFileName, Extensions);
        if Result <> '' then
          Result := SetAsFolder(CompressedArchives[i]) + Result;

        Inc(i);
      end;
    finally
      TempStrLst.Free;
      FreeAndNil(CompressedArchives);
    end;
    Result := SearchFirstFileInFolderByExtSL(DecompressFolder, Extensions);

  end;
end;

end.
