unit uEmutecaCommon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazFileUtils, LazUTF8, sha1,
  u7zWrapper,
  uCHXStrUtils, uCHXFileUtils;

const
  rsFmtWindowCaption = '%0:s : %1:s';
{<
  %0:s = Application.Title (derived from rsFmtApplicationTitle).
  %1:s = Window caption.
}

  krsEmutecaTempGameSubFolder = 'Game/';
  {< Subfolder in temp directory, where games will be decompressed.

    Please attach directory separator}

  // Extensions
  // ----------
  krsEmutecaGroupFileExt = '.egl';
  {< Extension for group lists. }
  krsEmutecaSoftFileExt = '.csv';
  {< Extension for soft lists. }

  // EXIT CODES for handling some errors
  // Praying for no emulator use these exit codes.
  kEmutecaExecErrorNoGame = -300;
  {< Error code when game is not found. }
  kEmutecaDecompressError = -301;
  {< Error decompressing archive. }

  kEmuTKSHA1Empty: TSHA1Digest =
    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);

  // IniKeys
  // -------
  // Shared
  krsIniKeyID = 'ID';
  krsIniKeyTitle = 'Title';
  krsIniKeyFileName = 'FileName';
  krsIniKeyYear = 'Year';

  // System
  krsIniKeyEnabled = 'Enabled';
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
  krsIniKeyGamesKey = 'GamesKey';
  krsIniKeyExtractAll = 'ExtractAll';

  // Group
  krsIniKeyDeveloper = 'Developer';

  // Soft
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

  // Playing Stats
  krsIniKeyPlayingTime = 'PlayingTime';
  krsIniKeyTimesPlayed = 'TimesPlayed';
  krsIniKeyLastTime = 'LastTime';

resourcestring
  rsNever = 'Never';

type
  TEmutecaProgressCallBack = function(const Title, Info1, Info2: string;
    const Value, MaxValue: int64): boolean of object;
{< Callback funtion to show progress }


procedure EmuTKSearchAllRelatedFiles(OutFileList: TStrings;
  aFolder: string; aFileName: string; Extensions: TStrings;
  SearchInComp: boolean; DecompressFolder: string);

function EmuTKSearchFirstRelatedFile(aFolder: string; aFileName: string;
  Extensions: TStrings; SearchInComp: boolean; AutoDecompress: boolean;
  DecompressFolder: string): string;

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
  aFile: String;
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

procedure EmuTKSearchAllRelatedFiles(OutFileList: TStrings; aFolder: string;
  aFileName: string; Extensions: TStrings; SearchInComp: boolean;
  DecompressFolder: string);
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
  FindAllFiles(CompressedArchives, aFolder, FileMaskFromCommaText(w7zGetFileExts), True);

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

function EmuTKSearchFirstRelatedFile(aFolder: string; aFileName: string;
  Extensions: TStrings; SearchInComp: boolean; AutoDecompress: boolean;
  DecompressFolder: string): string;

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
    FindAllFiles(CompressedArchives, aFolder, FileMaskFromCommaText(w7zGetFileExts), True);

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
      EmuTKSearchAllFilesByNameExtCT(CompressedArchives, aFolder, w7zGetFileExts);

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
