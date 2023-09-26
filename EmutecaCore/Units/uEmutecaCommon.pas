unit uEmutecaCommon;
{< Commons methods unit of Emuteca Core.

  This file is part of Emuteca Core.

  Copyright (C) 2011-2018 Chixpy
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazFileUtils, LazUTF8,
  // CHX units
  uCHX7zWrapper, uCHXStrUtils, uCHXFileUtils,
  // Emuteca Core units
  uEmutecaConst, uEmutecaRscStr;

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
  else if (aString[1] = DumpSt2Key(edsFavorite)) then
    Result := edsFavorite
  else if (aString[1] = 'a' {DumpSt2Key(edsAlternate)}) then
    Result := edsGood
  else if (aString[1] = DumpSt2Key(edsOverDump)) then
    Result := edsOverDump
  else if (aString[1] = DumpSt2Key(edsBadDump)) then
    Result := edsBadDump
  else if (aString[1] = DumpSt2Key(edsUnderDump)) then
    Result := edsUnderDump
  // Unknown is default and cost the same comparisons
  //else if (aString[1] = DumpSt2Key(edsUnknown)) then
  //  Result := edsUnknown
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

    // REMOVED: Too sloooow..., keeped for reference

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

    //  i := 0;
    //  while (i < CompressedArchives.Count) and (Result = '') do
    //  begin
    //    TempStrLst.Clear;
    //    w7zListFiles(CompressedArchives[i], TempStrLst, True, '');

    //    // Testing if a valid file is found
    //    Result := SearchComprFile(TempStrLst, '', Extensions);
    //    if Result <> '' then
    //      Result := SetAsFolder(CompressedArchives[i]) + Result;

    //    Inc(i);
    //  end;
    //finally
    //  TempStrLst.Free;
    //  FreeAndNil(CompressedArchives);
    //end;
    //if Result <> '' then
    //  Exit;

    //// 4. Without extracting

    // REMOVED: Too sloooow..., keeped for reference

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
  TempStr: string;
  i: integer;
begin
  Result := '';

  aFolder := SetAsFolder(aFolder);

  SimpleStringSplit(ExtractFileNameOnly(aFileName), ' (', aFileName, TempStr);

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
  Result := SearchFirstFileInFolderByExtSL(aFolder +
    SetAsFolder(aFileName), Extensions);
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

    // REMOVED: Too sloooow..., keeped for reference

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

    // REMOVED: Too sloooow..., keeped for reference

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
