{ 7z.exe and 7zG.exe Wrapper

  Copyright (C) 2011-2012 Chixpy

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
unit u7zWrapper;

{< Simple 7z.exe AND 7zG.exe wrapper until something better is found.

  @definitionList(
    @itemLabel(NOTE:)
    @item(7z.exe and 7zG.exe have their own licenses.)
  )

  @definitionList(
    @itemLabel(NOTE:)
    @item(Because PascalScript don't suport overloaded methods,
      we don't use them right here.)
  )
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Process, StrUtils, LazUTF8, sha1,
  uCustomUtils;

resourcestring
  w7zCacheFileExt = '.txt';
  w7zFileNotFound = '"%0:s" file not found';
  w7zExeError = '7z.exe/7zG.exe returned %0:d exit code.';

type
  w7zException = class(Exception);

var
  w7zFileExts: string;
  {< String with suported file extensions by 7z.

     Format: 'ext,ext,ext' for easy creating a TStringList. At least until
       we found a better way for searching files with different extension.

     Warning: It's not used for test if the files passed as params are
       compressed files. It's only a reference list.
  }
  w7zPathTo7zexe: string;
  {< Path to 7z.exe executable.

    It can be usefull for hidding the processes, but it's
      needed for listing archives anyways.
  }
  w7zPathTo7zGexe: string;
  {< Path to 7zG.exe executable.
  }

  w7zCacheDir: string;
  {< Directory were lists of files from compressed archives are stored.

    Defaults to '%USERTEMPDIR%/w7zCache', and the directory is deleted at
      program exit.
  }

procedure w7zListFiles(const aFilename: string; PackedFiles: TStrings;
  const OnlyPaths: boolean = False; const UseCache: boolean = True;
  const Password: string = '');
{< List files and properties in a 7z (or other format) archive.

  Executes "7z.exe l -slt aFilename" but don't use wildcards.

  7zG.exe can't list files so always 7z.exe is used, and console is hidden.

  I you want only the file names maybe you want use List7zFileNames

  @param(aFilename Name of the 7z archive.)
  @param(PackedFiles StringList where the files will be added. If the
    StringList = nil will be created @(and you must free it, of course@),
    otherwise it will be cleared. If OnlyPaths = @true, every string have the following format
    for easy TStringList.CommaText reading:
    "Dir/Filename","size","packed size","Date modified","CRC")
  @param(OnlyPaths List only file names and no properties)
  @param(UseCache Use cached file list?)
  @param(Password Is there archives that need a password to list files?
    Just in case.)
  @return(Error code)
}

function w7zExtractFile(const a7zArchive: string; const aFileMask: string;
  aFolder: string; const ShowProgress: boolean; const Password: string): integer;
{< Extract de file (or files) from 7z archive.

  @param(aFilename Name of the 7z archive.)
  @param(aFolder Folder where the file(s) will be extracted.)
  @param(aFileMask Mask or file to extract. Remember, for 7z.exe '*' means all
    files, not '*.*' wich means all files with extension.)
  @param(ShowProgress If true, progress of decompression will be shown.)
  @param(Password Password for 7z archive.)
}

function w7zCompressFile(const a7zArchive: string; aFileList: TStrings;
  const ShowProgress: boolean; const CompType: string = ''): integer;
{< Compress files in a 7z (or other type) archive.

  @param(a7zArchive Name of the 7z/zip archive.)
  @param(aFileList List of files to add to the archive.)
  @param(ShowProgress If true, progress of decompression will be shown.)
  @param(CompType Type of the archive.)
}

implementation

procedure w7zListFiles(const aFilename: string; PackedFiles: TStrings;
  const OnlyPaths: boolean = False; const UseCache: boolean = True;
  const Password: string = '');

  procedure ReturnOnlyPaths(aFileList: TStrings);
  var
    slLine: TStringList;
    i: integer;
  begin
    // Removing additional data
    // slLine is out of the iteration to avoid creating-deleting every time.
    slLine := TStringList.Create;
    try
      i := 0;
      while i < aFileList.Count do
      begin
        slLine.CommaText := aFileList[i];
        if slLine.Count > 0 then
        begin
          PackedFiles[i] := slLine[0];
          Inc(i);
        end
        else
        begin
          // Uhm... this must not happen... but...
          aFileList.Delete(i);
        end;
      end;
    finally
      FreeAndNil(slLine);
    end;
  end;

var
  FileSHA1: string;
  aPos, i: integer;
  slLine, slOutput: TStringList;
  msOutput: TMemoryStream;
  aProcess: TProcess;
  aParam, aValue: string;
  aPath, Size, PSize, aDate, aCRC: string;
begin
  // Just to be sure...
  w7zCacheDir := SetAsFolder(w7zCacheDir);

  // Clearing PackedFiles file list
  if PackedFiles <> nil then
    PackedFiles.Clear
  else
    PackedFiles := TStringList.Create;

  // Checking needed files
  if not FileExistsUTF8(w7zPathTo7zexe) then
    raise EInOutError.CreateFmt(w7zFileNotFound, [w7zPathTo7zexe]);

  if not FileExistsUTF8(aFilename) then
    raise EInOutError.CreateFmt(aFilename, [w7zPathTo7zexe]);

  // SHA1 of the file... cache file is saved allways
  FileSHA1 := SHA1Print(SHA1File(UTF8ToSys(aFilename)));

  // Searching for cache file
  // ------------------------
  if UseCache then
  begin
    if FileExistsUTF8(w7zCacheDir + FileSHA1 + w7zCacheFileExt) then
    begin
      PackedFiles.LoadFromFile(UTF8ToSys(w7zCacheDir + FileSHA1 + w7zCacheFileExt));
      if OnlyPaths then
        ReturnOnlyPaths(PackedFiles);
      Exit; // Job done.
    end;
  end;

  // Executing '7z.exe l -slt -scsUTF-8 -sccUTF-8 <archive>'
  // -------------------------------------------------------
  aProcess := TProcess.Create(nil);
  msOutput := TMemoryStream.Create;
  try
    aProcess.Executable := UTF8ToSys(w7zPathTo7zexe);
    aProcess.Parameters.Add('l');
    aProcess.Parameters.Add('-slt');
    aProcess.Parameters.Add('-scsUTF-8');
    aProcess.Parameters.Add('-sccUTF-8');
    if Password <> '' then
      aProcess.Parameters.Add('-p' + UTF8ToSys(Password));
    aProcess.Parameters.Add(UTF8ToSys(aFilename));
    aProcess.Options := aProcess.Options + [poUsePipes, poNoConsole];
    aProcess.Execute;

    // Reading output
    aPos := 0;
    while (aProcess.Running) or (aProcess.Output.NumBytesAvailable > 0) do
    begin
      i := aProcess.Output.NumBytesAvailable;
      if i > 0 then
      begin
        msOutput.SetSize(aPos + i);
        Inc(aPos, aProcess.Output.Read((msOutput.Memory + aPos)^, i));
      end;
          { Meh, don't sleep
          else
            Sleep(100); // Waiting for more output
          }
    end;
    msOutput.SaveToFile(UTF8ToSys(w7zCacheDir + 'w7zOutput' + w7zCacheFileExt));
  finally
    i := aProcess.ExitStatus;
    FreeAndNil(aProcess);
    FreeAndNil(msOutput);
  end;

  // TODO 3: Handle Warnings too...
  if (i <> 0) and (i <> 1) then // 1 = Warning
    raise w7zException.CreateFmt(w7zExeError, [i]);

  // Reading files and creating cache file
  // -------------------------------------

  slOutput := TStringList.Create;
  slLine := TStringList.Create;
  try
    slOutput.LoadFromFile(UTF8ToSys(w7zCacheDir + 'w7zOutput' + w7zCacheFileExt));

    // Skipping until '----------'
    i := 0;
    while (i < slOutput.Count) and (slOutput[i] <> '----------') do
      Inc(i);

    // Now adding files
    aPath := '';
    Size := '';
    PSize := '';
    aDate := '';
    aCRC := '';
    while (i < slOutput.Count) do
    begin
      aPos := UTF8Pos('=', slOutput[i]);

      if aPos <> 0 then
      begin
        aParam := UTF8LowerCase(Trim(UTF8Copy(slOutput[i], 1, aPos - 1)));
        aValue := Trim(UTF8Copy(slOutput[i], aPos + 1, MaxInt));

        // Well, I hope that always 'Path = ' will be the first line
        //   of the file data, because a new line to the StringList is added.
        if UTF8CompareText(aParam, 'path') = 0 then
        begin
          // Adding the file
          if aPath <> '' then
          begin
            slLine.Clear;
            slLine.Add(aPath);
            slLine.Add(Size);
            slLine.Add(PSize);
            slLine.Add(aDate);
            slLine.Add(aCRC);
            PackedFiles.Add(slLine.CommaText);
          end;

          aPath := aValue;
          Size := '';
          PSize := '';
          aDate := '';
          aCRC := '';
        end
        else if UTF8CompareText(aParam, 'size') = 0 then
          Size := aValue
        else if UTF8CompareText(aParam, 'packed size') = 0 then
          PSize := aValue
        else if UTF8CompareText(aParam, 'modified') = 0 then
          aDate := aValue
        else if UTF8CompareText(aParam, 'crc') = 0 then
          aCRC := aValue;
      end;
      Inc(i);
    end;

    // Adding the last packed file
    if aPath <> '' then
    begin
      slLine.Clear;
      slLine.Add(aPath);
      slLine.Add(Size);
      slLine.Add(PSize);
      slLine.Add(aDate);
      slLine.Add(aCRC);
      PackedFiles.Add(slLine.CommaText);

      // Only save if there is at least one file in the compressed archive
      PackedFiles.SaveToFile(UTF8ToSys(w7zCacheDir + FileSHA1 + w7zCacheFileExt));
    end;
  finally
    FreeAndNil(slLine);
    FreeAndNil(slOutput);
  end;

  if OnlyPaths then
    ReturnOnlyPaths(PackedFiles);

end;

function w7zExtractFile(const a7zArchive: string; const aFileMask: string;
  aFolder: string; const ShowProgress: boolean; const Password: string): integer;
var
  aProcess: TProcess;
  aOptions: TProcessOptions;
  aExeString: string;
begin
  Result := 0;
  if not FileExistsUTF8(a7zArchive) then
  raise EInOutError.CreateFmt(w7zFileNotFound, [w7zPathTo7zexe]);

  aOptions := [poWaitOnExit];
  aExeString := w7zPathTo7zexe;
  aFolder := SetAsFolder(aFolder);
  if (ShowProgress) and (FileExistsUTF8(w7zPathTo7zGexe)) then
    aExeString := w7zPathTo7zGexe
  else
  begin
    if not FileExistsUTF8(w7zPathTo7zexe) then
    raise EInOutError.CreateFmt(w7zFileNotFound, [w7zPathTo7zexe]);

    if ShowProgress then
      aOptions := aOptions + [poNewConsole]
    else
      aOptions := aOptions + [poNoConsole];

    // 7z.exe returns Fatal Error if not changed back to windows style :-(
    aFolder := AnsiReplaceStr(aFolder, '/', '\');
  end;

  aProcess := TProcess.Create(nil);
  try
    aProcess.Executable := UTF8ToSys(aExeString);
    aProcess.Options := aOptions;

    aProcess.Parameters.Add('x');
    aProcess.Parameters.Add(UTF8ToSys(a7zArchive));
    aProcess.Parameters.Add('-scsUTF-8');
    aProcess.Parameters.Add('-sccUTF-8');

    if not ShowProgress then
    begin
      // if progress is not shown then respond yes to all queries
      //   and overwrite if file exist... use it with care.
      aProcess.Parameters.Add('-aoa');
      aProcess.Parameters.Add('-y');
    end;
    if Password <> '' then
      aProcess.Parameters.Add('-p' + UTF8ToSys(Password));
    aProcess.Parameters.Add('-o' + UTF8ToSys(aFolder));
    aProcess.Parameters.Add('--');
    aProcess.Parameters.Add(UTF8ToSys(aFileMask));
    aProcess.Execute;
    Result := aProcess.ExitStatus;
  finally
    FreeAndNil(aProcess);
  end;
end;

function w7zCompressFile(const a7zArchive: string; aFileList: TStrings;
  const ShowProgress: boolean; const CompType: string): integer;
var
  aProcess: TProcess;
  aOptions: TProcessOptions;
  aExeString: string;
  i: integer;

begin
  aOptions := [poWaitOnExit];
  aExeString := w7zPathTo7zexe;
  if (ShowProgress) and (FileExistsUTF8(w7zPathTo7zGexe)) then
    aExeString := w7zPathTo7zGexe
  else
  begin
    if FileExistsUTF8(w7zPathTo7zexe) then
    raise EInOutError.CreateFmt(w7zFileNotFound, [w7zPathTo7zexe]);

    if ShowProgress then
      aOptions := aOptions + [poNewConsole]
    else
      aOptions := aOptions + [poNoConsole];
  end;

  aProcess := TProcess.Create(nil);
  try
    aProcess.Executable := UTF8ToSys(aExeString);
    aProcess.Options := aOptions;

    aProcess.Parameters.Add('a');
    if CompType <> '' then
      aProcess.Parameters.Add('-t' + UTF8ToSys(CompType));
    aProcess.Parameters.Add('-scsUTF-8');
    aProcess.Parameters.Add('-sccUTF-8');
    aProcess.Parameters.Add('-mx=9');

    if not ShowProgress then
    begin
      // if progress is not shown then respond yes to all queries
      //   and overwrite if file exist... use it with care.
      aProcess.Parameters.Add('-aoa');
      aProcess.Parameters.Add('-y');
    end;
    aProcess.Parameters.Add('--');

    aProcess.Parameters.Add(UTF8ToSys(a7zArchive));


    for i := 0 to aFileList.Count - 1 do
      aProcess.Parameters.Add(UTF8ToSys(aFileList[i]));

    aProcess.Execute;
    Result := aProcess.ExitStatus;
  finally
    FreeAndNil(aProcess);
  end;
end;

initialization
  // Meh, harcoding pseudo-constants

  w7zFileExts := '001,7z,arj,bpl,bzip2,cab,cb7,cbr,cbz,chi,chm,chq,chw,' +
    'cpio,cramfs,deb,dll,dmg,doc,exe,fat,flv,gz,gzip,hfs,hxi,hxq,hxr,hxs,' +
    'hxw,img,iso,jar,lha,lit,lzh,lzma,lzma86,mbr,msi,msp,nsis,ntfs,ppt,' +
    'r00,rar,rpm,scap,squashfs,swf,swm,sys,tar,taz,tbz,tbz2,tgz,tpz,vhd,' +
    'wim,xar,xls,xpi,xz,z,zip';

  // Little checks before default location...
  w7zPathTo7zexe := '7z.exe';
  if not FileExistsUTF8(w7zPathTo7zexe) then
    w7zPathTo7zexe := '7z/7z.exe';
  w7zPathTo7zGexe := '7zG.exe';
  if not FileExistsUTF8(w7zPathTo7zGexe) then
    w7zPathTo7zGexe := '7z/7z.exe';

  w7zCacheDir := SetAsFolder(GetTempDir(False)) + 'w7zCache';
  ForceDirectoriesUTF8(w7zCacheDir);

finalization

  // We want to delete this directory anyways on finalization.
  DeleteDirectory(SetAsFolder(GetTempDir(False)) + 'w7zCache', False);

  if DirectoryExistsUTF8(w7zCacheDir) = True then
    DeleteDirectory(w7zCacheDir, false);

end.










