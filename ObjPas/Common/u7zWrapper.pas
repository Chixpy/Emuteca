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
  Classes, SysUtils, FileUtil, Process, StrUtils, LazUTF8, uCustomUtils;

const
  C7zFileNotExists = 257;
  C7zExeNotExists = 258;

var
  w7zFileExts: String;
  {< String with suported file extensions by 7z.

     Format: 'ext,ext,ext' for easy creating a TStringList. At least until
       we found a better way for searching files with different extension.

     Warning: It's not used for test if the files passed as params are
       compressed files. It's only a reference list.
  }
  w7zPathTo7zexe: String;
  {< Path to 7z.exe executable.

    It can be usefull for hidding the processes, but it's
      needed for listing archives anyways.
  }
  w7zPathTo7zGexe: String;
  {< Path to 7zG.exe executable.
  }

  w7zCacheDir: String;
  {< Directory were lists of files from compressed archives are stored.

    Defaults to '%USERTEMPDIR%/w7zCache', and the directory is deleted at
      program exit.
  }

function w7zListFiles(const aFilename: String;
  PackedFiles: TStrings; OnlyPaths: boolean = False;
  const Password: String = ''): Integer;
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
  @param(Password Is there archives that need a password to list files?
    Just in case.)
  @return(Error code)
}

function Extract7zFile(const a7zArchive: String;
  const aFileMask: String; aFolder: String;
  ShowProgress: Boolean; const Password: String): Integer;
{< Extract de file (or files) from 7z archive.

  @param(aFilename Name of the 7z archive.)
  @param(aFolder Folder where the file(s) will be extracted.)
  @param(aFileMask Mask or file to extract. Remember in 7z '*' means all files,
    not '*.*' wich means all files with extension.)
  @param(ShowProgress If true, progress of decompression will be shown.)
  @param(Password Password for 7z archive.)
}

function Compress7zFile(const a7zArchive: String; aFileList: TStrings;
  ShowProgress: Boolean; CompType: String = ''): Integer;
{< Compress files in a 7z (or other type) archive.

  @param(a7zArchive Name of the 7z/zip archive.)
  @param(aFileList List of files to add to the archive.)
  @param(ShowProgress If true, progress of decompression will be shown.)
  @param(CompType Type of the archive.)
}

implementation

function w7zListFiles(const aFilename: String;
  PackedFiles: TStrings; OnlyPaths: boolean = False;
  const Password: String = ''): Integer;
var
  msOut: TMemoryStream;
  aProcess: TProcess;
  slOut: TStringList;
  slLine: TStringList;
  aPos, i, n: integer;
  aParam, aValue: String;
  aPath, Size, PSize, aDate, aCRC: String;
begin
  Result := 0;
  if PackedFiles <> nil then
    PackedFiles.Clear
  else
    PackedFiles := TStringList.Create;

  if not FileExistsUTF8(w7zPathTo7zexe) then
  begin
    // TODO 2: Show message or raise an exception?.
    Result := C7zExeNotExists;
    Exit;
  end;

  if not FileExistsUTF8(aFilename) then
  begin
    // TODO 2: Show message or raise an exception?.
    Result := C7zFileNotExists;
    Exit;
  end;

  slOut := TStringList.Create;
  try
    msOut := TMemoryStream.Create;
    try
      // Executing '7z.exe l archive'
      aProcess := TProcess.Create(nil);
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

        aPos := 0;
        // Reading output
        while (aProcess.Running) or (aProcess.Output.NumBytesAvailable > 0) do
        begin
          i := aProcess.Output.NumBytesAvailable;
          if i > 0 then
          begin
            msOut.SetSize(aPos + i);
            n := aProcess.Output.Read((msOut.Memory + aPos)^, i);
            Inc(aPos, n);
          end;
          {
          else
            Sleep(100); // Waiting for more output
          }
        end;
        slOut.LoadFromStream(msOut);
        slOut.SaveToFile('7zOutput.txt');
      finally
        FreeAndNil(aProcess);
      end;
    finally
      FreeAndNil(msOut);
    end;

    // TODO 2: Check errors...

    // Procesing the lines...

    // Skipping until '----------'
    n := 0;
    while (n < slOut.Count) and (slOut[n] <> '----------') do
      Inc(n);

    // Now adding files
    aPath := '';
    Size := '';
    PSize := '';
    aDate := '';
    aCRC := '';
    while (n < slOut.Count) do
    begin
      // Well, I hope that always 'Path = ' is the first line
      //   of the file data, because a new line to the StringList is added.

      aPos := UTF8Pos('=', slOut[n]);
      if aPos <> 0 then
      begin
        aParam := UTF8LowerCase(Trim(UTF8Copy(slOut[n], 1, aPos-1)));
        aValue := Trim(UTF8Copy(slOut[n], aPos + 1, MaxInt));
        if UTF8CompareText(aParam, 'path') = 0 then
        begin
          // Adding the file
          if aPath <> '' then
          begin
            if OnlyPaths then
              PackedFiles.Add(aPath)
            else
            begin
              slLine := TStringList.Create;
              try
                slLine.Add(aPath);
                slLine.Add(Size);
                slLine.Add(PSize);
                slLine.Add(aDate);
                slLine.Add(aCRC);
                PackedFiles.Add(slLine.CommaText)
              finally
                FreeAndNil(slLine);
              end;
            end;
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
      Inc(n);
    end;
  finally
    FreeAndNil(slOut);
  end;

  // Adding the last packed file
  if aPath <> '' then
  begin
    if OnlyPaths then
      PackedFiles.Add(aPath)
    else
    begin
      slLine := TStringList.Create;
      try
        slLine.Add(aPath);
        slLine.Add(Size);
        slLine.Add(PSize);
        slLine.Add(aDate);
        slLine.Add(aCRC);
        PackedFiles.Add(slLine.CommaText)
      finally
        FreeAndNil(slLine);
      end;
    end;
  end;
end;

function Extract7zFile(const a7zArchive: String;
  const aFileMask: String; aFolder: String;
  ShowProgress: boolean; const Password: String): integer;
var
  aProcess: TProcess;
  aOptions: TProcessOptions;
  aExeString: String;
begin
  Result := 0;
  if not FileExistsUTF8(a7zArchive) then
  begin
    Result:=C7zFileNotExists;
    Exit;
  end;

  aOptions := [poWaitOnExit];
  aExeString := w7zPathTo7zexe;
  aFolder := SetAsFolder(aFolder);
  if (ShowProgress) and (FileExistsUTF8(w7zPathTo7zGexe)) then
    aExeString := w7zPathTo7zGexe
  else
  begin
    if not FileExistsUTF8(w7zPathTo7zexe) then
    begin
      Result := C7zExeNotExists;
      Exit;
    end;

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

function Compress7zFile(const a7zArchive: String; aFileList: TStrings;
  ShowProgress: Boolean; CompType: String): Integer;
var
  aProcess: TProcess;
  aOptions: TProcessOptions;
  aExeString: String;
  i: Integer;

begin
  aOptions := [poWaitOnExit];
  aExeString := w7zPathTo7zexe;
  if (ShowProgress) and (FileExistsUTF8(w7zPathTo7zGexe)) then
    aExeString := w7zPathTo7zGexe
  else
  begin
    if FileExistsUTF8(w7zPathTo7zexe) then
    begin
      Result := C7zExeNotExists;
      Exit;
    end;

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

  w7zCacheDir := SetAsFolder(GetTempDir(false)) + 'w7zCache';
  ForceDirectoriesUTF8(w7zCacheDir);

finalization

  // We want to delete this directory anyways on finalization.
  DeleteDirectory(SetAsFolder(GetTempDir(false)) + 'w7zCache', false);

  if DirectoryExistsUTF8(w7zCacheDir) = True then
    DeleteDirectory(w7zCacheDir);

end.

