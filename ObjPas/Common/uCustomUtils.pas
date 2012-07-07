{ This file is part of Emuteca

  Copyright (C) 2006-2012 Chixpy

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

{ Unit with miscelaneous procedures and functions used in Emuteca }
unit uCustomUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Menus, Controls, FileUtil, IniFiles, Graphics,
  ActnList, Forms, dateutils, strutils, LConvEncoding,
  crc, LazUTF8;

resourcestring
  rsCUExcCardRange = '"%0:d" is not in cardinal range.';

const
  kCUHTMLBegin = '<html><body>';
  //< Simple HTML file struct begin
  kCUHTMLEnd = '</body></html>';
  //< Simple HTML file struct end
  kCUVirtualFolderExt = '.(folder)';
  //< Virtual extension used for folders y some contexts
  kCUVirtualGroupExt = '.(group)';
  //< Virtual extension used for groups filenames
  kCUVirtualGameExt = '.(game)';
  //< Virtual extension used for game filenames

  // WordDelimiters except utf8 bit mask (Dirty way ^_^)
  kCUUTF8Delimiters: set of char =
    [#0..#127] - ['a'..'z', 'A'..'Z', '1'..'9', '0'];

type
  TItFolderObj = function(Folder: String;
    FileInfo: TSearchRec): boolean of object;
  TItFolderFun = function(Folder: String; FileInfo: TSearchRec): boolean;

function CRC32File(const aFileName: String): cardinal;
{< Calculates the CRC32 checksum of a file.
}

// ---------------------
// IMAGES AND IMAGELISTS
// ---------------------

procedure ReadActionsIcons(const aFileName, Section, BaseDir: String;
  ImageList: TImageList; ActionList: TCustomActionList);

procedure ReadMenuIcons(const aFileName, Section, BaseDir: String;
  ImageList: TImageList; Menu: TMenu);

function AddToImageList(aImageList: TImageList;
  const FileName: String): integer;

function CorrectAspetRatio(OrigRect: TRect; aImage: TPicture): TRect;
{< Returns a TRect with the correct aspect ratio for the picture inside the
  OrigRect.
}


// -----------------
// FOLDERS AND FILES
// -----------------

function IterateFolderObj(Folder: String; aFunction: TItFolderObj;
  Recursive: boolean = True): boolean;
function IterateFolderFun(Folder: String; aFunction: TItFolderFun;
  Recursive: boolean = True): boolean;
{< Recorre el directorio especificado y ejecuta aFuncion(TSearchRec) con cada uno
  de los archivos encontrados

Parámetros:
  - Directorio: string -> Directorio a recorrer
  - Funcion: function(Directorio: string; Info: TSearchRec): Boolean -> Metodo
    de un objeto al que se le pasa el TSearchRec y el directorio del archivo
    actual como parámetro para operar con él. Si devuelve True continua con el
    siguiente fichero.
  - Recursivo: Boolean -> Indica si también se deben recorrer los subdirectorios
  - Result: Boolean -> Indica si se ha continuar la operación

Notas:
  - Si Funcion devuelve True continúa recorriendo el directorio, si devuelve
    False no pasa al siguiente archivo.
}

function FilesInFolder(Folder: String): integer;
//< TODO 2: Is there a better way?


//  --------------------------------
//  STRINGS AND STRINGLISTS HANDLING
//  --------------------------------

function AddToStringList(aList: TStrings; aString: String): integer;
{< Add a String to a StringList.

  Don't add repeated strings.
}

function CleanFileName(const AFileName: String): String;
{< Change some invalid characters in filenames.
}

function SetAsFolder(const aValue: String): String;
{< Add PathDelim at the end of string and changes it to '/'

  IncludeTrailingPathDelimiter includes the PathDelim even if aValue = ''.
    This function non't add it in this case, so when testing if it's empty
    we don't need @code (@(aFolder=''@) or @(aFolder=PathDelim@))

  In the other hand, paths are converted to Linux one as Windows AND
    MS-DOS (+2.0) recognise without problem
}

function SetAsFile(const aFileName: string): string;

function SysPath(aPath: string): string;
function WinPath(aPath: string): string;
function UnixPath(aPath: string): string;

function TextSimilarity(const aString1, aString2: String): byte;
{< Returns the similarity between 2 strings.

  Based in http://www.catalysoft.com/articles/StrikeAMatch.html method and
  tweaked a little.
}

function RemoveFromBrackets(const aFileName: String): String;

function StrToCardinal(const aString: String): cardinal;

function StrToCardinalDef(const aString: String;
  const Default: cardinal): cardinal;

function SecondToFmtStr(aValue: int64): String;

implementation

function CleanFileName(const AFileName: String): String;
begin
  // Windows (and Linux) invalid characters
  Result := AnsiReplaceText(AFileName, '?', '_');
  Result := AnsiReplaceText(Result, '*', '-');
  Result := AnsiReplaceText(Result, '"', '_');
  Result := AnsiReplaceText(Result, '\', '-');
  Result := AnsiReplaceText(Result, '/', '-');
  Result := AnsiReplaceText(Result, '|', '-');
  Result := AnsiReplaceText(Result, '<', '-');
  Result := AnsiReplaceText(Result, '>', '-');

  // Playing with ":"
  Result := AnsiReplaceText(Result, ' : ', ' - ');
  Result := AnsiReplaceText(Result, ': ', ' - ');
  Result := AnsiReplaceText(Result, ' :', ' - ');
  Result := AnsiReplaceText(Result, ':', ' - ');

  Result := Trim(Result);
end;

function CRC32File(const aFileName: String): cardinal;
var
  aFile: TFileStream;
  BufferCRC: array[0..32767] of char;
  BufferSize: cardinal;
begin
  BufferCRC[0] := #32; // Fix inicialization warning
  BufferSize := SizeOf(BufferCRC);
  Result := crc32(0, nil, 0);

  if not FileExistsUTF8(aFileName) then
    Exit;

  aFile := TFileStream.Create(UTF8ToSys(aFileName), fmOpenRead);
  try
    aFile.Position := 0;

    while (aFile.Position < aFile.Size) do
    begin
      if (aFile.Size - aFile.Position) < BufferSize then
        BufferSize := aFile.Size - aFile.Position;
      aFile.ReadBuffer(BufferCRC, BufferSize);
      Result := crc32(Result, @BufferCRC, BufferSize);
    end;
  finally
    FreeAndNil(aFile);
  end;
end;

procedure ReadActionsIcons(const aFileName, Section, BaseDir: String;
  ImageList: TImageList; ActionList: TCustomActionList);
var
  IniFile: TMemIniFile;
  Cont: integer;
  IconFile: String;
begin
  IniFile := TMemIniFile.Create(UTF8ToSys(aFileName));
  try
    Cont := 0;
    while Cont < ActionList.ActionCount do
    begin
      IconFile := IniFile.ReadString(Section, ActionList.Actions[Cont].Name, '');
      if IconFile = '' then
      begin
        IconFile := ActionList.Actions[Cont].Name + '.png';
        IniFile.WriteString(Section, ActionList.Actions[Cont].Name, IconFile);
        IniFile.UpdateFile;
      end;
      TCustomAction(ActionList.Actions[Cont]).ImageIndex :=
        AddToImageList(ImageList, SetAsFolder(BaseDir) + IconFile);
      Inc(Cont);
    end;
  finally
    FreeAndNil(IniFile);
  end;
end;

procedure ReadMenuIcons(const aFileName, Section, BaseDir: String;
  ImageList: TImageList; Menu: TMenu);

  procedure ReadIcon(IniFile: TMemIniFile; ImageList: TImageList;
    Menu: TMenuItem; Section: String; BaseDir: String);
  var
    IconFile: string;
    Cont: integer;
  begin
    BaseDir := SetAsFolder(BaseDir);

    if not (Menu.IsLine or Assigned(Menu.Action)) then
    begin
      IconFile := IniFile.ReadString(Section, Menu.Name, '');
      if IconFile = '' then
      begin
        IconFile := Menu.Name + '.png';
        IniFile.WriteString(Section, Menu.Name, IconFile);
        IniFile.UpdateFile;
      end;
      Menu.ImageIndex := AddToImageList(ImageList, BaseDir + IconFile);
    end;

    Cont := 0;
    while Cont < Menu.Count do
    begin
      ReadIcon(IniFile, ImageList, Menu.Items[Cont], Section, BaseDir);
      Inc(Cont);
    end;
  end;

  //procedure ReadMenuIcons(const aFileName, Section, BaseDir: String;
  //  ImageList: TImageList; Menu: TMenu);
var
  IniFile: TMemIniFile;
  Cont: integer;
begin
  IniFile := TMemIniFile.Create(UTF8ToSys(aFileName));
  try
    Cont := 0;
    while Cont < Menu.Items.Count do
    begin
      ReadIcon(IniFile, ImageList, Menu.Items[Cont], Section,
        SetAsFolder(BaseDir));
      Inc(Cont);
    end;
  finally
    FreeAndNil(IniFile);
  end;
end;

function AddToImageList(aImageList: TImageList;
  const FileName: String): integer;
var
  Image: TPicture;
  Extension: String;
begin
  Result := -1;
  if aImageList = nil then
    Exit;
  if FileExistsUTF8(FileName) then
  begin
    Image := TPicture.Create;
    try
      Image.LoadFromFile(FileName);
      // Cutrada para que los iconos se dibujen transparentes...
      Extension := ExtractFileExt(FileName);
      if (Extension = '.ico') or (Extension = '.icns') or
        (Extension = '.cur') then
        Result := aImageList.AddMasked(Image.Bitmap,
          Image.Icon.TransparentColor)
      else
        Result := aImageList.Add(Image.PNG, nil);
    finally
      FreeAndNil(Image);
    end;
  end;
end;

function CorrectAspetRatio(OrigRect: TRect; aImage: TPicture): TRect;
var
  Adjustment: integer;
begin
  Result := OrigRect;
  if aImage.Width > aImage.Height then
  begin
    // Crazy formula, don't ask
    Adjustment := Round(((OrigRect.Right - OrigRect.Left) *
      (1 - (aImage.Height / aImage.Width))) / 2);
    Result.Top := OrigRect.Top + Adjustment;
    Result.Bottom := OrigRect.Bottom - Adjustment;
  end
  else
  begin
    Adjustment := Round(((OrigRect.Bottom - OrigRect.Top) *
      (1 - (aImage.Width / aImage.Height))) / 2);
    Result.Left := OrigRect.Left + Adjustment;
    Result.Right := OrigRect.Right - Adjustment;
  end;
end;

function AddToStringList(aList: TStrings; aString: String): integer;
begin
  Result := -1;
  if (aList = nil) or (aString = '') then
    Exit;
  Result := aList.IndexOf(aString);
  if Result = -1 then
    Result := aList.Add(aString);
end;

function IterateFolderObj(Folder: String; aFunction: TItFolderObj;
  Recursive: boolean = True): boolean;
var
  Info: TSearchRec;
begin
  Result := True;
  Folder := SetAsFolder(Folder);
  if (Folder = '') or (not DirectoryExistsUTF8(Folder)) then
    Exit;

  if FindFirstUTF8(Folder + AllFilesMask, faAnyFile, Info) = 0 then
    try
      repeat
        Result := aFunction(Folder, Info);
      until (FindNextUTF8(Info) <> 0) or not Result;
    finally
      FindCloseUTF8(Info);
    end;

  if Recursive and Result then
    if FindFirstUTF8(Folder + AllFilesMask, faDirectory, Info) = 0 then
      try
        repeat
          if (Info.Name <> '.') and (Info.Name <> '') and
            (Info.Name <> '..') and
            ((Info.Attr and faDirectory) <> 0) then
            Result := IterateFolderObj(Folder + Info.Name, aFunction, True);
        until (FindNextUTF8(Info) <> 0) or not Result;
      finally
        FindCloseUTF8(Info);
      end;
end;

function IterateFolderFun(Folder: String; aFunction: TItFolderFun;
  Recursive: boolean): boolean;
var
  Info: TSearchRec;
begin
  Result := True;
  Folder := SetAsFolder(Folder);
  if (Folder = '') or (not DirectoryExistsUTF8(Folder)) then
    Exit;

  if FindFirstUTF8(Folder + AllFilesMask, faAnyFile, Info) = 0 then
    try
      repeat
        Result := aFunction(Folder, Info);
      until (FindNextUTF8(Info) <> 0) or not Result;
    finally
      FindCloseUTF8(Info);
    end;

  if Recursive and Result then
    if FindFirstUTF8(Folder + AllFilesMask, faDirectory, Info) = 0 then
      try
        repeat
          if (Info.Name <> '.') and (Info.Name <> '') and
            (Info.Name <> '..') and
            ((Info.Attr and faDirectory) <> 0) then
            Result := IterateFolderFun(Folder + Info.Name, aFunction, True);
        until (FindNextUTF8(Info) <> 0) or not Result;
      finally
        FindCloseUTF8(Info);
      end;
end;

function FilesInFolder(Folder: String): integer;
var
  Info: TSearchRec;
begin
  // Podría usar IterateFolderObj pero no es plan de complicar la cosa
  Result := 0;
  Folder := SetAsFolder(Folder);

  if FindFirstUTF8(Folder + '*', 0, Info) = 0 then
    try
      repeat
        Inc(Result);
      until (FindNextUTF8(Info) <> 0);
    finally
      FindCloseUTF8(Info);
    end;
end;

function SysPath(aPath: string): string;
begin
  {$IFDEF Windows}
  Result := WinPath(aPath);
  {$ELSE}
  Result := UnixPath(aPath);
  {$ENDIF}
end;

function WinPath(aPath: string): string;
begin
  Result := StringReplace(aPath, '/', '\', [rfReplaceAll, rfIgnoreCase]);
end;

function UnixPath(aPath: string): string;
begin
  Result := StringReplace(aPath, '\', '/', [rfReplaceAll, rfIgnoreCase]);
end;

function TextSimilarity(const aString1, aString2: String): byte;

  procedure LetterPairs(aStrList: TStrings; const aString: String);
  var
    i: integer;
    CurrPair: String;
    CharUTF8: String;
  begin
    if aStrList = nil then
      aStrList := TStringList.Create
    else
      aStrList.Clear;

    i := 1;
    while i < UTF8Length(aString) do
    begin
      CurrPair := UTF8Copy(aString, i, 2);

      if UTF8Length(CurrPair) <> 2 then
      begin
        Inc(i);
        Continue;
      end;

      // Removing some separators...
      if CurrPair[1] in kCUUTF8Delimiters then
      begin
        Inc(i);
        Continue;
      end;

      CharUTF8 := UTF8Copy(CurrPair, 2, 1);
      if CharUTF8[1] in kCUUTF8Delimiters then
      begin
        CurrPair := UTF8Copy(CurrPair, 1, 1);
      end;

      aStrList.Add(CurrPair);
      Inc(i, Length(CurrPair));
    end;
  end;

var
  StrList1, StrList2: TStringList;
  CurrPair: String;
  i, j: integer;
  Intersection: integer;
  Union: integer;
begin
  Result := 0;
  if (aString1 = '') or (aString2 = '') then
    Exit;

  StrList1 := TStringList.Create;
  StrList2 := TStringList.Create;
  StrList1.CaseSensitive := False;
  StrList2.CaseSensitive := False;
  try
    LetterPairs(StrList1, UTF8UpperCase(aString1));
    StrList1.Sort;
    LetterPairs(StrList2, UTF8UpperCase(aString2));
    StrList2.Sort;

    Intersection := 0;
    Union := StrList1.Count + StrList2.Count;

    i := StrList1.Count - 1;
    while i >= 0 do
    begin
      CurrPair := StrList1[i];
      j := StrList2.IndexOf(CurrPair);
      if j <> -1 then
      begin
        StrList2.Delete(j);
        Inc(Intersection, 2);
      end;
      Dec(i);
    end;
  finally
    FreeAndNil(StrList1);
    FreeAndNil(StrList2);
  end;

  if Union <> 0 then
    Result := Round(Intersection / Union * 100);
end;

function RemoveFromBrackets(const aFileName: String): String;
var
  Position: integer;
begin
  Result := ExtractFileNameOnly(aFileName);
  Position := UTF8Pos('(', Result);
  if Position <> 0 then
    Result := UTF8Copy(Result, 1, Position - 1);
  Position := UTF8Pos('[', Result);
  if Position <> 0 then
    Result := UTF8Copy(Result, 1, Position - 1);

  Result := Trim(Result);
end;

function StrToCardinalDef(const aString: String;
  const Default: cardinal): cardinal;
var
  h: int64;
begin
  h := StrToInt64Def(aString, Default);
  if (h > High(cardinal)) or (h < 0) then
    h := Default;
  Result := h;
end;

function SetAsFolder(const aValue: String): String;
begin
  Result := SysPath(aValue);

  // Always relative...
  if FilenameIsAbsolute(Result) then
    Result := CreateRelativePath(Result, GetCurrentDirUTF8, false);

  { Always with TrailingPathDelimiter, but only if it's not empty or root }
  if ExcludeTrailingPathDelimiter(Result) <> '' then
    Result := IncludeTrailingPathDelimiter(Result);

  // I like UNIX PathSep (and it's better for cross-configuring)
  Result := UnixPath(Result);
end;

function SetAsFile(const aFileName: string): string;
begin
  // CreateRelativePath doesn't like Unix Style under Windows... :-(
  Result := CreateRelativePath(SysPath(aFileName), GetCurrentDirUTF8, false);

  Result := UnixPath(Result);
end;

function SecondToFmtStr(aValue: int64): String;
begin
  Result := RightStr('00' + IntToStr(aValue mod 60), 2);
  aValue := aValue div 60;
  Result := RightStr('00' + IntToStr(aValue mod 60), 2) + ':' + Result;
  aValue := aValue div 60;
  Result := IntToStr(aValue) + ':' + Result;
end;

function StrToCardinal(const aString: String): cardinal;
var
  h: int64;
begin

  h := StrToInt64(aString);
  if (h > High(cardinal)) or (h < 0) then
    raise EConvertError.CreateFmt(rsCUExcCardRange, [h]);
  Result := h;
end;

end.

